use std::{arch::x86_64::*, cell::OnceCell};

use crossbeam::channel;

use crate::{
    chess_v2::*,
    engine::{
        chess_v2,
        search::{
            AbortSignal, SearchStrategy, eval::*, moves::*, repetition::RepetitionTable,
            search_params::SearchParams, see, transposition::*,
        },
        sorting,
        tables::{self},
    },
    nnue::nnue::{self, UpdatableNnue},
    nnue_load, util,
};

macro_rules! net_path {
    () => {
        "../../../nnue/w2-10M-512-b8.bin"
    };
}

const EVAL_NNUE: bool = true;
const EVAL_CACHE: bool = false;
const NNUE_HSIZE: usize = 512;
const NNUE_OSIZE: usize = 8;

const CORR_WEIGHT_SCALE: Eval = 128;
const CORR_MAX_ERROR: Eval = 1024;
const CORR_GRAIN: Eval = 8;

const PV_DEPTH: usize = 64;

const HISTORY_MAX: i16 = i16::MAX - 0_017;
const HISTORY_MIN: i16 = i16::MIN + 0_017;

const SEE_CAPTURE_PRUNE_MAX_DEPTH: u8 = 5;
const SEE_CAPTURE_MARGIN: Eval = 100;
const SEE_QUIET_PRUNE_MAX_DEPTH: u8 = 8;
const SEE_QUIET_MARGIN: Eval = 12;
const SEE_HISTORY_DIVISOR: Eval = 128;
const SEE_QS_THRESHOLD: Eval = -20;

pub struct SeeInfo {
    pub black_board: u64,
    pub white_board: u64,
    pub pieces_board: [u64; 8],
    pub pins: [see::Pinning; 2],
}

#[derive(Debug)]
pub struct PvTable {
    pub moves: [[u16; PV_DEPTH]; PV_DEPTH],
    pub lengths: [u8; PV_DEPTH],
}

impl PvTable {
    pub fn new() -> Self {
        PvTable {
            moves: [[0; PV_DEPTH]; PV_DEPTH],
            lengths: [0; PV_DEPTH],
        }
    }
}

// pub struct CorrStats {
//     pub original_eval: Eval,
//     pub error: Eval,
// }

pub struct Search<'a> {
    chess: ChessGame,
    tables: &'a tables::Tables,
    sig: Option<AbortSignal>,

    nnue: Box<nnue::LazyNnue<NNUE_HSIZE, NNUE_OSIZE>>,

    ply: u8,
    is_stopping: bool,

    pv_table: Box<PvTable>,
    pv: [u16; PV_DEPTH],
    pv_length: usize,
    pv_trace: bool,

    tt: TranspositionTable,
    rt: RepetitionTable,
    et: EvalTable,
    history_moves: Box<[[i16; 64]; 16]>,
    cut_moves: [[u16; 2]; PV_DEPTH],

    params: Box<SearchParams>,

    score: Eval,
    node_count: u64,
    quiet_nodes: u64,
    quiet_depth: u32,
    depth: u8,

    eval_stack: [Eval; PV_DEPTH],
    lmr_table: [[u8; 64]; 64],
    move_stack: [(u8, u8); PV_DEPTH],
    excluded_move: u16,
    cont_history: Box<[[[i16; 768]; 768]; 2]>,

    pawn_correction_heuristic: [[Eval; u16::MAX as usize + 1]; 2],
    // pub corr_stats: Vec<CorrStats>,
}

impl<'a> SearchStrategy<'a> for Search<'a> {
    fn search(&mut self) -> u16 {
        const INITIAL_BOUND_MARGIN: Eval = 17;

        let mut node_count = 0;
        let mut quiet_nodes = 0;

        let max_depth = PV_DEPTH as u8 - 1;
        let target_depth = self.params.depth.unwrap_or(max_depth).min(max_depth);

        if let Some(mv) = self.has_single_legal_move() {
            let static_eval = self.evaluate();

            self.pv_table.lengths[0] = 1;
            self.pv_table.moves[0][0] = mv;

            self.apply_pv(0, static_eval);
            return mv;
        }

        'outer: for depth in 1..=target_depth {
            let mut margin = INITIAL_BOUND_MARGIN;

            let (mut alpha, mut beta) = if depth >= 4 {
                (
                    self.score.saturating_sub(INITIAL_BOUND_MARGIN),
                    self.score.saturating_add(INITIAL_BOUND_MARGIN),
                )
            } else {
                (-Eval::MAX, Eval::MAX)
            };

            loop {
                self.ply = 0;

                // println!(
                //     "Going depth {} with alpha = {}, beta = {}",
                //     depth, alpha, beta
                // );
                // println!(
                //     "PV={}",
                //     self.pv[0..self.pv_length]
                //         .iter()
                //         .map(|mv| util::move_string_dbg(*mv))
                //         .collect::<Vec<_>>()
                //         .join(", ")
                // );

                let score = self.go(alpha, beta, depth);

                if self.is_stopping {
                    break 'outer;
                }

                if score <= alpha {
                    // beta = alpha; // search stability
                    alpha = score.saturating_sub(margin);
                    margin += margin / 3;
                    continue;
                }

                if score >= beta {
                    beta = score.saturating_add(margin);
                    margin += margin / 3;
                    continue;
                }

                quiet_nodes = self.quiet_nodes;
                node_count = self.node_count;
                self.apply_pv(depth, score);
                break;
            }
        }

        self.node_count = node_count;
        self.quiet_nodes = quiet_nodes;

        self.pv[0]
    }

    fn num_nodes_searched(&self) -> u64 {
        self.node_count
    }

    fn search_score(&self) -> i32 {
        self.score as i32
    }
}

impl<'a> Search<'a> {
    pub fn new(
        params: SearchParams,
        tables: &'a tables::Tables,
        tt_size_hint_mb: usize,
        rt: RepetitionTable,
    ) -> Search<'a> {
        let nnue = nnue_load!(net_path!(), NNUE_HSIZE, NNUE_OSIZE);

        let mut s = Search {
            nnue,
            sig: None,
            chess: chess_v2::ChessGame::new(),
            params: Box::new(params),
            tables,
            quiet_nodes: 0,
            quiet_depth: 0,
            node_count: 0,
            ply: 0,
            is_stopping: false,
            score: -SCORE_INF,
            pv_table: unsafe {
                let mut pv_table = Box::new_uninit();
                pv_table.write(PvTable::new());
                pv_table.assume_init()
            },
            depth: 0,
            pv: [0; PV_DEPTH],
            pv_length: 0,
            pv_trace: false,
            history_moves: Box::new([[0; 64]; 16]),
            cut_moves: [[0; 2]; PV_DEPTH],
            move_stack: [(0, 0); PV_DEPTH],
            excluded_move: 0,
            cont_history: unsafe {
                let layout = std::alloc::Layout::new::<[[[i16; 768]; 768]; 2]>();
                let ptr = std::alloc::alloc_zeroed(layout) as *mut [[[i16; 768]; 768]; 2];
                Box::from_raw(ptr)
            },
            eval_stack: [0; PV_DEPTH],
            lmr_table: {
                let mut table = [[0u8; 64]; 64];
                let mut d = 1;
                while d < 64 {
                    let mut m = 1;
                    while m < 64 {
                        table[d][m] = (0.75 + (d as f64).ln() * (m as f64).ln() / 2.2) as u8;
                        m += 1;
                    }
                    d += 1;
                }
                table
            },
            et: EvalTable::new(1024),
            rt,
            tt: TranspositionTable::new(tt_size_hint_mb),
            pawn_correction_heuristic: [[0; u16::MAX as usize + 1]; 2],
            // corr_stats: Vec::new(),
        };

        s.nnue.load(&s.chess);

        s
    }

    /// A soft reset. Clears all counters but does not zero the transposition
    /// or repetition tables, or change the board position.
    #[inline(always)]
    pub fn new_search(&mut self) {
        self.pv_length = 0;
        self.pv_trace = false;
        self.is_stopping = false;
        self.node_count = 0;
        self.quiet_nodes = 0;
        self.quiet_depth = 0;
        self.score = -SCORE_INF;
        self.ply = 0;
        self.depth = 0;

        for i in 0..PV_DEPTH {
            self.pv_table.lengths[i] = 0;
            for j in 0..PV_DEPTH {
                self.pv_table.moves[i][j] = 0;
            }
            self.pv[i] = 0;
        }

        self.cut_moves = [[0; 2]; PV_DEPTH];
        self.eval_stack = [0; PV_DEPTH];
        self.move_stack = [(0, 0); PV_DEPTH];
        self.excluded_move = 0;

        for entry in self.cont_history.iter_mut() {
            for p in entry.iter_mut() {
                p.fill(0);
            }
        }
        for piece in 0..16 {
            for square in 0..64 {
                self.history_moves[piece][square] = 0;
            }
        }

        self.tt.new_search();
    }

    /// Resets the board & nnue state without clearing the transposition or repetition tables.
    #[inline(always)]
    pub fn load_from_fen(&mut self, fen: &str, tables: &tables::Tables) -> Result<usize, String> {
        let fen_length = self.chess.load_fen(fen, tables)?;
        self.nnue.load(&self.chess);
        Ok(fen_length)
    }

    /// Resets the board & nnue state without clearing the transposition or repetition tables.
    #[inline(always)]
    pub fn load_from_board(&mut self, board: &ChessGame) {
        self.chess = *board;
        self.nnue.load(&self.chess);
    }

    /// A hard reset. Clears transposition and repetition tables and
    /// resets all heuristics.
    pub fn new_game(&mut self) {
        self.rt.clear();
        self.tt.clear();
        self.et.clear();

        for i in 0..2 {
            for j in 0..=u16::MAX as usize {
                self.pawn_correction_heuristic[i][j] = 0;
            }
        }
    }

    // pub fn resize_eval_table(&mut self, new_size_mb: usize) {
    //     self.et = EvalTable::new(new_size_mb);
    // }

    #[inline(always)]
    pub fn get_board_mut(&mut self) -> &mut ChessGame {
        &mut self.chess
    }
    #[inline(always)]
    pub fn get_pv(&self) -> &[u16] {
        &self.pv[0..self.pv_length]
    }
    #[inline(always)]
    pub fn get_depth(&self) -> u8 {
        self.depth
    }
    #[inline(always)]
    pub fn get_quiet_depth(&self) -> u32 {
        self.quiet_depth
    }
    #[inline(always)]
    pub fn get_quiet_nodes(&self) -> u64 {
        self.quiet_nodes
    }
    #[inline(always)]
    pub fn get_rt(&self) -> &RepetitionTable {
        &self.rt
    }
    #[inline(always)]
    pub fn get_rt_mut(&mut self) -> &mut RepetitionTable {
        &mut self.rt
    }
    // #[inline(always)]
    // pub fn get_et(&self) -> &EvalTable {
    //     &self.et
    // }
    #[inline(always)]
    pub fn get_nnue_mut(&mut self) -> &mut impl nnue::UpdatableNnue {
        self.nnue.as_mut()
    }

    pub fn set_sig(&mut self, sig: AbortSignal) {
        self.sig = Some(sig);
    }
    pub fn set_rt(&mut self, rt: RepetitionTable) {
        self.rt = rt;
    }
    pub fn set_search_params(&mut self, params: SearchParams) {
        self.params = Box::new(params);
    }

    fn go(&mut self, alpha: Eval, beta: Eval, depth: u8) -> Eval {
        let ply = self.ply as usize & (PV_DEPTH - 1);

        self.node_count += 1;
        self.pv_table.lengths[ply] = 0;

        if self.node_count & 0x7FF == 0 && self.check_sigabort() {
            self.is_stopping = true;
            return 0;
        }

        let mut tt_move_index = 0xFFu8;
        let mut bound_type = BoundType::UpperBound;
        let mut alpha = alpha;
        let mut depth = depth;

        let pv_move = if self.pv_trace {
            self.pv_trace = self.pv_length > (ply + 1);
            self.pv[ply]
        } else {
            0
        };

        let prune_node = ply > 0 && pv_move == 0;
        let non_pv_node = alpha == beta - 1;

        if prune_node
            && (self.chess.half_moves() >= 100 || self.rt.is_repeated(self.chess.zobrist_key()))
        {
            return 0;
        }

        let in_check = self.chess.in_check(self.tables, self.chess.b_move());
        depth += in_check as u8;

        let tt_probe = if self.excluded_move == 0 {
            self.tt
                .probe(&self.chess, self.chess.zobrist_key(), depth, alpha, beta)
        } else {
            // During singular verification, skip TT cutoffs to avoid infinite recursion
            None
        };

        let mut tt_score = 0;
        let mut tt_depth = 0u8;
        let mut tt_bound = BoundType::UpperBound;

        if let Some(ref probe) = tt_probe {
            if prune_node {
                if let Some(score) = probe.score {
                    return score;
                }
            }
            tt_move_index = probe.mv_index;
            tt_score = probe.tt_score;
            tt_depth = probe.tt_depth;
            tt_bound = probe.bound_type;
        }

        // IIR
        depth -= (depth >= 4 && tt_move_index == 0xFF && prune_node) as u8;

        let depth = depth;

        // Cut 2 plys before max PV depth since the previous call
        // can't copy over PV moves beyond this point
        if self.ply >= PV_DEPTH as u8 - 2 || depth == 0 {
            return self.quiescence(alpha, beta, ply as u32);
        }

        let static_eval = self.evaluate();
        let corrected_eval = self.eval_with_correction(static_eval);

        self.eval_stack[ply] = corrected_eval;

        let improving = if !in_check {
            ply >= 2 && corrected_eval > self.eval_stack[ply - 2]
        } else {
            false
        };

        if prune_node && !in_check {
            if non_pv_node && !is_mate(alpha) && !is_mate(beta) && depth < 8 {
                let eval_margin = 180 * depth as Eval / (1 + improving as Eval);

                if corrected_eval - eval_margin >= beta {
                    return corrected_eval - eval_margin;
                }
            }

            // Null move pruning
            if depth >= 3 && self.chess.non_pawn_mat()[self.chess.b_move() as usize] > 0 {
                let ep_square = self.chess.make_null_move(self.tables);

                let r = 2 + depth / 3;

                // @todo - self.move_stack
                // @todo - Prevent null move consequtively

                self.ply += 1;
                let score = -self.go(-beta, -beta + 1, depth - r);
                self.ply -= 1;

                self.chess.rollback_null_move(ep_square, self.tables);

                if self.is_stopping {
                    return 0;
                }

                if score >= beta {
                    return score;
                }
            }
        }

        self.rt
            .push_position(self.chess.zobrist_key(), self.chess.half_moves() == 0);

        let mut move_list = [0u32; 256];
        let mut original_move_list = [0u16; 256];

        let cont_idx_ply1 = if ply >= 1 {
            let (p, d) = self.move_stack[ply - 1];
            debug_assert!(p < 12);

            let index = p as usize * 64 + d as usize;
            unsafe {
                debug_assert!(index < 768);
                std::hint::assert_unchecked(index < 768);
            }
            Some(index)
        } else {
            None
        };

        let cont_idx_ply2 = if ply >= 2 {
            let (p, d) = self.move_stack[ply - 2];
            debug_assert!(p < 12);

            let index = p as usize * 64 + d as usize;
            unsafe {
                debug_assert!(index < 768);
                std::hint::assert_unchecked(index < 768);
            }
            Some(index)
        } else {
            None
        };

        let see_info = if depth > 1 {
            let bitboards = self.chess.bitboards();
            let black_board = bitboards.iter().skip(8).fold(0u64, |acc, &bb| acc | bb);
            let white_board = bitboards.iter().take(8).fold(0u64, |acc, &bb| acc | bb);
            let mut pieces_board = [0u64; 8];
            bitboards
                .iter()
                .take(8)
                .zip(bitboards.iter().skip(8))
                .enumerate()
                .for_each(|(i, (w, b))| pieces_board[i] = *w | *b);
            let pins = [
                see::calc_pinnings(false, &self.chess, black_board, white_board),
                see::calc_pinnings(true, &self.chess, black_board, white_board),
            ];
            Some(SeeInfo {
                black_board,
                white_board,
                pieces_board,
                pins,
            })
        } else {
            None
        };

        let move_count = {
            let cont_hist = ContHistRef {
                ply1: cont_idx_ply1.map(|idx| &self.cont_history[0][idx]),
                ply2: cont_idx_ply2.map(|idx| &self.cont_history[1][idx]),
            };

            if let Some(see_info) = &see_info {
                let mut moves = SeeOrdering::<HISTORY_MIN, HISTORY_MAX>::new(
                    pv_move,
                    tt_move_index,
                    self.cut_moves[ply],
                );
                moves.gen_moves(
                    &self.chess,
                    self.tables,
                    &self.history_moves,
                    &cont_hist,
                    see_info,
                    &mut original_move_list,
                    &mut move_list,
                )
            } else {
                let mut moves = MvvlvaOrdering::<HISTORY_MIN, HISTORY_MAX>::new(
                    pv_move,
                    tt_move_index,
                    self.cut_moves[ply],
                );
                moves.gen_moves(
                    &self.chess,
                    &self.history_moves,
                    &cont_hist,
                    &mut original_move_list,
                    &mut move_list,
                )
            }
        };

        let singular_move = if prune_node
            && depth >= 8
            && tt_move_index != 0xFF
            && tt_depth >= depth.saturating_sub(3)
            && tt_bound != BoundType::UpperBound
            && !is_mate(tt_score)
        {
            original_move_list[tt_move_index as usize]
        } else {
            0
        };

        let mut best_score = -SCORE_INF;
        let mut best_move = 0;
        let mut num_legal_moves = 0;

        let board_copy = self.chess.clone();

        for mv_index in 0..move_count {
            let mv = move_list[mv_index] as u16;

            if mv == self.excluded_move {
                continue;
            }

            let mut extension: u8 = 0;
            if mv == singular_move {
                let saved_excluded = self.excluded_move;
                self.excluded_move = mv;

                self.rt.pop_position();
                let singular_beta = tt_score - 3 * depth as Eval;
                let s_score = self.go(singular_beta - 1, singular_beta, depth / 2);

                self.excluded_move = saved_excluded;

                if self.is_stopping {
                    return 0;
                }

                if s_score < singular_beta {
                    extension = 1;
                } else if singular_beta >= beta {
                    // Multi-cut: even without the TT move, score >= beta
                    return singular_beta;
                }

                self.rt
                    .push_position(self.chess.zobrist_key(), self.chess.half_moves() == 0);
            }

            if num_legal_moves > 0
                && !in_check
                && depth <= SEE_QUIET_PRUNE_MAX_DEPTH.max(SEE_CAPTURE_PRUNE_MAX_DEPTH)
            {
                if let Some(see_info) = &see_info {
                    let is_capture = (mv & MV_FLAG_CAP) != 0;

                    // Capture SEE pruning: depth-scaled linear threshold.
                    // Fast path: good captures (SEE >= 0 from ordering) always pass
                    // any negative threshold, so skip the see_threshold call entirely.
                    if is_capture && depth <= SEE_CAPTURE_PRUNE_MAX_DEPTH {
                        let sort_score = (move_list[mv_index] >> 16) as u16;
                        if sort_score < SEE_ORDERING_BAD_CAP_LIMIT {
                            let threshold = -(SEE_CAPTURE_MARGIN * depth as Eval);
                            if !see::see_threshold(
                                &WEIGHT_TABLE_ABS,
                                self.tables,
                                &board_copy,
                                mv,
                                threshold,
                                see_info.black_board,
                                see_info.white_board,
                                see_info.pieces_board,
                                Some(&see_info.pins),
                            ) {
                                continue;
                            }
                        }
                    }

                    // Quiet SEE pruning: quadratic threshold with history adjustment.
                    // Quadratic scaling prunes less at shallow depths (safer) and more
                    // at deep depths (bigger savings). History shifts the threshold:
                    // bad history -> threshold closer to 0 -> more pruning.
                    if !is_capture && depth <= SEE_QUIET_PRUNE_MAX_DEPTH {
                        let src_piece = board_copy.spt()[(mv & 0x3F) as usize] as usize;
                        let dst_sq = ((mv >> 6) & 0x3F) as usize;
                        let history = self.history_moves[src_piece][dst_sq];

                        let threshold = -(SEE_QUIET_MARGIN * depth as Eval * depth as Eval)
                            - (history as Eval / SEE_HISTORY_DIVISOR);

                        if !see::see_threshold(
                            &WEIGHT_TABLE_ABS,
                            self.tables,
                            &board_copy,
                            mv,
                            threshold.min(0),
                            see_info.black_board,
                            see_info.white_board,
                            see_info.pieces_board,
                            Some(&see_info.pins),
                        ) {
                            continue;
                        }
                    }
                }
            }

            let nnue_update = unsafe {
                // Safety: mv is generated by gen_moves_avx512, so it is guaranteed to be valid
                self.chess.make_move_nnue(mv, self.tables)
            };

            let nnue_update = match nnue_update {
                Some(nnue_update) => {
                    if self.chess.in_check(self.tables, !self.chess.b_move()) {
                        self.chess = board_copy;
                        continue;
                    }
                    nnue_update
                }
                None => continue,
            };

            self.nnue.make_move(nnue_update);

            let src_piece = board_copy.spt()[(mv & 0x3F) as usize];
            let dst_sq = ((mv >> 6) & 0x3F) as u8;
            self.move_stack[ply] = (
                util::compress_piece_index_nonzero(src_piece as usize) as u8,
                dst_sq,
            );

            let is_non_capture_or_promotion = mv & (MV_FLAG_CAP | MV_FLAG_PROMOTION) == 0;

            let late_move_reduction =
                num_legal_moves > 3 && depth >= 3 && !in_check && is_non_capture_or_promotion;

            self.ply += 1;

            let new_depth = depth - 1 + extension;

            let score = if num_legal_moves == 0 {
                -self.go(-beta, -alpha, new_depth)
            } else if late_move_reduction {
                let r = {
                    let base_r =
                        self.lmr_table[depth.min(63) as usize][(num_legal_moves).min(63)] as i16;

                    let src_piece = board_copy.spt()[(mv & 0x3F) as usize] as usize;
                    debug_assert!(src_piece != 0);

                    let dst_sq = ((mv >> 6) & 0x3F) as usize;
                    let history = self.history_moves[src_piece][dst_sq] as i32;

                    let cont_bonus = {
                        let src_piece_comp = util::compress_piece_index_nonzero(src_piece);

                        let c1 = cont_idx_ply1.map_or(0i32, |idx| {
                            self.cont_history[0][idx][src_piece_comp * 64 + dst_sq] as i32
                        });
                        let c2 = cont_idx_ply2.map_or(0i32, |idx| {
                            self.cont_history[1][idx][src_piece_comp * 64 + dst_sq] as i32
                        });
                        (c1 + c2) / 2
                    };

                    let combined = history + cont_bonus;
                    let hist_adj = (combined / 8192).clamp(-2, 2) as i16;

                    let mut r = base_r - hist_adj;

                    r -= improving as i16;
                    r += non_pv_node as i16;

                    r.clamp(1, new_depth as i16) as u8
                };

                let proof_score = -self.go(-alpha - 1, -alpha, new_depth - r);
                if proof_score > alpha {
                    // The move might be good, search it again with full depth
                    let proof_score = -self.go(-alpha - 1, -alpha, new_depth);

                    if proof_score > alpha && proof_score < beta {
                        -self.go(-beta, -alpha, new_depth)
                    } else {
                        proof_score
                    }
                } else {
                    proof_score
                }
            } else {
                // Null search with fallback to full search
                let proof_score = -self.go(-alpha - 1, -alpha, new_depth);

                if proof_score > alpha && proof_score < beta {
                    -self.go(-beta, -alpha, new_depth)
                } else {
                    proof_score
                }
            };

            num_legal_moves += 1;

            self.ply -= 1;
            self.chess = board_copy;
            self.nnue.rollback_move();

            if self.is_stopping {
                self.rt.pop_position();
                return 0;
            }

            if score > best_score {
                best_score = score;
                best_move = mv;
            }

            if score <= alpha {
                continue;
            }

            bound_type = BoundType::Exact;
            alpha = score;

            unsafe {
                // Safety: ply+1 is guaranteed to be within PV_DEPTH from search_done check above
                debug_assert!(ply < PV_DEPTH - 1);
                std::hint::assert_unchecked(ply < PV_DEPTH - 1);
            }

            // Update PV
            let child_pv_length = self.pv_table.lengths[ply + 1];

            self.pv_table.moves[ply][0] = mv;
            self.pv_table.lengths[ply] = child_pv_length + 1;

            let [root_pv_moves, child_pv_moves] = self
                .pv_table
                .moves
                .get_disjoint_mut([ply, ply + 1])
                .unwrap();

            unsafe {
                // Safety: child_pv_length is guaranteed to be set due to a call to go()
                // it is either zero or a valid length within PV_DEPTH
                debug_assert!(child_pv_length as usize + 1 < PV_DEPTH);
                root_pv_moves
                    .get_unchecked_mut(1..child_pv_length as usize + 1)
                    .copy_from_slice(&child_pv_moves.get_unchecked(0..child_pv_length as usize));
            }

            if score >= beta {
                let is_non_capture = (mv & MV_FLAG_CAP) == 0;

                if is_non_capture && self.excluded_move == 0 {
                    let src_piece = self.chess.spt()[(mv & 0x3F) as usize] as usize;
                    let dst_square = ((mv >> 6) & 0x3F) as usize;

                    unsafe {
                        // Safety:
                        // - src_piece is guaranteed to be a valid piece index from the board state
                        // - dst_square is guaranteed to be within 0..64 from move generation
                        debug_assert!(src_piece < 16 && dst_square < 64);
                        std::hint::assert_unchecked(src_piece < 16);
                        std::hint::assert_unchecked(dst_square < 64);
                    }
                    debug_assert!(PieceIndex::from(src_piece) != PieceIndex::WhiteNullPiece);
                    debug_assert!(PieceIndex::from(src_piece) != PieceIndex::BlackNullPiece);

                    // @todo - Check removing original history heuristic in favor of continuation history
                    self.update_history_heuristic(
                        src_piece,
                        dst_square,
                        depth as i16 * depth as i16,
                    );

                    if mv != self.cut_moves[ply][0] {
                        self.cut_moves[ply][1] = self.cut_moves[ply][0];
                        self.cut_moves[ply][0] = mv;
                    }

                    self.update_cont_history(
                        ply,
                        src_piece,
                        dst_square,
                        depth as i16 * depth as i16,
                    );

                    for j in 0..mv_index {
                        let prev_mv = move_list[j] as u16;

                        debug_assert!(prev_mv != 0);
                        debug_assert!(prev_mv != mv);

                        let is_capture = prev_mv & MV_FLAG_CAP != 0;

                        if is_capture {
                            continue;
                        }

                        let src_piece = self.chess.spt()[(prev_mv & 0x3F) as usize] as usize;
                        let dst_square = ((prev_mv >> 6) & 0x3F) as usize;

                        unsafe {
                            // Safety:
                            // - src_piece is guaranteed to be a valid piece index from the board state
                            // - dst_square is guaranteed to be within 0..64 from move generation
                            debug_assert!(src_piece < 16 && dst_square < 64);
                            std::hint::assert_unchecked(src_piece < 16);
                            std::hint::assert_unchecked(dst_square < 64);
                        }

                        debug_assert!(PieceIndex::from(src_piece) != PieceIndex::WhiteNullPiece);
                        debug_assert!(PieceIndex::from(src_piece) != PieceIndex::BlackNullPiece);

                        self.update_history_heuristic(
                            src_piece,
                            dst_square,
                            -(depth as i16 * depth as i16),
                        );

                        self.update_cont_history(
                            ply,
                            src_piece,
                            dst_square,
                            -(depth as i16 * depth as i16),
                        );
                    }
                }

                if self.excluded_move == 0 && !in_check && best_move & MV_FLAG_CAP == 0 {
                    if score >= static_eval {
                        self.update_correction_heuristics(score, static_eval, depth as Eval);
                    }
                }

                if self.excluded_move == 0 {
                    self.tt.store(
                        self.chess.zobrist_key(),
                        score,
                        depth,
                        || Self::find_move_index_avx512(best_move, &original_move_list),
                        BoundType::LowerBound,
                    );
                }
                self.rt.pop_position();
                return score;
            }
        }
        self.rt.pop_position();

        if self.excluded_move == 0 && !in_check && best_move & MV_FLAG_CAP == 0 {
            let update_score = match bound_type {
                BoundType::UpperBound if best_move == 0 => Some(0),
                BoundType::UpperBound if best_score <= static_eval => Some(best_score),
                BoundType::Exact => Some(best_score),
                _ => None,
            };

            if let Some(score) = update_score {
                self.update_correction_heuristics(score, static_eval, depth as Eval);
            }
        }

        if num_legal_moves == 0 {
            if in_check {
                return -SCORE_INF + self.ply as Eval;
            }

            // Stalemate
            return 0;
        }

        if self.excluded_move == 0 {
            self.tt.store(
                self.chess.zobrist_key(),
                best_score,
                depth,
                || Self::find_move_index_avx512(best_move, &original_move_list),
                bound_type,
            );
        }

        best_score
    }

    fn quiescence(&mut self, alpha: Eval, beta: Eval, start_ply: u32) -> Eval {
        self.node_count += 1;
        self.quiet_nodes += 1;
        self.quiet_depth = self.quiet_depth.max(self.ply as u32 - start_ply);

        if self.node_count & 0x7FF == 0 && self.check_sigabort() {
            self.is_stopping = true;
            return 0;
        }

        let mut alpha = alpha;

        let in_check = self.chess.in_check(self.tables, self.chess.b_move());

        let static_eval = if !in_check {
            let raw_eval = self.evaluate();
            let static_eval = self.eval_with_correction(raw_eval);

            // If the current board position is bad enough to cause a
            // cutoff higher up, save the time and return it immediately
            if static_eval >= beta {
                return static_eval;
            }

            // If it is better than alpha, update alpha bound to it to cause more cuts, assuming
            // that making any move in this position will either be the same or better for the
            // playing side
            if static_eval > alpha {
                alpha = static_eval;
            }

            static_eval
        } else {
            -SCORE_INF
        };

        // Precompute bitboards + pins for qsearch SEE pruning of captures.
        // Only computed when not in check (in check -> quiescence_check_evasion, no SEE pruning).
        let (qsee_bb_black, qsee_bb_white, qsee_bb_pieces, qsee_pins) = if !in_check {
            let bbs = self.chess.bitboards();
            let bb_black = bbs.iter().skip(8).fold(0u64, |acc, &bb| acc | bb);
            let bb_white = bbs.iter().take(8).fold(0u64, |acc, &bb| acc | bb);
            let mut bb_pieces = [0u64; 8];
            bbs.iter()
                .take(8)
                .zip(bbs.iter().skip(8))
                .enumerate()
                .for_each(|(i, (w, b))| bb_pieces[i] = *w | *b);
            let pins = [
                see::calc_pinnings(false, &self.chess, bb_black, bb_white),
                see::calc_pinnings(true, &self.chess, bb_black, bb_white),
            ];
            (bb_black, bb_white, bb_pieces, Some(pins))
        } else {
            (0, 0, [0u64; 8], None)
        };

        let mut moves = CaptureOrdering::new();

        let mut best_score: Eval = static_eval;
        let board_copy = self.chess.clone();

        let mut move_list = [0u32; 256];
        for i in 0..moves.gen_moves(&self.chess, &mut move_list) {
            let mv = move_list[i] as u16;

            if let Some(ref pins) = qsee_pins {
                if !see::see_threshold(
                    &WEIGHT_TABLE_ABS,
                    self.tables,
                    &board_copy,
                    mv,
                    SEE_QS_THRESHOLD,
                    qsee_bb_black,
                    qsee_bb_white,
                    qsee_bb_pieces,
                    Some(pins),
                ) {
                    continue;
                }
            }

            let nnue_update = unsafe {
                // Safety: mv is generated by gen_moves_avx512, so it is guaranteed to be valid
                self.chess.make_move_nnue(mv, self.tables)
            };

            let nnue_update = match nnue_update {
                Some(nnue_update) => {
                    if self.chess.in_check(self.tables, !self.chess.b_move()) {
                        self.chess = board_copy;
                        continue;
                    }
                    nnue_update
                }
                None => continue,
            };

            self.nnue.make_move(nnue_update);

            self.ply += 1;

            let score = -self.quiescence(-beta, -alpha, start_ply);

            self.ply -= 1;

            self.chess = board_copy;
            self.nnue.rollback_move();

            if self.is_stopping {
                return 0;
            }

            if score > best_score {
                best_score = score;

                if score >= beta {
                    return score;
                }

                if score > alpha {
                    alpha = score;
                }
            }
        }

        if best_score == -SCORE_INF {
            return self.quiescence_check_evasion(alpha, beta, start_ply);
        }

        best_score
    }

    pub fn quiescence_check_evasion(&mut self, alpha: Eval, beta: Eval, start_ply: u32) -> Eval {
        debug_assert!(self.chess.in_check(self.tables, self.chess.b_move()));

        let mut alpha = alpha;

        let mut moves = MvvlvaOrdering::<HISTORY_MIN, HISTORY_MAX>::new(0, 0xFF, [0; 2]);

        let mut best_score = -SCORE_INF;
        let mut num_legal_moves = 0;

        let board_copy = self.chess.clone();

        let mut move_list = [0u32; 256];
        let mut original_move_list = [0u16; 256];
        let no_cont = ContHistRef {
            ply1: None,
            ply2: None,
        };
        for i in 0..moves.gen_moves(
            &self.chess,
            &self.history_moves,
            &no_cont,
            &mut original_move_list,
            &mut move_list,
        ) {
            let mv_index = i;
            let mv = move_list[mv_index] as u16;

            let nnue_update = unsafe {
                // Safety: mv is generated by gen_moves_avx512, so it is guaranteed to be valid
                self.chess.make_move_nnue(mv, self.tables)
            };

            let nnue_update = match nnue_update {
                Some(nnue_update) => {
                    if self.chess.in_check(self.tables, !self.chess.b_move()) {
                        self.chess = board_copy;
                        continue;
                    }
                    nnue_update
                }
                None => continue,
            };

            num_legal_moves += 1;

            self.nnue.make_move(nnue_update);

            self.ply += 1;

            let score = -self.quiescence(-beta, -alpha, start_ply);

            self.ply -= 1;

            self.chess = board_copy;
            self.nnue.rollback_move();

            if self.is_stopping {
                return 0;
            }

            if score > best_score {
                best_score = score;

                if score >= beta {
                    return score;
                }

                if score > alpha {
                    alpha = score;
                }
            }
        }

        if num_legal_moves == 0 {
            return -SCORE_INF + self.ply as Eval;
        }

        best_score
    }

    #[inline(always)]
    pub fn apply_pv(&mut self, depth: u8, score: Eval) {
        self.pv_length = self.pv_table.lengths[0] as usize;
        self.pv[0..self.pv_length].copy_from_slice(&self.pv_table.moves[0][0..self.pv_length]);
        self.pv_trace = self.pv_length > 0;
        self.depth = depth;
        self.score = score;
    }

    #[inline(always)]
    fn pawn_correction_heuristic_entry(&mut self) -> &mut Eval {
        &mut self.pawn_correction_heuristic[self.chess.b_move() as usize]
            [(self.chess.pawn_key() & 0xFFFF) as usize]
    }

    #[inline(always)]
    fn update_correction_heuristics(&mut self, score: Eval, static_eval: Eval, depth: i16) {
        let entry = self.pawn_correction_heuristic_entry();
        let mut current_value = *entry as i32;

        let depth = depth as i32;
        let diff = (score as i32) - (static_eval as i32);

        let diff_scaled = (diff * (CORR_GRAIN as i32))
            .max((-CORR_MAX_ERROR) as i32)
            .min(CORR_MAX_ERROR as i32);

        let depth_weight = (depth * depth + 2 * depth + 1).min(CORR_WEIGHT_SCALE as i32) as i32;

        current_value = (current_value * (CORR_WEIGHT_SCALE as i32 - depth_weight)
            + diff_scaled * depth_weight)
            / (CORR_WEIGHT_SCALE as i32);

        current_value = current_value
            .max((-CORR_MAX_ERROR) as i32)
            .min(CORR_MAX_ERROR as i32);

        *entry = current_value as Eval;
    }

    #[inline(always)]
    pub fn eval_with_correction(&mut self, base_eval: Eval) -> Eval {
        let pawn_corr = *self.pawn_correction_heuristic_entry();

        let correction = pawn_corr / CORR_GRAIN;

        (base_eval + correction)
            .min(SCORE_INF - PV_DEPTH as i16)
            .max((-SCORE_INF) + PV_DEPTH as i16)
    }

    #[inline(always)]
    pub fn calc_board_phase(&self) -> f32 {
        const MATERIAL_QUEEN: u16 = 700;
        const MATERIAL_ROOK: u16 = 350;
        const MATERIAL_BISHOP: u16 = 210;
        const MATERIAL_KNIGHT: u16 = 210;
        const MATERIAL_PAWN: u16 = 70;
        const MATERIAL_TABLE: [u16; 16] = [
            0, // NullPieceWhite
            0, // King has no material value
            MATERIAL_QUEEN as u16,
            MATERIAL_ROOK as u16,
            MATERIAL_BISHOP as u16,
            MATERIAL_KNIGHT as u16,
            MATERIAL_PAWN as u16,
            0, // Pad
            0, // NullPieceBlack
            0, // King has no material value
            MATERIAL_QUEEN as u16,
            MATERIAL_ROOK as u16,
            MATERIAL_BISHOP as u16,
            MATERIAL_KNIGHT as u16,
            MATERIAL_PAWN as u16,
            0, // Pad
        ];

        let bitboards = self.chess.bitboards();

        let material: u16 = bitboards
            .iter()
            .enumerate()
            .map(|(i, &bb)| bb.count_ones() as u16 * MATERIAL_TABLE[i])
            .sum();

        const MAT_MAX: u16 = 5320; // 5600 - 4 pawns
        const MAT_MIN: u16 = 280; // minor piece + pawn

        (((material + MAT_MIN) as f32) / ((MAT_MAX + MAT_MIN) as f32))
            .max(0.0)
            .min(1.0)
    }

    #[inline(always)]
    fn output_bucket(&self) -> u8 {
        if NNUE_OSIZE == 1 {
            return 0;
        }

        let divisor = 32usize.div_ceil(NNUE_OSIZE);
        let occupancy = self.chess.occupancy();

        (occupancy.count_ones() as u8 - 2) / divisor as u8
    }

    #[inline(always)]
    pub fn evaluate(&mut self) -> Eval {
        if EVAL_CACHE {
            if let Some(eval) = self.et.probe(self.chess.zobrist_key()) {
                return eval;
            }
        }

        let score = if EVAL_NNUE {
            self.nnue
                .evaluate(self.chess.b_move(), self.output_bucket()) as Eval
        } else {
            self.eval_hc()
        };

        if EVAL_CACHE {
            self.et.store(self.chess.zobrist_key(), score);
        }

        score
    }

    #[inline(always)]
    fn eval_hc(&mut self) -> Eval {
        use std::arch::x86_64::*;

        let bitboards = self.chess.bitboards();

        let mut bonuses_mg: Eval = 0;
        let mut bonuses_eg: Eval = 0;

        unsafe {
            let mut bonuses_mg_x64 = _mm512_setzero_si512();
            let mut bonuses_eg_x64 = _mm512_setzero_si512();

            macro_rules! acc_piece_bonus_avx512 {
                ($bonus_vec:ident,$is_eg:expr,$piece_id:expr) => {
                    $bonus_vec = _mm512_mask_blend_epi8(
                        bitboards[$piece_id as usize],
                        $bonus_vec,
                        _mm512_loadu_epi8(
                            EVAL_TABLES_INV_MGEG.0[$is_eg][$piece_id as usize].as_ptr()
                                as *const i8,
                        ),
                    )
                };
            }

            macro_rules! sum_bonus_avx512 {
                ($bonus_acc:ident,$bonus_vec:ident) => {
                    let bonuses_lsb = _mm512_castsi512_si256($bonus_vec);
                    let bonuses_msb = _mm512_extracti64x4_epi64($bonus_vec, 1);
                    let bonuses_lsb_i16 = _mm512_cvtepi8_epi16(bonuses_lsb);
                    let bonuses_msb_i16 = _mm512_cvtepi8_epi16(bonuses_msb);
                    let summed_bonuses = _mm512_add_epi16(bonuses_lsb_i16, bonuses_msb_i16);
                    let summed_bonuses_lsb = _mm512_castsi512_si256(summed_bonuses);
                    let summed_bonuses_msb = _mm512_extracti64x4_epi64(summed_bonuses, 1);

                    $bonus_acc += _mm256_reduce_add_epi16(summed_bonuses_lsb) as Eval;
                    $bonus_acc += _mm256_reduce_add_epi16(summed_bonuses_msb) as Eval;
                };
            }

            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteKing);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteQueen);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteRook);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteBishop);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteKnight);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhitePawn);

            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackKing);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackQueen);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackRook);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackBishop);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackKnight);
            acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackPawn);

            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteKing);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteQueen);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteRook);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteBishop);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteKnight);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhitePawn);

            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackKing);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackQueen);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackRook);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackBishop);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackKnight);
            acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackPawn);

            sum_bonus_avx512!(bonuses_mg, bonuses_mg_x64);
            sum_bonus_avx512!(bonuses_eg, bonuses_eg_x64);
        }

        let wb = bitboards[PieceIndex::WhiteBishop as usize].count_ones();
        let bb = bitboards[PieceIndex::BlackBishop as usize].count_ones();

        let wq_bq = bitboards[PieceIndex::WhiteQueen as usize].count_ones() as Eval
            - bitboards[PieceIndex::BlackQueen as usize].count_ones() as Eval;

        let wr_br = bitboards[PieceIndex::WhiteRook as usize].count_ones() as Eval
            - bitboards[PieceIndex::BlackRook as usize].count_ones() as Eval;

        let wb_bb = wb as Eval - bb as Eval;

        let wn_bn = bitboards[PieceIndex::WhiteKnight as usize].count_ones() as Eval
            - bitboards[PieceIndex::BlackKnight as usize].count_ones() as Eval;

        let wp_bp = bitboards[PieceIndex::WhitePawn as usize].count_ones() as Eval
            - bitboards[PieceIndex::BlackPawn as usize].count_ones() as Eval;

        let mut score_mg = bonuses_mg
            + wq_bq * WEIGHT_TABLE_MGEG[0][PieceIndex::WhiteQueen as usize]
            + wr_br * WEIGHT_TABLE_MGEG[0][PieceIndex::WhiteRook as usize]
            + wb_bb * WEIGHT_TABLE_MGEG[0][PieceIndex::WhiteBishop as usize]
            + wn_bn * WEIGHT_TABLE_MGEG[0][PieceIndex::WhiteKnight as usize]
            + wp_bp * WEIGHT_TABLE_MGEG[0][PieceIndex::WhitePawn as usize];

        let mut score_eg = bonuses_eg
            + wq_bq * WEIGHT_TABLE_MGEG[1][PieceIndex::WhiteQueen as usize]
            + wr_br * WEIGHT_TABLE_MGEG[1][PieceIndex::WhiteRook as usize]
            + wb_bb * WEIGHT_TABLE_MGEG[1][PieceIndex::WhiteBishop as usize]
            + wn_bn * WEIGHT_TABLE_MGEG[1][PieceIndex::WhiteKnight as usize]
            + wp_bp * WEIGHT_TABLE_MGEG[1][PieceIndex::WhitePawn as usize];

        let phase = self.calc_board_phase();

        if wb > 1 {
            score_mg += 20;
            score_eg += 40;
        }
        if bb > 1 {
            score_mg -= 20;
            score_eg -= 40;
        }

        let mut white_pawns = bitboards[PieceIndex::WhitePawn as usize];
        loop {
            let pawn_sq = crate::pop_ls1b!(white_pawns);
            let file = (0x101010101010101 << 8) << pawn_sq;
            let file_left = (file >> 1) & tables::EX_H_FILE;
            let file_right = (file << 1) & tables::EX_A_FILE;
            let full_mask = file | file_left | file_right;

            let rank_bonus = [10, 10, 20, 50, 90, 100, 0][(pawn_sq / 8) as usize];

            if full_mask & bitboards[PieceIndex::BlackPawn as usize] == 0 {
                score_mg += rank_bonus;
                score_eg += rank_bonus;
            }
        }

        let mut black_pawns = bitboards[PieceIndex::BlackPawn as usize];
        loop {
            let pawn_sq = crate::pop_ls1b!(black_pawns);
            let file = (0x101010101010101u64).wrapping_shr((64 - pawn_sq) as u32);
            let file_left = (file >> 1) & tables::EX_H_FILE;
            let file_right = (file << 1) & tables::EX_A_FILE;
            let full_mask = file | file_left | file_right;

            let rank_bonus = [0, 100, 90, 50, 20, 10, 10][(pawn_sq / 8) as usize];

            if full_mask & bitboards[PieceIndex::WhitePawn as usize] == 0 {
                score_mg -= rank_bonus;
                score_eg -= rank_bonus;
            }
        }

        // King safety
        let attacker_bonuses = [10, 30, 60, 100, 200];
        let attacker_bonuses_eg = [5, 10, 20, 40, 100];
        let white_king_sq = bitboards[PieceIndex::WhiteKing as usize].trailing_zeros() as u8;
        let mut white_king_surround_squares =
            tables::Tables::LT_KING_MOVE_MASKS[white_king_sq as usize];
        loop {
            let king_surround_sq = crate::pop_ls1b!(white_king_surround_squares);

            let num_attackers = self.chess.count_king_square_attackers_slow(
                king_surround_sq as u8,
                false,
                self.tables,
            );

            score_mg -= attacker_bonuses[num_attackers.min(4)];
            score_eg -= attacker_bonuses_eg[num_attackers.min(4)];
        }
        let black_king_sq = bitboards[PieceIndex::BlackKing as usize].trailing_zeros() as u8;
        let mut black_king_surround_squares =
            tables::Tables::LT_KING_MOVE_MASKS[black_king_sq as usize];
        loop {
            let king_surround_sq = crate::pop_ls1b!(black_king_surround_squares);

            let num_attackers = self.chess.count_king_square_attackers_slow(
                king_surround_sq as u8,
                true,
                self.tables,
            );

            score_mg += attacker_bonuses[num_attackers.min(4)];
            score_eg += attacker_bonuses_eg[num_attackers.min(4)];
        }

        let w_moves = self.chess.estimate_move_count(false, &self.tables);
        let b_moves = self.chess.estimate_move_count(true, &self.tables);
        let move_ratio = w_moves as Eval - b_moves as Eval;

        // let all_pawns =
        //     bitboards[PieceIndex::WhitePawn as usize] | bitboards[PieceIndex::BlackPawn as usize];

        let mut score =
            ((score_mg as f32 * phase + score_eg as f32 * (1.0 - phase)) as Eval) + move_ratio * 5;

        score += if self.chess.b_move() { -5 } else { 5 };

        let final_score = score * if self.chess.b_move() { -1 } else { 1 };

        // assert!(
        //     self.evaluate_legacy() == final_score,
        //     "Legacy and simd evaluation mismatch for {} != {}. b_move= {}",
        //     self.evaluate_legacy(),
        //     final_score,
        //     self.chess.b_move()
        // );

        final_score
    }

    #[inline(always)]
    fn update_history_heuristic(&mut self, piece_index: usize, dst_sq: usize, bonus: i16) {
        let entry = &mut self.history_moves[piece_index][dst_sq];
        let ratio = ((*entry as i64) * bonus.abs() as i64 / HISTORY_MAX as i64) as i16;
        let value = (*entry + (bonus - ratio)).clamp(-HISTORY_MAX, HISTORY_MAX);
        *entry = value;
    }

    #[inline(always)]
    fn update_cont_history(&mut self, ply: usize, cur_piece: usize, cur_dst: usize, bonus: i16) {
        unsafe {
            debug_assert!(cur_piece < 16);
            debug_assert!(cur_dst < 64);
            std::hint::assert_unchecked(cur_piece < 16);
            std::hint::assert_unchecked(cur_dst < 64);
        }

        let cur_piece_comp = util::compress_piece_index_nonzero(cur_piece);

        if ply >= 1 {
            let (p, d) = self.move_stack[ply - 1];
            let idx = p as usize * 64 + d as usize;

            unsafe {
                debug_assert!(idx < 768);
                std::hint::assert_unchecked(idx < 768);
            }

            let entry = &mut self.cont_history[0][idx][cur_piece_comp * 64 + cur_dst];
            let ratio = ((*entry as i64) * bonus.abs() as i64 / HISTORY_MAX as i64) as i16;
            *entry = (*entry + (bonus - ratio)).clamp(-HISTORY_MAX, HISTORY_MAX);
        }

        if ply >= 2 {
            let (p, d) = self.move_stack[ply - 2];
            let idx = p as usize * 64 + d as usize;

            unsafe {
                debug_assert!(idx < 768);
                std::hint::assert_unchecked(idx < 768);
            }

            let entry = &mut self.cont_history[1][idx][cur_piece_comp * 64 + cur_dst];
            let ratio = ((*entry as i64) * bonus.abs() as i64 / HISTORY_MAX as i64) as i16;
            *entry = (*entry + (bonus - ratio)).clamp(-HISTORY_MAX, HISTORY_MAX);
        }
    }

    fn check_sigabort(&self) -> bool {
        if let Some(ref sig) = self.sig {
            match sig.try_recv() {
                Ok(_) => true,
                Err(channel::TryRecvError::Empty) => false,
                Err(channel::TryRecvError::Disconnected) => {
                    panic!("sigabort channel disconnected while searching")
                }
            }
        } else {
            false
        }
    }

    pub fn has_single_legal_move(&mut self) -> Option<u16> {
        let mut legal_move = None;
        let mut board = self.chess.clone();

        let mut move_list = [0u16; 256];
        let move_count = board.gen_moves_avx512::<false, _>(&mut move_list);

        for i in 0..move_count {
            if !unsafe { board.make_move(move_list[i], self.tables) } {
                continue;
            }

            if board.in_check(self.tables, !board.b_move()) {
                board = self.chess.clone();
                continue;
            }

            if legal_move.is_some() {
                return None;
            }

            legal_move = Some(move_list[i]);
            board = self.chess.clone();
        }

        return legal_move;
    }

    pub fn find_move_index_avx512(mv: u16, move_list: &[u16; 256]) -> u8 {
        unsafe {
            let mv_list_ptr = move_list.as_ptr() as *const __m512i;
            let p0_x32 = _mm512_loadu_si512(mv_list_ptr);
            let p1_x32 = _mm512_loadu_si512(mv_list_ptr.add(1));
            let p2_x32 = _mm512_loadu_si512(mv_list_ptr.add(2));
            let p3_x32 = _mm512_loadu_si512(mv_list_ptr.add(3));

            let mv_x32 = _mm512_set1_epi16(mv as i16);

            let cmp_mask_0 = _mm512_cmpeq_epi16_mask(p0_x32, mv_x32) as u128;
            let cmp_mask_1 = _mm512_cmpeq_epi16_mask(p1_x32, mv_x32) as u128;
            let cmp_mask_2 = _mm512_cmpeq_epi16_mask(p2_x32, mv_x32) as u128;
            let cmp_mask_3 = _mm512_cmpeq_epi16_mask(p3_x32, mv_x32) as u128;

            let lower_mask: u128 =
                cmp_mask_0 | ((cmp_mask_1) << 32) | ((cmp_mask_2) << 64) | ((cmp_mask_3) << 96);

            // if util::should_log(hash) {
            //     println!(
            //         "Scanned for move {} ({}), at index {} in array {:?}",
            //         mv,
            //         util::move_string_dbg(mv),
            //         lower_mask.trailing_zeros(),
            //         move_list
            //     );
            //     println!(
            //         "Masks: {:032b} {:032b} {:032b} {:032b}",
            //         cmp_mask_0, cmp_mask_1, cmp_mask_2, cmp_mask_3
            //     );
            // }

            debug_assert!(lower_mask != 0, "Move {:04x} not found in move list", mv);
            debug_assert!(
                lower_mask.trailing_zeros() < 256,
                "Move {:04x} index overflow",
                mv
            );

            lower_mask.trailing_zeros() as u8
        }
    }
}
