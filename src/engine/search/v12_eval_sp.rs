use std::cell::OnceCell;

use crossbeam::channel;

use crate::{
    chess_v2::*,
    engine::{
        chess_v2,
        search::{
            AbortSignal, SearchStrategy,
            eval::*,
            repetition_v2::RepetitionTable,
            search_params::SearchParams,
            see,
            transposition_v2::{BoundType, TranspositionTable},
        },
        sorting,
        tables::{self},
    },
    nnue::nnue::{self, UpdatableNnue},
    nnue_load,
};

macro_rules! net_path {
    () => {
        "../../../nnue/w1-10M-512-b8.bin"
    };
}

const EVAL_NNUE: bool = true;
const NNUE_HSIZE: usize = 512;
const NNUE_OSIZE: usize = 8;

const CORR_WEIGHT_SCALE: Eval = 128;
const CORR_MAX_ERROR: Eval = 1024;
const CORR_GRAIN: Eval = 2;

const PV_DEPTH: usize = 64;

const SORT_CAPTURE_RANGE: u16 = 32;
const SORT_QUIET_RANGE: u16 = 65502;

const SORT_QUIET_BASE: u16 = 0;
const SORT_CAPTURE_BASE: u16 = SORT_QUIET_BASE + SORT_QUIET_RANGE;
const SORT_PVTT_BASE: u16 = SORT_CAPTURE_BASE + SORT_CAPTURE_RANGE;

const HISTORY_MAX: i16 = i16::MAX - 0_017;
const HISTORY_MIN: i16 = i16::MIN + 0_017;

const _ASSERT_SORT_RANGE: () = assert!(u16::MAX as usize - 1 == SORT_PVTT_BASE as usize);
const _ASSERT_SORT_HISTORY_MINMAX_RANGE: () =
    assert!(SORT_QUIET_RANGE as usize - 1 == HISTORY_MAX as usize + HISTORY_MIN.abs() as usize);

#[cfg_attr(any(), rustfmt::skip)]
const MVV_LVA_SCORES_U8: [[u8; 16]; 16] = [
    /* Ep Cap */      [0, 0, 0, 0, 0, 0, 26, 0, 0, 0, 0, 0, 0, 0, 26, 0],
    /* WhiteKing */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* WhiteQueen */  [0, 0, 0, 0, 0, 0, 0, 0, 0, 07, 06, 05, 04, 03, 02, 0],
    /* WhiteRook */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 12, 11, 10, 09, 08, 0],
    /* WhiteBishop */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 18, 17, 16, 15, 14, 0],
    /* WhiteKnight */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 24, 23, 22, 21, 20, 0],
    /* WhitePawn */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 31, 30, 29, 28, 27, 26, 0],
    /* Pad */         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* Black Null */  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackKing */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackQueen */  [0, 07, 06, 05, 04, 03, 02, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackRook */   [0, 13, 12, 11, 10, 09, 08, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackBishop */ [0, 19, 18, 17, 16, 15, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackKnight */ [0, 25, 24, 23, 22, 21, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackPawn */   [0, 31, 30, 29, 28, 27, 26, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* Pad */         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
];

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

    move_list: [u16; 256],

    tt: &'a mut TranspositionTable,
    rt: RepetitionTable,
    et: EvalTable,
    history_moves: Box<[[i16; 64]; 16]>,

    params: Box<SearchParams>,

    score: Eval,
    b_cut_count: u64,
    b_cut_null_count: u64,
    a_raise_count: u64,
    node_count: u64,
    quiet_nodes: u64,
    quiet_depth: u32,
    depth: u8,

    pawn_correction_heuristic: [[Eval; u16::MAX as usize + 1]; 2],
    // pub corr_stats: Vec<CorrStats>,
}

impl<'a> SearchStrategy<'a> for Search<'a> {
    fn search(&mut self) -> u16 {
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
            self.ply = 0;

            // if self.params.debug {
            //     println!("Search depth {}", depth);
            // }

            let score = self.go(-Eval::MAX, Eval::MAX, depth);

            if self.is_stopping {
                break 'outer;
            }

            quiet_nodes = self.quiet_nodes;
            node_count = self.node_count;
            self.apply_pv(depth, score);
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
        tt: &'a mut TranspositionTable,
        rt: RepetitionTable,
    ) -> Search<'a> {
        let nnue = nnue_load!(net_path!(), NNUE_HSIZE, NNUE_OSIZE);

        let mut s = Search {
            nnue,
            sig: None,
            chess: chess_v2::ChessGame::new(),
            move_list: [0; 256],
            params: Box::new(params),
            tables,
            quiet_nodes: 0,
            quiet_depth: 0,
            node_count: 0,
            a_raise_count: 0,
            b_cut_count: 0,
            b_cut_null_count: 0,
            ply: 0,
            is_stopping: false,
            score: -Eval::MAX,
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
            et: EvalTable::new(1024),
            rt,
            tt,
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
        self.score = -Eval::MAX;
        self.ply = 0;
        self.b_cut_count = 0;
        self.b_cut_null_count = 0;
        self.a_raise_count = 0;
        self.depth = 0;

        for i in 0..PV_DEPTH {
            self.pv_table.lengths[i] = 0;
            for j in 0..PV_DEPTH {
                self.pv_table.moves[i][j] = 0;
            }
            self.pv[i] = 0;
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
    pub fn b_cut_count(&self) -> u64 {
        self.b_cut_count
    }
    #[inline(always)]
    pub fn b_cut_null_count(&self) -> u64 {
        self.b_cut_null_count
    }
    #[inline(always)]
    pub fn a_raise_count(&self) -> u64 {
        self.a_raise_count
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
    #[inline(always)]
    pub fn get_tt_mut(&mut self) -> &mut TranspositionTable {
        &mut self.tt
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

        // debug_assert_eq!(
        //     self.nnue.acc,
        //     {
        //         let mut expected_pair = nnue::AccumulatorPair::new();
        //         expected_pair.load(&self.chess, &self.nnue.net);
        //         expected_pair
        //     },
        //     "NNUE accumulator mismatch at depth {}, ply {}",
        //     depth,
        //     self.ply
        // );

        // Cut 2 plys before max PV depth since the previous call
        // can't copy over PV moves beyond this point
        let search_done = self.ply >= PV_DEPTH as u8 - 2 || depth == 0;
        let apply_pruning = !self.pv_trace && ply > 0;

        let mut pv_move = 0; // Null move
        let mut tt_move = 0; // Null move
        let mut bound_type = BoundType::UpperBound;
        let mut alpha = alpha;
        let mut depth = depth;

        // assert!(!(self.pv_trace && tt_move != 0));

        if apply_pruning {
            if self.chess.half_moves() >= 100 || self.rt.is_repeated(self.chess.zobrist_key()) {
                return 0;
            }

            let (score, mv) = self.tt.probe(self.chess.zobrist_key(), depth, alpha, beta);

            if let Some(score) = score {
                return score;
            }

            tt_move = mv;
        }

        if search_done {
            debug_assert!(!self.pv_trace);
            return self.quiescence(alpha, beta, ply as u32);
        }

        let in_check = self.chess.in_check(self.tables, self.chess.b_move());
        depth += in_check as u8;

        let depth_pruning = depth >= 3 && !in_check;

        if self.pv_trace {
            self.pv_trace = self.pv_length > (ply + 1);
            pv_move = self.pv[ply];
        }

        let static_eval = OnceCell::new();

        if apply_pruning {
            if !in_check {
                // Reverse futility pruning
                if depth < 3 {
                    let eval_margin = 150 * depth as Eval;
                    let static_eval = *static_eval.get_or_init(|| self.evaluate());

                    let eval_corrected = self.eval_with_correction(static_eval);

                    if eval_corrected - eval_margin >= beta {
                        return eval_corrected - eval_margin;
                    }
                }
            }

            // Null move pruning
            if depth_pruning && !self.is_endgame() {
                let ep_square = self.chess.make_null_move(self.tables);

                self.ply += 1;
                let score = -self.go(-beta, -beta + 1, depth - 3);
                self.ply -= 1;

                self.chess.rollback_null_move(ep_square, self.tables);

                // debug_assert_eq!(
                //     self.nnue.acc,
                //     {
                //         let mut expected_pair = nnue::AccumulatorPair::new();
                //         expected_pair.load(&self.chess, &self.nnue.net);
                //         expected_pair
                //     },
                //     "NNUE accumulator mismatch after null move",
                // );

                if self.is_stopping {
                    return 0;
                }

                if score >= beta {
                    self.b_cut_null_count += 1;
                    return score;
                }
            }
        }

        self.rt
            .push_position(self.chess.zobrist_key(), self.chess.half_moves() == 0);

        let mut move_count = self
            .chess
            .gen_moves_avx512::<false>(self.tables, &mut self.move_list);

        std::hint::likely(move_count > 8 && move_count < 64);

        unsafe {
            // Safety: maximum number of legal moves in any position is 218.
            // Generated move count is guaranteed to be within bounds of 248 assuming
            // few possible pseudolegal moves like castling or moving into a check
            debug_assert!(move_count < 248);
            std::hint::assert_unchecked(move_count < 248);
        }

        // let black_board = self
        //     .chess
        //     .bitboards()
        //     .iter()
        //     .skip(8)
        //     .fold(0, |acc, &bb| acc | bb);
        // let white_board = self
        //     .chess
        //     .bitboards()
        //     .iter()
        //     .take(8)
        //     .fold(0, |acc, &bb| acc | bb);
        // let mut piece_board: [u64; 8] = [0u64; 8];
        // self.chess
        //     .bitboards()
        //     .iter()
        //     .take(8)
        //     .zip(self.chess.bitboards().iter().skip(8))
        //     .enumerate()
        //     .for_each(|(index, (w, b))| piece_board[index] = *w | *b);

        let mut move_list = [0u32; 256];
        for i in 0..move_count {
            move_list[i] = self.score_move_desc32(self.move_list[i], pv_move, tt_move);
        }
        sorting::u32::sort_256u32_desc_avx512(&mut move_list, move_count);
        debug_assert!(
            tt_move == 0
                || !move_list[0..move_count]
                    .iter()
                    .any(|mv| *mv as u16 == tt_move)
                || (move_list[0] as u16) == tt_move
        );
        debug_assert!(pv_move == 0 || move_list[0] as u16 == pv_move);

        let mut best_score = -Eval::MAX;
        let mut best_move = 0;
        let mut num_legal_moves = 0;

        let board_copy = self.chess.clone();

        let mut i = 0;
        while i < move_count {
            let mv_index = i;
            let mv = move_list[mv_index] as u16;
            i += 1;

            unsafe {
                // Safety: i is guaranteed to be less than 256 from the loop condition,
                // i -= 1 guarantees i < 256
                debug_assert!(i < 256);
                std::hint::assert_unchecked(i < 256);
            }

            if (mv & MV_FLAGS_PR_MASK) == MV_FLAGS_PR_QUEEN {
                i -= 1;

                let mv_unpromoted = (mv & !MV_FLAGS_PR_MASK) as u32;
                unsafe {
                    // Second promotion to check, override current slot
                    *move_list.get_unchecked_mut(i) = mv_unpromoted | MV_FLAGS_PR_KNIGHT as u32;

                    // Third promotion to check, append to the end of the list
                    *move_list.get_unchecked_mut(move_count) =
                        mv_unpromoted | MV_FLAGS_PR_BISHOP as u32;
                    move_count += 1;

                    // Fourth promotion to check, append to the end of the list
                    *move_list.get_unchecked_mut(move_count) =
                        mv_unpromoted | MV_FLAGS_PR_ROOK as u32;
                    move_count += 1;
                }
            }

            let nnue_update = unsafe {
                // Safety: mv is generated by gen_moves_avx512, so it is guaranteed to be legal
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

            // if apply_pruning && depth_pruning && !self.is_endgame() {
            //     let threshold = if mv & MV_FLAG_CAP != 0 {
            //         -50 * depth as i16
            //     } else {
            //         -10 * depth as i16 * depth as i16
            //     };

            //     if !see::see_threshold(
            //         &WEIGHT_TABLE_ABS,
            //         self.tables,
            //         &board_copy,
            //         mv,
            //         threshold,
            //         black_board,
            //         white_board,
            //         piece_board,
            //     ) {
            //         if !self.chess.in_check(self.tables, self.chess.b_move()) {
            //             self.chess = board_copy;
            //             continue;
            //         }
            //     }
            // }

            self.nnue.make_move(nnue_update);

            let is_non_capture_or_promotion = mv & (MV_FLAG_CAP | MV_FLAG_PROMOTION) == 0;

            let late_move_reduction =
                num_legal_moves > 3 && depth_pruning && is_non_capture_or_promotion;

            self.ply += 1;

            let score = if num_legal_moves == 0 {
                -self.go(-beta, -alpha, depth - 1)
            } else if late_move_reduction {
                // Late move, apply a small reduction of 1 ply and
                // search with window [-alpha - 1, -alpha] with the goal of
                // proving that the move is not good enough to be played
                let proof_score = -self.go(-alpha - 1, -alpha, depth - 2);
                if proof_score > alpha {
                    // The move might be good, search it again with full depth
                    // Null search with fallback to full search
                    let proof_score = -self.go(-alpha - 1, -alpha, depth - 1);

                    if proof_score > alpha && proof_score < beta {
                        -self.go(-beta, -alpha, depth - 1)
                    } else {
                        proof_score
                    }
                } else {
                    proof_score
                }
            } else {
                // Null search with fallback to full search
                let proof_score = -self.go(-alpha - 1, -alpha, depth - 1);

                if proof_score > alpha && proof_score < beta {
                    -self.go(-beta, -alpha, depth - 1)
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

            self.a_raise_count += 1;
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

                if is_non_capture {
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

                    self.update_history_heuristic(
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
                    }
                }

                if !in_check && best_move & MV_FLAG_CAP == 0 {
                    let static_eval = *static_eval.get_or_init(|| self.evaluate());
                    if score >= static_eval {
                        self.update_correction_heuristics(score - static_eval, depth as Eval);
                    }
                }

                self.tt.store(
                    self.chess.zobrist_key(),
                    score,
                    depth,
                    mv,
                    BoundType::LowerBound,
                );
                self.rt.pop_position();
                self.b_cut_count += 1;
                return score;
            }
        }
        self.rt.pop_position();

        if !in_check && best_move & MV_FLAG_CAP == 0 {
            let static_eval = *static_eval.get_or_init(|| self.evaluate());

            let update_score = match bound_type {
                BoundType::UpperBound if best_move == 0 => Some(0),
                BoundType::UpperBound if best_score <= static_eval => Some(best_score),
                BoundType::Exact => Some(best_score),
                _ => None,
            };

            if let Some(score) = update_score {
                self.update_correction_heuristics(score - static_eval, depth as Eval);
            }
        }

        if num_legal_moves == 0 {
            if in_check {
                return -SCORE_INF + self.ply as Eval;
            }

            // Stalemate
            return 0;
        }

        self.tt.store(
            self.chess.zobrist_key(),
            best_score,
            depth,
            best_move,
            bound_type,
        );

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

        // let debug_position = self
        //     .chess
        //     .gen_fen()
        //     .starts_with("r3k2r/p2pqpb1/bp2pnp1/3P4/4P3/2p1BQ1p/PPP1BPPP/R3K2R w");

        // debug_assert_eq!(
        //     self.nnue.acc,
        //     {
        //         let mut expected_pair = nnue::AccumulatorPair::new();
        //         expected_pair.load(&self.chess, &self.nnue.net);
        //         expected_pair
        //     },
        //     "NNUE accumulator mismatch in q search at ply {}",
        //     self.ply
        // );

        let raw_eval = self.evaluate();
        let static_eval = self.eval_with_correction(raw_eval);

        let mut alpha = alpha;

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

        let move_count = self
            .chess
            .gen_moves_avx512::<true>(self.tables, &mut self.move_list);

        std::hint::likely(move_count < 64);

        unsafe {
            // Safety: move count should not overflow
            debug_assert!(move_count < 256);
            std::hint::assert_unchecked(move_count < 256);
        }

        let mut move_list = [0u32; 256];
        for i in 0..move_count {
            move_list[i] = self.score_move_desc32_quiescence(self.move_list[i]);
        }
        sorting::u32::sort_256u32_desc_avx512(&mut move_list, move_count);

        // let mut move_scores = [0xFFFFu16; 256];
        // for i in 0..move_count {
        //     let mv = self.move_list[i];
        //     move_scores[i] = self.score_move_mvvlva_asc_quiescence(i as u8, mv);
        // }
        // sorting::u16::sort_256x16_asc_avx512(&mut move_scores, move_count);
        // let mut move_list = [0u16; 256];
        // for i in 0..move_count {
        //     move_list[i] = self.move_list[(move_scores[i] & 0xFF) as usize];
        // }

        let mut best_score: Eval = static_eval;
        let board_copy = self.chess.clone();

        let mut i = 0;
        while i < move_count {
            let mv = move_list[i] as u16;
            i += 1;

            // Quiescence search can't encounter new captures after queen or knight promotions, so
            // underpromotions to bishop and rook are skipped
            if (mv & MV_FLAGS_PR_MASK) == MV_FLAGS_PR_QUEEN {
                i -= 1;

                let mv_unpromoted = mv & !MV_FLAGS_PR_MASK;
                move_list[i] = (mv_unpromoted | MV_FLAGS_PR_KNIGHT) as u32; // Second promotion to check
            }

            let nnue_update = unsafe {
                // Safety: mv is generated by gen_moves_avx512, so it is guaranteed to be legal
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
                    // self.b_cut_count += 1;
                    return score;
                }

                if score > alpha {
                    // self.a_raise_count += 1;
                    alpha = score;
                }
            }
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
    fn update_correction_heuristics(&mut self, diff: Eval, depth: i16) {
        let diff_scaled = diff * CORR_GRAIN;

        let depth_weight = (depth * depth + 2 * depth + 1).min(CORR_WEIGHT_SCALE) as Eval;

        Self::update_correction_heuristic_entry(
            self.pawn_correction_heuristic_entry(),
            diff_scaled,
            depth_weight,
        );
    }

    #[inline(always)]
    fn update_correction_heuristic_entry(entry: &mut Eval, diff_scaled: Eval, depth_weight: Eval) {
        let mut current_value = *entry as Eval;

        current_value = (current_value * (CORR_WEIGHT_SCALE - depth_weight)
            + diff_scaled * depth_weight)
            / CORR_WEIGHT_SCALE;

        current_value = current_value.max(-CORR_MAX_ERROR).min(CORR_MAX_ERROR);

        *entry = current_value as Eval;
    }

    #[inline(always)]
    pub fn eval_with_correction(&mut self, base_eval: Eval) -> Eval {
        let pawn_corr = *self.pawn_correction_heuristic_entry();

        let mut correction = 0;
        correction += pawn_corr / CORR_GRAIN;

        // self.corr_stats.push(CorrStats {
        //     original_eval: base_eval,
        //     error: correction,
        // });

        (base_eval + correction)
            .min(SCORE_INF - PV_DEPTH as i16)
            .max((-SCORE_INF) + PV_DEPTH as i16)
    }

    #[inline(always)]
    pub fn is_endgame(&self) -> bool {
        let material = self.chess.material();
        ((material[0] & (!1023)) | (material[1] & (!1023))) == 0
    }

    #[inline(always)]
    pub fn calc_board_phase(&self) -> f32 {
        let material = self.chess.material();

        const MAT_MAX: u16 = 5320; // 5600 - 4 pawns
        const MAT_MIN: u16 = 280; // minor piece + pawn

        (((material[0] + material[1] + MAT_MIN) as f32) / ((MAT_MAX + MAT_MIN) as f32))
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
        if EVAL_NNUE {
            // if let Some(eval) = self.et.probe(self.chess.zobrist_key()) {
            //     return eval;
            // }
            let final_score =
                self.nnue
                    .evaluate(self.chess.b_move(), self.output_bucket()) as Eval;
            // self.et.store(self.chess.zobrist_key(), final_score);
            return final_score;
        } else {
            if let Some(eval) = self.et.probe(self.chess.zobrist_key()) {
                return eval;
            }
            let final_score = self.eval_hc();
            self.et.store(self.chess.zobrist_key(), final_score);
            return final_score;
        }
    }

    #[inline(always)]
    fn eval_hc(&self) -> Eval {
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
    fn score_move_desc32_quiescence(&self, mv: u16) -> u32 {
        macro_rules! score {
            ($score:expr) => {
                (mv as u32) | (($score as u32) << 16)
            };
        }

        let src_sq = mv & 0x3F;
        let dst_sq = (mv >> 6) & 0x3F;

        // MVV-LVA
        let mvvlva_score = unsafe {
            let spt = self.chess.spt();
            let dst_piece = *spt.get_unchecked(dst_sq as usize);
            let src_piece = *spt.get_unchecked(src_sq as usize);

            let mvvlva_score = *MVV_LVA_SCORES_U8
                .get_unchecked(dst_piece as usize)
                .get_unchecked(src_piece as usize) as u16;

            31 - mvvlva_score
        };

        score!(mvvlva_score)
    }

    #[inline(always)]
    fn score_move_desc32(&self, mv: u16, pv_move: u16, tt_move: u16) -> u32 {
        macro_rules! score {
            ($score:expr) => {
                (mv as u32) | (($score as u32) << 16)
            };
        }

        if mv == pv_move {
            return score!(SORT_PVTT_BASE + 1);
        }

        if mv == tt_move {
            return score!(SORT_PVTT_BASE);
        }

        let spt = self.chess.spt();

        let src_sq = mv & 0x3F;
        let dst_sq = (mv >> 6) & 0x3F;

        if (mv & MV_FLAG_CAP) == 0 {
            unsafe {
                const MAX_HISTORY_SCORE: u16 = SORT_CAPTURE_BASE - 1;

                // Safety:
                // - src_sq and dst_sq are always < 64
                // - src_piece is a PieceIndex < 16
                let src_piece = *spt.get_unchecked(src_sq as usize) as usize;

                let history_score = *self
                    .history_moves
                    .get_unchecked(src_piece)
                    .get_unchecked(dst_sq as usize);

                let history_score =
                    (history_score as i32 + HISTORY_MIN.abs() as i32) as u16 + SORT_QUIET_BASE;

                debug_assert!(
                    history_score >= SORT_QUIET_BASE,
                    "history score = {}, clamped score = {}",
                    *self
                        .history_moves
                        .get_unchecked(src_piece)
                        .get_unchecked(dst_sq as usize),
                    history_score
                );
                debug_assert!(
                    history_score <= MAX_HISTORY_SCORE,
                    "history score = {}, clamped score = {}",
                    *self
                        .history_moves
                        .get_unchecked(src_piece)
                        .get_unchecked(dst_sq as usize),
                    history_score
                );

                return score!(history_score);
            }
        }

        // MVV-LVA
        let mvvlva_score = unsafe {
            let spt = self.chess.spt();
            let dst_piece = *spt.get_unchecked(dst_sq as usize);
            let src_piece = *spt.get_unchecked(src_sq as usize);

            let mvvlva_score = *MVV_LVA_SCORES_U8
                .get_unchecked(dst_piece as usize)
                .get_unchecked(src_piece as usize) as u16;

            31 - mvvlva_score
        };

        debug_assert!(
            SORT_CAPTURE_BASE + mvvlva_score < SORT_PVTT_BASE,
            "mvv-lva score = {}, clamped score = {}",
            mvvlva_score,
            SORT_CAPTURE_BASE + mvvlva_score
        );

        score!(SORT_CAPTURE_BASE + mvvlva_score)
    }

    #[inline(always)]
    fn update_history_heuristic(&mut self, piece_index: usize, dst_sq: usize, bonus: i16) {
        let entry = &mut self.history_moves[piece_index][dst_sq];
        let ratio = ((*entry as i64) * bonus.abs() as i64 / HISTORY_MAX as i64) as i16;
        let value = (*entry + (bonus - ratio)).clamp(-HISTORY_MAX, HISTORY_MAX);
        *entry = value;
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
        let move_count = board.gen_moves_avx512::<false>(self.tables, &mut move_list);

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
}
