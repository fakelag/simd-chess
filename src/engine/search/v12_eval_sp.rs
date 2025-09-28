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
            see, sorting,
            transposition_v2::{BoundType, TranspositionTable},
        },
        tables::{self},
    },
    nnue::nnue::{self, UpdatableNnue},
    nnue_lazy_load,
};

const PV_DEPTH: usize = 64;

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

struct Scratch {
    captures: [[u16; 32]; 32],
    cursors: [*mut i16; 32],
}

pub struct Search<'a> {
    chess: ChessGame,
    tables: &'a tables::Tables,
    sig: Option<AbortSignal>,

    nnue: Box<nnue::LazyNnue>,

    ply: u8,
    is_stopping: bool,

    pv_table: Box<PvTable>,
    pv: [u16; PV_DEPTH],
    pv_length: usize,
    pv_trace: bool,

    move_list: [u16; 256],

    tt: &'a mut TranspositionTable,
    rt: RepetitionTable,
    // et: EvalTable,
    /// Quiet moves that caused a β-cutoff, indexed by ply
    beta_moves: Box<[[u16; 2]; PV_DEPTH]>,

    /// Depth-based scores of quiet moves that raised the α bound, indexed by `[piece_id][dst_square]`.
    /// When a quiet move raises alpha, a bonus is added to the score, higher score prioritises the move
    /// against other quiet moves.
    alpha_moves: Box<[[u8; 64]; 16]>,

    params: Box<SearchParams>,

    score: Eval,
    b_cut_count: u64,
    b_cut_null_count: u64,
    a_raise_count: u64,
    node_count: u64,
    quiet_nodes: u64,
    quiet_depth: u32,
    depth: u8,
}

impl<'a> SearchStrategy<'a> for Search<'a> {
    fn search(&mut self) -> u16 {
        let mut node_count = 0;
        let mut quiet_nodes = 0;

        let max_depth = PV_DEPTH as u8 - 1;
        let target_depth = self.params.depth.unwrap_or(max_depth).min(max_depth);

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
        let mut s = Search {
            nnue: nnue_lazy_load!("../../../nnue/y2.bin"), // nnue_load!("../../../nnue/y2.bin"),
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
            beta_moves: Box::new([[0; 2]; PV_DEPTH]),
            alpha_moves: Box::new([[0; 64]; 16]),
            // et: EvalTable::new(512),
            rt,
            tt,
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
            self.beta_moves[i] = [0; 2];
            self.pv[i] = 0;
        }

        for i in 0..16 {
            for j in 0..64 {
                self.alpha_moves[i][j] = 0;
            }
        }

        self.tt.new_search();
    }

    /// A hard reset. Clears all counters, the transposition and repetition tables.
    /// and resets the board position to the starting position.
    #[inline(always)]
    pub fn new_game_from_fen(
        &mut self,
        fen: &str,
        tables: &tables::Tables,
    ) -> Result<usize, String> {
        self.rt.clear();
        self.tt.clear();
        let fen_length = self.chess.load_fen(fen, tables)?;
        self.nnue.load(&self.chess);
        Ok(fen_length)
    }

    /// Resets the board & nnue state without clearing the transposition or repetition tables.
    #[inline(always)]
    pub fn new_game_from_board(&mut self, board: &ChessGame) {
        self.chess = *board;
        self.nnue.load(&self.chess);
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

        self.rt
            .push_position(self.chess.zobrist_key(), self.chess.half_moves() == 0);

        if self.pv_trace {
            self.pv_trace = self.pv_length > (ply + 1);
            pv_move = self.pv[ply];
        }

        let mut static_eval = None;

        // Reverse futility pruning
        if apply_pruning && depth < 3 && !in_check {
            let eval_margin = 150 * depth as Eval;
            let eval = self.evaluate();

            if eval - eval_margin >= beta {
                self.rt.pop_position();
                return eval - eval_margin;
            }

            static_eval = Some(eval);
        }

        // Null move pruning
        if apply_pruning && depth_pruning && !self.is_endgame() {
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
                self.rt.pop_position();
                return 0;
            }

            if score >= beta {
                self.rt.pop_position();
                self.b_cut_null_count += 1;
                return score;
            }
        }

        let move_count = self
            .chess
            .gen_moves_avx512::<false>(self.tables, &mut self.move_list);

        std::hint::likely(move_count > 8 && move_count < 64);

        unsafe {
            // Safety: maximum number of legal moves in any position is 218.
            // Generated move count is guaranteed to be within bounds of 254 assuming
            // few possible pseudolegal moves like castling or moving into a check
            debug_assert!(move_count < 254);
            std::hint::assert_unchecked(move_count < 254);
        }

        let mut move_scores = [0xFFFFu16; 256];
        for i in 0..move_count {
            let mv = self.move_list[i];
            move_scores[i] = self.score_move_mvvlva_asc(i as u8, mv, pv_move, tt_move);
        }

        sorting::sort_256x16_avx512(&mut move_scores, move_count);

        let mut move_list = [0u16; 256];
        for i in 0..move_count {
            let mv_index = i + 2;
            move_list[mv_index] = self.move_list[(move_scores[i] & 0xFF) as usize];
        }

        let mut best_move = 0;
        let mut num_legal_moves = 0;

        let board_copy = self.chess.clone();

        let mut i = 2;
        while i < move_count + 2 {
            let mv = move_list[i];
            i += 1;

            if (mv & MV_FLAGS_PR_MASK) == MV_FLAGS_PR_QUEEN {
                i -= 3;

                unsafe {
                    // Safety: i is guaranteed to be less than 256 from the loop condition,
                    // i -= 3 guarantees i < 254
                    debug_assert!(i < 254);
                    std::hint::assert_unchecked(i < 254);
                }

                let mv_unpromoted = mv & !MV_FLAGS_PR_MASK;
                move_list[i] = mv_unpromoted | MV_FLAGS_PR_KNIGHT; // Second promotion to check
                move_list[i + 1] = mv_unpromoted | MV_FLAGS_PR_ROOK; // Third promotion to check
                move_list[i + 2] = mv_unpromoted | MV_FLAGS_PR_BISHOP; // Fourth promotion to check
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

            if score <= alpha {
                continue;
            }

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

                self.alpha_moves[src_piece][dst_square] =
                    self.alpha_moves[src_piece][dst_square].saturating_add(depth as u8);
            }

            self.a_raise_count += 1;
            bound_type = BoundType::Exact;
            alpha = score;
            best_move = mv;

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
                if is_non_capture {
                    let beta_moves = &mut self.beta_moves[ply];
                    beta_moves[1] = beta_moves[0];
                    beta_moves[0] = mv;
                }

                // @todo - test fail-hard vs fail-soft
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

        if num_legal_moves == 0 {
            if in_check {
                return -SCORE_INF + self.ply as Eval;
            }

            // Stalemate
            return 0;
        }

        if ply == 0 && num_legal_moves == 1 {
            self.apply_pv(depth, alpha);
            self.is_stopping = true;
            return alpha;
        }

        self.tt.store(
            self.chess.zobrist_key(),
            alpha,
            depth,
            best_move,
            bound_type,
        );

        alpha
    }

    fn quiescence(&mut self, alpha: Eval, beta: Eval, start_ply: u32) -> Eval {
        self.node_count += 1;
        self.quiet_nodes += 1;
        self.quiet_depth = self.quiet_depth.max(self.ply as u32 - start_ply);

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
        //     "NNUE accumulator mismatch in q search at ply {}",
        //     self.ply
        // );

        let static_eval = self.evaluate();

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

        let mut move_scores = [0xFFFFu16; 256];
        for i in 0..move_count {
            let mv = self.move_list[i];
            move_scores[i] = self.score_move_mvvlva_asc_quiescence(i as u8, mv);
        }

        sorting::sort_256x16_avx512(&mut move_scores, move_count);

        let mut move_list = [0u16; 256];
        for i in 0..move_count {
            move_list[i] = self.move_list[(move_scores[i] & 0xFF) as usize];
        }

        let mut best_score: Eval = static_eval;
        let board_copy = self.chess.clone();

        let mut i = 0;
        while i < move_count {
            let mv = move_list[i];
            i += 1;

            // Quiescence search can't encounter new captures after queen or knight promotions, so
            // underpromotions to bishop and rook are skipped
            if (mv & MV_FLAGS_PR_MASK) == MV_FLAGS_PR_QUEEN {
                i -= 1;

                let mv_unpromoted = mv & !MV_FLAGS_PR_MASK;
                move_list[i] = mv_unpromoted | MV_FLAGS_PR_KNIGHT; // Second promotion to check
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
    pub fn evaluate(&mut self) -> Eval {
        // if let Some(eval) = self.et.probe(self.chess.zobrist_key()) {
        //     return eval;
        // }

        let final_score = self.nnue.evaluate(self.chess.b_move()) as Eval;

        // self.et.store(self.chess.zobrist_key(), final_score);

        return final_score;

        // use std::arch::x86_64::*;

        // let bitboards = self.chess.bitboards_new();

        // let mut bonuses_mg: Eval = 0;
        // let mut bonuses_eg: Eval = 0;

        // unsafe {
        //     let mut bonuses_mg_x64 = _mm512_setzero_si512();
        //     let mut bonuses_eg_x64 = _mm512_setzero_si512();

        //     macro_rules! acc_piece_bonus_avx512 {
        //         ($bonus_vec:ident,$is_eg:expr,$piece_id:expr) => {
        //             $bonus_vec = _mm512_mask_blend_epi8(
        //                 bitboards[$piece_id as usize],
        //                 $bonus_vec,
        //                 _mm512_loadu_epi8(
        //                     EVAL_TABLES_INV_MGEG.0[$is_eg][$piece_id as usize].as_ptr()
        //                         as *const i8,
        //                 ),
        //             )
        //         };
        //     }

        //     macro_rules! sum_bonus_avx512 {
        //         ($bonus_acc:ident,$bonus_vec:ident) => {
        //             let bonuses_lsb = _mm512_castsi512_si256($bonus_vec);
        //             let bonuses_msb = _mm512_extracti64x4_epi64($bonus_vec, 1);
        //             let bonuses_lsb_i16 = _mm512_cvtepi8_epi16(bonuses_lsb);
        //             let bonuses_msb_i16 = _mm512_cvtepi8_epi16(bonuses_msb);
        //             let summed_bonuses = _mm512_add_epi16(bonuses_lsb_i16, bonuses_msb_i16);
        //             let summed_bonuses_lsb = _mm512_castsi512_si256(summed_bonuses);
        //             let summed_bonuses_msb = _mm512_extracti64x4_epi64(summed_bonuses, 1);

        //             $bonus_acc += _mm256_reduce_add_epi16(summed_bonuses_lsb) as Eval;
        //             $bonus_acc += _mm256_reduce_add_epi16(summed_bonuses_msb) as Eval;
        //         };
        //     }

        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteKing);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteQueen);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteRook);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteBishop);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhiteKnight);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::WhitePawn);

        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackKing);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackQueen);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackRook);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackBishop);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackKnight);
        //     acc_piece_bonus_avx512!(bonuses_mg_x64, 0, PieceIndex::BlackPawn);

        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteKing);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteQueen);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteRook);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteBishop);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhiteKnight);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::WhitePawn);

        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackKing);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackQueen);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackRook);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackBishop);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackKnight);
        //     acc_piece_bonus_avx512!(bonuses_eg_x64, 1, PieceIndex::BlackPawn);

        //     sum_bonus_avx512!(bonuses_mg, bonuses_mg_x64);
        //     sum_bonus_avx512!(bonuses_eg, bonuses_eg_x64);
        // }

        // let wb = bitboards[PieceIndex::WhiteBishop as usize].count_ones();
        // let bb = bitboards[PieceIndex::BlackBishop as usize].count_ones();

        // let wq_bq = bitboards[PieceIndex::WhiteQueen as usize].count_ones() as Eval
        //     - bitboards[PieceIndex::BlackQueen as usize].count_ones() as Eval;

        // let wr_br = bitboards[PieceIndex::WhiteRook as usize].count_ones() as Eval
        //     - bitboards[PieceIndex::BlackRook as usize].count_ones() as Eval;

        // let wb_bb = wb as Eval - bb as Eval;

        // let wn_bn = bitboards[PieceIndex::WhiteKnight as usize].count_ones() as Eval
        //     - bitboards[PieceIndex::BlackKnight as usize].count_ones() as Eval;

        // let wp_bp = bitboards[PieceIndex::WhitePawn as usize].count_ones() as Eval
        //     - bitboards[PieceIndex::BlackPawn as usize].count_ones() as Eval;

        // let mut score_mg = bonuses_mg
        //     + wq_bq * WEIGHT_TABLE_MGEG[0][PieceIndex::WhiteQueen as usize]
        //     + wr_br * WEIGHT_TABLE_MGEG[0][PieceIndex::WhiteRook as usize]
        //     + wb_bb * WEIGHT_TABLE_MGEG[0][PieceIndex::WhiteBishop as usize]
        //     + wn_bn * WEIGHT_TABLE_MGEG[0][PieceIndex::WhiteKnight as usize]
        //     + wp_bp * WEIGHT_TABLE_MGEG[0][PieceIndex::WhitePawn as usize];

        // let mut score_eg = bonuses_eg
        //     + wq_bq * WEIGHT_TABLE_MGEG[1][PieceIndex::WhiteQueen as usize]
        //     + wr_br * WEIGHT_TABLE_MGEG[1][PieceIndex::WhiteRook as usize]
        //     + wb_bb * WEIGHT_TABLE_MGEG[1][PieceIndex::WhiteBishop as usize]
        //     + wn_bn * WEIGHT_TABLE_MGEG[1][PieceIndex::WhiteKnight as usize]
        //     + wp_bp * WEIGHT_TABLE_MGEG[1][PieceIndex::WhitePawn as usize];

        // let phase = self.calc_board_phase();

        // if wb > 1 {
        //     score_mg += 20;
        //     score_eg += 40;
        // }
        // if bb > 1 {
        //     score_mg -= 20;
        //     score_eg -= 40;
        // }

        // let mut white_pawns = bitboards[PieceIndex::WhitePawn as usize];
        // loop {
        //     let pawn_sq = pop_ls1b!(white_pawns);
        //     let file = (0x101010101010101 << 8) << pawn_sq;
        //     let file_left = (file >> 1) & tables::EX_H_FILE;
        //     let file_right = (file << 1) & tables::EX_A_FILE;
        //     let full_mask = file | file_left | file_right;

        //     let rank_bonus = [10, 10, 20, 50, 90, 100, 0][(pawn_sq / 8) as usize];

        //     if full_mask & bitboards[PieceIndex::BlackPawn as usize] == 0 {
        //         score_mg += rank_bonus;
        //         score_eg += rank_bonus;
        //     }
        // }

        // let mut black_pawns = bitboards[PieceIndex::BlackPawn as usize];
        // loop {
        //     let pawn_sq = pop_ls1b!(black_pawns);
        //     let file = (0x101010101010101u64).wrapping_shr((64 - pawn_sq) as u32);
        //     let file_left = (file >> 1) & tables::EX_H_FILE;
        //     let file_right = (file << 1) & tables::EX_A_FILE;
        //     let full_mask = file | file_left | file_right;

        //     let rank_bonus = [0, 100, 90, 50, 20, 10, 10][(pawn_sq / 8) as usize];

        //     if full_mask & bitboards[PieceIndex::WhitePawn as usize] == 0 {
        //         score_mg -= rank_bonus;
        //         score_eg -= rank_bonus;
        //     }
        // }

        // // King safety
        // let attacker_bonuses = [10, 30, 60, 100, 200];
        // let attacker_bonuses_eg = [5, 10, 20, 40, 100];
        // let white_king_sq = bitboards[PieceIndex::WhiteKing as usize].trailing_zeros() as u8;
        // let mut white_king_surround_squares =
        //     tables::Tables::LT_KING_MOVE_MASKS[white_king_sq as usize];
        // loop {
        //     let king_surround_sq = pop_ls1b!(white_king_surround_squares);

        //     let num_attackers = self.chess.count_king_square_attackers_slow(
        //         king_surround_sq as u8,
        //         false,
        //         self.tables,
        //     );

        //     score_mg -= attacker_bonuses[num_attackers.min(4)];
        //     score_eg -= attacker_bonuses_eg[num_attackers.min(4)];
        // }
        // let black_king_sq = bitboards[PieceIndex::BlackKing as usize].trailing_zeros() as u8;
        // let mut black_king_surround_squares =
        //     tables::Tables::LT_KING_MOVE_MASKS[black_king_sq as usize];
        // loop {
        //     let king_surround_sq = pop_ls1b!(black_king_surround_squares);

        //     let num_attackers = self.chess.count_king_square_attackers_slow(
        //         king_surround_sq as u8,
        //         true,
        //         self.tables,
        //     );

        //     score_mg += attacker_bonuses[num_attackers.min(4)];
        //     score_eg += attacker_bonuses_eg[num_attackers.min(4)];
        // }

        // let w_moves = self.chess.estimate_move_count(false, &self.tables);
        // let b_moves = self.chess.estimate_move_count(true, &self.tables);
        // let move_ratio = w_moves as Eval - b_moves as Eval;

        // // let all_pawns =
        // //     bitboards[PieceIndex::WhitePawn as usize] | bitboards[PieceIndex::BlackPawn as usize];

        // let mut score =
        //     ((score_mg as f32 * phase + score_eg as f32 * (1.0 - phase)) as Eval) + move_ratio * 5;

        // score += if self.chess.b_move() { -5 } else { 5 };

        // let final_score = score * if self.chess.b_move() { -1 } else { 1 };

        // // assert!(
        // //     self.evaluate_legacy() == final_score,
        // //     "Legacy and simd evaluation mismatch for {} != {}. b_move= {}",
        // //     self.evaluate_legacy(),
        // //     final_score,
        // //     self.chess.b_move()
        // // );

        // final_score
    }

    #[inline(always)]
    fn score_move_mvvlva_asc(&self, index: u8, mv: u16, pv_move: u16, tt_move: u16) -> u16 {
        let index = index as u16;

        if mv == pv_move {
            return index + (0 << 8);
        }

        if mv == tt_move {
            return index + (1 << 8);
        }

        let spt = self.chess.spt();

        let src_sq = mv & 0x3F;
        let dst_sq = (mv >> 6) & 0x3F;

        if (mv & MV_FLAG_CAP) == 0 {
            unsafe {
                // Safety: self.ply is always < PV_DEPTH during main (non-q) search
                let beta_mv0 = self.beta_moves.get_unchecked(self.ply as usize)[0];
                let beta_mv1 = self.beta_moves.get_unchecked(self.ply as usize)[1];

                if mv == beta_mv0 {
                    return index | (35 << 8);
                }

                if mv == beta_mv1 {
                    return index | (36 << 8);
                }

                // Safety:
                // - src_sq and dst_sq are always < 64
                // - src_piece is a PieceIndex < 16
                let src_piece = *spt.get_unchecked(src_sq as usize) as usize;

                let alpha_score = *self
                    .alpha_moves
                    .get_unchecked(src_piece)
                    .get_unchecked(dst_sq as usize);

                return index | ((37 + 218u16.saturating_sub(alpha_score as u16)) << 8);
            }
        }

        // MVV-LVA
        unsafe {
            let spt = self.chess.spt();
            let dst_piece = *spt.get_unchecked(dst_sq as usize);
            let src_piece = *spt.get_unchecked(src_sq as usize);

            let mvvlva_score = *MVV_LVA_SCORES_U8
                .get_unchecked(dst_piece as usize)
                .get_unchecked(src_piece as usize) as u16;

            return index | (mvvlva_score << 8);
        }
    }

    #[inline(always)]
    fn score_move_mvvlva_asc_quiescence(&self, index: u8, mv: u16) -> u16 {
        let index = index as u16;
        let src_sq = mv & 0x3F;
        let dst_sq = (mv >> 6) & 0x3F;

        // MVV-LVA
        unsafe {
            let spt = self.chess.spt();
            let dst_piece = *spt.get_unchecked(dst_sq as usize);
            let src_piece = *spt.get_unchecked(src_sq as usize);

            let mvvlva_score = *MVV_LVA_SCORES_U8
                .get_unchecked(dst_piece as usize)
                .get_unchecked(src_piece as usize) as u16;

            return index | (mvvlva_score << 8);
        }
    }

    // SEE based move ordering
    #[inline(always)]
    fn score_move_see_asc(
        &self,
        index: u8,
        mv: u16,
        pv_move: u16,
        tt_move: u16,
        black_board: u64,
        white_board: u64,
        piece_board: [u64; 8],
    ) -> u16 {
        let index = index as u16;

        if mv == pv_move {
            return index + (0 << 8);
        }

        if mv == tt_move {
            return index + (1 << 8);
        }

        let see_score = see::static_exchange_eval(
            &WEIGHT_TABLE_ABS_I8,
            self.tables,
            &self.chess,
            black_board,
            white_board,
            piece_board,
            mv,
        );

        let spt = self.chess.spt();

        let src_sq = mv & 0x3F;
        let dst_sq = (mv >> 6) & 0x3F;

        if (mv & MV_FLAG_CAP) == 0 {
            unsafe {
                // Safety: self.ply is always < PV_DEPTH during main (non-q) search
                let beta_mv0 = self.beta_moves.get_unchecked(self.ply as usize)[0];
                let beta_mv1 = self.beta_moves.get_unchecked(self.ply as usize)[1];

                if mv == beta_mv0 {
                    return index | (35 << 8);
                }

                if mv == beta_mv1 {
                    return index | (36 << 8);
                }

                // Safety:
                // - src_sq and dst_sq are always < 64
                // - src_piece is a PieceIndex < 16
                let src_piece = *spt.get_unchecked(src_sq as usize) as usize;

                let alpha_score = *self
                    .alpha_moves
                    .get_unchecked(src_piece)
                    .get_unchecked(dst_sq as usize);

                return index | ((37 + 218u16.saturating_sub(alpha_score as u16)) << 8);
            }
        }

        // MVV-LVA
        unsafe {
            let spt = self.chess.spt();
            let dst_piece = *spt.get_unchecked(dst_sq as usize);
            let src_piece = *spt.get_unchecked(src_sq as usize);

            let mvvlva_score = *MVV_LVA_SCORES_U8
                .get_unchecked(dst_piece as usize)
                .get_unchecked(src_piece as usize) as u16;

            return index | (mvvlva_score << 8);
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
}
