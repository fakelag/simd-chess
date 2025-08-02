use crossbeam::channel;

use crate::engine::{
    chess,
    search::{
        AbortSignal, SearchStrategy,
        repetition::RepetitionTable,
        search_params::SearchParams,
        transposition::{BoundType, TranspositionTable},
    },
    tables,
};

const SCORE_INF: i32 = i32::MAX - 1;
const WEIGHT_KING: i32 = 10000;
const WEIGHT_QUEEN: i32 = 1000;
const WEIGHT_ROOK: i32 = 525;
const WEIGHT_BISHOP: i32 = 350;
const WEIGHT_KNIGHT: i32 = 350;
const WEIGHT_PAWN: i32 = 100;
const PV_DEPTH: usize = 64;

const WEIGHT_TABLE: [i32; 12] = [
    WEIGHT_KING,
    WEIGHT_QUEEN,
    WEIGHT_ROOK,
    WEIGHT_BISHOP,
    WEIGHT_KNIGHT,
    WEIGHT_PAWN,
    -WEIGHT_KING,
    -WEIGHT_QUEEN,
    -WEIGHT_ROOK,
    -WEIGHT_BISHOP,
    -WEIGHT_KNIGHT,
    -WEIGHT_PAWN,
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

pub struct Search<'a> {
    params: SearchParams,
    chess: chess::ChessGame,
    tables: &'a tables::Tables,
    sig: &'a AbortSignal,

    ply: u8,
    is_stopping: bool,

    nodes: u64,
    score: i32,

    pv_table: Box<PvTable>,
    pv: [u16; PV_DEPTH],
    pv_length: usize,
    pv_trace: bool,

    tt: &'a mut TranspositionTable,
    rt: RepetitionTable,
}

impl<'a> SearchStrategy<'a> for Search<'a> {
    fn search(&mut self) -> u16 {
        let mut best_score = -i32::MAX;
        let mut node_count = 0;

        let max_depth = self.params.depth.unwrap_or(63);

        'outer: for depth in 1..=max_depth {
            self.ply = 0;

            let score = self.go(-i32::MAX, i32::MAX, depth);

            if self.is_stopping {
                break 'outer;
            }

            node_count = self.nodes;
            self.pv_length = self.pv_table.lengths[0] as usize;
            self.pv[0..self.pv_length].copy_from_slice(&self.pv_table.moves[0][0..self.pv_length]);
            self.pv_trace = self.pv_length > 0;

            best_score = score;
        }

        self.nodes = node_count;
        self.score = best_score;

        self.pv[0]
    }

    fn num_nodes_searched(&self) -> u64 {
        self.nodes
    }

    fn search_score(&self) -> i32 {
        self.score
    }
}

impl<'a> Search<'a> {
    pub fn new(
        params: SearchParams,
        chess: chess::ChessGame,
        tables: &'a tables::Tables,
        tt: &'a mut TranspositionTable,
        rt: RepetitionTable,
        sig: &'a AbortSignal,
    ) -> Search<'a> {
        let s = Search {
            sig,
            chess,
            params,
            tables,
            nodes: 0,
            ply: 0,
            is_stopping: false,
            score: -i32::MAX,
            pv_table: unsafe {
                let mut pv_table = Box::new_uninit();
                pv_table.write(PvTable::new());
                pv_table.assume_init()
            },
            pv: [0; PV_DEPTH],
            pv_length: 0,
            pv_trace: false,
            rt,
            tt,
        };

        s
    }

    pub fn get_pv(&self) -> &[u16] {
        &self.pv[0..self.pv_length]
    }

    fn go(&mut self, alpha: i32, beta: i32, depth: u8) -> i32 {
        // @perf - Can rust reason ply < PV_DEPTH without recursive assertions
        assert!(self.ply < PV_DEPTH as u8);

        self.nodes += 1;
        self.pv_table.lengths[self.ply as usize] = 0;

        if self.nodes & 0x7FF == 0 && self.check_sigabort() {
            self.is_stopping = true;
            return 0;
        }

        let mut move_list = [0u16; 256];
        let mut move_count = 0;

        if self.ply > 0 && !self.pv_trace {
            if self.chess.half_moves >= 100 || self.rt.is_repeated(self.chess.zobrist_key) {
                return 0;
            }

            let (score, mv) = self.tt.probe(self.chess.zobrist_key, depth, alpha, beta);

            if let Some(score) = score {
                return score;
            }

            if let Some(mv) = mv {
                // @todo correctness - Zobrist keys can clash, which could generate an invalid move
                // for the current position. Move insertion is done now for perf, but if sorting is added
                // we can also remove the move in case its not valid for the current position.
                #[cfg(debug_assertions)]
                {
                    let mut verify_move_list = [0u16; 256];

                    debug_assert!(
                        (0..self
                            .chess
                            .gen_moves_slow(self.tables, &mut verify_move_list))
                            .any(|mv_index| {
                                if verify_move_list[mv_index] != mv {
                                    return false;
                                }
                                let mut board_copy = self.chess.clone();
                                board_copy.make_move_slow(mv, self.tables)
                                    && !board_copy.in_check_slow(self.tables, !board_copy.b_move)
                            })
                    );
                }

                move_list[0] = mv;
                move_count += 1;
            }
        }

        if depth == 0 {
            debug_assert!(!self.pv_trace);
            return self.evaluate();
        }

        let mut bound_type = BoundType::UpperBound;

        let mut alpha = alpha;

        if self.pv_trace {
            // PV Tracing: Generate the PV-move from the last iteration as an extra first move to play.
            // This makes sure that PV is played first on subsequent searches. Though it means that the move
            // is possibly played twice for a given board position, it reduces overall nodes searched due to AB cutoffs.
            self.pv_trace = self.pv_length > (self.ply as usize + 1);

            move_list[1] = move_list[0]; // Copy TT move (if any) to the second position
            move_list[0] = self.pv[self.ply as usize];
            move_count += 1;
        }

        move_count += self
            .chess
            .gen_moves_slow(self.tables, &mut move_list[move_count..]);

        let mut best_score = -i32::MAX;
        let mut best_move = 0;
        let mut has_legal_moves = false;

        self.rt
            .push_position(self.chess.zobrist_key, self.chess.half_moves == 0);

        for i in 0..move_count {
            let mv = move_list[i];

            let board_copy = self.chess.clone();

            let is_valid_move = self.chess.make_move_slow(mv, self.tables)
                && !self.chess.in_check_slow(self.tables, !self.chess.b_move);

            if !is_valid_move {
                self.chess = board_copy;
                continue;
            }

            has_legal_moves = true;

            self.ply += 1;

            let score = -self.go(-beta, -alpha, depth - 1);

            self.ply -= 1;

            self.chess = board_copy;

            if self.is_stopping {
                return 0;
            }

            if score > best_score {
                best_score = score;
                best_move = mv;

                // Update PV
                let child_pv_length = self.pv_table.lengths[self.ply as usize + 1];

                self.pv_table.moves[self.ply as usize][0] = mv;
                self.pv_table.lengths[self.ply as usize] = child_pv_length + 1;

                // @perf - use disjoint unchecked
                let [root_pv_moves, child_pv_moves] = self
                    .pv_table
                    .moves
                    .get_disjoint_mut([self.ply as usize, self.ply as usize + 1])
                    .unwrap();

                root_pv_moves[1..child_pv_length as usize + 1]
                    .copy_from_slice(&child_pv_moves[0..child_pv_length as usize]);

                if score >= beta {
                    self.tt.store(
                        self.chess.zobrist_key,
                        score,
                        depth,
                        mv,
                        BoundType::LowerBound,
                    );
                    self.rt.pop_position();
                    return score;
                }

                if score > alpha {
                    bound_type = BoundType::Exact;
                    alpha = score;
                }
            }
        }
        self.rt.pop_position();

        if !has_legal_moves {
            if self.chess.in_check_slow(self.tables, self.chess.b_move) {
                return -SCORE_INF + self.ply as i32;
            }

            // Stalemate
            return 0;
        }

        self.tt.store(
            self.chess.zobrist_key,
            best_score,
            depth,
            best_move,
            bound_type,
        );

        best_score
    }

    // #[inline(never)]
    fn evaluate(&mut self) -> i32 {
        use std::arch::x86_64::*;

        // @todo - Transition king to an engame variant of the table
        let mut score = 0;

        let boards = &self.chess.board.bitboards;

        unsafe {
            let const_1vec = _mm512_set1_epi64(1);
            let const_63vec = _mm512_set1_epi64(63);
            let const_64vec = _mm512_set1_epi64(64);
            let const_0vec = _mm256_setzero_si256();
            let const_bonus_gather_offsets_0_vec = _mm512_set_epi64(
                0x40 * 5,
                0x40 * 5,
                0x40 * 5,
                0x40 * 4,
                0x40 * 3,
                0x40 * 2,
                0x40,
                0,
            );
            let const_bonus_gather_offsets_1_vec = _mm512_set_epi64(
                0x40 * 5 + 0x180,
                0x40 * 4 + 0x180,
                0x40 * 3 + 0x180,
                0x40 * 2 + 0x180,
                0x40 + 0x180,
                0x180,
                0x40 * 5 + 0x180,
                0x40 * 5 + 0x180,
            );
            let const_weights_0_vec = _mm256_set_epi32(
                WEIGHT_PAWN,
                WEIGHT_PAWN,
                WEIGHT_PAWN,
                WEIGHT_KNIGHT,
                WEIGHT_BISHOP,
                WEIGHT_ROOK,
                WEIGHT_QUEEN,
                WEIGHT_KING,
            );
            let const_weights_1_vec = _mm256_set_epi32(
                -WEIGHT_PAWN,
                -WEIGHT_KNIGHT,
                -WEIGHT_BISHOP,
                -WEIGHT_ROOK,
                -WEIGHT_QUEEN,
                -WEIGHT_KING,
                -WEIGHT_PAWN,
                -WEIGHT_PAWN,
            );

            let const_pawn_split_mask_abc: u64 = 0xE0E0E0E0E0E0E0E0u64;
            let const_pawn_split_mask_def: u64 = 0x1C1C1C1C1C1C1C1Cu64;
            let const_pawn_split_mask_gh: u64 = 0x0303030303030303u64;

            debug_assert!(
                const_pawn_split_mask_abc ^ const_pawn_split_mask_def ^ const_pawn_split_mask_gh
                    == 0xFFFFFFFFFFFFFFFFu64
            );

            let const_pawn_split_mask_0_vec = _mm512_set_epi64(
                const_pawn_split_mask_gh as i64,
                const_pawn_split_mask_def as i64,
                const_pawn_split_mask_abc as i64,
                !0,
                !0,
                !0,
                !0,
                !0,
            );
            let const_pawn_split_mask_1_vec = _mm512_set_epi64(
                const_pawn_split_mask_abc as i64,
                !0,
                !0,
                !0,
                !0,
                !0,
                const_pawn_split_mask_def as i64,
                const_pawn_split_mask_gh as i64,
            );
            let const_pawn_split_1_cross_lane_selector =
                _mm512_set_epi64(0, 0, 0, 0, 0, 0, 0x7, 0x7);

            let mut boards_0_vec = _mm512_loadu_epi64(boards.as_ptr() as *const i64);
            let mut boards_1_vec = _mm512_loadu_epi64(boards.as_ptr().add(4) as *const i64);

            // Split pawns into three lanes
            boards_0_vec =
                _mm512_mask_permutex_epi64(boards_0_vec, 0b11000000, boards_0_vec, 0b1010000);
            boards_0_vec = _mm512_and_si512(boards_0_vec, const_pawn_split_mask_0_vec);

            boards_1_vec = _mm512_mask_permutex2var_epi64(
                boards_1_vec,
                0b11,
                const_pawn_split_1_cross_lane_selector,
                boards_1_vec,
            );
            boards_1_vec = _mm512_and_si512(boards_1_vec, const_pawn_split_mask_1_vec);

            let mut score_vec = _mm256_setzero_si256();

            loop {
                let lzcnt_0_vec = _mm512_lzcnt_epi64(boards_0_vec);
                let lzcnt_1_vec = _mm512_lzcnt_epi64(boards_1_vec);

                let active_pieces_0_mask = _mm512_cmpneq_epi64_mask(lzcnt_0_vec, const_64vec);
                let active_pieces_1_mask = _mm512_cmpneq_epi64_mask(lzcnt_1_vec, const_64vec);

                if (active_pieces_0_mask | active_pieces_1_mask) == 0 {
                    break;
                }

                let piece_index_0_vec = _mm512_sub_epi64(const_63vec, lzcnt_0_vec);
                let piece_index_1_vec = _mm512_sub_epi64(const_63vec, lzcnt_1_vec);

                let bonuses_0 = _mm512_mask_i64gather_epi32(
                    const_0vec,
                    active_pieces_0_mask,
                    _mm512_add_epi64(const_bonus_gather_offsets_0_vec, piece_index_0_vec),
                    tables::Tables::EVAL_TABLES_INV.as_ptr() as *const i32,
                    4,
                );

                let bonuses_1 = _mm512_mask_i64gather_epi32(
                    const_0vec,
                    active_pieces_1_mask,
                    _mm512_add_epi64(const_bonus_gather_offsets_1_vec, piece_index_1_vec),
                    tables::Tables::EVAL_TABLES_INV.as_ptr() as *const i32,
                    4,
                );

                let scores_0 =
                    _mm256_maskz_add_epi32(active_pieces_0_mask, bonuses_0, const_weights_0_vec);

                let scores_1 =
                    _mm256_maskz_add_epi32(active_pieces_1_mask, bonuses_1, const_weights_1_vec);

                let scores = _mm256_add_epi32(scores_0, scores_1);
                score_vec = _mm256_add_epi32(score_vec, scores);

                let pop_vec = _mm512_sllv_epi64(const_1vec, piece_index_0_vec);
                boards_0_vec = _mm512_xor_epi64(boards_0_vec, pop_vec);

                let pop_vec = _mm512_sllv_epi64(const_1vec, piece_index_1_vec);
                boards_1_vec = _mm512_xor_epi64(boards_1_vec, pop_vec);
            }

            score =
                _mm512_mask_reduce_add_epi32(0b0000000011111111, _mm512_castsi256_si512(score_vec));
        }

        let final_score = score * if self.chess.b_move { -1 } else { 1 };

        // assert!(
        //     self.evaluate_legacy() == final_score,
        //     "Legacy and simd evaluation mismatch for {} != {}. b_move= {}",
        //     self.evaluate_legacy(),
        //     final_score,
        //     self.chess.b_move
        // );

        final_score
    }

    fn evaluate_legacy(&mut self) -> i32 {
        let boards = &self.chess.board.bitboards;

        let mut final_score = 0;
        for piece_id in
            crate::util::PieceId::WhiteKing as usize..crate::util::PieceId::PieceMax as usize
        {
            let mut board_bits = boards[piece_id];
            loop {
                let piece_square = crate::pop_ls1b!(board_bits);
                let square_bonus = tables::Tables::EVAL_TABLES_INV[piece_id][piece_square as usize];
                final_score += WEIGHT_TABLE[piece_id] + square_bonus as i32;
            }
        }

        final_score * if self.chess.b_move { -1 } else { 1 }
    }

    fn check_sigabort(&self) -> bool {
        match self.sig.try_recv() {
            Ok(_) => true,
            Err(channel::TryRecvError::Empty) => false,
            Err(channel::TryRecvError::Disconnected) => {
                panic!("sigabort channel disconnected while searching")
            }
        }
    }
}
