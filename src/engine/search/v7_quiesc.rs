use crossbeam::channel;

use crate::{
    engine::{
        chess_v2,
        search::{
            AbortSignal, SearchStrategy,
            repetition::RepetitionTable,
            search_params::SearchParams,
            transposition::{BoundType, TranspositionTable},
        },
        tables::{self},
    },
    util::PieceId,
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
    chess: chess_v2::ChessGame,
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
        chess: chess_v2::ChessGame,
        tables: &'a tables::Tables,
        tt: &'a mut TranspositionTable,
        rt: RepetitionTable,
        sig: &'a AbortSignal,
    ) -> Search<'a> {
        let mut s = Search {
            sig,
            chess: (chess),
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
            if self.chess.half_moves() >= 100 || self.rt.is_repeated(self.chess.zobrist_key()) {
                return 0;
            }

            let (score, mv) = self.tt.probe(self.chess.zobrist_key(), depth, alpha, beta);

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
                                    && !board_copy.in_check_slow(self.tables, !board_copy.b_move())
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
            .push_position(self.chess.zobrist_key(), self.chess.half_moves() == 0);

        // +~200 Mcycles
        // for i in 0..move_count {
        //     // let src_piece = self.chess.piece_at_slow(1 << (move_list[i] & 0x3F));
        //     // let dst_piece = self.chess.piece_at_slow(1 << ((move_list[i] >> 6) & 0x3F));
        //     let src_piece = self.chess.spt()[(move_list[i] & 0x3F) as usize];
        //     let dst_piece = self.chess.spt()[((move_list[i] >> 6) & 0x3F) as usize];
        //     std::hint::black_box(src_piece);
        //     std::hint::black_box(dst_piece);
        // }

        let board_copy = self.chess.clone();

        for i in 0..move_count {
            let mv = move_list[i];

            let is_valid_move = self.chess.make_move_slow(mv, self.tables)
                && !self.chess.in_check_slow(self.tables, !self.chess.b_move());

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
                        self.chess.zobrist_key(),
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
            if self.chess.in_check_slow(self.tables, self.chess.b_move()) {
                return -SCORE_INF + self.ply as i32;
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

    #[inline(always)]
    pub fn evaluate(&mut self) -> i32 {
        use std::arch::x86_64::*;

        let mut score = 0;

        let boards = self.chess.bitboards();

        const PST: &[[i8; 64]; 14] = &tables::Tables::EVAL_TABLES_INV_I8;

        unsafe {
            let material = self.chess.material();
            let is_endgame = ((material[0] & (!1023)) | (material[1] & (!1023))) == 0;
            let endgame_offset = (is_endgame as usize) << 6;

            let mut bonuses_vec = _mm512_setzero_si512();

            macro_rules! acc_piece_bonus_white_avx512 {
                ($piece_id:expr) => {
                    bonuses_vec = _mm512_mask_blend_epi8(
                        boards[$piece_id as usize],
                        bonuses_vec,
                        _mm512_loadu_epi8(PST[$piece_id as usize + 1].as_ptr() as *const i8),
                    );
                };
            }
            macro_rules! acc_piece_bonus_black_avx512 {
                ($piece_id:expr) => {
                    bonuses_vec = _mm512_mask_blend_epi8(
                        boards[$piece_id as usize],
                        bonuses_vec,
                        _mm512_loadu_epi8(PST[$piece_id as usize + 2].as_ptr() as *const i8),
                    );
                };
            }

            bonuses_vec = _mm512_mask_blend_epi8(
                boards[PieceId::WhiteKing as usize],
                bonuses_vec,
                _mm512_loadu_epi8(
                    PST[PieceId::WhiteKing as usize]
                        .as_ptr()
                        .add(endgame_offset) as *const i8,
                ),
            );

            acc_piece_bonus_white_avx512!(PieceId::WhiteQueen);
            acc_piece_bonus_white_avx512!(PieceId::WhiteRook);
            acc_piece_bonus_white_avx512!(PieceId::WhiteBishop);
            acc_piece_bonus_white_avx512!(PieceId::WhiteKnight);
            acc_piece_bonus_white_avx512!(PieceId::WhitePawn);

            bonuses_vec = _mm512_mask_blend_epi8(
                boards[PieceId::BlackKing as usize],
                bonuses_vec,
                _mm512_loadu_epi8(
                    PST[PieceId::BlackKing as usize + 1]
                        .as_ptr()
                        .add(endgame_offset) as *const i8,
                ),
            );

            acc_piece_bonus_black_avx512!(PieceId::BlackQueen);
            acc_piece_bonus_black_avx512!(PieceId::BlackRook);
            acc_piece_bonus_black_avx512!(PieceId::BlackBishop);
            acc_piece_bonus_black_avx512!(PieceId::BlackKnight);
            acc_piece_bonus_black_avx512!(PieceId::BlackPawn);

            let bonuses_lsb = _mm512_castsi512_si256(bonuses_vec);
            let bonuses_msb = _mm512_extracti64x4_epi64(bonuses_vec, 1);

            let bonuses_lsb_i16 = _mm512_cvtepi8_epi16(bonuses_lsb);
            let bonuses_msb_i16 = _mm512_cvtepi8_epi16(bonuses_msb);

            let summed_bonuses = _mm512_add_epi16(bonuses_lsb_i16, bonuses_msb_i16);

            let summed_bonuses_lsb = _mm512_castsi512_si256(summed_bonuses);
            let summed_bonuses_msb = _mm512_extracti64x4_epi64(summed_bonuses, 1);

            score += _mm256_reduce_add_epi16(summed_bonuses_lsb) as i32;
            score += _mm256_reduce_add_epi16(summed_bonuses_msb) as i32;
        }

        // There is always 1 king on the board, skip king weight itself which would be +-0
        score += (boards[PieceId::WhiteQueen as usize].count_ones() as i32
            - boards[PieceId::BlackQueen as usize].count_ones() as i32)
            * WEIGHT_QUEEN;

        score += (boards[PieceId::WhiteRook as usize].count_ones() as i32
            - boards[PieceId::BlackRook as usize].count_ones() as i32)
            * WEIGHT_ROOK;

        score += (boards[PieceId::WhiteBishop as usize].count_ones() as i32
            - boards[PieceId::BlackBishop as usize].count_ones() as i32)
            * WEIGHT_BISHOP;

        score += (boards[PieceId::WhiteKnight as usize].count_ones() as i32
            - boards[PieceId::BlackKnight as usize].count_ones() as i32)
            * WEIGHT_KNIGHT;

        score += (boards[PieceId::WhitePawn as usize].count_ones() as i32
            - boards[PieceId::BlackPawn as usize].count_ones() as i32)
            * WEIGHT_PAWN;

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

    fn evaluate_legacy(&mut self) -> i32 {
        let material = self.chess.material();
        let is_endgame = (((material[0] & (!1023)) | (material[1] & (!1023))) == 0) as usize;

        let boards = self.chess.bitboards();

        let mut final_score = 0;

        for piece_id in
            crate::util::PieceId::WhiteKing as usize..crate::util::PieceId::PieceMax as usize
        {
            let mut board_bits = boards[piece_id];
            loop {
                let piece_square = crate::pop_ls1b!(board_bits);

                let pst_index = match PieceId::from(piece_id) {
                    PieceId::WhiteKing => is_endgame as usize,
                    PieceId::WhiteQueen => piece_id + 1,
                    PieceId::WhiteRook => piece_id + 1,
                    PieceId::WhiteBishop => piece_id + 1,
                    PieceId::WhiteKnight => piece_id + 1,
                    PieceId::WhitePawn => piece_id + 1,
                    PieceId::BlackKing => piece_id + is_endgame + 1,
                    PieceId::BlackQueen => piece_id + 2,
                    PieceId::BlackRook => piece_id + 2,
                    PieceId::BlackBishop => piece_id + 2,
                    PieceId::BlackKnight => piece_id + 2,
                    PieceId::BlackPawn => piece_id + 2,
                    _ => unreachable!(),
                };

                let square_bonus =
                    tables::Tables::EVAL_TABLES_INV_I8[pst_index][piece_square as usize];
                final_score += WEIGHT_TABLE[piece_id] + square_bonus as i32;
            }
        }

        final_score * if self.chess.b_move() { -1 } else { 1 }
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
