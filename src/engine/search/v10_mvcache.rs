use crossbeam::channel;

use crate::{
    engine::{
        chess,
        search::{
            AbortSignal, SearchStrategy,
            repetition::RepetitionTable,
            search_params::SearchParams,
            transposition::{BoundType, TranspositionTable},
        },
        tables::{self},
    },
    util::{self, PieceId},
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
    chess: chess::ChessGame,
    tables: &'a tables::Tables,
    sig: &'a AbortSignal,

    ply: u8,
    is_stopping: bool,

    pv_table: Box<PvTable>,
    pv: [u16; PV_DEPTH],
    pv_length: usize,
    pv_trace: bool,

    tt: &'a mut TranspositionTable,
    rt: RepetitionTable,

    params: Box<SearchParams>,

    score: i32,
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

        let max_depth = self.params.depth.unwrap_or(63);

        'outer: for depth in 1..=max_depth {
            self.ply = 0;

            let score = self.go(-i32::MAX, i32::MAX, depth);

            if self.is_stopping {
                break 'outer;
            }

            quiet_nodes = self.quiet_nodes;
            node_count = self.node_count;
            self.pv_length = self.pv_table.lengths[0] as usize;
            self.pv[0..self.pv_length].copy_from_slice(&self.pv_table.moves[0][0..self.pv_length]);
            self.pv_trace = self.pv_length > 0;
            self.depth = depth;
            self.score = score;
        }

        self.node_count = node_count;
        self.quiet_nodes = quiet_nodes;

        self.pv[0]
    }

    fn num_nodes_searched(&self) -> u64 {
        self.node_count
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
        let mut s = Search {
            sig,
            chess: chess,
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
            score: -i32::MAX,
            pv_table: unsafe {
                let mut pv_table = Box::new_uninit();
                pv_table.write(PvTable::new());
                pv_table.assume_init()
            },
            depth: 0,
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
    pub fn b_cut_count(&self) -> u64 {
        self.b_cut_count
    }
    pub fn b_cut_null_count(&self) -> u64 {
        self.b_cut_null_count
    }
    pub fn a_raise_count(&self) -> u64 {
        self.a_raise_count
    }
    pub fn get_depth(&self) -> u8 {
        self.depth
    }
    pub fn get_quiet_depth(&self) -> u32 {
        self.quiet_depth
    }
    pub fn get_quiet_nodes(&self) -> u64 {
        self.quiet_nodes
    }

    fn go(&mut self, alpha: i32, beta: i32, depth: u8) -> i32 {
        self.node_count += 1;

        if self.node_count & 0x7FF == 0 && self.check_sigabort() {
            self.is_stopping = true;
            return 0;
        }

        if self.ply >= PV_DEPTH as u8 - 1 {
            return self.quiescence(alpha, beta, self.ply as u32);
        }

        self.pv_table.lengths[self.ply as usize] = 0;

        let mut pv_move = 0; // Null move
        let mut tt_move = 0; // Null move

        let mut move_list = [0u16; 256];

        if self.ply > 0 && !self.pv_trace {
            if self.chess.half_moves() >= 100 || self.rt.is_repeated(self.chess.zobrist_key()) {
                return 0;
            }

            let (score, mv) = self.tt.probe(self.chess.zobrist_key(), depth, alpha, beta);

            if let Some(score) = score {
                return score;
            }

            if let Some(mv) = mv {
                tt_move = mv;
            }
        }

        let mut bound_type = BoundType::UpperBound;
        let mut alpha = alpha;
        let mut depth = depth;

        if depth == 0 {
            debug_assert!(!self.pv_trace);
            return self.quiescence(alpha, beta, self.ply as u32);
        }

        let in_check = self.chess.in_check_slow(self.tables, self.chess.b_move());
        depth += in_check as u8;

        self.rt
            .push_position(self.chess.zobrist_key(), self.chess.half_moves() == 0);

        if self.pv_trace {
            self.pv_trace = self.pv_length > (self.ply as usize + 1);
            pv_move = self.pv[self.ply as usize];
        }

        // Null move pruning
        if !self.pv_trace && self.ply > 0 && depth >= 3 && !in_check && !self.is_endgame() {
            let ep_square = self.chess.make_null_move(self.tables);

            self.ply += 1;
            let score = -self.go(-beta, -beta + 1, depth - 3);
            self.ply -= 1;

            self.chess.rollback_null_move(ep_square, self.tables);

            if self.is_stopping {
                return 0;
            }

            if score >= beta {
                self.rt.pop_position();
                self.b_cut_null_count += 1;
                return score;
            }
        }

        let move_count = self.chess.gen_moves_slow(self.tables, &mut move_list);
        move_list[0..move_count].sort_by(|a, b| self.sort_moves_mvvlva(a, b, pv_move, tt_move));

        let mut best_move = 0;
        let mut has_legal_moves = false;

        let board_copy = self.chess.clone();

        let mut moves_searched = 0;

        for i in 0..move_count {
            let mv = move_list[i];

            let is_valid_move = self.chess.make_move_slow(mv, self.tables)
                && !self.chess.in_check_slow(self.tables, !self.chess.b_move());

            if !is_valid_move {
                self.chess = board_copy;
                continue;
            }

            let late_move_reduction = moves_searched > 3
                && !in_check
                && depth >= 3
                && (mv & (chess::MV_FLAG_CAP | chess::MV_FLAG_PROMOTION) == 0);

            has_legal_moves = true;

            self.ply += 1;

            let score = if late_move_reduction {
                // Late move, apply a small reduction of 1 ply and
                // search with window [-alpha - 1, -alpha] with the goal of
                // proving that the move is not good enough to be played
                let reduced_score = -self.go(-alpha - 1, -alpha, depth - 2);
                if reduced_score > alpha {
                    // The move might be good, search it again with full depth
                    -self.go(-beta, -alpha, depth - 1)
                } else {
                    reduced_score
                }
            } else {
                -self.go(-beta, -alpha, depth - 1)
            };
            // let score = -self.go(-beta, -alpha, depth - 1);

            self.ply -= 1;
            self.chess = board_copy;
            moves_searched += 1;

            if self.is_stopping {
                return 0;
            }

            if score <= alpha {
                continue;
            }

            self.a_raise_count += 1;
            bound_type = BoundType::Exact;
            alpha = score;
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
                self.b_cut_count += 1;
                return score;
            }
        }
        self.rt.pop_position();

        if !has_legal_moves {
            if in_check {
                return -SCORE_INF + self.ply as i32;
            }

            // Stalemate
            return 0;
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

    fn quiescence(&mut self, alpha: i32, beta: i32, start_ply: u32) -> i32 {
        self.node_count += 1;
        self.quiet_nodes += 1;
        self.quiet_depth = self.quiet_depth.max(self.ply as u32 - start_ply);

        if self.node_count & 0x7FF == 0 && self.check_sigabort() {
            self.is_stopping = true;
            return 0;
        }

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

        let mut move_list = [0u16; 256];

        let move_count = self.chess.gen_moves_slow(self.tables, &mut move_list);
        move_list[0..move_count].sort_by(|a, b| {
            // Moves are sorted by MVV-LVA, meaning that all captures are placed at the start of the list
            self.sort_moves_mvvlva(a, b, 0, 0)
        });

        let mut best_score = static_eval;

        let board_copy = self.chess.clone();

        for i in 0..move_count {
            let mv = move_list[i];

            if (mv & chess::MV_FLAG_CAP) == 0 {
                break;
            }

            let is_valid_move = self.chess.make_move_slow(mv, self.tables)
                && !self.chess.in_check_slow(self.tables, !self.chess.b_move());

            if !is_valid_move {
                self.chess = board_copy;
                continue;
            }

            self.ply += 1;

            let score = -self.quiescence(-beta, -alpha, start_ply);

            self.ply -= 1;

            self.chess = board_copy;

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
    pub fn sort_moves_mvvlva(
        &mut self,
        a: &u16,
        b: &u16,
        pv_move: u16,
        tt_move: u16,
    ) -> std::cmp::Ordering {
        debug_assert!(*a != 0, "a should not be a null move");
        debug_assert!(*b != 0, "b should not be a null move");

        if *a == pv_move {
            return std::cmp::Ordering::Less;
        }
        if *b == pv_move {
            return std::cmp::Ordering::Greater;
        }
        if *a == tt_move {
            return std::cmp::Ordering::Less;
        }
        if *b == tt_move {
            return std::cmp::Ordering::Greater;
        }

        if (*a & chess::MV_FLAG_CAP) == 0 {
            // a is a quiet move
            return std::cmp::Ordering::Greater;
        }

        if (*b & chess::MV_FLAG_CAP) == 0 {
            // b is a quiet move
            return std::cmp::Ordering::Less;
        }

        let a_src_sq = a & 0x3F;
        let b_src_sq = b & 0x3F;

        let a_dst_sq = (a >> 6) & 0x3F;
        let b_dst_sq = (b >> 6) & 0x3F;

        // MVV-LVA
        let spt = self.chess.spt();
        let a_dst_piece = spt[a_dst_sq as usize];
        let b_dst_piece = spt[b_dst_sq as usize];
        let a_src_piece = spt[a_src_sq as usize];
        let b_src_piece = spt[b_src_sq as usize];

        if a_dst_piece == 0 {
            // a is an en passant capture
            return std::cmp::Ordering::Greater;
        }

        if b_dst_piece == 0 {
            // b is an en passant capture
            return std::cmp::Ordering::Less;
        }

        if a_dst_piece < b_dst_piece {
            // a captures a piece of higher value than b
            return std::cmp::Ordering::Less;
        }

        if a_dst_piece > b_dst_piece {
            // a captures a piece of lower value than b
            return std::cmp::Ordering::Greater;
        }

        if a_src_piece < b_src_piece {
            // a moves a piece of higher value than b
            return std::cmp::Ordering::Greater;
        }

        if a_src_piece > b_src_piece {
            // a moves a piece of lower value than b
            return std::cmp::Ordering::Less;
        }

        std::cmp::Ordering::Equal
    }

    #[inline(always)]
    pub fn is_endgame(&self) -> bool {
        let material = self.chess.material();
        ((material[0] & (!1023)) | (material[1] & (!1023))) == 0
    }

    #[inline(always)]
    pub fn evaluate(&mut self) -> i32 {
        use std::arch::x86_64::*;

        let mut score = 0;

        let boards = self.chess.bitboards();

        const PST: &[[i8; 64]; 14] = &tables::Tables::EVAL_TABLES_INV_I8;

        unsafe {
            let is_endgame = self.is_endgame() as usize;
            let endgame_offset = is_endgame << 6;

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
