use crossbeam::channel;

use crate::{
    engine::{
        chess,
        search::{AbortSignal, SearchStrategy, search_params::SearchParams},
        tables,
    },
    util,
};

const SCORE_INF: i32 = i32::MAX - 1;
const WEIGHT_KING: i32 = 10000;
const WEIGHT_QUEEN: i32 = 1000;
const WEIGHT_ROOK: i32 = 525;
const WEIGHT_BISHOP: i32 = 350;
const WEIGHT_KNIGHT: i32 = 350;
const WEIGHT_PAWN: i32 = 100;

pub struct IterativeDeepening<'a> {
    params: SearchParams,
    chess: chess::ChessGame,
    tables: &'a tables::Tables,
    sig: &'a AbortSignal,

    ply: u8,
    is_stopping: bool,

    nodes: u64,
    score: i32,
}

impl<'a> SearchStrategy<'a> for IterativeDeepening<'a> {
    fn new(
        params: SearchParams,
        chess: chess::ChessGame,
        tables: &'a tables::Tables,
        sig: &'a AbortSignal,
    ) -> IterativeDeepening<'a> {
        IterativeDeepening {
            chess,
            params,
            tables,
            nodes: 0,
            ply: 0,
            is_stopping: false,
            score: -i32::MAX,
            sig,
        }
    }

    fn search(&mut self) -> u16 {
        let mut best_move = 0;
        let mut best_score = -i32::MAX;

        let mut move_list = [0u16; 256];
        let move_count = self.chess.gen_moves_slow(&self.tables, &mut move_list);

        self.nodes += 1;

        let max_depth = self.params.depth.unwrap_or(63);

        'outer: for depth in 1..=max_depth {
            let mut iter_best_score = -i32::MAX;
            let mut iter_best_move = 0;

            for i in 0..move_count {
                let mv = move_list[i];

                let board_copy = self.chess.clone();

                let is_legal_move = self.chess.make_move_slow(mv, &self.tables)
                    && !self.chess.in_check_slow(&self.tables, !self.chess.b_move);

                if !is_legal_move {
                    self.chess = board_copy;
                    continue;
                }

                self.ply += 1;

                let score = -self.go(-i32::MAX, i32::MAX, depth - 1);

                self.ply -= 1;

                self.chess = board_copy;

                if self.is_stopping {
                    break 'outer;
                }

                if score > iter_best_score {
                    iter_best_score = score;
                    iter_best_move = mv;
                }
            }

            best_move = iter_best_move;
            best_score = iter_best_score;
        }

        self.score = best_score;

        best_move
    }

    fn num_nodes_searched(&self) -> u64 {
        self.nodes
    }

    fn search_score(&self) -> i32 {
        self.score
    }
}

impl<'a> IterativeDeepening<'a> {
    fn go(&mut self, alpha: i32, beta: i32, depth: u8) -> i32 {
        self.nodes += 1;

        if depth == 0 {
            return self.evaluate();
        }

        if self.nodes & 0x7FF == 0 && self.check_sigabort() {
            self.is_stopping = true;
            return 0;
        }

        let mut alpha = alpha;

        let mut move_list = [0u16; 256];
        let move_count = self.chess.gen_moves_slow(self.tables, &mut move_list);

        let mut best_score = -i32::MAX;
        let mut has_legal_moves = false;

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

                if best_score > alpha {
                    alpha = best_score;
                }
            }

            if best_score >= beta {
                return best_score;
            }
        }

        if !has_legal_moves {
            if self.chess.in_check_slow(self.tables, self.chess.b_move) {
                return -SCORE_INF + self.ply as i32;
            }

            // Stalemate
            return 0;
        }

        best_score
    }

    fn evaluate(&self) -> i32 {
        let boards = &self.chess.board.bitboards;

        let w_q = boards[util::PieceId::WhiteQueen as usize].count_ones() as i32;
        let w_r = boards[util::PieceId::WhiteRook as usize].count_ones() as i32;
        let w_b = boards[util::PieceId::WhiteBishop as usize].count_ones() as i32;
        let w_n = boards[util::PieceId::WhiteKnight as usize].count_ones() as i32;
        let w_p = boards[util::PieceId::WhitePawn as usize].count_ones() as i32;

        let b_q = boards[util::PieceId::BlackQueen as usize].count_ones() as i32;
        let b_r = boards[util::PieceId::BlackRook as usize].count_ones() as i32;
        let b_b = boards[util::PieceId::BlackBishop as usize].count_ones() as i32;
        let b_n = boards[util::PieceId::BlackKnight as usize].count_ones() as i32;
        let b_p = boards[util::PieceId::BlackPawn as usize].count_ones() as i32;

        let material_score = (w_q - b_q) * WEIGHT_QUEEN
            + (w_r - b_r) * WEIGHT_ROOK
            + (w_b - b_b) * WEIGHT_BISHOP
            + (w_n - b_n) * WEIGHT_KNIGHT
            + (w_p - b_p) * WEIGHT_PAWN;

        let final_score = material_score;

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
