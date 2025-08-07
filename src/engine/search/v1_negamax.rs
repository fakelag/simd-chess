use crate::{
    engine::{
        chess,
        search::{SearchStrategy, search_params::SearchParams},
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

pub struct Negamax<'a> {
    params: SearchParams,
    chess: chess::ChessGame,
    tables: &'a tables::Tables,
    nodes: u64,
    score: i32,
}

impl<'a> SearchStrategy<'a> for Negamax<'a> {
    fn search(&mut self) -> u16 {
        let mut move_list = [0u16; 256];
        let move_count = self.chess.gen_moves_slow(&self.tables, &mut move_list);

        let mut best_score = -i32::MAX;
        let mut best_move = 0;

        self.nodes += 1;

        for i in 0..move_count {
            let mv = move_list[i];

            let board_copy = self.chess.clone();

            let is_legal_move = self.chess.make_move_slow(mv, &self.tables)
                && !self.chess.in_check_slow(&self.tables, !self.chess.b_move());

            if !is_legal_move {
                self.chess = board_copy;
                continue;
            }

            let score = -self.negamax(
                self.params
                    .depth
                    .expect("Expected \"depth\" command for negamax")
                    - 1,
            );

            if score > best_score {
                best_score = score;
                best_move = mv;
            }

            self.chess = board_copy;
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

impl<'a> Negamax<'a> {
    pub fn new(
        params: SearchParams,
        chess: chess::ChessGame,
        tables: &'a tables::Tables,
    ) -> Negamax<'a> {
        Negamax {
            chess,
            params,
            tables,
            nodes: 0,
            score: -i32::MAX,
        }
    }

    fn negamax(&mut self, depth: u8) -> i32 {
        self.nodes += 1;

        if depth == 0 {
            return self.evaluate();
        }

        let mut move_list = [0u16; 256];
        let move_count = self.chess.gen_moves_slow(self.tables, &mut move_list);

        let mut best_score = -i32::MAX;
        let mut has_legal_moves = false;

        for i in 0..move_count {
            let mv = move_list[i];

            let board_copy = self.chess.clone();

            let is_valid_move = self.chess.make_move_slow(mv, self.tables)
                && !self.chess.in_check_slow(self.tables, !self.chess.b_move());

            if !is_valid_move {
                self.chess = board_copy;
                continue;
            }

            has_legal_moves = true;

            let score = -self.negamax(depth - 1);

            best_score = best_score.max(score);

            self.chess = board_copy;
        }

        if !has_legal_moves {
            if self.chess.in_check_slow(self.tables, self.chess.b_move()) {
                return -SCORE_INF;
            }

            // Stalemate
            return 0;
        }

        best_score
    }

    fn evaluate(&self) -> i32 {
        let boards = self.chess.bitboards();

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

        final_score * if self.chess.b_move() { -1 } else { 1 }
    }
}
