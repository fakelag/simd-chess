use crate::{
    constant,
    engine::{chess, tables},
};

const WEIGHT_KING: i32 = 10000;
const WEIGHT_QUEEN: i32 = 1000;
const WEIGHT_ROOK: i32 = 525;
const WEIGHT_BISHOP: i32 = 350;
const WEIGHT_KNIGHT: i32 = 350;
const WEIGHT_PAWN: i32 = 100;

pub struct Search<'a> {
    pub chess: &'a mut chess::ChessGame,
    pub tables: &'a tables::Tables,
    pub moves: Vec<String>,
}

impl<'a> Search<'a> {
    pub fn new(chess: &'a mut chess::ChessGame, tables: &'a tables::Tables) -> Self {
        Search {
            chess,
            tables,
            moves: Vec::new(),
        }
    }

    pub fn search(&mut self, depth: u8) -> i32 {
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
                && !self.chess.in_check_slow(self.tables, !self.chess.b_move);

            if !is_valid_move {
                *self.chess = board_copy;
                continue;
            }

            has_legal_moves = true;

            self.moves.push(constant::move_string(mv));
            let score = -self.search(depth - 1);
            self.moves.pop();

            best_score = best_score.max(score);

            *self.chess = board_copy;
        }

        if !has_legal_moves {
            if self.chess.in_check_slow(self.tables, self.chess.b_move) {
                return -i32::MAX;
            }

            // Stalemate
            return 0;
        }

        best_score
    }

    fn evaluate(&self) -> i32 {
        let boards = &self.chess.board.bitboards;
        let mut material_score = 0;

        // let w_k = boards[constant::PieceId::WhiteKing as usize].count_ones() as i32;
        let w_q = boards[constant::PieceId::WhiteQueen as usize].count_ones() as i32;
        let w_r = boards[constant::PieceId::WhiteRook as usize].count_ones() as i32;
        let w_b = boards[constant::PieceId::WhiteBishop as usize].count_ones() as i32;
        let w_n = boards[constant::PieceId::WhiteKnight as usize].count_ones() as i32;
        let w_p = boards[constant::PieceId::WhitePawn as usize].count_ones() as i32;

        // let b_k = boards[constant::PieceId::BlackKing as usize].count_ones() as i32;
        let b_q = boards[constant::PieceId::BlackQueen as usize].count_ones() as i32;
        let b_r = boards[constant::PieceId::BlackRook as usize].count_ones() as i32;
        let b_b = boards[constant::PieceId::BlackBishop as usize].count_ones() as i32;
        let b_n = boards[constant::PieceId::BlackKnight as usize].count_ones() as i32;
        let b_p = boards[constant::PieceId::BlackPawn as usize].count_ones() as i32;

        material_score = (w_q - b_q) * WEIGHT_QUEEN
            + (w_r - b_r) * WEIGHT_ROOK
            + (w_b - b_b) * WEIGHT_BISHOP
            + (w_n - b_n) * WEIGHT_KNIGHT
            + (w_p - b_p) * WEIGHT_PAWN;

        let final_score = material_score;

        final_score * if self.chess.b_move { -1 } else { 1 }
    }
}
