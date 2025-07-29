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

pub struct Alphabeta<'a> {
    params: SearchParams,
    chess: chess::ChessGame,
    tables: &'a tables::Tables,
    sig: &'a AbortSignal,
    is_stopping: bool,
    nodes: u64,
    score: i32,
}

impl<'a> SearchStrategy<'a> for Alphabeta<'a> {
    fn search(&mut self) -> u16 {
        let mut move_list = [0u16; 256];
        let move_count = self.chess.gen_moves_slow(&self.tables, &mut move_list);

        let mut best_score = -i32::MAX;
        let mut best_move = 0;

        for i in 0..move_count {
            let mv = move_list[i];

            let board_copy = self.chess.clone();

            let is_legal_move = self.chess.make_move_slow(mv, &self.tables)
                && !self.chess.in_check_slow(&self.tables, !self.chess.b_move);

            if !is_legal_move {
                self.chess = board_copy;
                continue;
            }

            let depth = self
                .params
                .depth
                .expect("Expected \"depth\" command for alphabeta")
                - 1;

            let score = -self.alphabeta(-i32::MAX, i32::MAX, depth);

            self.chess = board_copy;

            if self.is_stopping {
                return 0; // Return 0 if search was stopped
            }

            if score > best_score {
                best_score = score;
                best_move = mv;
            }
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

impl<'a> Alphabeta<'a> {
    pub fn new(
        params: SearchParams,
        chess: chess::ChessGame,
        tables: &'a tables::Tables,
        sig: &'a AbortSignal,
    ) -> Alphabeta<'a> {
        Alphabeta {
            chess,
            params,
            tables,
            nodes: 0,
            is_stopping: false,
            score: -i32::MAX,
            sig,
        }
    }

    // Alpha-Beta pruning cuts off branches of the search tree where the opponent could play a move on
    // their current turn that will result in a worse outcome for engine's side than what a previously
    // explored branch will lead to. This will only cut off branches that are worse than a previously
    // found move, since an opponent playing optimally will choose a move leading to a better outcome for them than
    // what we have already found on another branch, with respect to our evaluation function.
    //
    // In the example below, white should NOT play move #2 because it contains a move for black that will lead to a worse
    // outcome (W50) than an already found move #1 (W100). The search will continue from move #3 without spending time on
    // exploring sub-branches of #2.
    //
    //
    // Branch#1:                α = -∞
    // Swap + negate            β = +∞           #1               /------\     #3
    //  α and β:            /-------------------------------------| W100 |-----------...
    //      α = -β (-∞)     |                  Branch#2:          \------/
    //      β = -α (+∞)     |                  α=100 (100 > -∞)      |
    //                      |                                        |
    //                      |                  Swap + negate         | #2
    //                      |                   α and β:             |
    // No branches can be   |                       α = -β (-∞)      |
    // pruned because β=+∞  \/                      β = -α (-100)    \/
    //                   /-------\                                /------\
    //       /-----------| B-100 |----------\          /----------| B-50 |
    //       |           \-------/          |          |          \------/
    //       |              |               |          |              X <- Beta prune due to -50 >= β (-50 >= -100).
    //       |              |               |          |                   β tells that a better move for white was already
    //       \/             \/              \/         \/                 found from an earlier branch (#1).
    //    /------\       /------\       /------\      /------\            Continue search from #3.
    //    | W500 |       | W200 |       | W100 |      | W50  |
    //    \------/       \------/       \------/      \------/
    fn alphabeta(&mut self, alpha: i32, beta: i32, depth: u8) -> i32 {
        self.nodes += 1;

        if depth == 0 {
            return self.evaluate();
        }

        if self.nodes & 0xFFF == 0 && self.check_sigabort() {
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

            let score = -self.alphabeta(-beta, -alpha, depth - 1);

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
                return -SCORE_INF;
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
