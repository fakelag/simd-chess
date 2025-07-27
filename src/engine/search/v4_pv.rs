use std::{cell::UnsafeCell, mem::MaybeUninit};

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
const PV_DEPTH: usize = 64;

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
}

impl<'a> SearchStrategy<'a> for Search<'a> {
    #[inline(never)]
    fn new(
        params: SearchParams,
        chess: chess::ChessGame,
        tables: &'a tables::Tables,
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
        };

        s
    }

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

        if depth == 0 {
            return self.evaluate();
        }

        let mut alpha = alpha;

        let mut move_list = [0u16; 256];

        let move_count = if self.pv_trace {
            // PV Tracing: Generate the PV-move from the last iteration as an extra first move to play.
            // This makes sure that PV is played first on subsequent searches. Though it means that the move
            // is possibly played twice for a given board position, it reduces overall nodes searched due to AB cutoffs.
            self.pv_trace = self.pv_length > (self.ply as usize + 1);

            move_list[0] = self.pv[self.ply as usize];
            self.chess.gen_moves_slow(self.tables, &mut move_list[1..]) + 1
        } else {
            self.chess.gen_moves_slow(self.tables, &mut move_list)
        };

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
