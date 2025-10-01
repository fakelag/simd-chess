use crossbeam::channel;

use crate::{
    chess_v2::*,
    engine::{
        chess_v2,
        search::{
            AbortSignal, SearchStrategy,
            repetition_v2::RepetitionTable,
            search_params::SearchParams,
            transposition_v2::{BoundType, TranspositionTable},
        },
        sorting,
        tables::{self},
    },
    util::{self, Align64, table_mirror, table_negate_i8},
};

type Eval = i16;

const SCORE_INF: Eval = i16::MAX - 1;
const WEIGHT_KING: Eval = 0;
const WEIGHT_QUEEN: Eval = 1000;
const WEIGHT_ROOK: Eval = 525;
const WEIGHT_BISHOP: Eval = 350;
const WEIGHT_KNIGHT: Eval = 350;
const WEIGHT_PAWN: Eval = 100;
const PV_DEPTH: usize = 64;

const WEIGHT_TABLE_MGEG: [[Eval; PieceIndex::PieceIndexMax as usize]; 2] = [
    [
        0,
        WEIGHT_KING,
        WEIGHT_QUEEN,
        WEIGHT_ROOK,
        WEIGHT_BISHOP,
        WEIGHT_KNIGHT,
        WEIGHT_PAWN,
        0,
        0,
        -WEIGHT_KING,
        -WEIGHT_QUEEN,
        -WEIGHT_ROOK,
        -WEIGHT_BISHOP,
        -WEIGHT_KNIGHT,
        -WEIGHT_PAWN,
        0,
    ],
    [
        0,
        WEIGHT_KING,
        WEIGHT_QUEEN + 50,
        WEIGHT_ROOK + 30,
        WEIGHT_BISHOP - 30,
        WEIGHT_KNIGHT - 30,
        WEIGHT_PAWN + 20,
        0,
        0,
        -WEIGHT_KING,
        -(WEIGHT_QUEEN + 50),
        -(WEIGHT_ROOK + 30),
        -(WEIGHT_BISHOP - 30),
        -(WEIGHT_KNIGHT - 30),
        -(WEIGHT_PAWN + 20),
        0,
    ],
];

const WEIGHT_TABLE: [Eval; 12] = [
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

/*
    Evals for white pieces in square format. Black pieces are mirrored
    and inverted for quick negative scoring. MG = midgame, EG = endgame.
    [a8, b8, c8, d8, e8, f8, g8, h8,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a1, b1, c1, d1, e1, f1, g1, h1]
*/
#[cfg_attr(any(), rustfmt::skip)]
pub const EVAL_TABLES_INV_MGEG: Align64<[[[i8; 64]; 16]; 2]> = const {
    let eval_white_king_mg = [
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -30,-40,-40,-50,-50,-40,-40,-30,
        -20,-30,-30,-40,-40,-30,-30,-20,
        -10,-20,-20,-20,-20,-20,-20,-10,
        20, 20,  0,  0,  0,  0, 20, 20,
        20, 30, 10,  0,  0, 10, 30, 20
    ];
    let eval_white_king_eg = [
        -50,-40,-30,-20,-20,-30,-40,-50,
        -30,-20,-10,  0,  0,-10,-20,-30,
        -30,-10, 20, 30, 30, 20,-10,-30,
        -30,-10, 30, 40, 40, 30,-10,-30,
        -30,-10, 30, 40, 40, 30,-10,-30,
        -30,-10, 20, 30, 30, 20,-10,-30,
        -30,-30,  0,  0,  0,  0,-30,-30,
        -50,-30,-30,-30,-30,-30,-30,-50
    ];

    let eval_white_queen_mg = [
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -5,   0,  5,  5,  5,  5,  0, -5,
         0,   0,  5,  5,  5,  5,  0, -5,
        -10,  5,  5,  5,  5,  5,  0,-10,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -20,-10,-10, 0, 0,-10,-10,-20
    ];
    let eval_white_queen_eg = [
        -20,-10,-10, -5, -5,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5,  5,  5,  5,  0,-10,
        -5,   0,  5,  5,  5,  5,  0, -5,
         0,   0,  5,  5,  5,  5,  0, -5,
        -10,  5,  5,  5,  5,  5,  0,-10,
        -10,  0,  5,  0,  0,  0,  0,-10,
        -20,-10,-10, -10, -10,-10,-10,-20
    ];

    let eval_white_rook = [
        0,  0,  0,  0,  0,  0,  0,  0,
        5, 10, 10, 10, 10, 10, 10,  5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        -5,  0,  0,  0,  0,  0,  0, -5,
        0,  0,  0,  5,  5,  0,  0,  0
    ];
    let eval_white_bishop = [
        -20,-10,-10,-10,-10,-10,-10,-20,
        -10,  0,  0,  0,  0,  0,  0,-10,
        -10,  0,  5, 10, 10,  5,  0,-10,
        -10,  5,  5, 10, 10,  5,  5,-10,
        -10,  0, 10, 10, 10, 10,  0,-10,
        -10, 10, 10, 10, 10, 10, 10,-10,
        -10,  5,  0,  0,  0,  0,  5,-10,
        -20,-10,-10,-10,-10,-10,-10,-20,
    ];
    let eval_white_knight = [
        -50,-40,-30,-30,-30,-30,-40,-50,
        -40,-20,  0,  0,  0,  0,-20,-40,
        -30,  0, 10, 15, 15, 10,  0,-30,
        -30,  5, 15, 20, 20, 15,  5,-30,
        -30,  0, 15, 20, 20, 15,  0,-30,
        -30,  5, 10, 15, 15, 10,  5,-30,
        -40,-20,  0,  5,  5,  0,-20,-40,
        -50,-40,-30,-30,-30,-30,-40,-50,
    ];

    let eval_white_pawn_mg = [
        0,   0,  0,  0,  0,  0,  0,  0,
        30, 30, 30, 30, 30, 30, 30, 30,
        10, 10, 20, 30, 30, 20, 10, 10,
        5,   5, 10, 25, 25, 10,  5,  5,
        0,   0,  0, 20, 20,  0,  0,  0,
        5,  -5,-10,  0,  0,-10, -5,  5,
        5,  10, 10,-20,-20, 10, 10,  5,
        0,   0,  0,  0,  0,  0,  0,  0,
    ];
    let eval_white_pawn_eg = [
        0,  0,  0,  0,  0,  0,  0,  0,
        100,100,100,100,100,100,100,100,
        90,  90, 90, 90, 90, 90, 90, 90,
        80,  80, 80, 80, 80, 80, 80, 80,
        0,    0, 10, 20, 20, 10,  0,  0,
        5,   -5,-10,  0,  0,-10, -5,  5,
        5,   10, 10,-20,-20, 10, 10,  5,
        0,    0,  0,  0,  0,  0,  0,  0,
    ];
    let tabl_zero = [
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0
    ];

    Align64([
        [
        tabl_zero,
        // Mirror white pieces to LERF endianness
        table_mirror(eval_white_king_mg, 8),
        table_mirror(eval_white_queen_mg, 8),
        table_mirror(eval_white_rook, 8),
        table_mirror(eval_white_bishop, 8),
        table_mirror(eval_white_knight, 8),
        table_mirror(eval_white_pawn_mg, 8),
        tabl_zero,
        tabl_zero,
        table_negate_i8(eval_white_king_mg),
        table_negate_i8(eval_white_queen_mg),
        table_negate_i8(eval_white_rook),
        table_negate_i8(eval_white_bishop),
        table_negate_i8(eval_white_knight),
        table_negate_i8(eval_white_pawn_mg),
        tabl_zero,
    ], [
        tabl_zero,
        table_mirror(eval_white_king_eg, 8),
        table_mirror(eval_white_queen_eg, 8),
        table_mirror(eval_white_rook, 8),
        table_mirror(eval_white_bishop, 8),
        table_mirror(eval_white_knight, 8),
        table_mirror(eval_white_pawn_eg, 8),
        tabl_zero,
        tabl_zero,
        table_negate_i8(eval_white_king_eg),
        table_negate_i8(eval_white_queen_eg),
        table_negate_i8(eval_white_rook),
        table_negate_i8(eval_white_bishop),
        table_negate_i8(eval_white_knight),
        table_negate_i8(eval_white_pawn_eg),
        tabl_zero,
    ]])
};

#[cfg_attr(any(), rustfmt::skip)]
const MVV_LVA_SCORES: [[usize; 16]; 16] = [
    /* Ep Cap */      [0, 0, 0, 0, 0, 0, 5000, 0, 0, 0, 0, 0, 0, 0, 5000, 0],
    /* WhiteKing */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* WhiteQueen */  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1500, 1400, 1300, 1200, 1100, 1000, 0],
    /* WhiteRook */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 2500, 2400, 2300, 2200, 2100, 2000, 0],
    /* WhiteBishop */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 3500, 3400, 3300, 3200, 3100, 3000, 0],
    /* WhiteKnight */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 4500, 4400, 4300, 4200, 4100, 4000, 0],
    /* WhitePawn */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 5500, 5400, 5300, 5200, 5100, 5000, 0],
    /* Pad */         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* Black Null */  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackKing */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackQueen */  [0, 1500, 1400, 1300, 1200, 1100, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackRook */   [0, 2500, 2400, 2300, 2200, 2100, 2000, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackBishop */ [0, 3500, 3400, 3300, 3200, 3100, 3000, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackKnight */ [0, 4500, 4400, 4300, 4200, 4100, 4000, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackPawn */   [0, 5500, 5400, 5300, 5200, 5100, 5000, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* Pad */         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
];

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
    sig: &'a AbortSignal,

    ply: u8,
    is_stopping: bool,

    pv_table: Box<PvTable>,
    pv: [u16; PV_DEPTH],
    pv_length: usize,
    pv_trace: bool,

    move_list: [u16; 256],

    tt: &'a mut TranspositionTable,
    rt: RepetitionTable,

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
        chess: ChessGame,
        tables: &'a tables::Tables,
        tt: &'a mut TranspositionTable,
        rt: RepetitionTable,
        sig: &'a AbortSignal,
    ) -> Search<'a> {
        let mut s = Search {
            sig,
            chess: chess,
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
    pub fn get_rt(&self) -> &RepetitionTable {
        &self.rt
    }

    fn go(&mut self, alpha: Eval, beta: Eval, depth: u8) -> Eval {
        let ply = self.ply as usize & (PV_DEPTH - 1);

        self.node_count += 1;
        self.pv_table.lengths[ply] = 0;

        if self.node_count & 0x7FF == 0 && self.check_sigabort() {
            self.is_stopping = true;
            return 0;
        }

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

        // Null move pruning
        if apply_pruning && depth_pruning && !self.is_endgame() {
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
            move_scores[i] = self.score_move_asc(i as u8, mv, pv_move, tt_move);
        }

        sorting::u16::sort_256x16_asc_avx512(&mut move_scores, move_count);

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

            let move_ok = unsafe {
                // Safety: mv is generated by gen_moves_avx512, so it is guaranteed to be legal
                self.chess.make_move(mv, self.tables)
            };

            let is_valid_move = move_ok && !self.chess.in_check(self.tables, !self.chess.b_move());

            if !is_valid_move {
                self.chess = board_copy;
                continue;
            }

            let is_non_capture_or_promotion = mv & (MV_FLAG_CAP | MV_FLAG_PROMOTION) == 0;

            let late_move_reduction =
                num_legal_moves > 3 && depth_pruning && is_non_capture_or_promotion;

            num_legal_moves += 1;

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

            // if ply == 0 && self.params.debug {
            //     println!(
            //         "move {}: score {} lmr {} raise alpha {} raise beta {}",
            //         util::move_string_dbg(mv),
            //         score,
            //         late_move_reduction,
            //         score > alpha,
            //         score >= beta
            //     );
            // }

            self.ply -= 1;
            self.chess = board_copy;

            if self.is_stopping {
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
            move_scores[i] = self.score_move_asc_quiescence(i as u8, mv);
        }

        sorting::u16::sort_256x16_asc_avx512(&mut move_scores, move_count);

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

            let move_ok = unsafe {
                // Safety: mv is generated by gen_moves_avx512, so it is guaranteed to be legal
                self.chess.make_move(mv, self.tables)
            };

            let is_valid_move = move_ok && !self.chess.in_check(self.tables, !self.chess.b_move());

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
    pub fn evaluate(&self) -> Eval {
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
            score_mg += 24;
            score_eg += 48;
        }
        if bb > 1 {
            score_mg -= 24;
            score_eg -= 48;
        }

        let w_moves = self.chess.estimate_move_count(false, &self.tables);
        let b_moves = self.chess.estimate_move_count(true, &self.tables);
        let move_ratio = w_moves as Eval - b_moves as Eval;

        // let all_pawns =
        //     bitboards[PieceIndex::WhitePawn as usize] | bitboards[PieceIndex::BlackPawn as usize];

        let score =
            ((score_mg as f32 * phase + score_eg as f32 * (1.0 - phase)) as Eval) + move_ratio * 10;

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

    fn evaluate_legacy(&mut self) -> Eval {
        let material = self.chess.material();
        let is_endgame = (((material[0] & (!1023)) | (material[1] & (!1023))) == 0) as usize;

        let boards = self.chess.bitboards();

        let mut final_score: Eval = 0;

        for piece_id in
            crate::util::PieceId::WhiteKing as usize..crate::util::PieceId::PieceMax as usize
        {
            let mut board_bits = boards[PieceIndex::from(util::PieceId::from(piece_id)) as usize];
            loop {
                let piece_square = crate::pop_ls1b!(board_bits);

                let pst_index = match util::PieceId::from(piece_id) {
                    util::PieceId::WhiteKing => is_endgame as usize,
                    util::PieceId::WhiteQueen => piece_id + 1,
                    util::PieceId::WhiteRook => piece_id + 1,
                    util::PieceId::WhiteBishop => piece_id + 1,
                    util::PieceId::WhiteKnight => piece_id + 1,
                    util::PieceId::WhitePawn => piece_id + 1,
                    util::PieceId::BlackKing => piece_id + is_endgame + 1,
                    util::PieceId::BlackQueen => piece_id + 2,
                    util::PieceId::BlackRook => piece_id + 2,
                    util::PieceId::BlackBishop => piece_id + 2,
                    util::PieceId::BlackKnight => piece_id + 2,
                    util::PieceId::BlackPawn => piece_id + 2,
                    _ => unreachable!(),
                };

                let square_bonus =
                    tables::Tables::EVAL_TABLES_INV_I8_OLD[pst_index][piece_square as usize];
                final_score += WEIGHT_TABLE[piece_id] + square_bonus as Eval;
            }
        }

        final_score * if self.chess.b_move() { -1 } else { 1 }
    }

    #[inline(always)]
    fn sort_moves_quiet(&self, a: &u16, b: &u16) -> std::cmp::Ordering {
        unsafe {
            let beta_mv0 = self.beta_moves.get_unchecked(self.ply as usize)[0];
            let beta_mv1 = self.beta_moves.get_unchecked(self.ply as usize)[1];

            if *a == beta_mv0 {
                return std::cmp::Ordering::Less;
            }

            if *b == beta_mv0 {
                return std::cmp::Ordering::Greater;
            }

            if *a == beta_mv1 {
                return std::cmp::Ordering::Less;
            }

            if *b == beta_mv1 {
                return std::cmp::Ordering::Greater;
            }

            let a_src_sq = a & 0x3F;
            let b_src_sq = b & 0x3F;

            let a_dst_sq = (a >> 6) & 0x3F;
            let b_dst_sq = (b >> 6) & 0x3F;

            let spt = self.chess.spt();

            let a_src_piece = *spt.get_unchecked(a_src_sq as usize) as usize;
            let b_src_piece = *spt.get_unchecked(b_src_sq as usize) as usize;

            let a_score = self
                .alpha_moves
                .get_unchecked(a_src_piece)
                .get_unchecked(a_dst_sq as usize);
            let b_score = self
                .alpha_moves
                .get_unchecked(b_src_piece)
                .get_unchecked(b_dst_sq as usize);

            return b_score.cmp(&a_score);
        }
    }

    #[inline(always)]
    fn sort_moves_cap(&self, a_capture: &u16, b: &u16) -> std::cmp::Ordering {
        if (*b & MV_FLAG_CAP) == 0 {
            // b is a quiet move
            return std::cmp::Ordering::Less;
        }

        let a_src_sq = a_capture & 0x3F;
        let b_src_sq = b & 0x3F;

        let a_dst_sq = (a_capture >> 6) & 0x3F;
        let b_dst_sq = (b >> 6) & 0x3F;

        // MVV-LVA
        unsafe {
            let spt = self.chess.spt();
            let a_dst_piece = *spt.get_unchecked(a_dst_sq as usize);
            let b_dst_piece = *spt.get_unchecked(b_dst_sq as usize);
            let a_src_piece = *spt.get_unchecked(a_src_sq as usize);
            let b_src_piece = *spt.get_unchecked(b_src_sq as usize);

            let a_score = *MVV_LVA_SCORES
                .get_unchecked(a_dst_piece as usize)
                .get_unchecked(a_src_piece as usize);

            let b_score = *MVV_LVA_SCORES
                .get_unchecked(b_dst_piece as usize)
                .get_unchecked(b_src_piece as usize);

            return a_score.cmp(&b_score);
        }
    }

    #[inline(always)]
    fn sort_moves(&self, a: &u16, b: &u16, pv_move: u16, tt_move: u16) -> std::cmp::Ordering {
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

        if (*a & MV_FLAG_CAP) == 0 {
            // a is a quiet move

            if (*b & MV_FLAG_CAP) == 0 {
                // Both moves are quiet
                return self.sort_moves_quiet(a, b);
            }

            // b is a capture
            return std::cmp::Ordering::Greater;
        }

        self.sort_moves_cap(a, b)
    }

    #[inline(always)]
    fn sort_moves_quiescence(&self, a: &u16, b: &u16) -> std::cmp::Ordering {
        debug_assert!(*a != 0, "a should not be a null move");
        debug_assert!(*b != 0, "b should not be a null move");

        if (*a & MV_FLAG_CAP) == 0 {
            // a is a quiet move

            if (*b & MV_FLAG_CAP) == 0 {
                return std::cmp::Ordering::Equal;
            }

            // b is a capture
            return std::cmp::Ordering::Greater;
        }

        self.sort_moves_cap(a, b)
    }

    #[inline(always)]
    fn score_move_asc(&self, index: u8, mv: u16, pv_move: u16, tt_move: u16) -> u16 {
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
    fn score_move_asc_quiescence(&self, index: u8, mv: u16) -> u16 {
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
