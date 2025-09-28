use crate::{
    engine::chess_v2::PieceIndex,
    util::{Align64, table_mirror, table_negate_i8},
};

pub type Eval = i16;

pub const SCORE_INF: Eval = i16::MAX - 1;
pub const WEIGHT_KING: Eval = 0;
pub const WEIGHT_QUEEN: Eval = 1000;
pub const WEIGHT_ROOK: Eval = 525;
pub const WEIGHT_BISHOP: Eval = 350;
pub const WEIGHT_KNIGHT: Eval = 350;
pub const WEIGHT_PAWN: Eval = 100;

// Diet versions
pub const WEIGHT_KING_I8: i8 = 0;
pub const WEIGHT_QUEEN_I8: i8 = 30;
pub const WEIGHT_ROOK_I8: i8 = 15;
pub const WEIGHT_BISHOP_I8: i8 = 10;
pub const WEIGHT_KNIGHT_I8: i8 = 10;
pub const WEIGHT_PAWN_I8: i8 = 3;

const COLLECT_STATS: bool = true;

pub struct EvalStats {
    pub probe_hit: usize,
    pub probe_miss: usize,
    pub fill_percentage: f64,
    pub collisions: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct EvalEntry {
    hash_0: u16,
    hash_1: u16,
    hash_2: u16,
    eval: Eval,
}

impl EvalEntry {
    pub fn new() -> Self {
        EvalEntry {
            hash_0: 0,
            hash_1: 0,
            hash_2: 0,
            eval: 0,
        }
    }
}

pub struct EvalTable {
    entries: Box<[EvalEntry]>,
    table_size_mask: usize,
    index_bits: u32,

    // Debugging
    hash64: Box<[u64]>,
    stats: Box<EvalStats>,
}

impl EvalTable {
    pub fn new(size_hint_kb: usize) -> Self {
        const ENTRY_SIZE: usize = std::mem::size_of::<EvalEntry>();

        let max_entries = size_hint_kb * 1024 / ENTRY_SIZE;
        let table_size = max_entries.next_power_of_two();

        let table_mask = table_size - 1;
        let index_bits = table_mask.count_ones();

        assert!(
            64 - index_bits <= 48,
            "Not enough index bits for eval table: {}",
            index_bits
        );

        EvalTable {
            table_size_mask: table_mask,
            entries: vec![EvalEntry::new(); table_size].into_boxed_slice(),
            index_bits,
            hash64: vec![0u64; table_size].into_boxed_slice(),
            stats: Box::new(EvalStats {
                probe_hit: 0,
                probe_miss: 0,
                fill_percentage: 0.0,
                collisions: 0,
            }),
        }
    }

    #[inline(always)]
    pub fn probe(&mut self, hash: u64) -> Option<Eval> {
        let entry_index = (hash as usize) & self.table_size_mask;

        // Safety: entry_index is guaranteed to be within bounds due to table_size_mask
        unsafe { std::hint::assert_unchecked(entry_index < self.entries.len()) };

        let entry = &mut self.entries[entry_index];

        let part = Self::get_hash_part(self.index_bits, hash);
        let p0 = part as u16;
        let p1 = (part >> 16) as u16;
        let p2 = (part >> 32) as u16;

        let score = if (((entry.hash_0 == p0) as u8)
            & ((entry.hash_1 == p1) as u8)
            & ((entry.hash_2 == p2) as u8))
            == 1
        {
            Some(entry.eval)
        } else {
            None
        };

        if COLLECT_STATS {
            if score.is_some() {
                self.stats.probe_hit += 1;

                let hash64 = self.hash64[entry_index];
                if hash64 != 0 && hash64 != hash {
                    println!(
                        "Eval table collision detected: existing: {:016x}, new: {:016x}",
                        hash64, hash
                    );
                    self.stats.collisions += 1;
                }
            } else {
                self.stats.probe_miss += 1;
            }
        }

        score
    }

    #[inline(always)]
    pub fn store(&mut self, hash: u64, eval: Eval) {
        let entry_index = (hash as usize) & self.table_size_mask;

        // Safety: entry_index is guaranteed to be within bounds due to table_size_mask
        unsafe { std::hint::assert_unchecked(entry_index < self.entries.len()) };

        let entry = &mut self.entries[entry_index];

        let tag_parts = Self::get_hash_part(self.index_bits, hash);
        let p0 = tag_parts as u16;
        let p1 = (tag_parts >> 16) as u16;
        let p2 = (tag_parts >> 32) as u16;

        entry.hash_0 = p0;
        entry.hash_1 = p1;
        entry.hash_2 = p2;

        entry.eval = eval;

        if COLLECT_STATS {
            self.hash64[entry_index] = hash;
        }
    }

    #[inline(always)]
    pub fn get_hash_part(index_bits: u32, hash: u64) -> u64 {
        let hash_bits = hash >> index_bits;
        hash_bits
    }

    pub fn calc_stats(&self) -> EvalStats {
        if !COLLECT_STATS {
            return EvalStats {
                fill_percentage: 0.0,
                probe_hit: 0,
                probe_miss: 0,
                collisions: 0,
            };
        }

        let mut num_filled = 0;

        for entry in &self.entries {
            num_filled += (entry.hash_0 != 0 || entry.hash_1 != 0 || entry.hash_2 != 0) as usize;
        }

        EvalStats {
            fill_percentage: num_filled as f64 / self.entries.len() as f64,
            probe_hit: self.stats.probe_hit,
            probe_miss: self.stats.probe_miss,
            collisions: self.stats.collisions,
        }
    }
}

pub const WEIGHT_TABLE_ABS: [Eval; PieceIndex::PieceIndexMax as usize] = [
    0,
    WEIGHT_KING,
    WEIGHT_QUEEN,
    WEIGHT_ROOK,
    WEIGHT_BISHOP,
    WEIGHT_KNIGHT,
    WEIGHT_PAWN,
    0,
    0,
    WEIGHT_KING,
    WEIGHT_QUEEN,
    WEIGHT_ROOK,
    WEIGHT_BISHOP,
    WEIGHT_KNIGHT,
    WEIGHT_PAWN,
    0,
];

pub const WEIGHT_TABLE_ABS_I8: [i8; PieceIndex::PieceIndexMax as usize] = [
    0,
    WEIGHT_KING_I8,
    WEIGHT_QUEEN_I8,
    WEIGHT_ROOK_I8,
    WEIGHT_BISHOP_I8,
    WEIGHT_KNIGHT_I8,
    WEIGHT_PAWN_I8,
    0,
    0,
    WEIGHT_KING_I8,
    WEIGHT_QUEEN_I8,
    WEIGHT_ROOK_I8,
    WEIGHT_BISHOP_I8,
    WEIGHT_KNIGHT_I8,
    WEIGHT_PAWN_I8,
    0,
];

pub const WEIGHT_TABLE_MGEG: [[Eval; PieceIndex::PieceIndexMax as usize]; 2] = [
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
        60,  60, 60, 60, 60, 60, 60, 60,
        50,  50, 50, 50, 50, 50, 50, 50,
        5,   5,  10, 25, 25, 10,  5,  5,
        0,   0,  0, 20, 20,  0,  0,  0,
        5,  -5,-10,  0,  0,-10, -5,  5,
        5,  10, 10,-20,-20, 10, 10,  5,
        0,   0,  0,  0,  0,  0,  0,  0,
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

    // Final eval_v2:
    // Versus match ended: 146 evaltest_v2.exe wins, 95 evaltest_v1.exe wins, 59 draws
    // Versus match ended: 177 evaltest_v3.exe wins, 144 evaltest_v2.exe wins, 79 draws

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
