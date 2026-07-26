use rand::{Rng, SeedableRng};

use crate::{
    engine::chess_v2,
    util::{self, Align64, Side, table_mirror, table_negate_i8},
};
use std::arch::x86_64::*;

enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

enum Rank {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
}

enum FileOrRank {
    File(File),
    Rank(Rank),
}

macro_rules! ex_mask {
    ($file_or_rank:expr) => {{
        let mut square = 0;
        let mut mask = u64::MAX;
        while square < 64 {
            let rank = square / 8;
            let file = square % 8;
            match $file_or_rank {
                FileOrRank::File(ex_file) => {
                    if file == ex_file as usize {
                        mask &= !(1 << square);
                    }
                }
                FileOrRank::Rank(ex_rank) => {
                    if rank == ex_rank as usize {
                        mask &= !(1 << square);
                    }
                }
            }
            square += 1;
        }
        mask
    }};
}

pub const EX_H_FILE: u64 = const { ex_mask!(FileOrRank::File(File::H)) };
pub const EX_A_FILE: u64 = const { ex_mask!(FileOrRank::File(File::A)) };
pub const EX_G_FILE: u64 = const { ex_mask!(FileOrRank::File(File::G)) };
pub const EX_B_FILE: u64 = const { ex_mask!(FileOrRank::File(File::B)) };
const EX_OUTER: u64 = const {
    ex_mask!(FileOrRank::File(File::A))
        & ex_mask!(FileOrRank::File(File::H))
        & ex_mask!(FileOrRank::Rank(Rank::One))
        & ex_mask!(FileOrRank::Rank(Rank::Eight))
};

pub struct ZobristKeys {
    pub hash_piece_squares_new: [[u64; 64]; 16],
    pub hash_side_to_move: u64,
    pub hash_castling_rights: [u64; 16],
    pub hash_en_passant_squares: [u64; 64],
    pub no_pawn_key: u64,
    pub hash_pawn_squares: [[u64; 64]; 16],
}

pub struct Tables {
    rook_move_mask: Box<[u64; Self::ROOK_TABLE_SIZE]>,
    bishop_move_mask: Box<[u64; Self::BISHOP_TABLE_SIZE]>,
    pub zobrist_hash_keys: Box<ZobristKeys>,
    cuckoo_keys: Box<[u64; Self::CUCKOO_SIZE]>,
    cuckoo_moves: Box<[u16; Self::CUCKOO_SIZE]>,
    between_squares: Box<[u64; 64 * 64]>,
}

impl Tables {
    pub fn new() -> Self {
        let (
            zobrist_hash_squares,
            zobrist_side_to_move,
            zobrist_castling_rights,
            zobrist_en_passant_squares,
            no_pawn_key,
            hash_pawn_squares,
        ) = Self::gen_zobrist_hashes();

        let rook_move_mask = Self::gen_rook_move_table();
        let bishop_move_mask = Self::gen_bishop_move_table();

        let mut tables = Self {
            rook_move_mask,
            bishop_move_mask,
            zobrist_hash_keys: Box::new(ZobristKeys {
                hash_piece_squares_new: zobrist_hash_squares,
                hash_side_to_move: zobrist_side_to_move,
                hash_castling_rights: zobrist_castling_rights,
                hash_en_passant_squares: zobrist_en_passant_squares,
                no_pawn_key,
                hash_pawn_squares,
            }),
            cuckoo_keys: vec![0u64; Self::CUCKOO_SIZE]
                .into_boxed_slice()
                .try_into()
                .unwrap(),
            cuckoo_moves: vec![0u16; Self::CUCKOO_SIZE]
                .into_boxed_slice()
                .try_into()
                .unwrap(),
            between_squares: vec![0u64; 64 * 64].into_boxed_slice().try_into().unwrap(),
        };

        tables.gen_between_squares();
        tables.gen_cuckoo_table();

        tables
    }

    pub const CUCKOO_SIZE: usize = 1 << 13;

    pub const ROOK_OCCUPANCY_BITS: usize = 12;
    pub const BISHOP_OCCUPANCY_BITS: usize = 9;
    pub const ROOK_OCCUPANCY_MAX: usize = 1 << Self::ROOK_OCCUPANCY_BITS;
    pub const BISHOP_OCCUPANCY_MAX: usize = 1 << Self::BISHOP_OCCUPANCY_BITS;

    pub const LT_KING_MOVE_MASKS: [u64; 64] = const {
        let mut moves = [0; 64];
        let mut square = 0;

        while square < 64 {
            let sq_bit = 1 << square;

            moves[square] |= (sq_bit >> 1) & EX_H_FILE;
            moves[square] |= (sq_bit << 1) & EX_A_FILE;
            moves[square] |= sq_bit << 8;
            moves[square] |= sq_bit >> 8;

            moves[square] |= (sq_bit >> 9) & EX_H_FILE;
            moves[square] |= (sq_bit >> 7) & EX_A_FILE;
            moves[square] |= (sq_bit << 9) & EX_A_FILE;
            moves[square] |= (sq_bit << 7) & EX_H_FILE;

            square += 1;
        }

        moves
    };

    pub const LT_PAWN_CAPTURE_MASKS: [[u64; 64]; Side::SideMax as usize] = const {
        let mut moves = [[0; 64]; Side::SideMax as usize];
        let mut square = 0;

        while square < 64 {
            let sq_bit = 1 << square;

            moves[Side::White as usize][square] |= (sq_bit << 9) & EX_A_FILE;
            moves[Side::White as usize][square] |= (sq_bit << 7) & EX_H_FILE;
            moves[Side::Black as usize][square] |= (sq_bit >> 9) & EX_H_FILE;
            moves[Side::Black as usize][square] |= (sq_bit >> 7) & EX_A_FILE;

            square += 1;
        }

        moves
    };

    pub const LT_KNIGHT_MOVE_MASKS: [u64; 64] = const {
        let mut moves = [0; 64];
        let mut square = 0;

        while square < 64 {
            let sq_bit = 1 << square;

            moves[square] |= (sq_bit << 15) & EX_H_FILE;
            moves[square] |= (sq_bit << 17) & EX_A_FILE;

            moves[square] |= (sq_bit << 6) & EX_G_FILE & EX_H_FILE;
            moves[square] |= (sq_bit << 10) & EX_A_FILE & EX_B_FILE;

            moves[square] |= (sq_bit >> 6) & EX_A_FILE & EX_B_FILE;
            moves[square] |= (sq_bit >> 10) & EX_G_FILE & EX_H_FILE;

            moves[square] |= (sq_bit >> 15) & EX_A_FILE;
            moves[square] |= (sq_bit >> 17) & EX_H_FILE;

            square += 1;
        }

        moves
    };

    pub const LT_ROOK_OCCUPANCY_MASKS: [u64; 64] = const {
        let mut moves = [0; 64];
        let mut square: usize = 0;

        while square < 64 {
            let mut rank = square as u64 / 8;
            if rank > 1 {
                rank = 1;
            }
            let mut file = square as u64 % 8;

            while rank < 8 {
                moves[square] |= 1 << (rank * 8 + file);
                rank += 1;

                if rank > 6 {
                    break;
                }
            }

            rank = square as u64 / 8;

            if file > 1 {
                file = 1;
            }

            while file < 7 {
                moves[square] |= 1 << (rank * 8 + file);
                file += 1;

                if file > 6 {
                    break;
                }
            }

            moves[square] &= !(1 << square);

            square += 1;
        }

        moves
    };

    pub const LT_ROOK_TABLE_OFFSETS: [u32; 64] = const {
        let mut offsets = [0u32; 64];
        let mut acc = 0u32;
        let mut square = 0;

        while square < 64 {
            offsets[square] = acc;
            acc += 1 << Self::LT_ROOK_OCCUPANCY_MASKS[square].count_ones();
            square += 1;
        }

        offsets
    };

    pub const ROOK_TABLE_SIZE: usize = (Self::LT_ROOK_TABLE_OFFSETS[63]
        + (1 << Self::LT_ROOK_OCCUPANCY_MASKS[63].count_ones()))
        as usize;

    pub const LT_BISHOP_OCCUPANCY_MASKS: [u64; 64] = const {
        let mut moves = [0; 64];
        let mut square: usize = 0;

        while square < 64 {
            let rank = square as u64 / 8;
            let file = square as u64 % 8;

            let mut rank_it = rank;
            let mut file_it = file;

            while rank_it < 8 && file_it > 0 {
                moves[square] |= 1 << (rank_it * 8 + file_it);
                file_it -= 1;
                rank_it += 1;
            }

            rank_it = rank;
            file_it = file;

            while rank_it < 8 && file_it < 8 {
                moves[square] |= 1 << (rank_it * 8 + file_it);
                file_it += 1;
                rank_it += 1;
            }

            rank_it = rank;
            file_it = file;

            while rank_it > 0 && file_it < 8 {
                moves[square] |= 1 << (rank_it * 8 + file_it);
                file_it += 1;
                rank_it -= 1;
            }

            rank_it = rank;
            file_it = file;

            while rank_it > 0 && file_it > 0 {
                moves[square] |= 1 << (rank_it * 8 + file_it);
                file_it -= 1;
                rank_it -= 1;
            }

            moves[square] &= !(1 << square);
            moves[square] &= EX_OUTER;

            square += 1;
        }

        moves
    };

    pub const LT_BISHOP_TABLE_OFFSETS: [u32; 64] = const {
        let mut offsets = [0u32; 64];
        let mut acc = 0u32;
        let mut square = 0;

        while square < 64 {
            offsets[square] = acc;
            acc += 1 << Self::LT_BISHOP_OCCUPANCY_MASKS[square].count_ones();
            square += 1;
        }

        offsets
    };

    pub const BISHOP_TABLE_SIZE: usize = (Self::LT_BISHOP_TABLE_OFFSETS[63]
        + (1 << Self::LT_BISHOP_OCCUPANCY_MASKS[63].count_ones()))
        as usize;

    pub const LT_LINES: [[u64; 64]; 64] = const {
        let mut result = [[0; 64]; 64];
        let mut rs0 = 0;

        while rs0 < 64 {
            let mut rs1 = 0;
            while rs1 < 64 {
                let rs0_rank = rs0 / 8;
                let rs0_file = rs0 % 8;
                let rs1_rank = rs1 / 8;
                let rs1_file = rs1 % 8;

                let mut diagonal_mask = 0;
                let mut line_mask = 0;

                let mut rank_it = rs0_rank;
                let mut file_it = rs0_file;

                while rank_it < rs1_rank && file_it > rs1_file {
                    diagonal_mask |= 1 << (rank_it * 8 + file_it);
                    file_it -= 1;
                    rank_it += 1;
                }

                let mut rank_it = rs0_rank;
                let mut file_it = rs0_file;

                while rank_it < rs1_rank && file_it < rs1_file {
                    diagonal_mask |= 1 << (rank_it * 8 + file_it);
                    file_it += 1;
                    rank_it += 1;
                }

                let mut rank_it = rs0_rank;
                let mut file_it = rs0_file;

                while rank_it > rs1_rank && file_it < rs1_file {
                    diagonal_mask |= 1 << (rank_it * 8 + file_it);
                    file_it += 1;
                    rank_it -= 1;
                }

                let mut rank_it = rs0_rank;
                let mut file_it = rs0_file;

                while rank_it > rs1_rank && file_it > rs1_file {
                    diagonal_mask |= 1 << (rank_it * 8 + file_it);
                    file_it -= 1;
                    rank_it -= 1;
                }

                diagonal_mask &= Tables::LT_BISHOP_OCCUPANCY_MASKS[rs1];

                // Rook moves
                let mut rank_it = rs0_rank;

                while rank_it < rs1_rank {
                    line_mask |= 1 << (rank_it * 8 + rs0_file);
                    rank_it += 1;
                }

                let mut rank_it = rs0_rank;

                while rank_it > rs1_rank {
                    line_mask |= 1 << (rank_it * 8 + rs0_file);
                    rank_it -= 1;
                }

                let mut file_it = rs0_file;

                while file_it < rs1_file {
                    line_mask |= 1 << (rs0_rank * 8 + file_it);
                    file_it += 1;
                }

                let mut file_it = rs0_file;

                while file_it > rs1_file {
                    line_mask |= 1 << (rs0_rank * 8 + file_it);
                    file_it -= 1;
                }

                line_mask &= Tables::LT_ROOK_OCCUPANCY_MASKS[rs1];

                result[rs0][rs1] |= line_mask;
                result[rs0][rs1] |= diagonal_mask;
                result[rs0][rs1] |= 1 << rs1;
                result[rs0][rs1] &= !(1 << rs0);
                // moves[square] &= !(1 << square);

                rs1 += 1;
            }
            rs0 += 1;
        }

        result
    };

    // Edge-inclusive king rays, split by slider type. Unlike LT_*_OCCUPANCY_MASKS these keep the
    // board-edge squares, so a pinner sitting on an edge is detectable.
    pub const LT_ROOK_RAY_MASKS: [u64; 64] = const {
        let mut moves = [0; 64];
        let mut square = 0;

        while square < 64 {
            let file = square as u64 % 8;
            let rank = square as u64 / 8;

            let mut r = 0;
            while r < 8 {
                moves[square] |= 1 << (r * 8 + file);
                r += 1;
            }

            let mut f = 0;
            while f < 8 {
                moves[square] |= 1 << (rank * 8 + f);
                f += 1;
            }

            moves[square] &= !(1 << square);

            square += 1;
        }

        moves
    };

    pub const LT_BISHOP_RAY_MASKS: [u64; 64] = const {
        let mut moves = [0; 64];
        let mut square = 0;

        while square < 64 {
            let rank = square as i64 / 8;
            let file = square as i64 % 8;

            let dirs = [(1i64, -1i64), (1, 1), (-1, 1), (-1, -1)];
            let mut d = 0;
            while d < 4 {
                let (dr, df) = dirs[d];
                let mut r = rank + dr;
                let mut f = file + df;
                while r >= 0 && r < 8 && f >= 0 && f < 8 {
                    moves[square] |= 1 << (r * 8 + f);
                    r += dr;
                    f += df;
                }
                d += 1;
            }

            square += 1;
        }

        moves
    };

    // Full edge-to-edge line through two collinear squares (rank/file/diagonal), else 0.
    pub const LT_FULL_LINE: [[u64; 64]; 64] = const {
        let mut result = [[0u64; 64]; 64];

        let mut a = 0;
        while a < 64 {
            let ar = (a / 8) as i64;
            let af = (a % 8) as i64;

            let mut b = 0;
            while b < 64 {
                let br = (b / 8) as i64;
                let bf = (b % 8) as i64;
                let dr = br - ar;
                let df = bf - af;

                if a != b && (dr == 0 || df == 0 || dr == df || dr == -df) {
                    let sr = dr.signum();
                    let sf = df.signum();

                    let mut line = 0u64;

                    let mut r = ar;
                    let mut f = af;
                    while r >= 0 && r < 8 && f >= 0 && f < 8 {
                        line |= 1u64 << ((r * 8 + f) as u32);
                        r += sr;
                        f += sf;
                    }

                    let mut r = ar - sr;
                    let mut f = af - sf;
                    while r >= 0 && r < 8 && f >= 0 && f < 8 {
                        line |= 1u64 << ((r * 8 + f) as u32);
                        r -= sr;
                        f -= sf;
                    }

                    result[a][b] = line;
                }

                b += 1;
            }

            a += 1;
        }

        result
    };

    #[cfg_attr(any(), rustfmt::skip)]
    pub const EVAL_TABLES_INV_I8_OLD: [[i8; 64]; util::PieceId::PieceMax as usize + 2] = const {
        /*
            Evals for white pieces in square format. Black pieces are mirrored
            and inverted for quick negative scoring.
            [a8, b8, c8, d8, e8, f8, g8, h8,
            a7, b7, c7, d7, e7, f7, g7, h7,
            a6, b6, c6, d6, e6, f6, g6, h6,
            a5, b5, c5, d5, e5, f5, g5, h5,
            a4, b4, c4, d4, e4, f4, g4, h4,
            a3, b3, c3, d3, e3, f3, g3, h3,
            a2, b2, c2, d2, e2, f2, g2, h2,
            a1, b1, c1, d1, e1, f1, g1, h1]
        */
        let eval_white_king = [
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
        let eval_white_queen = [
            -20,-10,-10, -5, -5,-10,-10,-20,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -10,  0,  5,  5,  5,  5,  0,-10,
            -5,  0,  5,  5,  5,  5,  0, -5,
            0,  0,  5,  5,  5,  5,  0, -5,
            -10,  5,  5,  5,  5,  5,  0,-10,
            -10,  0,  5,  0,  0,  0,  0,-10,
            -20,-10,-10, -5, -5,-10,-10,-20
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
        let eval_white_pawn = [
            0,  0,  0,  0,  0,  0,  0,  0,
            50, 50, 50, 50, 50, 50, 50, 50,
            10, 10, 20, 30, 30, 20, 10, 10,
            5,  5, 10, 25, 25, 10,  5,  5,
            0,  0,  0, 20, 20,  0,  0,  0,
            5, -5,-10,  0,  0,-10, -5,  5,
            5, 10, 10,-20,-20, 10, 10,  5,
            0,  0,  0,  0,  0,  0,  0,  0,
        ];

        [
            // Mirror white pieces to LERF endianness
            table_mirror(eval_white_king, 8),
            table_mirror(eval_white_king_eg, 8),
            table_mirror(eval_white_queen, 8),
            table_mirror(eval_white_rook, 8),
            table_mirror(eval_white_bishop, 8),
            table_mirror(eval_white_knight, 8),
            table_mirror(eval_white_pawn, 8),
            // Black pieces have mappings mirrored to white pieces
            table_negate_i8(eval_white_king),
            table_negate_i8(eval_white_king_eg),
            table_negate_i8(eval_white_queen),
            table_negate_i8(eval_white_rook),
            table_negate_i8(eval_white_bishop),
            table_negate_i8(eval_white_knight),
            table_negate_i8(eval_white_pawn),
        ]
    };

    #[cfg_attr(any(), rustfmt::skip)]
    pub const EVAL_TABLES_INV_I8: Align64<[[i8; 64]; chess_v2::PieceIndex::PieceIndexMax as usize]> = const {
        /*
            Evals for white pieces in square format. Black pieces are mirrored
            and inverted for quick negative scoring.
            [a8, b8, c8, d8, e8, f8, g8, h8,
            a7, b7, c7, d7, e7, f7, g7, h7,
            a6, b6, c6, d6, e6, f6, g6, h6,
            a5, b5, c5, d5, e5, f5, g5, h5,
            a4, b4, c4, d4, e4, f4, g4, h4,
            a3, b3, c3, d3, e3, f3, g3, h3,
            a2, b2, c2, d2, e2, f2, g2, h2,
            a1, b1, c1, d1, e1, f1, g1, h1]
        */
        let eval_white_king = [
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
        let eval_white_queen = [
            -20,-10,-10, -5, -5,-10,-10,-20,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -10,  0,  5,  5,  5,  5,  0,-10,
            -5,  0,  5,  5,  5,  5,  0, -5,
            0,  0,  5,  5,  5,  5,  0, -5,
            -10,  5,  5,  5,  5,  5,  0,-10,
            -10,  0,  5,  0,  0,  0,  0,-10,
            -20,-10,-10, -5, -5,-10,-10,-20
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
        let eval_white_pawn = [
            0,  0,  0,  0,  0,  0,  0,  0,
            50, 50, 50, 50, 50, 50, 50, 50,
            10, 10, 20, 30, 30, 20, 10, 10,
            5,  5, 10, 25, 25, 10,  5,  5,
            0,  0,  0, 20, 20,  0,  0,  0,
            5, -5,-10,  0,  0,-10, -5,  5,
            5, 10, 10,-20,-20, 10, 10,  5,
            0,  0,  0,  0,  0,  0,  0,  0,
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
            // Mirror white pieces to LERF endianness
            tabl_zero,
            table_mirror(eval_white_king, 8),
            table_mirror(eval_white_king_eg, 8),
            table_mirror(eval_white_queen, 8),
            table_mirror(eval_white_rook, 8),
            table_mirror(eval_white_bishop, 8),
            table_mirror(eval_white_knight, 8),
            table_mirror(eval_white_pawn, 8),
            // Black pieces have mappings mirrored to white pieces
            tabl_zero,
            table_negate_i8(eval_white_king),
            table_negate_i8(eval_white_king_eg),
            table_negate_i8(eval_white_queen),
            table_negate_i8(eval_white_rook),
            table_negate_i8(eval_white_bishop),
            table_negate_i8(eval_white_knight),
            table_negate_i8(eval_white_pawn),
        ])
    };

    #[inline(always)]
    pub fn calc_occupancy_index<const IS_ROOK: bool>(square: usize, occupancy: u64) -> usize {
        let mask = if IS_ROOK {
            Tables::LT_ROOK_OCCUPANCY_MASKS[square]
        } else {
            Tables::LT_BISHOP_OCCUPANCY_MASKS[square]
        };
        unsafe { _pext_u64(occupancy, mask) as usize }
    }

    #[inline(always)]
    pub unsafe fn calc_occupancy_index_unchecked<const IS_ROOK: bool>(
        square: usize,
        occupancy: u64,
    ) -> usize {
        unsafe {
            let mask = if IS_ROOK {
                *Tables::LT_ROOK_OCCUPANCY_MASKS.get_unchecked(square)
            } else {
                *Tables::LT_BISHOP_OCCUPANCY_MASKS.get_unchecked(square)
            };
            _pext_u64(occupancy, mask) as usize
        }
    }

    #[inline(always)]
    pub fn get_slider_move_mask<const IS_ROOK: bool>(&self, square: usize, occupancy: u64) -> u64 {
        debug_assert!(square < 64, "Square index out of bounds");

        let occupancy_index = Self::calc_occupancy_index::<IS_ROOK>(square, occupancy);

        let table_index = if IS_ROOK {
            Self::LT_ROOK_TABLE_OFFSETS[square] as usize + occupancy_index
        } else {
            Self::LT_BISHOP_TABLE_OFFSETS[square] as usize + occupancy_index
        };

        debug_assert!(
            table_index
                < if IS_ROOK {
                    Self::ROOK_TABLE_SIZE
                } else {
                    Self::BISHOP_TABLE_SIZE
                },
            "Occupancy index out of bounds"
        );

        if IS_ROOK {
            self.rook_move_mask[table_index]
        } else {
            self.bishop_move_mask[table_index]
        }
    }

    #[inline(always)]
    pub unsafe fn get_slider_move_mask_unchecked<const IS_ROOK: bool>(
        &self,
        square: usize,
        occupancy: u64,
    ) -> u64 {
        debug_assert!(square < 64, "Square index out of bounds");

        unsafe {
            let occupancy_index =
                Self::calc_occupancy_index_unchecked::<IS_ROOK>(square, occupancy);

            let table_index = if IS_ROOK {
                *Self::LT_ROOK_TABLE_OFFSETS.get_unchecked(square) as usize + occupancy_index
            } else {
                *Self::LT_BISHOP_TABLE_OFFSETS.get_unchecked(square) as usize + occupancy_index
            };

            debug_assert!(
                table_index
                    < if IS_ROOK {
                        Self::ROOK_TABLE_SIZE
                    } else {
                        Self::BISHOP_TABLE_SIZE
                    },
                "Occupancy index out of bounds"
            );

            if IS_ROOK {
                *self.rook_move_mask.get_unchecked(table_index)
            } else {
                *self.bishop_move_mask.get_unchecked(table_index)
            }
        }
    }

    fn gen_between_squares(&mut self) {
        for sq1 in 0..64 {
            for sq2 in 0..64 {
                let (b1, b2) = (1u64 << sq1, 1u64 << sq2);

                let between = if sq1 == sq2 {
                    0
                } else if self.get_slider_move_mask::<true>(sq1, 0) & b2 != 0 {
                    self.get_slider_move_mask::<true>(sq1, b2)
                        & self.get_slider_move_mask::<true>(sq2, b1)
                } else if self.get_slider_move_mask::<false>(sq1, 0) & b2 != 0 {
                    self.get_slider_move_mask::<false>(sq1, b2)
                        & self.get_slider_move_mask::<false>(sq2, b1)
                } else {
                    0
                };
                self.between_squares[sq1 * 64 + sq2] = between;
            }
        }
    }

    fn gen_cuckoo_table(&mut self) {
        use chess_v2::PieceIndex::*;

        let side = self.zobrist_hash_keys.hash_side_to_move;
        let mut count = 0usize;

        for pc in [
            WhiteKing,
            WhiteQueen,
            WhiteRook,
            WhiteBishop,
            WhiteKnight,
            BlackKing,
            BlackQueen,
            BlackRook,
            BlackBishop,
            BlackKnight,
        ] {
            for s1 in 0..64 {
                let attacks = match pc {
                    WhiteKing | BlackKing => Self::LT_KING_MOVE_MASKS[s1],
                    WhiteKnight | BlackKnight => Self::LT_KNIGHT_MOVE_MASKS[s1],
                    WhiteRook | BlackRook => self.get_slider_move_mask::<true>(s1, 0),
                    WhiteBishop | BlackBishop => self.get_slider_move_mask::<false>(s1, 0),
                    WhiteQueen | BlackQueen => {
                        self.get_slider_move_mask::<true>(s1, 0)
                            | self.get_slider_move_mask::<false>(s1, 0)
                    }
                    _ => 0,
                };

                for s2 in (s1 + 1)..64usize {
                    if attacks & (1u64 << s2) == 0 {
                        continue;
                    }

                    let psq = &self.zobrist_hash_keys.hash_piece_squares_new;
                    let mut key = psq[pc as usize][s1] ^ psq[pc as usize][s2] ^ side;
                    let mut mv = (s1 as u16) | ((s2 as u16) << 6);

                    let mut i = (key & 0x1fff) as usize;
                    loop {
                        std::mem::swap(&mut self.cuckoo_keys[i], &mut key);
                        std::mem::swap(&mut self.cuckoo_moves[i], &mut mv);
                        if mv == 0 {
                            break;
                        }
                        i = if i == (key & 0x1fff) as usize {
                            ((key >> 16) & 0x1fff) as usize
                        } else {
                            (key & 0x1fff) as usize
                        };
                    }
                    count += 1;
                }
            }
        }

        debug_assert_eq!(count, 3668, "cuckoo table entry count mismatch");
    }

    #[inline(always)]
    pub fn cuckoo_lookup(&self, key: u64) -> Option<u16> {
        if key == 0 {
            return None;
        }
        let j1 = (key & 0x1fff) as usize;
        if self.cuckoo_keys[j1] == key {
            return Some(self.cuckoo_moves[j1]);
        }
        let j2 = ((key >> 16) & 0x1fff) as usize;
        if self.cuckoo_keys[j2] == key {
            return Some(self.cuckoo_moves[j2]);
        }
        None
    }

    #[inline(always)]
    pub fn between(&self, s1: usize, s2: usize) -> u64 {
        self.between_squares[s1 * 64 + s2]
    }

    fn gen_rook_move_table() -> Box<[u64; Self::ROOK_TABLE_SIZE]> {
        let mut moves: Box<[u64; Self::ROOK_TABLE_SIZE]> = vec![0u64; Self::ROOK_TABLE_SIZE]
            .into_boxed_slice()
            .try_into()
            .unwrap();

        let occupancy_premutations = Self::gen_slider_occupancy_premutations::<true>();

        for square in 0..64 {
            let rank = square / 8;
            let file = square % 8;

            for occ_id in 0..Self::ROOK_OCCUPANCY_MAX {
                let blockers = occupancy_premutations[square * Self::ROOK_OCCUPANCY_MAX + occ_id];
                let occupancy_index = Self::calc_occupancy_index::<true>(square, blockers);
                let table_index = Self::LT_ROOK_TABLE_OFFSETS[square] as usize + occupancy_index;

                for file_it in file + 1..8 {
                    let bit = 1 << (rank * 8 + file_it);
                    moves[table_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                }

                for file_it in (0..=file.max(1) - 1).rev() {
                    let bit = 1 << (rank * 8 + file_it);
                    moves[table_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                }

                for rank_it in rank + 1..8 {
                    let bit = 1 << (rank_it * 8 + file);
                    moves[table_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                }

                for rank_it in (0..=rank.max(1) - 1).rev() {
                    let bit = 1 << (rank_it * 8 + file);
                    moves[table_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                }

                moves[table_index] &= !(1 << square);
            }
        }

        moves
    }

    fn gen_bishop_move_table() -> Box<[u64; Self::BISHOP_TABLE_SIZE]> {
        let mut moves: Box<[u64; Self::BISHOP_TABLE_SIZE]> = vec![0u64; Self::BISHOP_TABLE_SIZE]
            .into_boxed_slice()
            .try_into()
            .unwrap();

        let occupancy_premutations = Self::gen_slider_occupancy_premutations::<false>();

        for square in 0..64 {
            let rank = (square / 8) as i32;
            let file = (square % 8) as i32;

            for occ_id in 0..Self::BISHOP_OCCUPANCY_MAX {
                let blockers = occupancy_premutations[square * Self::BISHOP_OCCUPANCY_MAX + occ_id];
                let occupancy_index = Self::calc_occupancy_index::<false>(square, blockers);
                let table_index = Self::LT_BISHOP_TABLE_OFFSETS[square] as usize + occupancy_index;

                let mut file_it = file - 1;

                for rank_it in rank + 1..8 {
                    if file_it == -1 {
                        break;
                    }
                    let bit = 1 << (rank_it * 8 + file_it);
                    moves[table_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                    file_it -= 1;
                }

                file_it = file + 1;

                for rank_it in rank + 1..8 {
                    if file_it == 8 {
                        break;
                    }
                    let bit = 1 << (rank_it * 8 + file_it);
                    moves[table_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                    file_it += 1;
                }

                file_it = file + 1;

                for rank_it in (0..=rank - 1).rev() {
                    if file_it == 8 {
                        break;
                    }
                    let bit = 1 << (rank_it * 8 + file_it);
                    moves[table_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                    file_it += 1;
                }

                file_it = file - 1;

                for rank_it in (0..=rank - 1).rev() {
                    if file_it == -1 {
                        break;
                    }
                    let bit = 1 << (rank_it * 8 + file_it);
                    moves[table_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                    file_it -= 1;
                }
            }
        }

        moves
    }

    fn gen_slider_occupancy_premutations<const IS_ROOK: bool>() -> Box<[u64]> {
        let occupancy_table_size = if IS_ROOK {
            Self::ROOK_OCCUPANCY_MAX
        } else {
            Self::BISHOP_OCCUPANCY_MAX
        };

        let mut masks: Box<[u64]> = vec![0u64; 64 * occupancy_table_size]
            .into_boxed_slice()
            .try_into()
            .unwrap();

        for square in 0..64 {
            let occupancy_mask = if IS_ROOK {
                Self::LT_ROOK_OCCUPANCY_MASKS[square]
            } else {
                Self::LT_BISHOP_OCCUPANCY_MASKS[square]
            };

            let popcnt = occupancy_mask.count_ones();
            debug_assert!(popcnt < 13, "Popcount exceeds 12 bits");

            for premut_index in 0..(1 << popcnt) {
                let premut = unsafe { _pdep_u64(!premut_index, occupancy_mask) };
                masks[square * occupancy_table_size + premut_index as usize] = premut;
            }
        }

        masks
    }

    pub fn gen_zobrist_hashes() -> (
        [[u64; 64]; 16],
        u64,
        [u64; 16],
        [u64; 64],
        u64,
        [[u64; 64]; 16],
    ) {
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);

        let mut hash_squares = [[0u64; 64]; 16];
        let mut hash_side_to_move = 0;
        let mut hash_castling_rights: [u64; 16] = [0; 16];
        let mut zobrist_en_passant_squares = [0; 64];
        let mut no_pawn_key: u64 = 0;
        let mut hash_pawn_squares = [[0u64; 64]; 16];

        for piece in 1..13 {
            for square in 0..64 {
                let hash_key = rng.random::<u64>();
                hash_squares[chess_v2::PieceIndex::from(util::PieceId::from(piece - 1)) as usize]
                    [square] = hash_key;
            }
        }

        for castles in 0..16 {
            hash_castling_rights[castles] = rng.random::<u64>();
        }

        hash_side_to_move = rng.random::<u64>();

        for square in 1..64 {
            zobrist_en_passant_squares[square] = rng.random::<u64>();
        }

        no_pawn_key = rng.random::<u64>();

        let mut testset = std::collections::BTreeSet::<u16>::new();

        for square in 0..64 {
            hash_pawn_squares[chess_v2::PieceIndex::WhitePawn as usize][square] =
                hash_squares[chess_v2::PieceIndex::WhitePawn as usize][square];

            hash_pawn_squares[chess_v2::PieceIndex::BlackPawn as usize][square] =
                hash_squares[chess_v2::PieceIndex::BlackPawn as usize][square];
        }

        (
            hash_squares,
            hash_side_to_move,
            hash_castling_rights,
            zobrist_en_passant_squares,
            no_pawn_key,
            hash_pawn_squares,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ray_attacks(square: usize, blockers: u64, dirs: &[(i32, i32)]) -> u64 {
        let mut attacks = 0u64;
        let rank = (square / 8) as i32;
        let file = (square % 8) as i32;

        for &(dr, df) in dirs {
            let mut r = rank + dr;
            let mut f = file + df;
            while (0..8).contains(&r) && (0..8).contains(&f) {
                let bit = 1u64 << (r * 8 + f);
                attacks |= bit;
                if blockers & bit != 0 {
                    break;
                }
                r += dr;
                f += df;
            }
        }

        attacks
    }

    #[test]
    fn test_slider_move_masks_exhaustive() {
        let tables = Tables::new();

        let rook_dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)];
        let bishop_dirs = [(1, 1), (1, -1), (-1, 1), (-1, -1)];

        for square in 0..64 {
            for (mask, dirs, is_rook) in [
                (Tables::LT_ROOK_OCCUPANCY_MASKS[square], &rook_dirs, true),
                (
                    Tables::LT_BISHOP_OCCUPANCY_MASKS[square],
                    &bishop_dirs,
                    false,
                ),
            ] {
                for i in 0..(1u64 << mask.count_ones()) {
                    let blockers = unsafe { _pdep_u64(i, mask) };
                    let occupancy = blockers | (0xAAAA_5555_AAAA_5555 & !mask);
                    let expected = ray_attacks(square, blockers, dirs);

                    let (result, result_unchecked) = if is_rook {
                        (
                            tables.get_slider_move_mask::<true>(square, occupancy),
                            unsafe {
                                tables.get_slider_move_mask_unchecked::<true>(square, occupancy)
                            },
                        )
                    } else {
                        (
                            tables.get_slider_move_mask::<false>(square, occupancy),
                            unsafe {
                                tables.get_slider_move_mask_unchecked::<false>(square, occupancy)
                            },
                        )
                    };

                    assert_eq!(
                        result, expected,
                        "slider attacks mismatch: is_rook {} square {} subset {}",
                        is_rook, square, i
                    );
                    assert_eq!(
                        result_unchecked, expected,
                        "unchecked slider attacks mismatch: is_rook {} square {} subset {}",
                        is_rook, square, i
                    );
                }
            }
        }
    }
}
