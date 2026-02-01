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
    pub hash_piece_squares: [[u64; 64]; 12],
    pub hash_piece_squares_new: [[u64; 64]; 16],
    pub hash_side_to_move: u64,
    pub hash_castling_rights: [u64; 16],
    pub hash_en_passant_squares: [u64; 64],
    pub no_pawn_key: u64,
    pub hash_pawn_squares: [[u64; 64]; 16],
}

pub struct Tables {
    rook_move_mask: Box<[u64; 64 * Self::ROOK_OCCUPANCY_MAX]>,
    bishop_move_mask: Box<[u64; 64 * Self::BISHOP_OCCUPANCY_MAX]>,
    pub slider_combined_move_masks:
        Box<[u64; 64 * Self::ROOK_OCCUPANCY_MAX + 64 * Self::BISHOP_OCCUPANCY_MAX]>,
    pub zobrist_hash_keys: Box<ZobristKeys>,
}

impl Tables {
    pub fn new() -> Self {
        let (
            zobrist_hash_squares_new,
            zobrist_hash_squares_old,
            zobrist_side_to_move,
            zobrist_castling_rights,
            zobrist_en_passant_squares,
            no_pawn_key,
            hash_pawn_squares,
        ) = Self::gen_zobrist_hashes();

        let rook_move_mask = Self::gen_rook_move_table();
        let bishop_move_mask = Self::gen_bishop_move_table();

        let slider_combined_move_masks = {
            let mut masks =
                vec![0; 64 * Self::ROOK_OCCUPANCY_MAX + 64 * Self::BISHOP_OCCUPANCY_MAX];
            for square in 0..64 {
                for j in 0..Self::ROOK_OCCUPANCY_MAX {
                    masks[square * Self::ROOK_OCCUPANCY_MAX + j] =
                        rook_move_mask[square * Self::ROOK_OCCUPANCY_MAX + j];
                }
                for j in 0..Self::BISHOP_OCCUPANCY_MAX {
                    masks
                        [64 * Self::ROOK_OCCUPANCY_MAX + square * Self::BISHOP_OCCUPANCY_MAX + j] =
                        bishop_move_mask[square * Self::BISHOP_OCCUPANCY_MAX + j];
                }
            }
            masks.into_boxed_slice().try_into().unwrap()
        };

        Self {
            rook_move_mask,
            bishop_move_mask,
            slider_combined_move_masks,
            zobrist_hash_keys: Box::new(ZobristKeys {
                hash_piece_squares: zobrist_hash_squares_old[1..].try_into().unwrap(),
                hash_piece_squares_new: zobrist_hash_squares_new,
                hash_side_to_move: zobrist_side_to_move,
                hash_castling_rights: zobrist_castling_rights,
                hash_en_passant_squares: zobrist_en_passant_squares,
                no_pawn_key,
                hash_pawn_squares,
            }),
        }
    }

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

    pub const LT_ROOK_OCCUPANCY_SHIFTS: [u8; 64] = const {
        let mut shifts = [0; 64];
        let mut square = 0;

        while square < 64 {
            let shamt = (64 - Self::LT_ROOK_OCCUPANCY_MASKS[square].count_ones()) as u8;
            shifts[square] = shamt;
            square += 1;
        }

        shifts
    };

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

    pub const LT_BISHOP_OCCUPANCY_SHIFTS: [u8; 64] = const {
        let mut shifts = [0; 64];
        let mut square = 0;

        while square < 64 {
            let shamt = (64 - Self::LT_BISHOP_OCCUPANCY_MASKS[square].count_ones()) as u8;
            shifts[square] = shamt;
            square += 1;
        }

        shifts
    };

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
    pub fn calc_occupancy_index<const IS_ROOK: bool>(square: usize, blockers: u64) -> usize {
        let (magic, shamt) = if IS_ROOK {
            (
                Tables::LT_ROOK_OCCUPANCY_MAGICS[square],
                Tables::LT_ROOK_OCCUPANCY_SHIFTS[square],
            )
        } else {
            (
                Tables::LT_BISHOP_OCCUPANCY_MAGICS[square],
                Tables::LT_BISHOP_OCCUPANCY_SHIFTS[square],
            )
        };
        (blockers.wrapping_mul(magic) >> shamt) as usize
    }

    #[inline(always)]
    pub unsafe fn calc_occupancy_index_unchecked<const IS_ROOK: bool>(
        square: usize,
        blockers: u64,
    ) -> usize {
        unsafe {
            let (magic, shamt) = if IS_ROOK {
                (
                    *Tables::LT_ROOK_OCCUPANCY_MAGICS.get_unchecked(square),
                    *Tables::LT_ROOK_OCCUPANCY_SHIFTS.get_unchecked(square),
                )
            } else {
                (
                    *Tables::LT_BISHOP_OCCUPANCY_MAGICS.get_unchecked(square),
                    *Tables::LT_BISHOP_OCCUPANCY_SHIFTS.get_unchecked(square),
                )
            };
            (blockers.wrapping_mul(magic) >> shamt) as usize
        }
    }

    #[inline(always)]
    pub fn get_slider_move_mask<const IS_ROOK: bool>(&self, square: usize, blockers: u64) -> u64 {
        debug_assert!(square < 64, "Square index out of bounds");

        let occupancy_index = Self::calc_occupancy_index::<IS_ROOK>(square, blockers);

        debug_assert!(
            occupancy_index
                < if IS_ROOK {
                    Self::ROOK_OCCUPANCY_MAX
                } else {
                    Self::BISHOP_OCCUPANCY_MAX
                },
            "Occupancy index out of bounds"
        );

        if IS_ROOK {
            self.rook_move_mask[square * Self::ROOK_OCCUPANCY_MAX + occupancy_index]
        } else {
            self.bishop_move_mask[square * Self::BISHOP_OCCUPANCY_MAX + occupancy_index]
        }
    }

    #[inline(always)]
    pub unsafe fn get_slider_move_mask_unchecked<const IS_ROOK: bool>(
        &self,
        square: usize,
        blockers: u64,
    ) -> u64 {
        debug_assert!(square < 64, "Square index out of bounds");

        unsafe {
            let occupancy_index = Self::calc_occupancy_index_unchecked::<IS_ROOK>(square, blockers);

            debug_assert!(
                occupancy_index
                    < if IS_ROOK {
                        Self::ROOK_OCCUPANCY_MAX
                    } else {
                        Self::BISHOP_OCCUPANCY_MAX
                    },
                "Occupancy index out of bounds"
            );

            if IS_ROOK {
                *self
                    .rook_move_mask
                    .get_unchecked(square * Self::ROOK_OCCUPANCY_MAX + occupancy_index)
            } else {
                *self
                    .bishop_move_mask
                    .get_unchecked(square * Self::BISHOP_OCCUPANCY_MAX + occupancy_index)
            }
        }
    }

    fn gen_rook_move_table() -> Box<[u64; 64 * Self::ROOK_OCCUPANCY_MAX]> {
        let mut moves: Box<[u64; 64 * Self::ROOK_OCCUPANCY_MAX]> =
            vec![0u64; 64 * Self::ROOK_OCCUPANCY_MAX]
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

                for file_it in file + 1..8 {
                    let bit = 1 << (rank * 8 + file_it);
                    moves[square * Self::ROOK_OCCUPANCY_MAX + occupancy_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                }

                for file_it in (0..=file.max(1) - 1).rev() {
                    let bit = 1 << (rank * 8 + file_it);
                    moves[square * Self::ROOK_OCCUPANCY_MAX + occupancy_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                }

                for rank_it in rank + 1..8 {
                    let bit = 1 << (rank_it * 8 + file);
                    moves[square * Self::ROOK_OCCUPANCY_MAX + occupancy_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                }

                for rank_it in (0..=rank.max(1) - 1).rev() {
                    let bit = 1 << (rank_it * 8 + file);
                    moves[square * Self::ROOK_OCCUPANCY_MAX + occupancy_index] |= bit;
                    if blockers & bit != 0 {
                        break;
                    }
                }

                moves[square * Self::ROOK_OCCUPANCY_MAX + occupancy_index] &= !(1 << square);
            }
        }

        moves
    }

    fn gen_bishop_move_table() -> Box<[u64; 64 * Self::BISHOP_OCCUPANCY_MAX]> {
        let mut moves: Box<[u64; 64 * Self::BISHOP_OCCUPANCY_MAX]> =
            vec![0u64; 64 * Self::BISHOP_OCCUPANCY_MAX]
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

                let mut file_it = file - 1;

                for rank_it in rank + 1..8 {
                    if file_it == -1 {
                        break;
                    }
                    let bit = 1 << (rank_it * 8 + file_it);
                    moves[square * Self::BISHOP_OCCUPANCY_MAX + occupancy_index] |= bit;
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
                    moves[square * Self::BISHOP_OCCUPANCY_MAX + occupancy_index] |= bit;
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
                    moves[square * Self::BISHOP_OCCUPANCY_MAX + occupancy_index] |= bit;
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
                    moves[square * Self::BISHOP_OCCUPANCY_MAX + occupancy_index] |= bit;
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

    pub fn gen_slider_hash_functions<const IS_ROOK: bool>() -> ([u64; 64], [u8; 64]) {
        use rand::Rng;
        let mut rng = rand::rng();

        let occupancy_table_size = if IS_ROOK {
            Self::ROOK_OCCUPANCY_MAX
        } else {
            Self::BISHOP_OCCUPANCY_MAX
        };

        rng.reseed().unwrap();

        let premuts = Self::gen_slider_occupancy_premutations::<IS_ROOK>();
        let mut hash_magics = [0u64; 64];
        let mut hash_shifts = [0u8; 64];

        for square in 0..64 {
            let occupancy_mask = if IS_ROOK {
                Self::LT_ROOK_OCCUPANCY_MASKS[square]
            } else {
                Self::LT_BISHOP_OCCUPANCY_MASKS[square]
            };

            let occupancy_bits = occupancy_mask.count_ones() as usize;
            let shamt = (64 - occupancy_bits) as u8;

            hash_shifts[square] = shamt;

            let mut rng_magic: u64;
            let mut used_keys = vec![false; 1 << occupancy_bits];

            'outer: loop {
                used_keys.fill(false);
                rng_magic = rng.random::<u64>();
                rng_magic &= rng.random::<u64>();
                rng_magic &= rng.random::<u64>();
                rng_magic &= 0x00FFFFFF_FFFFFFFF; // Ensure the magic is 48 bits
                rng_magic |= (shamt as u64) << 56; // Set the shift bits

                for occupancy_index in 0..(1 << occupancy_bits) {
                    let premutation = premuts[square * occupancy_table_size + occupancy_index];
                    let hash_key = premutation.wrapping_mul(rng_magic) >> shamt;

                    if used_keys[hash_key as usize] {
                        continue 'outer;
                    }

                    used_keys[hash_key as usize] = true;
                }
                break;
            }

            hash_magics[square] = rng_magic;
        }

        (hash_magics, hash_shifts)
    }

    pub fn gen_zobrist_hashes() -> (
        [[u64; 64]; 16],
        [[u64; 64]; 13],
        u64,
        [u64; 16],
        [u64; 64],
        u64,
        [[u64; 64]; 16],
    ) {
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);

        let mut hash_squares_old = [[0u64; 64]; 13];
        let mut hash_squares_new = [[0u64; 64]; 16];
        let mut hash_side_to_move = 0;
        let mut hash_castling_rights: [u64; 16] = [0; 16];
        let mut zobrist_en_passant_squares = [0; 64];
        let mut no_pawn_key: u64 = 0;
        let mut hash_pawn_squares = [[0u64; 64]; 16];

        for piece in 1..13 {
            for square in 0..64 {
                let hash_key = rng.random::<u64>();
                hash_squares_old[piece][square] = hash_key;
                hash_squares_new
                    [chess_v2::PieceIndex::from(util::PieceId::from(piece - 1)) as usize][square] =
                    hash_key;
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
                hash_squares_new[chess_v2::PieceIndex::WhitePawn as usize][square];

            hash_pawn_squares[chess_v2::PieceIndex::BlackPawn as usize][square] =
                hash_squares_new[chess_v2::PieceIndex::BlackPawn as usize][square];
        }

        (
            hash_squares_new,
            hash_squares_old,
            hash_side_to_move,
            hash_castling_rights,
            zobrist_en_passant_squares,
            no_pawn_key,
            hash_pawn_squares,
        )
    }

    #[cfg_attr(any(), rustfmt::skip)]
    pub const LT_ROOK_OCCUPANCY_MAGICS: [u64; 64] = [
        0x3480022080400650, 0x3540004820021000, 0x3500200040100900, 0x3500100189004420,
        0x3500080011000422, 0x3500040011000802, 0x350050a402000500, 0x34800040a1800100,
        0x3508800040043080, 0x3608400050006000, 0x3600806000801000, 0x3620800800500084,
        0x3609802400808800, 0x360200100200880c, 0x3601005401001200, 0x3582000204004085,
        0x3508208000400080, 0x3621848020004000, 0x3608808020001000, 0x3601050018201000,
        0x3642808008002400, 0x3601010004000802, 0x3600040010184502, 0x350842000c128041,
        0x3580004040092000, 0x36406000c01000c0, 0x36c6028200204010, 0x3601022900201001,
        0x3694140080080280, 0x3646002200087004, 0x3681020400088110, 0x3500624a00008104,
        0x3588488001002100, 0x3600c00901002080, 0x36860020820050c0, 0x3602090121001000,
        0x3600800402800800, 0x3606008802004c10, 0x3602000442000811, 0x350000a242000401,
        0x3500804001208000, 0x361000a004404000, 0x3601082000410010, 0x3602001140620008,
        0x3655908801010004, 0x3642009008020004, 0x3603290810140002, 0x3581808a44020005,
        0x35c0048000204180, 0x3640002010080020, 0x3600100820008080, 0x3600300021000900,
        0x3600800400080080, 0x3600020080140080, 0x3600182605101400, 0x35062c540a810600,
        0x3401008000443029, 0x3588803600c10022, 0x350831a00100416b, 0x35100051000904a1,
        0x3505030800100301, 0x3501002208040001, 0x350009024801900c, 0x3400002400850042,
    ];

    #[cfg_attr(any(), rustfmt::skip)]
    pub const LT_BISHOP_OCCUPANCY_MAGICS: [u64; 64] = [
        0x3a90441d06440101, 0x3b02244802004002, 0x3b300404f0442100, 0x3b04040480040100,
        0x3b01104008080208, 0x3b018804c0004800, 0x3b00880410040510, 0x3a04508410021002,
        0x3b00401019020088, 0x3b00884208062220, 0x3b02042400a60020, 0x3b20424081031900,
        0x3b00511040000040, 0x3b00010422401000, 0x3b0001c210242008, 0x3bc0008404010400,
        0x3bc0060444040401, 0x3b600c8608010514, 0x3922041040820200, 0x3908200404001092,
        0x3904008080a08013, 0x390600a108011400, 0x3b05040201100200, 0x3b0180090404a620,
        0x3b24421020080100, 0x3b13106009501100, 0x3904280010084cc0, 0x3704004044010002,
        0x37008c000a802009, 0x3901010002014120, 0x3b08110116110110, 0x3b40518002008400,
        0x3b90022020190891, 0x3b0128210c020400, 0x3900220800404884, 0x3722040108040100,
        0x3750038a00016200, 0x3920808500820120, 0x3b02008201090800, 0x3b31004110020108,
        0x3b18443208402000, 0x3b920201210844a0, 0x3904a10802400800, 0x39004020130a5801,
        0x3900200208802404, 0x3910200103000032, 0x3b5202020c000200, 0x3b82420401101120,
        0x3b06011008040100, 0x3b20248404200820, 0x3b1002a402280114, 0x3b20808084040440,
        0x3b10044005044000, 0x3b08600401020002, 0x3b50a00104388126, 0x3b2008020080250a,
        0x3a8900880c020a10, 0x3b002503009a2000, 0x3b00280242080400, 0x3b18108000411080,
        0x3b40020031120202, 0x3b30e25011100120, 0x3b40282004440060, 0x3aa2902200830200
    ];

    pub const LT_SLIDER_MAGICS_GATHER: Align64<[[u64; 64]; 2]> = const {
        Align64([
            Tables::LT_ROOK_OCCUPANCY_MAGICS,
            Tables::LT_BISHOP_OCCUPANCY_MAGICS,
        ])
    };

    pub const LT_SLIDER_MASKS_GATHER: Align64<[[u64; 64]; 2]> = const {
        Align64([
            Tables::LT_ROOK_OCCUPANCY_MASKS,
            Tables::LT_BISHOP_OCCUPANCY_MASKS,
        ])
    };

    pub const LT_NON_SLIDER_MASKS_GATHER: Align64<[[u64; 64]; 16]> = const {
        let mut masks = [[0u64; 64]; 16];

        masks[chess_v2::PieceIndex::WhiteKing as usize] = Self::LT_KING_MOVE_MASKS;
        masks[chess_v2::PieceIndex::WhiteKnight as usize] = Self::LT_KNIGHT_MOVE_MASKS;
        masks[chess_v2::PieceIndex::WhitePawn as usize] =
            Self::LT_PAWN_CAPTURE_MASKS[Side::White as usize];

        masks[chess_v2::PieceIndex::BlackKing as usize] = Self::LT_KING_MOVE_MASKS;
        masks[chess_v2::PieceIndex::BlackKnight as usize] = Self::LT_KNIGHT_MOVE_MASKS;
        masks[chess_v2::PieceIndex::BlackPawn as usize] =
            Self::LT_PAWN_CAPTURE_MASKS[Side::Black as usize];

        Align64(masks)
    };
}
