use rand::{Rng, SeedableRng};

use crate::util::{self, Side, table_mirror, table_negate_i8};
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

const EX_H_FILE: u64 = const { ex_mask!(FileOrRank::File(File::H)) };
const EX_A_FILE: u64 = const { ex_mask!(FileOrRank::File(File::A)) };
const EX_G_FILE: u64 = const { ex_mask!(FileOrRank::File(File::G)) };
const EX_B_FILE: u64 = const { ex_mask!(FileOrRank::File(File::B)) };
const EX_OUTER: u64 = const {
    ex_mask!(FileOrRank::File(File::A))
        & ex_mask!(FileOrRank::File(File::H))
        & ex_mask!(FileOrRank::Rank(Rank::One))
        & ex_mask!(FileOrRank::Rank(Rank::Eight))
};

pub struct ZobristKeys {
    pub hash_piece_squares: [[u64; 64]; 12],
    pub hash_side_to_move: u64,
    pub hash_castling_rights: [u64; 16],
    pub hash_en_passant_squares: [u64; 64],
}

pub struct Tables {
    rook_move_mask: Box<[u64; 64 * Self::ROOK_OCCUPANCY_MAX]>,
    bishop_move_mask: Box<[u64; 64 * Self::BISHOP_OCCUPANCY_MAX]>,
    pub zobrist_hash_keys: Box<ZobristKeys>,
}

impl Tables {
    pub fn new() -> Self {
        let (
            zobrist_hash_squares,
            zobrist_side_to_move,
            zobrist_castling_rights,
            zobrist_en_passant_squares,
        ) = Self::gen_zobrist_hashes();

        Self {
            rook_move_mask: Self::gen_rook_move_table(),
            bishop_move_mask: Self::gen_bishop_move_table(),
            zobrist_hash_keys: Box::new(ZobristKeys {
                hash_piece_squares: zobrist_hash_squares,
                hash_side_to_move: zobrist_side_to_move,
                hash_castling_rights: zobrist_castling_rights,
                hash_en_passant_squares: zobrist_en_passant_squares,
            }),
        }
    }

    const ROOK_OCCUPANCY_MAX: usize = 1 << 12;
    const BISHOP_OCCUPANCY_MAX: usize = 1 << 9;

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

    #[cfg_attr(any(), rustfmt::skip)]
    pub const EVAL_TABLES_INV_I8: [[i8; 64]; util::PieceId::PieceMax as usize] = const {
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
            table_mirror(eval_white_queen, 8),
            table_mirror(eval_white_rook, 8),
            table_mirror(eval_white_bishop, 8),
            table_mirror(eval_white_knight, 8),
            table_mirror(eval_white_pawn, 8),
            // Black pieces have mappings mirrored to white pieces
            table_negate_i8(eval_white_king),
            table_negate_i8(eval_white_queen),
            table_negate_i8(eval_white_rook),
            table_negate_i8(eval_white_bishop),
            table_negate_i8(eval_white_knight),
            table_negate_i8(eval_white_pawn),
        ]
    };

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

    fn calc_occupancy_index<const IS_ROOK: bool>(square: usize, blockers: u64) -> usize {
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

    fn gen_slider_hash_functions<const IS_ROOK: bool>() -> ([u64; 64], [u8; 64]) {
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

    pub fn gen_zobrist_hashes() -> ([[u64; 64]; 12], u64, [u64; 16], [u64; 64]) {
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);

        let mut hash_squares = [[0u64; 64]; 12];
        let mut hash_side_to_move = 0;
        let mut hash_castling_rights: [u64; 16] = [0; 16];
        let mut zobrist_en_passant_squares = [0; 64];

        for piece in 0..12 {
            for square in 0..64 {
                let hash_key = rng.random::<u64>();
                hash_squares[piece][square] = hash_key;
            }
        }

        for castles in 0..16 {
            hash_castling_rights[castles] = rng.random::<u64>();
        }

        hash_side_to_move = rng.random::<u64>();

        for square in 0..64 {
            zobrist_en_passant_squares[square] = rng.random::<u64>();
        }

        (
            hash_squares,
            hash_side_to_move,
            hash_castling_rights,
            zobrist_en_passant_squares,
        )
    }

    #[cfg_attr(any(), rustfmt::skip)]
    pub const LT_ROOK_OCCUPANCY_MAGICS: [u64; 64] = [
        0x2a0010c201006080, 0x3080200212814000, 0x0100084020001100, 0x0100100100a08844,
        0x8a00220004102088, 0x8100010002285400, 0x4400011048118204, 0x0100014021000682,
        0x0000802040028001, 0x0000401000402000, 0x0001002001004810, 0x0203002049045000,
        0x0211001004080300, 0x0002002200241059, 0x0482000402003881, 0x0603800100004080,
        0x04108180024000a1, 0x0402404000a01000, 0x2440110040200100, 0xc000120028204201,
        0x5208008008808401, 0x0002008002800400, 0x00080c0010080122, 0x4228020004088851,
        0x4102400180218001, 0x0069400040201000, 0x00a0016080500082, 0x0008001010010200,
        0x0008000900049100, 0x0404040080800200, 0x0801280400021110, 0x0524012200008044,
        0x0080002000400140, 0x5860045000400420, 0x0240520082002240, 0x0100825000802800,
        0x4000800400804800, 0x1001000209001c00, 0x0002800200801100, 0x2149040042001491,
        0x1020400180068020, 0x8000810040010020, 0x0810200010008080, 0xc0300021004b0010,
        0x8024008040080800, 0x1082000491020008, 0x00808802810c0030, 0x6002010880420004,
        0x8001034082002200, 0x0020044008a18080, 0x8400100020008680, 0x11b0008212080080,
        0x0404840008008080, 0x000a000400801280, 0x0360800200010080, 0x4401004104208200,
        0x0042800101302441, 0x0000120109c02082, 0x800c20010140120b, 0x0012350120300029,
        0x0026000820b08c02, 0x1005000204000801, 0x0000009005020804, 0x004020c024008906,
    ];

    #[cfg_attr(any(), rustfmt::skip)]
    pub const LT_BISHOP_OCCUPANCY_MAGICS: [u64; 64] = [
        0x0004100a00410a00, 0x080a104411004020, 0x010c08008304820a, 0x1004040180900000,
        0x0111114001060000, 0x0208882008166000, 0x0911420220603000, 0x0001004644200812,
        0x2880c80208080100, 0x0100040104440182, 0x0000100186004051, 0x0001084845002540,
        0x20d0020a1000a048, 0x0820011002900540, 0x0464040b11082012, 0x8804020200c20804,
        0x81c0400810240282, 0x0488004210040180, 0x0048001000242022, 0x8041000824050130,
        0x0000800400a02101, 0x004a000410440401, 0x4003204201104220, 0x0110844904880100,
        0x0010080005081004, 0x0208180020434100, 0x0204010050010060, 0x0400404014010200,
        0x8110040002802102, 0x282800c0820100a1, 0x0014142220410404, 0x0820411802010501,
        0x801009081445b001, 0x0200882040440400, 0x3820402800100040, 0x4004120281080080,
        0x0009100400008021, 0x0030004044060100, 0x0008010840840a01, 0x2022020200004248,
        0x0040981d40001010, 0x0044010d10000918, 0x1042540228040400, 0x0204420202009420,
        0x0008200410422400, 0x40c4008809400200, 0x8220440142138040, 0x0011012210816e08,
        0x2048424230400480, 0x0203040211140580, 0x0800208404060020, 0x000a0400420200c0,
        0x130000c0104100a8, 0x4044396008128000, 0x8020240c02b41004, 0x0444080481020000,
        0x4042008404020238, 0x0040002582082000, 0x0880400022011090, 0x40a00001802a0800,
        0x40018a0c04104405, 0x0400002020220182, 0x0000216002008302, 0x40d0024809002208,
    ];
}
