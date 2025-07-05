use crate::constant::Side;
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

pub struct Tables {
    rook_move_mask: Box<[u64; 64 * Self::ROOK_OCCUPANCY_MAX]>,
}

impl Tables {
    pub fn new() -> Self {
        Self {
            rook_move_mask: Self::gen_rook_move_table(),
        }
    }

    const ROOK_OCCUPANCY_MAX: usize = 1 << 12;

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

    pub const LT_PAWN_ATTACK_MASKS: [[u64; 64]; Side::SideMax as usize] = const {
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

    pub fn get_rook_move_mask(&self, square: usize, blockers: u64) -> u64 {
        debug_assert!(square < 64, "Square index out of bounds");

        let occupancy_index = Self::calc_occupancy_index(square, blockers);

        debug_assert!(
            occupancy_index < Self::ROOK_OCCUPANCY_MAX,
            "Occupancy index out of bounds"
        );

        self.rook_move_mask[square * Self::ROOK_OCCUPANCY_MAX + occupancy_index]
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
                let occupancy_index = Self::calc_occupancy_index(square, blockers);

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

    fn calc_occupancy_index(square: usize, blockers: u64) -> usize {
        (blockers.wrapping_mul(Tables::LT_ROOK_OCCUPANCY_MAGICS[square])
            >> Tables::LT_ROOK_OCCUPANCY_SHIFTS[square]) as usize
    }

    fn gen_slider_occupancy_premutations<const IS_ROOK: bool>()
    -> Box<[u64; 64 * Self::ROOK_OCCUPANCY_MAX]> {
        let mut masks: Box<[u64; 64 * Self::ROOK_OCCUPANCY_MAX]> =
            vec![0u64; 64 * Self::ROOK_OCCUPANCY_MAX]
                .into_boxed_slice()
                .try_into()
                .unwrap();

        for square in 0..64 {
            let rook_occupancy_mask = Self::LT_ROOK_OCCUPANCY_MASKS[square];

            let popcnt = rook_occupancy_mask.count_ones();
            debug_assert!(popcnt < 13, "Popcount exceeds 12 bits");

            for premut_index in 0..(1 << popcnt) {
                let premut = unsafe { _pdep_u64(!premut_index, rook_occupancy_mask) };
                masks[square * Self::ROOK_OCCUPANCY_MAX + premut_index as usize] = premut;
            }
        }

        masks
    }

    fn gen_slider_hash_functions<const IS_ROOK: bool>() -> ([u64; 64], [u8; 64]) {
        use rand::Rng;
        let mut rng = rand::rng();

        rng.reseed().unwrap();

        let premuts = Self::gen_slider_occupancy_premutations::<IS_ROOK>();
        let mut hash_magics = [0u64; 64];
        let mut hash_shifts = [0u8; 64];

        for square in 0..64 {
            let rook_occupancy_mask = Self::LT_ROOK_OCCUPANCY_MASKS[square];
            let occupancy_bits = rook_occupancy_mask.count_ones() as usize;
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
                    let premutation = premuts[square * Self::ROOK_OCCUPANCY_MAX + occupancy_index];
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
}
