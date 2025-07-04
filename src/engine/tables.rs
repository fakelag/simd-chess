use crate::constant::Side;

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

pub const LT_ROOK_MOVE_MASKS: [u64; 64] = const {
    let mut moves = [0; 64];
    let mut square: usize = 0;

    while square < 64 {
        let rank = square / 8;
        let file = square % 8;

        let file_mask = 0x101010101010101;
        let rank_mask = 0xFF;

        moves[square] |= file_mask << file;
        moves[square] |= rank_mask << (rank * 8);
        moves[square] &= !(1 << square);

        square += 1;
    }

    moves
};
