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

macro_rules! ex_file_mask {
    ($ex_file:expr) => {{
        let mut mask = 0u64;
        let mut square = 0;
        while square < 64 {
            let file = square % 8;
            if file != ($ex_file as usize) {
                mask |= 1 << square;
            }
            square += 1;
        }
        mask
    }};
}

const EX_H_FILE: u64 = const { ex_file_mask!(File::H) };
const EX_A_FILE: u64 = const { ex_file_mask!(File::A) };
const EX_G_FILE: u64 = const { ex_file_mask!(File::G) };
const EX_B_FILE: u64 = const { ex_file_mask!(File::B) };

pub const LT_KING_MOVES: [u64; 64] = const {
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

pub const LT_PAWN_ATTACKS: [[u64; 64]; Side::SideMax as usize] = const {
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

pub const LT_KNIGHT_MOVES: [u64; 64] = const {
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
