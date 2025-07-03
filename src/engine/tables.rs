pub const EX_H_FILE: u64 = const {
    let mut mask = 0u64;
    let mut square = 0;
    while square < 64 {
        if square % 8 != 7 {
            mask |= 1 << square;
        }
        square += 1;
    }
    mask
};

pub const EX_A_FILE: u64 = const {
    let mut mask = 0u64;
    let mut square = 0;
    while square < 64 {
        if square % 8 != 0 {
            mask |= 1 << square;
        }
        square += 1;
    }
    mask
};

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
