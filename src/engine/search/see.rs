use std::arch::x86_64::*;

use crate::{
    engine::{
        chess_v2::{ChessGame, MV_FLAG_EPCAP, MV_FLAG_PROMOTION, MV_FLAGS, PieceIndex},
        tables,
    },
    util,
};

const PIECE_QUEEN: usize = PieceIndex::WhiteQueen as usize;
const PIECE_ROOK: usize = PieceIndex::WhiteRook as usize;
const PIECE_BISHOP: usize = PieceIndex::WhiteBishop as usize;
const PIECE_PAWN: usize = PieceIndex::WhitePawn as usize;
const PIECE_KING: usize = PieceIndex::WhiteKing as usize;

#[derive(Clone, Copy, Debug)]
pub struct Pinning {
    pub relations: [u8; 64],
    pub pinners: u64,
    pub pinned: u64,
}

impl Pinning {
    #[inline(always)]
    pub fn update_pinned_mask(&self, attacker_sq_mask: u64, out_pinned: &mut u64) {
        let is_pinner_mask = ((attacker_sq_mask & self.pinners == 0) as u64).wrapping_sub(1);
        println!(
            "lol {:064b} - {}",
            is_pinner_mask,
            self.relations[attacker_sq_mask.trailing_zeros() as usize]
        );
        let pin_removed_mask = (1u64
            .wrapping_shl(self.relations[attacker_sq_mask.trailing_zeros() as usize] as u32))
            & is_pinner_mask;

        *out_pinned &= !pin_removed_mask
    }
}

#[inline(always)]
pub fn calc_pinnings(
    b_move: bool,
    board: &ChessGame,
    black_board: u64,
    white_board: u64,
) -> Pinning {
    let mut pinners = 0u64;
    let mut pinned = 0u64;
    let mut relations = [0u8; 64];

    let bitboards = board.bitboards();
    let occupancy = black_board | white_board;

    let king_sq =
        bitboards[PieceIndex::WhiteKing as usize + (b_move as usize) * 8].trailing_zeros();
    let king_lines = &tables::Tables::LT_LINES[king_sq as usize];

    let rook_moves = tables::Tables::LT_ROOK_OCCUPANCY_MASKS[king_sq as usize];
    let bishop_moves = tables::Tables::LT_BISHOP_OCCUPANCY_MASKS[king_sq as usize];

    let rook_attacks = rook_moves
        & (bitboards[PieceIndex::WhiteRook as usize + !b_move as usize * 8]
            | bitboards[PieceIndex::WhiteQueen as usize + !b_move as usize * 8]);

    let bishop_attacks = bishop_moves
        & (bitboards[PieceIndex::WhiteBishop as usize + !b_move as usize * 8]
            | bitboards[PieceIndex::WhiteQueen as usize + !b_move as usize * 8]);

    let mut attackers = rook_attacks | bishop_attacks;
    let attacker_occ = occupancy ^ attackers;

    loop {
        let attacker_sq = crate::pop_ls1b!(attackers) as usize;

        let path_occupied = king_lines[attacker_sq as usize] & attacker_occ;

        let is_blocker_mask = ((path_occupied.count_ones() != 1) as u64).wrapping_sub(1);

        let blocker_sq = path_occupied.trailing_zeros() as usize;
        let attacker_sq = attacker_sq as usize;

        relations[attacker_sq] = blocker_sq as u8;
        pinned |= path_occupied & is_blocker_mask;
        pinners |= (1u64 << attacker_sq) & is_blocker_mask;
    }

    Pinning {
        relations,
        pinners,
        pinned,
    }
}

#[inline(always)]
pub fn static_exchange_eval(
    score_table: &[i16; 16],
    tables: &tables::Tables,
    board: &ChessGame,
    mv: u16,
    mut black_board: u64,
    mut white_board: u64,
    mut piece_board: [u64; 8],
    pins: Option<[Pinning; 2]>,
) -> i16 {
    let mut pinned = if let Some(pins) = &pins {
        [pins[0].pinned, pins[1].pinned]
    } else {
        [0u64; 2]
    };

    let from_sq = (mv & 0x3F) as usize;
    let to_sq = ((mv >> 6) & 0x3F) as usize;
    let to_sq_mask = 1u64 << to_sq;
    let from_sq_mask = 1u64 << from_sq;

    let from_piece = board.spt()[from_sq] & 7;

    let (remove_piece_mask, to_piece) = if mv & MV_FLAGS == MV_FLAG_EPCAP {
        let ep_sq = if board.b_move() { to_sq + 8 } else { to_sq - 8 };
        (1 << ep_sq, PieceIndex::WhitePawn as u8)
    } else {
        (to_sq_mask, board.spt()[to_sq])
    };

    unsafe {
        std::hint::assert_unchecked(to_piece < 16);
    }

    let mut b_move = board.b_move();

    if let Some(pins) = &pins {
        pins[b_move as usize].update_pinned_mask(to_sq_mask, &mut pinned[b_move as usize]);

        if from_sq_mask & pinned[b_move as usize] != 0 {
            return 0;
        }

        pins[!b_move as usize].update_pinned_mask(from_sq_mask, &mut pinned[!b_move as usize]);
    }

    *[&mut white_board, &mut black_board][b_move as usize] ^= from_sq_mask | to_sq_mask;
    *[&mut white_board, &mut black_board][!b_move as usize] &= !remove_piece_mask;

    // Make the move on the bitboards
    piece_board[from_piece as usize] ^= from_sq_mask | to_sq_mask;
    piece_board[to_piece as usize & 7] ^= remove_piece_mask;

    let mut full_board = black_board | white_board;
    let mut exchanges = [0; 32];
    let mut exchange_index = 1;

    let mut last_moved_piece = if mv & MV_FLAG_PROMOTION != 0 {
        PieceIndex::WhiteQueen as u8
    } else {
        from_piece
    };

    exchanges[0] = score_table[to_piece as usize];

    // Swap side
    b_move = !b_move;

    // Safety: to_sq is guaranteed to satisfy < 64
    let mut all_attackers = unsafe {
        calc_attackers(
            tables,
            full_board,
            black_board,
            white_board,
            piece_board,
            to_sq as u8,
        )
    };

    let lva = |stm_attackers: u64, piece_board: &[u64; 8]| unsafe {
        let piece_board_x8 = _mm512_loadu_epi64(piece_board.as_ptr() as *const i64);
        let stm_attackers_x8 = _mm512_set1_epi64(stm_attackers as i64);
        let and_result = _mm512_and_epi64(piece_board_x8, stm_attackers_x8);
        let lane_mask = _mm512_test_epi64_mask(and_result, and_result);

        debug_assert!(lane_mask != 0, "LVA called with no attackers");
        let piece_index = 7 - lane_mask.leading_zeros();

        let attacker_ls_lane = _mm512_permutexvar_epi64(
            _mm512_castsi128_si512(_mm_cvtsi32_si128(piece_index as i32)),
            and_result,
        );
        let attacker_sq_mask = _mm_cvtsi128_si64(_mm512_castsi512_si128(attacker_ls_lane)) as u64;

        (piece_index as u8, attacker_sq_mask.isolate_lowest_one())
    };

    let mut stm_attackers;

    loop {
        stm_attackers = all_attackers & [white_board, black_board][b_move as usize];

        println!(
            "Pinned pieces: {:064b} (stm={}), stm_attackers: {:064b}",
            pinned[b_move as usize], b_move, stm_attackers
        );

        stm_attackers &= !pinned[b_move as usize];

        // if let Some(pins) = &pins {
        //     stm_attackers &= !pins[b_move as usize].pinned;
        // }

        if stm_attackers == 0 {
            break;
        }

        let (attacker_piece, attacker_sq_mask) = lva(stm_attackers, &piece_board);

        unsafe {
            std::hint::assert_unchecked(attacker_piece < 8);
        }

        println!(
            "[SEE] Attacker piece {:?} on square {}",
            PieceIndex::from(attacker_piece as usize),
            util::square_name(attacker_sq_mask.trailing_zeros() as u8),
        );

        piece_board[last_moved_piece as usize] ^= to_sq_mask;
        piece_board[attacker_piece as usize] ^= to_sq_mask | attacker_sq_mask;
        all_attackers ^= attacker_sq_mask;
        full_board ^= attacker_sq_mask;

        exchanges[exchange_index] =
            score_table[last_moved_piece as usize] - exchanges[exchange_index - 1];
        exchange_index += 1;
        last_moved_piece = attacker_piece;

        if let Some(pins) = &pins {
            pins[!b_move as usize]
                .update_pinned_mask(attacker_sq_mask, &mut pinned[!b_move as usize]);
        }

        b_move = !b_move;

        match attacker_piece as usize {
            PIECE_KING => {
                if (all_attackers & !to_sq_mask) & [white_board, black_board][b_move as usize] != 0
                {
                    // Revert king capture if there are still attackers left
                    exchange_index -= 1;
                }
            }
            PIECE_PAWN | PIECE_BISHOP => {
                let attack_mask =
                    unsafe { calc_slider_attacks::<false>(tables, full_board, to_sq) };

                let queen_board = piece_board[PieceIndex::WhiteQueen as usize];
                let bishop_board = piece_board[PieceIndex::WhiteBishop as usize];

                all_attackers |= (queen_board | bishop_board) & attack_mask;
            }
            PIECE_ROOK => {
                let attack_mask = unsafe { calc_slider_attacks::<true>(tables, full_board, to_sq) };

                let queen_board = piece_board[PieceIndex::WhiteQueen as usize];
                let rook_board = piece_board[PieceIndex::WhiteRook as usize];

                all_attackers |= (queen_board | rook_board) & attack_mask;
            }
            PIECE_QUEEN => {
                let rook_attack_mask =
                    unsafe { calc_slider_attacks::<true>(tables, full_board, to_sq) };
                let bishop_attack_mask =
                    unsafe { calc_slider_attacks::<false>(tables, full_board, to_sq) };

                let queen_board = piece_board[PieceIndex::WhiteQueen as usize];
                let rook_board = piece_board[PieceIndex::WhiteRook as usize];
                let bishop_board = piece_board[PieceIndex::WhiteBishop as usize];

                all_attackers |= (queen_board | rook_board) & rook_attack_mask;
                all_attackers |= (queen_board | bishop_board) & bishop_attack_mask;
            }
            _ => {}
        }

        all_attackers &= !to_sq_mask;
    }

    while exchange_index > 1 {
        exchange_index -= 1;

        exchanges[exchange_index - 1] =
            exchanges[exchange_index - 1].min(-exchanges[exchange_index]);
    }

    exchanges[0]
}

#[inline(always)]
pub fn see_threshold(
    score_table: &[i16; 16],
    tables: &tables::Tables,
    board: &ChessGame,
    mv: u16,
    threshold: i16,
    mut black_board: u64,
    mut white_board: u64,
    mut piece_board: [u64; 8],
    pins: Option<&[Pinning; 2]>,
) -> bool {
    let from_sq = (mv & 0x3F) as usize;
    let to_sq = ((mv >> 6) & 0x3F) as usize;
    let to_sq_mask = 1u64 << to_sq;

    let from_piece = board.spt()[from_sq] & 7;

    let mut last_moved_piece = if mv & MV_FLAG_PROMOTION != 0 {
        PieceIndex::WhiteQueen as u8
    } else {
        from_piece
    };

    let (remove_piece_mask, to_piece) = if mv & MV_FLAGS == MV_FLAG_EPCAP {
        let ep_sq = if board.b_move() { to_sq + 8 } else { to_sq - 8 };
        (1 << ep_sq, PieceIndex::WhitePawn as u8)
    } else {
        (to_sq_mask, board.spt()[to_sq])
    };

    // 1. First capture:
    // `exchange` tracks the current capture sequence value offset to the
    // given threshold. It starts from the score of the initially captured
    // piece (if any) minus the threshold. This means that the exchange will
    // track the net gain/loss of the capture sequence with respect to the
    // threshold value
    let mut exchange = score_table[to_piece as usize] - threshold;

    if exchange < 0 {
        // Captured piece is not valuable enough to reach the threshold
        return false;
    }

    // 2. Possible recapture:
    // Update to net gain/loss after initial stm's piece has been recaptured
    exchange = score_table[last_moved_piece as usize] - exchange;

    if exchange <= 0 {
        // Capturing piece can be lost while still reaching the threshold
        return true;
    }

    unsafe {
        std::hint::assert_unchecked(to_piece < 16);
    }

    let mut b_move = board.b_move();
    *[&mut white_board, &mut black_board][b_move as usize] ^= (1u64 << from_sq) | to_sq_mask;
    *[&mut white_board, &mut black_board][!b_move as usize] &= !remove_piece_mask;

    // Make the move on the bitboards
    piece_board[from_piece as usize] ^= (1u64 << from_sq) | to_sq_mask;
    piece_board[to_piece as usize & 7] ^= remove_piece_mask;

    let mut full_board = black_board | white_board;

    // Swap side
    b_move = !b_move;

    // Safety: to_sq is guaranteed to satisfy < 64
    let mut all_attackers = unsafe {
        calc_attackers(
            tables,
            full_board,
            black_board,
            white_board,
            piece_board,
            to_sq as u8,
        )
    };
    let mut stm_attackers = all_attackers & [white_board, black_board][b_move as usize];

    let lva = |stm_attackers: u64, piece_board: &[u64; 8]| unsafe {
        let piece_board_x8 = _mm512_loadu_epi64(piece_board.as_ptr() as *const i64);
        let stm_attackers_x8 = _mm512_set1_epi64(stm_attackers as i64);
        let and_result = _mm512_and_epi64(piece_board_x8, stm_attackers_x8);
        let lane_mask = _mm512_test_epi64_mask(and_result, and_result);

        debug_assert!(lane_mask != 0, "LVA called with no attackers");
        let piece_index = 7 - lane_mask.leading_zeros();

        let attacker_ls_lane = _mm512_permutexvar_epi64(
            _mm512_castsi128_si512(_mm_cvtsi32_si128(piece_index as i32)),
            and_result,
        );
        let attacker_sq_mask = _mm_cvtsi128_si64(_mm512_castsi512_si128(attacker_ls_lane)) as u64;

        (piece_index as u8, attacker_sq_mask.isolate_lowest_one())
    };

    // 3. Start calculating capture sequence alternating sides.
    // The opponent is stm so pass starts as true since the initial
    // capture is checked to be valuable enough
    let mut is_pass = true;

    while stm_attackers != 0 {
        let (attacker_piece, attacker_sq_mask) = lva(stm_attackers, &piece_board);

        unsafe {
            std::hint::assert_unchecked(attacker_piece < 8);
        }

        piece_board[last_moved_piece as usize] ^= to_sq_mask;
        piece_board[attacker_piece as usize] ^= to_sq_mask | attacker_sq_mask;
        all_attackers ^= attacker_sq_mask;
        full_board ^= attacker_sq_mask;
        is_pass = !is_pass;
        b_move = !b_move;

        // 4. Confirmed recapture, update exchange to net gain/loss
        // if the attacking piece would get captured in turn
        exchange = score_table[attacker_piece as usize] - exchange;

        last_moved_piece = attacker_piece;

        if attacker_piece == PieceIndex::WhiteKing as u8 {
            if (all_attackers & !to_sq_mask) & [white_board, black_board][b_move as usize] != 0 {
                // King capture won't be applied if there are still attackers left
                is_pass = !is_pass;
            }
            break;
        }

        if exchange < is_pass as i16 {
            break;
        }

        match attacker_piece as usize {
            PIECE_PAWN | PIECE_BISHOP => {
                let attack_mask =
                    unsafe { calc_slider_attacks::<false>(tables, full_board, to_sq) };

                let queen_board = piece_board[PieceIndex::WhiteQueen as usize];
                let bishop_board = piece_board[PieceIndex::WhiteBishop as usize];

                all_attackers |= (queen_board | bishop_board) & attack_mask;
            }
            PIECE_ROOK => {
                let attack_mask = unsafe { calc_slider_attacks::<true>(tables, full_board, to_sq) };

                let queen_board = piece_board[PieceIndex::WhiteQueen as usize];
                let rook_board = piece_board[PieceIndex::WhiteRook as usize];

                all_attackers |= (queen_board | rook_board) & attack_mask;
            }
            PIECE_QUEEN => {
                let rook_attack_mask =
                    unsafe { calc_slider_attacks::<true>(tables, full_board, to_sq) };
                let bishop_attack_mask =
                    unsafe { calc_slider_attacks::<false>(tables, full_board, to_sq) };

                let queen_board = piece_board[PieceIndex::WhiteQueen as usize];
                let rook_board = piece_board[PieceIndex::WhiteRook as usize];
                let bishop_board = piece_board[PieceIndex::WhiteBishop as usize];

                all_attackers |= (queen_board | rook_board) & rook_attack_mask;
                all_attackers |= (queen_board | bishop_board) & bishop_attack_mask;
            }
            _ => {}
        }

        all_attackers &= !to_sq_mask;
        stm_attackers = all_attackers & [white_board, black_board][b_move as usize];
    }

    is_pass
}

/// Safety: sq_index must be < 64
pub unsafe fn calc_slider_attacks<const IS_ROOK: bool>(
    tables: &tables::Tables,
    full_board: u64,
    sq_index: usize,
) -> u64 {
    unsafe {
        std::hint::assert_unchecked(sq_index < 64);
    }

    let occupancy_mask = if IS_ROOK {
        tables::Tables::LT_ROOK_OCCUPANCY_MASKS[sq_index as usize]
    } else {
        tables::Tables::LT_BISHOP_OCCUPANCY_MASKS[sq_index as usize]
    };

    let blockers = full_board & occupancy_mask;

    unsafe { tables.get_slider_move_mask_unchecked::<IS_ROOK>(sq_index as usize, blockers) }
}

/// Safety: sq_index must be < 64
pub unsafe fn calc_attackers(
    tables: &tables::Tables,
    full_board: u64,
    black_board: u64,
    white_board: u64,
    piece_board: [u64; 8],
    sq_index: u8,
) -> u64 {
    let sq_index = sq_index as usize;

    unsafe {
        std::hint::assert_unchecked(sq_index < 64);
    }

    let pawn_attack_mask_white = tables::Tables::LT_PAWN_CAPTURE_MASKS[0][sq_index as usize];
    let pawn_attack_mask_black = tables::Tables::LT_PAWN_CAPTURE_MASKS[1][sq_index as usize];

    let all_pawn_board = piece_board[PieceIndex::WhitePawn as usize];
    let mut attackers = all_pawn_board & black_board & pawn_attack_mask_white;
    attackers |= all_pawn_board & white_board & pawn_attack_mask_black;

    let knight_attack_mask = tables::Tables::LT_KNIGHT_MOVE_MASKS[sq_index as usize];
    attackers |= piece_board[PieceIndex::WhiteKnight as usize] & knight_attack_mask;

    let king_attack_mask = tables::Tables::LT_KING_MOVE_MASKS[sq_index as usize];
    attackers |= piece_board[PieceIndex::WhiteKing as usize] & king_attack_mask;

    let rook_board = piece_board[PieceIndex::WhiteRook as usize];
    let bishop_board = piece_board[PieceIndex::WhiteBishop as usize];
    let queen_board = piece_board[PieceIndex::WhiteQueen as usize];

    let rook_moves = unsafe { calc_slider_attacks::<true>(tables, full_board, sq_index) };
    let bishop_moves = unsafe { calc_slider_attacks::<false>(tables, full_board, sq_index) };

    attackers |= (rook_board | queen_board) & rook_moves;
    attackers |= (bishop_board | queen_board) & bishop_moves;

    attackers
}

#[cfg(test)]
mod tests {
    use crate::{
        engine::{
            chess_v2::{self, MV_FLAG_CAP, MV_FLAG_EPCAP, MV_FLAG_PROMOTION, PieceIndex},
            search::eval::{Eval, WEIGHT_TABLE_ABS},
            tables,
        },
        util,
    };

    use super::*;

    pub fn is_legal_slow(tables: &tables::Tables, board: &chess_v2::ChessGame, mv: u16) -> bool {
        let mut board_copy = board.clone();

        let mut move_list = [0u16; 256];
        for i in 0..board_copy.gen_moves_avx512::<false>(&mut move_list) {
            if move_list[i] == mv {
                if !unsafe { board_copy.make_move(mv, tables) }
                    || board_copy.in_check(tables, !board_copy.b_move())
                {
                    return false;
                }

                return true;
            }
        }
        return false;
    }

    pub fn see_naive<T>(
        score_table: &[T; 16],
        tables: &tables::Tables,
        board: &ChessGame,
        mv: u16,
    ) -> Option<T>
    where
        T: std::ops::Sub<Output = T>
            + std::ops::Neg<Output = T>
            + Ord
            + Copy
            + From<i8>
            + std::fmt::Debug,
    {
        pub const PIECE_VALUES_LVA: [Eval; PieceIndex::PieceIndexMax as usize] = [
            0, 10_000, 1000, 525, 351, 350, 100, 0, 0, 10_000, 1000, 525, 351, 350, 100, 0,
        ];

        let mut board = board.clone();

        let from_sq = (mv & 0x3F) as usize;
        let to_sq = ((mv >> 6) & 0x3F) as usize;

        let to_piece = if mv & MV_FLAGS == MV_FLAG_EPCAP {
            PieceIndex::WhitePawn as u8
        } else {
            board.spt()[to_sq]
        };

        let mut exchanges = [T::from(0); 32];
        let mut exchange_index = 1;
        let mut last_moved_piece = if mv & MV_FLAG_PROMOTION != 0 {
            PieceIndex::WhiteQueen as u8
        } else {
            board.spt()[from_sq] & 7
        };

        exchanges[0] = score_table[to_piece as usize];

        // println!(
        //     "[NAIVE] To piece {:?} on square {}.Mv={}",
        //     PieceIndex::from(to_piece as usize),
        //     util::square_name(to_sq as u8),
        //     util::move_string_dbg(mv),
        // );

        if !unsafe { board.make_move(mv, tables) } {
            return None;
        }

        if board.in_check(tables, !board.b_move()) {
            return None;
        }

        // let mut banned_moves = std::collections::BTreeSet::new();

        loop {
            let mut move_list = [0u16; 256];

            let move_count = board.gen_moves_avx512::<true>(&mut move_list);

            let lva_capture_move = move_list
                .iter()
                .take(move_count)
                .filter(|&&mv| (mv & MV_FLAG_CAP) != 0 && ((mv >> 6) & 0x3F) as usize == to_sq)
                // .filter(|&&mv| !banned_moves.contains(&mv))
                .min_by_key(|&&mv| {
                    let src_sq = (mv & 0x3F) as usize;
                    let mut score = (PIECE_VALUES_LVA[board.spt()[src_sq] as usize] as u64) << 32;
                    score |= src_sq as u64;
                    score
                });

            let lva_capture_move = match lva_capture_move {
                Some(&m) => m,
                None => break,
            };

            let moved_piece = board.spt()[(lva_capture_move & 0x3F) as usize] & 7;

            // println!(
            //     "[NAIVE-INNER] Making move {:?}",
            //     util::move_string_dbg(lva_capture_move)
            // );

            // let board_copy = board.clone();

            if !unsafe { board.make_move(lva_capture_move, tables) } {
                panic!("Failed to make LVA capture move in SEE");
            }

            if board.in_check(tables, !board.b_move()) {
                if moved_piece == PieceIndex::WhiteKing as u8 {
                    // Revert king capture if there are still attackers left
                    // exchange_index -= 1;
                    break;
                }
                // banned_moves.insert(lva_capture_move);
                // board = board_copy;
                // println!("[NAIVE-INNER] Move was illegal due to check, reverting.");
                // continue;
            }

            // banned_moves.clear();

            exchanges[exchange_index] =
                score_table[last_moved_piece as usize] - exchanges[exchange_index - 1];
            exchange_index += 1;
            last_moved_piece = moved_piece;
        }

        while exchange_index > 1 {
            exchange_index -= 1;

            if exchanges[exchange_index - 1] > -exchanges[exchange_index] {
                exchanges[exchange_index - 1] = -exchanges[exchange_index];
            }
        }

        Some(exchanges[0])
    }

    fn piece_value(piece: u8) -> Eval {
        WEIGHT_TABLE_ABS[piece as usize]
    }

    fn see_test_threshold(
        tables: &tables::Tables,
        board: &ChessGame,
        mv: u16,
        real_eval: Eval,
        black_board: u64,
        white_board: u64,
        piece_board: [u64; 8],
    ) {
        [
            ("equal", real_eval, true),
            ("greater (-1)", real_eval - 1, true),
            ("greater (-500)", real_eval - 500, true),
            ("less (+1)", real_eval + 1, false),
            ("less (+500)", real_eval + 500, false),
        ]
        .iter()
        .for_each(|(name, threshold, expected)| {
            let result = see_threshold(
                &WEIGHT_TABLE_ABS,
                &tables,
                &board,
                mv,
                *threshold,
                black_board,
                white_board,
                piece_board,
                None,
            );

            assert_eq!(
                result, *expected,
                "SEE threshold test '{}' failed: expected {}, got {} (eval {}, threshold {})",
                name, expected, result, real_eval, threshold
            );
        });
    }

    fn see_eval(fen: &'static str, mv_string: &'static str, check_pins: bool) -> Eval {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();

        assert!(board.load_fen(fen, &tables).is_ok());

        let mv = board.fix_move(util::create_move(mv_string));

        // assert!(mv & MV_FLAG_CAP != 0);

        let bitboards = board.bitboards();

        let black_board = bitboards.iter().skip(8).fold(0, |acc, &bb| acc | bb);
        let white_board = bitboards.iter().take(8).fold(0, |acc, &bb| acc | bb);

        let mut piece_board: [u64; 8] = [0u64; 8];
        bitboards
            .iter()
            .take(8)
            .zip(bitboards.iter().skip(8))
            .enumerate()
            .for_each(|(index, (w, b))| piece_board[index] = *w | *b);

        let pins = if check_pins {
            Some([
                calc_pinnings(false, &board, black_board, white_board),
                calc_pinnings(true, &board, black_board, white_board),
            ])
        } else {
            None
        };

        let result = static_exchange_eval(
            &WEIGHT_TABLE_ABS,
            &tables,
            &board,
            mv,
            black_board,
            white_board,
            piece_board,
            pins,
        );

        result
    }

    fn see_test(fen: &'static str, mv_string: &'static str) -> Eval {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();

        assert!(board.load_fen(fen, &tables).is_ok());

        let mv = board.fix_move(util::create_move(mv_string));

        assert!(is_legal_slow(&tables, &board, mv));
        // assert!(mv & MV_FLAG_CAP != 0);

        let bitboards = board.bitboards();

        let black_board = bitboards.iter().skip(8).fold(0, |acc, &bb| acc | bb);
        let white_board = bitboards.iter().take(8).fold(0, |acc, &bb| acc | bb);

        let mut piece_board: [u64; 8] = [0u64; 8];
        bitboards
            .iter()
            .take(8)
            .zip(bitboards.iter().skip(8))
            .enumerate()
            .for_each(|(index, (w, b))| piece_board[index] = *w | *b);

        let real_eval = static_exchange_eval(
            &WEIGHT_TABLE_ABS,
            &tables,
            &board,
            mv,
            black_board,
            white_board,
            piece_board,
            None,
        );

        let naive_eval = see_naive(&WEIGHT_TABLE_ABS, &tables, &board, mv).unwrap();

        assert_eq!(
            real_eval, naive_eval,
            "Mismatch between real SEE and naive SEE.Fen={}.Mv={}",
            fen, mv_string
        );

        // Check eval with pinning enabled
        let pin_eval = see_eval(fen, mv_string, true);
        assert_eq!(
            real_eval, pin_eval,
            "Mismatch between real SEE and pinned SEE.Fen={}.Mv={}",
            fen, mv_string
        );

        see_test_threshold(
            &tables,
            &board,
            mv,
            real_eval,
            black_board,
            white_board,
            piece_board,
        );

        real_eval
    }

    fn see_test_fuzz(board: &mut ChessGame, tables: &tables::Tables, depth: u8) {
        if depth == 0 {
            return;
        }

        let mut move_list = [0u16; 256];
        let move_count = board.gen_moves_avx512::<false>(&mut move_list);

        let board_copy = board.clone();

        for &mv in move_list.iter().take(move_count) {
            if !unsafe { board.make_move(mv, tables) } {
                continue;
            }

            if board.in_check(tables, !board.b_move()) {
                *board = board_copy.clone();
                continue;
            }

            if true {
                see_test_fuzz(board, tables, depth - 1);
            } else {
                if mv & MV_FLAG_CAP == 0 {
                    see_test_fuzz(board, tables, depth - 1);
                    *board = board_copy.clone();
                    continue;
                }
            }

            *board = board_copy.clone();

            // println!(
            //     "Fuzzing SEE on move {} in position {}",
            //     util::move_string_dbg(mv),
            //     board.gen_fen()
            // );

            let bitboards = board.bitboards();

            let black_board = bitboards.iter().skip(8).fold(0, |acc, &bb| acc | bb);
            let white_board = bitboards.iter().take(8).fold(0, |acc, &bb| acc | bb);

            let mut piece_board: [u64; 8] = [0u64; 8];
            bitboards
                .iter()
                .take(8)
                .zip(bitboards.iter().skip(8))
                .enumerate()
                .for_each(|(index, (w, b))| piece_board[index] = *w | *b);

            let real_eval = static_exchange_eval(
                &WEIGHT_TABLE_ABS,
                &tables,
                &board,
                mv,
                black_board,
                white_board,
                piece_board,
                None,
            );

            match see_naive(&WEIGHT_TABLE_ABS, &tables, &board, mv) {
                Some(naive_eval) => {
                    assert_eq!(
                        real_eval,
                        naive_eval,
                        "Mismatch between real SEE and naive SEE. RealEval={}, NaiveEval={}. Position: {}. Move: {}",
                        real_eval,
                        naive_eval,
                        board.gen_fen(),
                        util::move_string_dbg(mv),
                    );

                    see_test_threshold(
                        &tables,
                        &board,
                        mv,
                        real_eval,
                        black_board,
                        white_board,
                        piece_board,
                    );
                }
                None => {}
            }
        }
    }

    #[test]
    fn test_see_fuzz() {
        let test_fens = [
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            "8/3PPP2/4K3/8/P2qN3/3k4/3N4/1q6 w - - 0 1",
            "2r5/1P4pk/p2p1b1p/5b1n/BB3p2/2R2p2/P1P2P2/4RK2 w - - 0 1",
            "1R2r2k/5bp1/3Q2p1/2p5/4P2P/1p6/2r1n1BK/8 b - - 0 1",
        ];

        for fen in test_fens {
            let tables = tables::Tables::new();
            let mut board = chess_v2::ChessGame::new();

            assert!(board.load_fen(fen, &tables).is_ok());

            see_test_fuzz(&mut board, &tables, 4);
        }
    }

    #[test]
    fn test_see_zero() {
        let mv = "a6a5";
        let see_score = see_test(
            "r3k2r/p1ppqpb1/Qn3np1/3pN3/4PB2/2p4p/PPP2PPP/R3K2R w KQkq - 0 4",
            mv,
        );
        assert_eq!(see_score, 0, "Expected SEE to be 0, got {}", see_score);
    }

    #[test]
    fn test_see_simple() {
        let qxp = "b3b6";
        let see_score = see_test("7k/p7/1p6/8/8/1Q6/8/7K w - - 0 1", qxp);
        let expected =
            piece_value(PieceIndex::BlackPawn as u8) - piece_value(PieceIndex::WhiteQueen as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_simple_2() {
        let mv = "e1e5";
        let see_score = see_test("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1", mv);
        let expected = piece_value(PieceIndex::BlackPawn as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_no_attackers() {
        let mv = "a4c6";
        let see_score = see_test("7k/8/2q5/8/Q7/8/8/7K w - - 0 1", mv);
        let expected = piece_value(PieceIndex::BlackQueen as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_quiet() {
        let mv = "a4c6";
        let see_score = see_test("7k/8/8/8/Q7/8/8/7K w - - 0 1", mv);
        let expected = 0;

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_quiet_blunder() {
        let mv = "a4c6";
        let see_score = see_test("7k/3p4/8/8/Q7/8/8/7K w - - 0 1", mv);
        let expected = -piece_value(PieceIndex::WhiteQueen as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_quiet_recapture() {
        let mv = "b5c6";
        let see_score = see_test("7k/3p4/8/1Q6/B7/8/8/7K w - - 0 1", mv);
        let expected =
            -piece_value(PieceIndex::WhiteQueen as u8) + piece_value(PieceIndex::BlackPawn as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_multi() {
        let mv = "b3b6";
        let see_score = see_test("7k/p7/1p4R1/8/8/1Q6/8/7K w - - 0 1", mv);
        let expected = piece_value(PieceIndex::BlackPawn as u8)
            - piece_value(PieceIndex::WhiteQueen as u8)
            + piece_value(PieceIndex::BlackPawn as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_multi_2() {
        let mv = "b1b6";
        let see_score = see_test("7k/3n4/Rq6/8/8/8/8/1Q4K1 w - - 0 1", mv);
        let expected = piece_value(PieceIndex::BlackQueen as u8)
            - piece_value(PieceIndex::WhiteQueen as u8)
            + piece_value(PieceIndex::BlackKnight as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_lva() {
        let mv = "b1b6";
        let see_score = see_test("n1n4k/8/1p2R3/P7/8/8/8/1Q4K1 w - - 0 1", mv);
        // White pawn should recapture first instead of the rook
        let expected = piece_value(PieceIndex::BlackPawn as u8)
            - piece_value(PieceIndex::WhiteQueen as u8)
            + piece_value(PieceIndex::BlackKnight as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_multi_early_stop() {
        let mv = "b3b6";
        let see_score = see_test("7k/p2n4/1p4R1/8/8/1Q6/8/7K w - - 0 1", mv);
        // White will stop the exchange after the queen has been recaptured to not blunder the rook
        let expected =
            piece_value(PieceIndex::BlackPawn as u8) - piece_value(PieceIndex::WhiteQueen as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_xray() {
        let mv = "b3b7";
        let see_score = see_test("7k/1q6/3n4/8/8/1Q6/8/1R4K1 w - - 0 1", mv);
        // Simple x-ray exposed after the first forced queen move
        // Same as test_see_multi_2 but with rook behind queen
        let expected = piece_value(PieceIndex::BlackQueen as u8)
            - piece_value(PieceIndex::WhiteQueen as u8)
            + piece_value(PieceIndex::BlackKnight as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_xray_2() {
        let mv = "b4b6";
        let see_score = see_test("n6k/3n4/1q6/8/1Q6/1R6/1R6/1R4K1 w - - 0 1", mv);
        // More complex x-ray where a rook attack is exposed after the first rook capture
        let expected = piece_value(PieceIndex::BlackQueen as u8)
            - piece_value(PieceIndex::WhiteQueen as u8)
            + piece_value(PieceIndex::BlackKnight as u8)
            - piece_value(PieceIndex::WhiteRook as u8)
            + piece_value(PieceIndex::BlackKnight as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_xray_complex() {
        let mv = "d3e5";
        let see_score = see_test(
            "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1",
            mv,
        );
        let expected =
            piece_value(PieceIndex::BlackPawn as u8) - piece_value(PieceIndex::WhiteKnight as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_tricky() {
        let mv = "b4a3";
        let see_score = see_test(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/P1N2Q1p/1PPBBPPP/R3K2R b KQkq - 0 1",
            mv,
        );
        let expected = 0; // +- pawn
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_tricky_2() {
        let mv = "e5e3";
        let see_score = see_test(
            "r3k2r/p1pp1pb1/B4np1/3Pq3/1p6/4Q2p/PPPB1PPP/R3K2R b KQkq - 1 4",
            mv,
        );
        let expected = 0; // +- queen
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_tricky_king() {
        let mv = "f8f4";
        let see_score = see_test("k4r2/8/8/8/5PK1/8/8/8 b - - 0 1", mv);
        let expected =
            piece_value(PieceIndex::WhitePawn as u8) - piece_value(PieceIndex::BlackRook as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_tricky_king_threat() {
        let mv = "f8f4";
        let see_score = see_test("k4r2/8/8/4p3/5PK1/8/8/8 b - - 0 1", mv);
        let expected = piece_value(PieceIndex::WhitePawn as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_tricky_king_threat_2() {
        let mv = "f3f7";
        let see_score = see_test(
            "r3k2r/p1pp1pb1/1n4p1/3B4/1p2P3/5Q1p/PqPB1PPP/R3K2R w KQkq - 0 5",
            mv,
        );
        let expected = piece_value(PieceIndex::WhitePawn as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_tricky_3() {
        let mv = "f3a8";
        let see_score = see_test(
            "r3k2r/p1ppqpb1/4Pnp1/1N2N3/1p6/5Q2/PPPB1n1P/R3K3 w Qkq - 0 6",
            mv,
        );
        let expected = piece_value(PieceIndex::BlackRook as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_tricky_4() {
        let mv = "e3a7";
        let see_score = see_test(
            "1rn1k2r/p1ppqpb1/b2Ppnp1/4N3/1p2P3/2N1Q2p/PPPBBPPP/R3K2R w KQk - 3 3",
            mv,
        );
        let expected =
            piece_value(PieceIndex::WhitePawn as u8) - piece_value(PieceIndex::WhiteQueen as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_promotion() {
        let mv = "e7d8q";
        let see_score = see_test("r2q4/3QPP2/4K3/8/P2qN3/3k4/3N4/8 w - - 3 3", mv);
        let expected = 0; // +- queen

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_pin_simple() {
        let mv = "e4e3";
        let see_score = see_eval("k7/3r4/8/8/3Bp3/8/8/3KB3 b - - 0 4", mv, true);
        let expected = 0;

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_pin_firstmove() {
        let mv = "d4e3";
        let see_score = see_eval("k7/3r4/8/8/3B4/4p3/8/3KB3 w - - 0 4", mv, true);
        let expected = 0;

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_pin_remove_pinned() {
        let mv = "c4d3";
        let see_score = see_eval("k7/3r4/8/8/2B5/3p4/8/3KB3 w - - 0 4", mv, true);
        let expected =
            piece_value(PieceIndex::BlackPawn as u8) - piece_value(PieceIndex::WhiteBishop as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_pin_remove_pinned_x2() {
        let mv = "c3d3";
        let see_score = see_eval("k7/3r4/8/8/4B3/2Rb4/8/3KB3 w - - 0 4", mv, true);
        let expected = piece_value(PieceIndex::BlackBishop as u8)
            - piece_value(PieceIndex::WhiteRook as u8)
            + piece_value(PieceIndex::BlackRook as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_pin_remove_pinner_firstmove() {
        let mv = "d5e4";
        let see_score = see_eval("1k6/8/8/3q4/4B3/3B4/8/3K4 b - - 0 4", mv, true);
        let expected =
            piece_value(PieceIndex::WhiteBishop as u8) - piece_value(PieceIndex::BlackQueen as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_pin_remove_self_pinner_firstmove() {
        let mv = "d5e4";
        let see_score = see_eval("k7/8/8/3q4/4B3/8/8/4K3 b - - 0 4", mv, true);
        let expected = piece_value(PieceIndex::WhiteBishop as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_pin_multi_pinned() {
        let mv = "f7g6";
        let see_score = see_eval("k7/5q2/r1r3P1/8/R3B3/8/8/4K3 b - - 0 4", mv, true);
        let expected = piece_value(PieceIndex::WhitePawn as u8)
            - piece_value(PieceIndex::BlackQueen as u8)
            + piece_value(PieceIndex::WhiteBishop as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_pin_multi_pinned_2() {
        let mv = "f7g6";
        let see_score = see_eval("k7/1b3q2/r5P1/8/R3B3/8/8/4K3 b - - 0 4", mv, true);
        let expected =
            piece_value(PieceIndex::WhitePawn as u8) - piece_value(PieceIndex::BlackQueen as u8);

        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }
    // @todo - Pins for threshold SEE & more test cases
}
