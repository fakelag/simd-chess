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

#[inline(always)]
pub fn static_exchange_eval(
    score_table: &[i16; 16],
    tables: &tables::Tables,
    board: &ChessGame,
    mut black_board: u64,
    mut white_board: u64,
    mut piece_board: [u64; 8],
    mv: u16,
) -> i16 {
    let from_sq = (mv & 0x3F) as usize;
    let to_sq = ((mv >> 6) & 0x3F) as usize;
    let to_sq_mask = 1u64 << to_sq;

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
    *[&mut white_board, &mut black_board][b_move as usize] ^= (1u64 << from_sq) | to_sq_mask;
    *[&mut white_board, &mut black_board][!b_move as usize] &= !remove_piece_mask;

    // Make the move on the bitboards
    piece_board[from_piece as usize] ^= (1u64 << from_sq) | to_sq_mask;
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

        (
            piece_index as u8,
            attacker_sq_mask.isolate_least_significant_one(),
        )
    };

    while stm_attackers != 0 {
        let (attacker_piece, attacker_sq_mask) = lva(stm_attackers, &piece_board);

        unsafe {
            std::hint::assert_unchecked(attacker_piece < 8);
        }

        piece_board[last_moved_piece as usize] ^= to_sq_mask;
        piece_board[attacker_piece as usize] ^= to_sq_mask | attacker_sq_mask;
        all_attackers ^= attacker_sq_mask;
        full_board ^= attacker_sq_mask;

        exchanges[exchange_index] =
            score_table[last_moved_piece as usize] - exchanges[exchange_index - 1];
        exchange_index += 1;
        last_moved_piece = attacker_piece;

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
        stm_attackers = all_attackers & [white_board, black_board][b_move as usize];
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

        (
            piece_index as u8,
            attacker_sq_mask.isolate_least_significant_one(),
        )
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
            search::eval::{Eval, WEIGHT_TABLE_ABS, WEIGHT_TABLE_ABS_I8},
            tables,
        },
        util,
    };

    use super::*;

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
        //     "[NAIVE] To piece {:?} on square {}",
        //     PieceIndex::from(to_piece as usize),
        //     util::square_name(to_sq as u8)
        // );

        if !unsafe { board.make_move(mv, tables) } {
            return None;
        }

        if board.in_check(tables, !board.b_move()) {
            return None;
        }

        loop {
            let mut move_list = [0u16; 256];

            let move_count = board.gen_moves_avx512::<true>(tables, &mut move_list);

            let lva_capture_move = move_list
                .iter()
                .take(move_count)
                .filter(|&&mv| (mv & MV_FLAG_CAP) != 0 && ((mv >> 6) & 0x3F) as usize == to_sq)
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

            exchanges[exchange_index] =
                score_table[last_moved_piece as usize] - exchanges[exchange_index - 1];
            exchange_index += 1;
            last_moved_piece = board.spt()[(lva_capture_move & 0x3F) as usize] & 7;

            if !unsafe { board.make_move(lva_capture_move, tables) } {
                panic!("Failed to make LVA capture move in SEE");
            }

            if board.in_check(tables, !board.b_move()) {
                if last_moved_piece == PieceIndex::WhiteKing as u8 {
                    // Revert king capture if there are still attackers left
                    exchange_index -= 1;
                    break;
                }
            }
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

    fn piece_value_i8(piece: u8) -> i8 {
        WEIGHT_TABLE_ABS_I8[piece as usize]
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
            );

            assert_eq!(
                result, *expected,
                "SEE threshold test '{}' failed: expected {}, got {} (eval {}, threshold {})",
                name, expected, result, real_eval, threshold
            );
        });
    }

    fn see_test(fen: &'static str, mv_string: &'static str) -> Eval {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();

        assert!(board.load_fen(fen, &tables).is_ok());

        let mv = board.fix_move(util::create_move(mv_string));

        assert!(util::is_legal_slow(&tables, &board, mv));
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
            black_board,
            white_board,
            piece_board,
            mv,
        );

        let naive_eval = see_naive(&WEIGHT_TABLE_ABS, &tables, &board, mv).unwrap();

        assert_eq!(
            real_eval, naive_eval,
            "Mismatch between real SEE and naive SEE"
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
        let move_count = board.gen_moves_avx512::<false>(tables, &mut move_list);

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
                black_board,
                white_board,
                piece_board,
                mv,
            );

            match see_naive(&WEIGHT_TABLE_ABS, &tables, &board, mv) {
                Some(naive_eval) => {
                    assert_eq!(
                        real_eval, naive_eval,
                        "Mismatch between real SEE and naive SEE"
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
    fn test_see_xxx() {
        let mv = "a6a5";
        let see_score = see_test(
            "r3k2r/p1ppqpb1/Qn3np1/3pN3/4PB2/2p4p/PPP2PPP/R3K2R w KQkq - 0 4",
            mv,
        );
        println!("SEE score for {}: {}", mv, see_score);
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
}
