use std::arch::x86_64::*;

use crate::{
    engine::{
        chess_v2::{ChessGame, PieceIndex},
        tables,
    },
    util,
};

const PIECE_QUEEN: usize = PieceIndex::WhiteQueen as usize;
const PIECE_ROOK: usize = PieceIndex::WhiteRook as usize;
const PIECE_BISHOP: usize = PieceIndex::WhiteBishop as usize;
const PIECE_PAWN: usize = PieceIndex::WhitePawn as usize;
const PIECE_KING: usize = PieceIndex::WhiteKing as usize;

#[inline(never)]
pub fn static_exchange_eval<T>(
    score_table: &[T; 16],
    tables: &tables::Tables,
    board: &ChessGame,
    mut black_board: u64,
    mut white_board: u64,
    mut piece_board: [u64; 8],
    mv: u16,
) -> T
where
    T: std::ops::Sub<Output = T> + std::ops::Neg<Output = T> + Ord + Copy + From<i8>,
{
    let from_sq = (mv & 0x3F) as usize;
    let to_sq = ((mv >> 6) & 0x3F) as usize;
    let to_sq_mask = 1u64 << to_sq;

    let from_piece = board.spt()[from_sq];
    let to_piece = board.spt()[to_sq];

    unsafe {
        std::hint::assert_unchecked(to_piece < 16);
    }

    let mut b_move = board.b_move();
    *[&mut white_board, &mut black_board][b_move as usize] ^= (1u64 << from_sq) | to_sq_mask;

    // Make the move on the bitboards
    piece_board[from_piece as usize & 7] ^= (1u64 << from_sq) | to_sq_mask;
    piece_board[to_piece as usize & 7] ^= to_sq_mask;

    let mut full_board = black_board | white_board;
    let mut exchanges = [T::from(0); 32];
    let mut exchange_index = 1;

    let mut last_moved_piece = from_piece & 7;

    exchanges[0] = score_table[to_piece as usize];

    // Swap side
    b_move = !b_move;

    // Safety: to_sq is guaranteed to satisfy < 64
    let mut all_attackers = unsafe { calc_attackers(tables, full_board, piece_board, to_sq as u8) };
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
                if all_attackers & [white_board, black_board][b_move as usize] != 0 {
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
                all_attackers &= !to_sq_mask;
            }
            PIECE_ROOK => {
                let attack_mask = unsafe { calc_slider_attacks::<true>(tables, full_board, to_sq) };

                let queen_board = piece_board[PieceIndex::WhiteQueen as usize];
                let rook_board = piece_board[PieceIndex::WhiteRook as usize];

                all_attackers |= (queen_board | rook_board) & attack_mask;
                all_attackers &= !to_sq_mask;
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
                all_attackers &= !to_sq_mask;
            }
            _ => {}
        }

        stm_attackers = all_attackers & [white_board, black_board][b_move as usize];
    }

    while exchange_index > 1 {
        exchange_index -= 1;

        exchanges[exchange_index - 1] =
            exchanges[exchange_index - 1].min(-exchanges[exchange_index]);
    }

    exchanges[0]
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
    piece_board: [u64; 8],
    sq_index: u8,
) -> u64 {
    let sq_index = sq_index as usize;

    unsafe {
        std::hint::assert_unchecked(sq_index < 64);
    }

    let pawn_attack_mask = tables::Tables::LT_PAWN_CAPTURE_MASKS[0][sq_index as usize]
        | tables::Tables::LT_PAWN_CAPTURE_MASKS[1][sq_index as usize];

    let mut attackers = piece_board[PieceIndex::WhitePawn as usize] & pawn_attack_mask;

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
            chess_v2::{self, PieceIndex},
            search::eval::{Eval, WEIGHT_TABLE_ABS, WEIGHT_TABLE_ABS_I8},
            tables,
        },
        util,
    };

    use super::*;

    fn piece_value(piece: u8) -> Eval {
        WEIGHT_TABLE_ABS[piece as usize]
    }

    fn piece_value_i8(piece: u8) -> i8 {
        WEIGHT_TABLE_ABS_I8[piece as usize]
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

        let mut piece_boards: [u64; 8] = [0u64; 8];
        bitboards
            .iter()
            .take(8)
            .zip(bitboards.iter().skip(8))
            .enumerate()
            .for_each(|(index, (w, b))| piece_boards[index] = *w | *b);

        static_exchange_eval(
            &WEIGHT_TABLE_ABS,
            &tables,
            &board,
            black_board,
            white_board,
            piece_boards,
            mv,
        )
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
    fn test_see_multi() {
        let mv = "b3b6";
        let see_score = see_test("7k/p7/1p5R/8/8/1Q6/8/7K w - - 0 1", mv);
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
    fn test_see_i8() {
        let mv_string = "b1b6";

        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();

        assert!(
            board
                .load_fen("7k/3n4/Rq6/8/8/8/8/1Q4K1 w - - 0 1", &tables)
                .is_ok()
        );

        let mv = board.fix_move(util::create_move(mv_string));

        assert!(util::is_legal_slow(&tables, &board, mv));
        // assert!(mv & MV_FLAG_CAP != 0);

        let bitboards = board.bitboards();

        let black_board = bitboards.iter().skip(8).fold(0, |acc, &bb| acc | bb);
        let white_board = bitboards.iter().take(8).fold(0, |acc, &bb| acc | bb);

        let mut piece_boards: [u64; 8] = [0u64; 8];
        bitboards
            .iter()
            .take(8)
            .zip(bitboards.iter().skip(8))
            .enumerate()
            .for_each(|(index, (w, b))| piece_boards[index] = *w | *b);

        let see_score = static_exchange_eval(
            &WEIGHT_TABLE_ABS_I8,
            &tables,
            &board,
            black_board,
            white_board,
            piece_boards,
            mv,
        );

        let expected = piece_value_i8(PieceIndex::BlackQueen as u8)
            - piece_value_i8(PieceIndex::WhiteQueen as u8)
            + piece_value_i8(PieceIndex::BlackKnight as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }

    #[test]
    fn test_see_multi_early_stop() {
        let mv = "b3b6";
        let see_score = see_test("7k/p2n4/1p5R/8/8/1Q6/8/7K w - - 0 1", mv);
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
    fn test_see_tricky_king_threatened() {
        let mv = "f8f4";
        let see_score = see_test("k4r2/8/8/4p3/5PK1/8/8/8 b - - 0 1", mv);
        let expected = piece_value(PieceIndex::WhitePawn as u8);
        assert_eq!(
            see_score, expected,
            "Expected SEE to be {}, got {}",
            expected, see_score
        );
    }
}
