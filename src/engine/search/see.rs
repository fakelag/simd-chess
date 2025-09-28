use crate::engine::{
    chess_v2::{ChessGame, PieceIndex},
    search::eval::{Eval, WEIGHT_TABLE_ABS},
    tables,
};

fn piece_value(piece: u8) -> Eval {
    WEIGHT_TABLE_ABS[piece as usize]
}

pub fn static_exchange_eval(
    tables: &tables::Tables,
    board: &ChessGame,
    mut black_board: u64,
    mut white_board: u64,
    mut piece_board: [u64; 8],
    mv: u16,
) -> Eval {
    let from_sq = (mv & 0x3F) as usize;
    let to_sq = ((mv >> 6) & 0x3F) as usize;
    let to_sq_mask = 1u64 << to_sq;

    let from_piece = board.spt()[from_sq];
    let to_piece = board.spt()[to_sq];

    let mut b_move = board.b_move();
    *[&mut white_board, &mut black_board][b_move as usize] ^= (1u64 << from_sq) | to_sq_mask;

    // Make the move on the bitboards
    piece_board[from_piece as usize & 7] ^= (1u64 << from_sq) | to_sq_mask;
    piece_board[to_piece as usize & 7] ^= to_sq_mask;

    let mut full_board = black_board | white_board;
    let mut exchanges = [0 as Eval; 32];
    let mut exchange_index = 1;

    let mut last_moved_piece = from_piece;

    exchanges[0] = piece_value(to_piece);

    // Swap side
    b_move = !b_move;

    // Safety: to_sq is guaranteed to satisfy < 64
    let mut all_attackers = unsafe { calc_attackers(tables, full_board, piece_board, to_sq as u8) };
    let mut stm_attackers = all_attackers & [white_board, black_board][b_move as usize];

    println!(
        "SEE: all_attackers = {all_attackers:064b} - {}",
        all_attackers.trailing_zeros()
    );
    println!(
        "SEE: stm_attackers = {stm_attackers:064b} - {}",
        stm_attackers.trailing_zeros()
    );

    let lva = |stm_attackers: u64, piece_board: &[u64; 8]| {
        // @todo - check perf with unaligned vs aligned array accesses
        for piece_index in (0..8).rev() {
            let attackers = stm_attackers & piece_board[piece_index];

            if attackers != 0 {
                return Some((piece_index as u8, attackers.trailing_zeros() as u8));
            }
        }
        return None;
    };

    while stm_attackers != 0 {
        // 1. Take least valueable attacker
        let (attacker_piece, attacker_sq) = match lva(stm_attackers, &piece_board) {
            Some(v) => v,
            None => unreachable!(),
        };

        println!("SEE: Attacker attacker_piece = {attacker_piece}, attacker_sq = {attacker_sq}");

        // 2. Move it to the square, update bitboards
        piece_board[last_moved_piece as usize] ^= to_sq_mask;
        piece_board[attacker_piece as usize] ^= to_sq_mask | (1u64 << attacker_sq);
        all_attackers ^= 1u64 << attacker_sq;
        full_board ^= 1u64 << attacker_sq;

        // 3. Update scores & last moved piece
        exchanges[exchange_index] = piece_value(last_moved_piece) - exchanges[exchange_index - 1];
        exchange_index += 1;
        last_moved_piece = attacker_piece;

        // 4. Swap side
        b_move = !b_move;

        // 5. Update stm attackers (including x-rays)
        // all_attackers = unsafe { calc_attackers(tables, full_board, piece_board, to_sq as u8) };
        stm_attackers = all_attackers & [white_board, black_board][b_move as usize];
    }

    println!("SEE: exchanges = {:?}", &exchanges[0..exchange_index]);

    while exchange_index > 1 {
        exchange_index -= 1;
        if exchanges[exchange_index - 1] > -exchanges[exchange_index] {
            exchanges[exchange_index - 1] = -exchanges[exchange_index];
        } else {
            println!(
                "SEE: Early out at index {} [{}, {}]",
                exchange_index,
                exchanges[exchange_index - 1],
                exchanges[exchange_index]
            );
        }
    }

    exchanges[0]
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

    let rook_occupancy_mask = tables::Tables::LT_ROOK_OCCUPANCY_MASKS[sq_index as usize];
    let rook_blockers = full_board & rook_occupancy_mask;
    let rook_moves =
        unsafe { tables.get_slider_move_mask_unchecked::<true>(sq_index as usize, rook_blockers) };

    let bishop_occupancy_mask = tables::Tables::LT_BISHOP_OCCUPANCY_MASKS[sq_index as usize];
    let bishop_blockers = full_board & bishop_occupancy_mask;
    let bishop_moves = unsafe {
        tables.get_slider_move_mask_unchecked::<false>(sq_index as usize, bishop_blockers)
    };

    attackers |= (rook_board | queen_board) & rook_moves;
    attackers |= (bishop_board | queen_board) & bishop_moves;

    attackers
}

#[cfg(test)]
mod tests {
    use crate::{
        engine::{
            chess::{self, MV_FLAG_CAP},
            chess_v2::{self, PieceIndex},
            tables,
        },
        util,
    };

    use super::*;

    fn see_test(fen: &'static str, mv_string: &'static str) -> Eval {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();

        assert!(board.load_fen(fen, &tables).is_ok());

        let mv = board.fix_move(util::create_move(mv_string));

        assert!(util::is_legal_slow(&tables, &board, mv));
        assert!(mv & MV_FLAG_CAP != 0);

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

        static_exchange_eval(&tables, &board, black_board, white_board, piece_boards, mv)
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
    fn test_see_multi() {
        let qxp = "b3b6";
        let see_score = see_test("7k/p7/1p5R/8/8/1Q6/8/7K w - - 0 1", qxp);
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
        let qxp = "b1b6";
        let see_score = see_test("7k/3n4/Rq6/8/8/8/8/1Q4K1 w - - 0 1", qxp);
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
    fn test_see_multi_early_stop() {
        let qxp = "b3b6";
        let see_score = see_test("7k/p2n4/1p5R/8/8/1Q6/8/7K w - - 0 1", qxp);
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
        let qxp = "b3b7";
        let see_score = see_test("7k/1q6/3n4/8/8/1Q6/8/1R4K1 w - - 0 1", qxp);
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
        let qxp = "b4b6";
        let see_score = see_test("n6k/3n4/1q6/8/1Q6/1R6/1R6/1R4K1 w - - 0 1", qxp);
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
}
