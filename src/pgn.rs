use crate::{
    engine::{
        chess_v2::{self, PieceIndex},
        tables,
    },
    util::{self, Side},
};

#[derive(Debug, Clone, Copy)]
enum PgnState {
    MoveNumber,
    Move(Side, bool),
    Comment,
}

pub fn parse_pgn<'a>(
    move_str: &'a str,
    board: &mut chess_v2::ChessGame,
    tables: &tables::Tables,
    moves_out: &mut Vec<u16>,
) -> anyhow::Result<()> {
    let mut pgn_state = PgnState::MoveNumber;

    let mut it = move_str.split_whitespace();

    loop {
        let move_part = match it.next() {
            Some(part) => part,
            None => break,
        };

        match (pgn_state, move_part) {
            (PgnState::Comment, "}") => pgn_state = PgnState::MoveNumber,
            (PgnState::Comment, _) => continue,
            (PgnState::MoveNumber, part) => {
                match part {
                    "1-0" | "0-1" | "1/2-1/2" => break,
                    _ => {}
                }
                if part.starts_with('{') {
                    pgn_state = PgnState::Comment;
                    continue;
                }
                if !part.contains('.') {
                    return Err(anyhow::anyhow!("Expected move number in pgn: {}", part));
                }

                let is_black_continuation = part.contains("...");

                if !board.b_move() && is_black_continuation {
                    return Err(anyhow::anyhow!(
                        "Pgn should not start with a black move: {}",
                        part
                    ));
                }
                pgn_state = PgnState::Move(Side::from(board.b_move()), is_black_continuation);

                if !part.ends_with('.') {
                    let next_part_start_index = part.rfind('.').unwrap() + 1;

                    let offset =
                        part.as_ptr() as usize - move_str.as_ptr() as usize + next_part_start_index;

                    it = move_str[offset..].split_whitespace();
                }
            }
            (PgnState::Move(side, both_moves), part) => {
                let mut legal_moves = [0u16; 218];
                let mut legal_moves_count = 0;
                {
                    let board_copy = board.clone();
                    let mut move_list = [0u16; 256];
                    for mv_index in 0..board.gen_moves_avx512::<false>(&tables, &mut move_list) {
                        let mv = move_list[mv_index];

                        let is_legal = unsafe { board.make_move(mv, tables, None) }
                            && !board.in_check(tables, !board.b_move());

                        *board = board_copy;

                        if is_legal {
                            if (mv & chess_v2::MV_FLAGS_PR_MASK) != chess_v2::MV_FLAGS_PR_QUEEN {
                                legal_moves[legal_moves_count] = mv;
                                legal_moves_count += 1;
                            } else {
                                let mv_unpromoted = mv & !chess_v2::MV_FLAGS_PR_MASK;
                                legal_moves[legal_moves_count] =
                                    mv_unpromoted | chess_v2::MV_FLAGS_PR_QUEEN;
                                legal_moves_count += 1;
                                legal_moves[legal_moves_count] =
                                    mv_unpromoted | chess_v2::MV_FLAGS_PR_ROOK;
                                legal_moves_count += 1;
                                legal_moves[legal_moves_count] =
                                    mv_unpromoted | chess_v2::MV_FLAGS_PR_BISHOP;
                                legal_moves_count += 1;
                                legal_moves[legal_moves_count] =
                                    mv_unpromoted | chess_v2::MV_FLAGS_PR_KNIGHT;
                                legal_moves_count += 1;
                            }
                        }
                    }
                }

                let part_len = part.len();

                let castle_queen = part_len > 4 && &part[0..5] == "O-O-O";
                let castle_king = part_len > 2 && &part[0..3] == "O-O";

                let move_to_make = if castle_king || castle_queen {
                    let mv_index = (0..legal_moves_count).find(|&mv_index| {
                        let mv = legal_moves[mv_index];

                        let castle_flag = if castle_queen {
                            chess_v2::MV_FLAGS_CASTLE_QUEEN
                        } else {
                            chess_v2::MV_FLAGS_CASTLE_KING
                        };

                        if mv & chess_v2::MV_FLAGS == castle_flag {
                            return true;
                        }

                        false
                    });

                    match mv_index {
                        Some(mv_index) => legal_moves[mv_index],
                        None => {
                            return Err(anyhow::anyhow!(
                                "No legal castling move found for pgn move: {}",
                                part
                            ));
                        }
                    }
                } else {
                    match part {
                        "{" => {
                            pgn_state = PgnState::Comment;
                            continue;
                        }
                        "1-0" | "0-1" | "1/2-1/2" => break,
                        _ => {
                            let mut dst_file: Option<u8> = None;
                            let mut src_file: Option<u8> = None;
                            let mut src_rank: Option<u8> = None;
                            let mut dst_rank: Option<u8> = None;
                            let mut piece = chess_v2::PieceIndex::WhitePawn;
                            let mut promote_to = None;
                            let mut is_capture = false;
                            let mut is_checkmate = false;
                            let mut is_check = false;

                            for move_part in part.chars() {
                                match move_part {
                                    'a'..='h' => {
                                        if let Some(dst_file) = dst_file {
                                            src_file = Some(dst_file);
                                        }
                                        dst_file = Some(move_part as u8 - b'a');
                                    }
                                    'x' => is_capture = true,
                                    '+' => is_check = true,
                                    '#' => {
                                        is_checkmate = true;
                                        is_check = true;
                                    }
                                    '1'..='8' => {
                                        if let Some(dst_rank) = dst_rank {
                                            src_rank = Some(dst_rank);
                                        }
                                        dst_rank = Some(move_part as u8 - b'1');
                                    }
                                    'N' | 'B' | 'R' | 'Q' | 'K' if dst_file.is_none() => {
                                        piece = match move_part {
                                            'N' => chess_v2::PieceIndex::WhiteKnight,
                                            'B' => chess_v2::PieceIndex::WhiteBishop,
                                            'R' => chess_v2::PieceIndex::WhiteRook,
                                            'Q' => chess_v2::PieceIndex::WhiteQueen,
                                            'K' => chess_v2::PieceIndex::WhiteKing,
                                            'P' => chess_v2::PieceIndex::WhitePawn,
                                            _ => {
                                                return Err(anyhow::anyhow!(
                                                    "Invalid piece in pgn move: {}",
                                                    move_part
                                                ));
                                            }
                                        };
                                    }
                                    'Q' | 'R' | 'B' | 'N' if dst_file.is_some() => {
                                        if promote_to.is_some() {
                                            return Err(anyhow::anyhow!(
                                                "Multiple promotion pieces in pgn move: {}",
                                                move_part
                                            ));
                                        }

                                        if piece != chess_v2::PieceIndex::WhitePawn {
                                            return Err(anyhow::anyhow!(
                                                "Non-pawn promotion in pgn move: {}",
                                                move_part
                                            ));
                                        }

                                        match (side, dst_rank) {
                                            (Side::White, Some(7)) => {}
                                            (Side::Black, Some(0)) => {}
                                            _ => {
                                                return Err(anyhow::anyhow!(
                                                    "Invalid promotion rank in pgn move: {} (side={:?})",
                                                    move_part,
                                                    side
                                                ));
                                            }
                                        }

                                        promote_to = Some(match move_part {
                                            'Q' => chess_v2::PieceIndex::WhiteQueen,
                                            'R' => chess_v2::PieceIndex::WhiteRook,
                                            'B' => chess_v2::PieceIndex::WhiteBishop,
                                            'N' => chess_v2::PieceIndex::WhiteKnight,
                                            _ => unreachable!(),
                                        });
                                    }
                                    '?' | '!' => { /* Ignore annotations */ }
                                    '=' | '(' | ')' | '/' => { /* Separators for promotions */ }
                                    c => {
                                        return Err(anyhow::anyhow!(
                                            "Invalid character '{}' in pgn move: {}",
                                            c,
                                            move_part
                                        ));
                                    }
                                }
                            }

                            if !dst_file.is_some() {
                                return Err(anyhow::anyhow!(
                                    "Destination file must be specified in an pgn move: {}",
                                    part
                                ));
                            }

                            if !dst_rank.is_some() {
                                return Err(anyhow::anyhow!(
                                    "Destination rank must be specified in an pgn move: {}",
                                    part
                                ));
                            }

                            // Fix piece for side
                            piece =
                                chess_v2::PieceIndex::from((piece as usize) + (side as usize * 8));

                            let mut found_move = None;
                            for mv_index in 0..legal_moves_count {
                                let mv = legal_moves[mv_index];

                                let src_sq = (mv & 0x3F) as u8;
                                let dst_sq = ((mv >> 6) & 0x3F) as u8;

                                if let Some(src_file) = src_file {
                                    if src_sq % 8 != src_file {
                                        continue;
                                    }
                                }

                                if let Some(src_rank) = src_rank {
                                    if src_sq / 8 != src_rank {
                                        continue;
                                    }
                                }

                                if dst_sq % 8 != dst_file.unwrap() {
                                    continue;
                                }

                                if dst_sq / 8 != dst_rank.unwrap() {
                                    continue;
                                }

                                if is_capture
                                    && (mv & chess_v2::MV_FLAG_EPCAP) != chess_v2::MV_FLAG_EPCAP
                                    && board.piece_at(dst_sq) == PieceIndex::WhiteNullPiece as usize
                                {
                                    continue;
                                }

                                let sq_piece = board.piece_at(src_sq);

                                if sq_piece != piece as usize {
                                    continue;
                                }

                                if let Some(promote_to) = promote_to {
                                    match (promote_to, mv & chess_v2::MV_FLAGS_PR_MASK) {
                                        (
                                            chess_v2::PieceIndex::WhiteQueen,
                                            chess_v2::MV_FLAGS_PR_QUEEN,
                                        )
                                        | (
                                            chess_v2::PieceIndex::WhiteRook,
                                            chess_v2::MV_FLAGS_PR_ROOK,
                                        )
                                        | (
                                            chess_v2::PieceIndex::WhiteBishop,
                                            chess_v2::MV_FLAGS_PR_BISHOP,
                                        )
                                        | (
                                            chess_v2::PieceIndex::WhiteKnight,
                                            chess_v2::MV_FLAGS_PR_KNIGHT,
                                        ) => {}
                                        _ => {
                                            continue;
                                        }
                                    }
                                }

                                match found_move {
                                    Some(existing) => {
                                        return Err(anyhow::anyhow!(
                                            "Expected exactly one move for opening part: {}, found multiple: {} and {}",
                                            part,
                                            util::move_string(existing),
                                            util::move_string(mv),
                                        ));
                                    }
                                    None => {
                                        found_move = Some(mv);
                                    }
                                }
                            }

                            match found_move {
                                Some(mv) => mv,
                                None => {
                                    return Err(anyhow::anyhow!(
                                        "Expected exactly one move for opening part: {}, found none",
                                        part,
                                    ));
                                }
                            }
                        }
                    }
                };

                moves_out.push(move_to_make);

                if unsafe { !board.make_move(move_to_make, &tables, None) } {
                    return Err(anyhow::anyhow!(
                        "Invalid move in pgn: {}, move: {}",
                        part,
                        util::move_string(move_to_make)
                    ));
                }

                if board.in_check(&tables, !board.b_move()) {
                    return Err(anyhow::anyhow!(
                        "Pgn move {} leaves the king in check",
                        part
                    ));
                }

                pgn_state = if both_moves {
                    PgnState::MoveNumber
                } else if side == Side::White {
                    PgnState::Move(Side::Black, true)
                } else {
                    PgnState::Move(Side::White, false)
                };
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use sfbinpack::chess::r#move;

    use crate::{
        engine::{chess_v2, tables},
        util,
    };

    #[test]
    fn test_parse_pgn() {
        let mut board = chess_v2::ChessGame::new();
        assert!(
            board
                .load_fen(util::FEN_STARTPOS, &tables::Tables::new())
                .is_ok()
        );
        let tables = tables::Tables::new();

        let pgn = "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7";
        let mut moves = Vec::new();
        assert!(super::parse_pgn(pgn, &mut board, &tables, &mut moves).is_ok());

        assert_eq!(moves.len(), 20);
        assert_eq!(
            board.gen_fen(),
            "r1bq1rk1/2pnbppp/p2p1n2/1p2p3/3PP3/1BP2N1P/PP3PP1/RNBQR1K1 w - - 1 11"
        );
    }

    #[test]
    fn test_parse_pgn_invalid_move() {
        let mut board = chess_v2::ChessGame::new();
        assert!(
            board
                .load_fen(util::FEN_STARTPOS, &tables::Tables::new())
                .is_ok()
        );
        let tables = tables::Tables::new();

        let pgn = "1. e4 e5 2. Nf7 Nc6";
        let mut moves = Vec::new();
        assert_eq!(
            super::parse_pgn(pgn, &mut board, &tables, &mut moves)
                .unwrap_err()
                .to_string(),
            "Expected exactly one move for opening part: Nf7, found none"
        );
    }

    #[test]
    fn test_parse_pgn_termination() {
        let mut board = chess_v2::ChessGame::new();
        assert!(
            board
                .load_fen(util::FEN_STARTPOS, &tables::Tables::new())
                .is_ok()
        );
        let tables = tables::Tables::new();

        let pgns = [
            ("1. e4 1-0", 1),
            ("1. e4 0-1", 1),
            ("1. e4 1/2-1/2", 1),
            ("1-0", 0),
            ("0-1", 0),
            ("1/2-1/2", 0),
        ];

        let mut moves = Vec::new();
        for pgn in pgns {
            moves.clear();
            let result = super::parse_pgn(pgn.0, &mut board.clone(), &tables, &mut moves);
            assert!(
                result.is_ok(),
                "Failed to parse PGN: {}: {}",
                pgn.0,
                result.err().unwrap()
            );
            assert_eq!(moves.len(), pgn.1);
        }
    }

    #[test]
    fn test_parse_pgn_comments() {
        let mut board = chess_v2::ChessGame::new();
        assert!(
            board
                .load_fen(util::FEN_STARTPOS, &tables::Tables::new())
                .is_ok()
        );
        let tables = tables::Tables::new();

        let pgns = [(
            "1. e4 { [%eval 0.10] } 1... e6 { [%eval 0.30] } 2. d4 { [%eval 0.42] } 2... d5 { [%eval 0.0] } 3. Nd2 { [%eval #-1337] }",
            5,
        )];

        let mut moves = Vec::new();
        for pgn in pgns {
            moves.clear();
            let result = super::parse_pgn(pgn.0, &mut board.clone(), &tables, &mut moves);
            assert!(
                result.is_ok(),
                "Failed to parse PGN: {}: {}",
                pgn.0,
                result.err().unwrap()
            );
            assert_eq!(moves.len(), pgn.1);
        }
    }

    #[test]
    fn test_parse_pgn_annotations() {
        let mut board = chess_v2::ChessGame::new();
        assert!(
            board
                .load_fen(util::FEN_STARTPOS, &tables::Tables::new())
                .is_ok()
        );
        let tables = tables::Tables::new();

        let pgns = [(
            "1. d4! { [%eval 0.10] } 1... e6!! { [%eval 0.10] } 2. e4? { [%eval 0.10] } 2... d5?? { [%eval 0.10] } 3. Nd2?! { [%eval 0.10] } 3... c5!? { [%eval 0.10] }",
            6,
        )];

        let mut moves = Vec::new();
        for pgn in pgns {
            moves.clear();
            let result = super::parse_pgn(pgn.0, &mut board.clone(), &tables, &mut moves);
            assert!(
                result.is_ok(),
                "Failed to parse PGN: {}: {}",
                pgn.0,
                result.err().unwrap()
            );
            assert_eq!(moves.len(), pgn.1);
        }
    }

    #[test]
    fn test_parse_pgn_e2e() {
        let mut board = chess_v2::ChessGame::new();
        assert!(
            board
                .load_fen(util::FEN_STARTPOS, &tables::Tables::new())
                .is_ok()
        );
        let tables = tables::Tables::new();

        let pgns = [(
            "
1.e4 e5 2.Nf3 Nf6 3.Nc3 Nc6 4.Bb5 Bb4 5.O-O O-O 6.d3 Bxc3 7.bxc3 d6 8.Re1 Bd7
9.Rb1 a6 10.Bxc6 Bxc6 11.Bg5 h6 12.Bh4 b6 13.Nd2 g5 14.Bg3 Ne8 15.d4 f6 16.c4 Ng7
17.c3 Qe8 18.f3 Qg6 19.Nf1 g4 20.fxg4 Bxe4 21.Rb2 Rae8 22.Rf2 exd4 23.cxd4 h5
24.gxh5 Nxh5 25.Re3 Bc6 26.Rc3 Re4 27.c5 bxc5 28.dxc5 d5 29.Bxc7 d4 30.Rd3 Rg4
31.Ng3 Re8 32.Rxd4 Rxd4 33.Qxd4 Nxg3 34.Bxg3 Re1+ 35.Rf1 Rxf1+ 36.Kxf1 Qb1+
37.Be1 Bb5+ 38.Kf2 Qxa2+ 39.Kg3 Kf7 40.Bc3 Qe6 41.Qf4 Bc6 42.h4 a5 43.Qc7+ Bd7
44.Qf4 a4 45.h5 a3 46.Kh2 a2 47.Qh4 Qe7 48.Qf2 Qe6 49.Qc2 Qf5 50.Qxf5 Bxf5
51.c6 Be4 52.c7 Bf5 53.Kg3 Bc8 54.Kf4 Bb7 55.Ba1 Bc8 56.g3 Bb7 57.g4 Ba6
58.Kg3 Bc8 59.Kh4 Ba6 60.Bb2 Bb7 61.h6 Kg6 62.h7 Kxh7 63.Kh5 Bc8 64.Bxf6 Kg8
65.Ba1 Kf7 66.g5 Ke8 67.g6 Be6 68.g7 Kd7 69.Be5 a1=B 70.Bxa1 Kxc7 71.g8=Q Bxg8  1/2-1/2
",
            142,
        )];

        let mut moves = Vec::new();
        for pgn in pgns {
            moves.clear();
            let b = &mut board.clone();
            let result = super::parse_pgn(pgn.0, b, &tables, &mut moves);
            assert!(
                result.is_ok(),
                "Failed to parse PGN: {}: {}",
                pgn.0,
                result.err().unwrap()
            );
            assert_eq!(moves.len(), pgn.1);
            assert_eq!(b.gen_fen(), "6b1/2k5/8/7K/8/8/8/B7 w - - 0 72");
        }
    }
}
