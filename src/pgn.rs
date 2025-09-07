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
}

pub fn parse_pgn<'a>(
    move_str: &'a str,
    board: &mut chess_v2::ChessGame,
    tables: &tables::Tables,
) -> anyhow::Result<Vec<u16>> {
    let mut moves = Vec::new();
    let mut pgn_state = PgnState::MoveNumber;

    for move_part in move_str.split_whitespace() {
        match (pgn_state, move_part) {
            (PgnState::MoveNumber, part) => {
                if !part.contains('.') {
                    return Err(anyhow::anyhow!("Expected move number in opening: {}", part));
                }
                if part.contains("...") {
                    return Err(anyhow::anyhow!(
                        "Opening move should not start with black (e.g ...): {}",
                        part
                    ));
                }
                pgn_state = PgnState::Move(Side::White, false);
            }
            (PgnState::Move(side, both_moves), part) => {
                let mut move_list = [0u16; 256];

                let move_list = (0..board.gen_moves_avx512::<false>(&tables, &mut move_list))
                    .map(|mv_index| -> Vec<u16> {
                        let mv = move_list[mv_index];

                        if (mv & chess_v2::MV_FLAGS_PR_MASK) != chess_v2::MV_FLAGS_PR_QUEEN {
                            return vec![mv];
                        }

                        let mv_unpromoted = mv & !chess_v2::MV_FLAGS_PR_MASK;

                        vec![
                            mv_unpromoted | chess_v2::MV_FLAGS_PR_QUEEN,
                            mv_unpromoted | chess_v2::MV_FLAGS_PR_ROOK,
                            mv_unpromoted | chess_v2::MV_FLAGS_PR_BISHOP,
                            mv_unpromoted | chess_v2::MV_FLAGS_PR_KNIGHT,
                        ]
                    })
                    .flatten()
                    .collect::<Vec<_>>();
                let move_count = move_list.len();

                let pseudolegal_moves = match part {
                    "O-O" | "O-O-O" => {
                        let found_moves = (0..move_count)
                            .filter_map(|mv_index| {
                                let mv = move_list[mv_index];

                                let castle_flag = match part {
                                    "O-O" => chess_v2::MV_FLAGS_CASTLE_KING,
                                    "O-O-O" => chess_v2::MV_FLAGS_CASTLE_QUEEN,
                                    _ => unreachable!(),
                                };

                                if mv & chess_v2::MV_FLAGS == castle_flag {
                                    Some(mv)
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>();

                        found_moves
                    }
                    _ => {
                        let mut dst_file: Option<u8> = None;
                        let mut src_file: Option<u8> = None;
                        let mut src_rank: Option<u8> = None;
                        let mut dst_rank: Option<u8> = None;
                        let mut piece = chess_v2::PieceIndex::WhitePawn;
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
                                'N' | 'B' | 'R' | 'Q' | 'K' => {
                                    piece = match move_part {
                                        'N' => chess_v2::PieceIndex::WhiteKnight,
                                        'B' => chess_v2::PieceIndex::WhiteBishop,
                                        'R' => chess_v2::PieceIndex::WhiteRook,
                                        'Q' => chess_v2::PieceIndex::WhiteQueen,
                                        'K' => chess_v2::PieceIndex::WhiteKing,
                                        'P' => chess_v2::PieceIndex::WhitePawn,
                                        _ => {
                                            return Err(anyhow::anyhow!(
                                                "Invalid piece in opening: {}",
                                                move_part
                                            ));
                                        }
                                    };
                                }
                                c => {
                                    return Err(anyhow::anyhow!(
                                        "Invalid character '{}' in opening move: {}",
                                        c,
                                        move_part
                                    ));
                                }
                            }
                        }

                        if !dst_file.is_some() {
                            return Err(anyhow::anyhow!(
                                "Destination file must be specified in an opening move: {}",
                                part
                            ));
                        }

                        if !dst_rank.is_some() {
                            return Err(anyhow::anyhow!(
                                "Destination rank must be specified in an opening move: {}",
                                part
                            ));
                        }

                        // Fix piece for side
                        piece = chess_v2::PieceIndex::from((piece as usize) + (side as usize * 8));

                        let found_moves = (0..move_count)
                            .filter_map(|mv_index| {
                                let mv = move_list[mv_index];

                                let src_sq = (mv & 0x3F) as u8;
                                let dst_sq = ((mv >> 6) & 0x3F) as u8;

                                if let Some(src_file) = src_file {
                                    if src_sq % 8 != src_file {
                                        return None;
                                    }
                                }

                                if let Some(src_rank) = src_rank {
                                    if src_sq / 8 != src_rank {
                                        return None;
                                    }
                                }

                                if dst_sq % 8 != dst_file.unwrap() {
                                    return None;
                                }

                                if dst_sq / 8 != dst_rank.unwrap() {
                                    return None;
                                }

                                if is_capture
                                    && (mv & chess_v2::MV_FLAG_EPCAP) != chess_v2::MV_FLAG_EPCAP
                                    && board.piece_at(dst_sq) == PieceIndex::WhiteNullPiece as usize
                                {
                                    return None;
                                }

                                let sq_piece = board.piece_at(src_sq);

                                if sq_piece != piece as usize {
                                    return None;
                                }

                                Some(mv)
                            })
                            .collect::<Vec<_>>();

                        found_moves
                    }
                };

                let legal_moves = pseudolegal_moves
                    .iter()
                    .filter(|&&mv| {
                        let mut board_copy = board.clone();

                        if unsafe { !board_copy.make_move(mv, tables) } {
                            return false;
                        }

                        if board_copy.in_check(tables, !board_copy.b_move()) {
                            return false;
                        }

                        true
                    })
                    .cloned()
                    .collect::<Vec<_>>();

                if legal_moves.len() != 1 {
                    return Err(anyhow::anyhow!(
                        "Expected exactly one move for opening part: {}, found: {} ({})",
                        part,
                        legal_moves.len(),
                        legal_moves
                            .iter()
                            .map(|mv| util::move_string(*mv))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                }

                let move_to_make = legal_moves[0];

                moves.push(move_to_make);

                if unsafe { !board.make_move(move_to_make, &tables) } {
                    return Err(anyhow::anyhow!(
                        "Invalid move in opening: {}, move: {}",
                        part,
                        util::move_string(move_to_make)
                    ));
                }

                if board.in_check(&tables, !board.b_move()) {
                    return Err(anyhow::anyhow!(
                        "Opening move {} leaves the king in check",
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

    Ok(moves)
}

#[cfg(test)]
mod tests {
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
        let moves = super::parse_pgn(pgn, &mut board, &tables).unwrap();

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
        assert_eq!(
            super::parse_pgn(pgn, &mut board, &tables)
                .unwrap_err()
                .to_string(),
            "Expected exactly one move for opening part: Nf7, found: 0 ()"
        );
    }
}
