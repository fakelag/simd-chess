use crate::{
    engine::{chess, tables},
    util::{self, PieceId, Side},
};

#[derive(Debug, Clone, Copy)]
enum PgnState {
    MoveNumber,
    Move(Side, bool),
}

pub fn parse_pgn<'a>(
    parts: &mut impl Iterator<Item = &'a str>,
    board: &mut chess::ChessGame,
    tables: &tables::Tables,
) -> anyhow::Result<Vec<u16>> {
    let mut moves = Vec::new();
    let mut pgn_state = PgnState::MoveNumber;

    for move_part in parts
        .next()
        .expect("Missing moves in opening line")
        .split_whitespace()
    {
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
                let move_count = board.gen_moves_slow(&tables, &mut move_list);

                let pseudolegal_moves = match part {
                    "O-O" | "O-O-O" => {
                        let found_moves = (0..move_count)
                            .filter_map(|mv_index| {
                                let mv = move_list[mv_index];

                                let castle_flag = match part {
                                    "O-O" => chess::MV_FLAGS_CASTLE_KING,
                                    "O-O-O" => chess::MV_FLAGS_CASTLE_QUEEN,
                                    _ => unreachable!(),
                                };

                                if mv & chess::MV_FLAGS == castle_flag {
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
                        let mut piece = PieceId::WhitePawn;
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
                                        'N' => PieceId::WhiteKnight,
                                        'B' => PieceId::WhiteBishop,
                                        'R' => PieceId::WhiteRook,
                                        'Q' => PieceId::WhiteQueen,
                                        'K' => PieceId::WhiteKing,
                                        'P' => PieceId::WhitePawn,
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
                        piece = PieceId::from((piece as usize) + (side as usize * 6));

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
                                    && mv & chess::MV_FLAG_EPCAP != chess::MV_FLAG_EPCAP
                                    && board.piece_at_slow(1 << dst_sq) == 0
                                {
                                    return None;
                                }

                                let sq_piece = board.piece_at_slow(1 << src_sq);

                                let sq_piece = if sq_piece == 0 {
                                    return None;
                                } else {
                                    PieceId::from(sq_piece - 1)
                                };

                                if sq_piece != piece {
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

                        if !board_copy.make_move_slow(mv, tables) {
                            return false;
                        }

                        if board_copy.in_check_slow(tables, !board_copy.b_move) {
                            return false;
                        }

                        true
                    })
                    .cloned()
                    .collect::<Vec<_>>();

                if legal_moves.len() != 1 {
                    return Err(anyhow::anyhow!(
                        "Expected exactly one move for opening part: {}, found: {}",
                        part,
                        legal_moves
                            .iter()
                            .map(|mv| util::move_string(*mv))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                }

                let move_to_make = legal_moves[0];

                moves.push(move_to_make);

                if !board.make_move_slow(move_to_make, &tables) {
                    return Err(anyhow::anyhow!(
                        "Invalid move in opening: {}, move: {}",
                        part,
                        util::move_string(move_to_make)
                    ));
                }

                if board.in_check_slow(&tables, !board.b_move) {
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
