use crate::constant::PieceId;

pub struct Board {
    pub w_king: u64,
    pub w_queen: u64,
    pub w_rook: u64,
    pub w_bishop: u64,
    pub w_knight: u64,
    pub w_pawn: u64,

    pub b_king: u64,
    pub b_queen: u64,
    pub b_rook: u64,
    pub b_bishop: u64,
    pub b_knight: u64,
    pub b_pawn: u64,

    pub w_move: bool,
    pub castles: [bool; 4], // [white kingside, white queenside, black kingside, black queenside]
    pub en_passant: Option<u8>,
    pub half_moves: u32,
    pub full_moves: u32,
}

impl Board {
    pub fn new() -> Self {
        Self {
            w_king: 0,
            w_queen: 0,
            w_rook: 0,
            w_bishop: 0,
            w_knight: 0,
            w_pawn: 0,

            b_king: 0,
            b_queen: 0,
            b_rook: 0,
            b_bishop: 0,
            b_knight: 0,
            b_pawn: 0,

            w_move: true,
            castles: [true, true, true, true],
            en_passant: None,
            half_moves: 0,
            full_moves: 1,
        }
    }

    pub fn piece_at_slow(&self, rank: u8, file: u8) -> Option<PieceId> {
        if rank > 7 || file > 7 {
            return None;
        }

        let bit = 1 << (rank * 8 + file);

        match bit {
            _ if self.w_king & bit != 0 => Some(PieceId::WhiteKing),
            _ if self.w_queen & bit != 0 => Some(PieceId::WhiteQueen),
            _ if self.w_rook & bit != 0 => Some(PieceId::WhiteRook),
            _ if self.w_bishop & bit != 0 => Some(PieceId::WhiteBishop),
            _ if self.w_knight & bit != 0 => Some(PieceId::WhiteKnight),
            _ if self.w_pawn & bit != 0 => Some(PieceId::WhitePawn),
            _ if self.b_king & bit != 0 => Some(PieceId::BlackKing),
            _ if self.b_queen & bit != 0 => Some(PieceId::BlackQueen),
            _ if self.b_rook & bit != 0 => Some(PieceId::BlackRook),
            _ if self.b_bishop & bit != 0 => Some(PieceId::BlackBishop),
            _ if self.b_knight & bit != 0 => Some(PieceId::BlackKnight),
            _ if self.b_pawn & bit != 0 => Some(PieceId::BlackPawn),
            _ => None,
        }
    }

    pub fn load_fen(&mut self, fen: &str) -> Result<(), String> {
        self.reset();

        #[derive(Debug)]
        enum FenState {
            Placement,
            Turn,
            Castling,
            EnPassant,
            MovesHalf,
            MovesFull,
        }

        let mut rank = 7;
        let mut file = 0;
        let mut state = FenState::Placement;

        for c in fen.chars() {
            match state {
                FenState::Placement => {
                    let pos = (rank * 8) + file;

                    if rank == 0 && file == 8 {
                        if c != ' ' {
                            return Err(format!(
                                "Expected space at end of placements, got '{}' (rank: {}, file: {})",
                                c, rank, file
                            ));
                        }
                        state = FenState::Turn;
                        continue;
                    }

                    match (c, file < 8) {
                        ('r', true) => self.b_rook |= 1 << pos,
                        ('n', true) => self.b_knight |= 1 << pos,
                        ('b', true) => self.b_bishop |= 1 << pos,
                        ('q', true) => self.b_queen |= 1 << pos,
                        ('k', true) => self.b_king |= 1 << pos,
                        ('p', true) => self.b_pawn |= 1 << pos,
                        ('R', true) => self.w_rook |= 1 << pos,
                        ('N', true) => self.w_knight |= 1 << pos,
                        ('B', true) => self.w_bishop |= 1 << pos,
                        ('Q', true) => self.w_queen |= 1 << pos,
                        ('K', true) => self.w_king |= 1 << pos,
                        ('P', true) => self.w_pawn |= 1 << pos,
                        ('1'..='8', true) => {
                            let empty_squares = c.to_digit(10).unwrap();
                            file += empty_squares;
                            if file > 8 {
                                return Err("Invalid FEN: Too many files".into());
                            }
                            continue;
                        }
                        ('/', false) => {
                            if file != 8 {
                                return Err("Invalid FEN: Unexpected '/' character".into());
                            }
                            if rank == 0 {
                                return Err("Invalid FEN: Too many ranks".into());
                            }
                            rank -= 1;
                            file = 0;
                            continue;
                        }
                        _ => {
                            return Err(format!("Invalid character '{}' in FEN", c));
                        }
                    }
                    file += 1;
                }
                FenState::Turn => match c {
                    ' ' => {
                        state = FenState::Castling;
                        continue;
                    }
                    'w' => self.w_move = true,
                    'b' => self.w_move = false,
                    _ => return Err(format!("Invalid turn character '{}'", c)),
                },
                FenState::Castling => match c {
                    ' ' => {
                        state = FenState::EnPassant;
                        continue;
                    }
                    'K' => self.castles[0] = true,
                    'Q' => self.castles[1] = true,
                    'k' => self.castles[2] = true,
                    'q' => self.castles[3] = true,
                    '-' => continue,
                    _ => return Err(format!("Invalid castling character '{}'", c)),
                },
                FenState::EnPassant => match c {
                    '-' => {
                        self.en_passant = None;
                        continue;
                    }
                    'a'..='h' => {
                        let file_index = c as u8 - b'a';

                        if file_index > 7 {
                            return Err(format!("Invalid en passant file '{}'", c));
                        }

                        self.en_passant = Some(file_index);
                    }
                    '1'..='8' => {
                        if self.en_passant.is_none() {
                            return Err("En passant square specified without file".into());
                        }

                        let rank_index = c as u8 - b'1';

                        if rank_index > 7 {
                            return Err(format!("Invalid en passant rank '{}'", c));
                        }

                        if let Some(file_index) = self.en_passant {
                            self.en_passant = Some((rank_index * 8) | file_index);
                        } else {
                            return Err("En passant square specified without file".into());
                        }
                    }
                    ' ' => {
                        state = FenState::MovesHalf;
                        continue;
                    }
                    _ => return Err(format!("Invalid en passant character '{}'", c)),
                },
                FenState::MovesHalf => match c {
                    ' ' => {
                        state = FenState::MovesFull;
                        continue;
                    }
                    '0'..='9' => {
                        self.half_moves = self.half_moves * 10 + (c as u8 - b'0') as u32;
                    }
                    _ => return Err(format!("Invalid half move character '{}'", c)),
                },
                FenState::MovesFull => match c {
                    ' ' => break,
                    '0'..='9' => {
                        self.full_moves = self.full_moves * 10 + (c as u8 - b'0') as u32;
                    }
                    _ => return Err(format!("Invalid full move character '{}'", c)),
                },
            }
        }

        Ok(())
    }

    pub fn reset(&mut self) {
        *self = Self::new();
    }
}
