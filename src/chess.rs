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
                    '-' => self.castles = [false, false, false, false],
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
                        self.full_moves = 0;
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

mod tests {
    use super::*;

    #[test]
    fn test_load_fen_start() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        assert!(board.load_fen(fen).is_ok());
        assert_eq!(board.w_king, 0x0000000000000010);
        assert_eq!(board.b_king, 0x1000000000000000);
        assert_eq!(board.w_queen, 0x0000000000000008);
        assert_eq!(board.b_queen, 0x0800000000000000);
        assert_eq!(board.w_rook, 0x0000000000000081);
        assert_eq!(board.b_rook, 0x8100000000000000);
        assert_eq!(board.w_bishop, 0x0000000000000024);
        assert_eq!(board.b_bishop, 0x2400000000000000);
        assert_eq!(board.w_knight, 0x0000000000000042);
        assert_eq!(board.b_knight, 0x4200000000000000);
        assert_eq!(board.w_pawn, 0x000000000000FF00);
        assert_eq!(board.b_pawn, 0x00FF000000000000);
        assert!(board.w_move);
        assert_eq!(board.castles, [true, true, true, true]);
        assert!(board.en_passant.is_none());
        assert_eq!(board.half_moves, 0);
        assert_eq!(board.full_moves, 1);
    }

    #[test]
    fn test_load_fen_e4() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";
        assert!(board.load_fen(fen).is_ok());
        assert_eq!(board.w_king, 0x0000000000000010);
        assert_eq!(board.b_king, 0x1000000000000000);
        assert_eq!(board.w_queen, 0x0000000000000008);
        assert_eq!(board.b_queen, 0x0800000000000000);
        assert_eq!(board.w_rook, 0x0000000000000081);
        assert_eq!(board.b_rook, 0x8100000000000000);
        assert_eq!(board.w_bishop, 0x0000000000000024);
        assert_eq!(board.b_bishop, 0x2400000000000000);
        assert_eq!(board.w_knight, 0x0000000000000042);
        assert_eq!(board.b_knight, 0x4200000000000000);
        assert_eq!(board.w_pawn, 0x000000001000EF00);
        assert_eq!(board.b_pawn, 0x00FF000000000000);
        assert_eq!(board.w_move, false);
        assert_eq!(board.castles, [true, true, true, true]);
        assert_eq!(board.en_passant, Some(20));
        assert_eq!(board.half_moves, 0);
        assert_eq!(board.full_moves, 1);
    }

    #[test]
    fn test_load_fen_endgame() {
        let mut board = Board::new();
        let fen = "6Q1/p1p3P1/1k1p2N1/p1n1p2P/5r2/1b6/2n4K/b1q2b2 b - - 29 30";
        assert!(board.load_fen(fen).is_ok());
        assert_eq!(board.w_king, 0x0000000000008000);
        assert_eq!(board.b_king, 0x0000020000000000);
        assert_eq!(board.w_queen, 0x4000000000000000);
        assert_eq!(board.b_queen, 0x0000000000000004);
        assert_eq!(board.w_rook, 0x0000000000000000);
        assert_eq!(board.b_rook, 0x0000000020000000);
        assert_eq!(board.w_bishop, 0x0000000000000000);
        assert_eq!(board.b_bishop, 0x0000000000020021);
        assert_eq!(board.w_knight, 0x0000400000000000);
        assert_eq!(board.b_knight, 0x0000000400000400);
        assert_eq!(board.w_pawn, 0x0040008000000000);
        assert_eq!(board.b_pawn, 0x0005081100000000);
        assert_eq!(board.w_move, false);
        assert_eq!(board.castles, [false, false, false, false]);
        assert_eq!(board.en_passant, None);
        assert_eq!(board.half_moves, 29);
        assert_eq!(board.full_moves, 30);
    }
}
