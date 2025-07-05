use crate::{constant::PieceId, engine::tables::Tables};
use std::arch::x86_64::*;

#[repr(C)]
pub struct Bitboards {
    pub bitboards: [u64; 12],
}

pub struct Board {
    pub board: Bitboards,
    pub b_move: bool,
    pub castles: [bool; 4], // [white kingside, white queenside, black kingside, black queenside]
    pub en_passant: Option<u8>,
    pub half_moves: u32,
    pub full_moves: u32,
}

impl Board {
    pub fn new() -> Self {
        Self {
            board: Bitboards { bitboards: [0; 12] },
            b_move: false,
            castles: [true, true, true, true],
            en_passant: None,
            half_moves: 0,
            full_moves: 1,
        }
    }

    pub fn gen_moves_slow(&self, move_list: &mut [u16; 256]) -> usize {
        // @perf - total rewrite
        let mut move_cursor = 0;
        let side_cursor = 6 * self.b_move as usize;
        let bitboards = &self.board.bitboards[side_cursor..side_cursor + 6];
        let friendly_board = bitboards
            .iter()
            .copied()
            .reduce(|acc, curr| acc | curr)
            .unwrap();

        for square in 0..64 {
            let sq_bit: u64 = 1 << square;

            // Only friendly pieces
            if friendly_board & sq_bit == 0 {
                continue;
            }

            let piece_at_square = self.piece_at_slow(sq_bit);

            // Skip if no piece
            if piece_at_square == 0 {
                continue;
            }

            match PieceId::from(piece_at_square - 1) {
                PieceId::WhiteKing | PieceId::BlackKing => {
                    let mut move_mask = Tables::LT_KING_MOVE_MASKS[square] & !friendly_board;

                    loop {
                        if move_mask == 0 {
                            break;
                        }

                        let tz = move_mask.trailing_zeros();

                        move_list[move_cursor] =
                            (((tz & 0x3F) as u16) << 6) | (square & 0x3F) as u16;

                        move_mask &= !(1 << tz);

                        move_cursor += 1;
                    }
                }
                _ => {}
            }
        }

        move_cursor
    }

    // Returns 0 if there is no piece, PieceId+1 otherwise
    pub fn piece_at_slow(&self, sq_bit: u64) -> usize {
        for i in PieceId::WhiteKing as usize..PieceId::PieceMax as usize {
            if self.board.bitboards[i] & sq_bit != 0 {
                return i + 1;
            }
        }

        0
    }

    // Returns 0 if there is no piece, PieceId+1 otherwise
    // Cost estimate: 15 cycles on Skylake
    #[inline(always)]
    fn piece_at_avx512(&mut self, sq_bit: u64) -> usize {
        unsafe {
            let select_vec = _mm512_set1_epi64(sq_bit as i64);
            let index_vec = _mm512_set_epi32(12, 11, 10, 9, 8, 7, 6, 5, 8, 7, 6, 5, 4, 3, 2, 1);

            let white_vec = _mm512_loadu_si512(self.board.bitboards.as_ptr() as *const _);
            let rest_vec = _mm512_loadu_si512(self.board.bitboards.as_ptr().add(4) as *const _);

            let from_mask_white = _mm512_test_epi64_mask(white_vec, select_vec) as u16;
            let from_mask_rest = _mm512_test_epi64_mask(rest_vec, select_vec) as u16;

            let piece_vec = _mm512_mask_compress_epi32(
                _mm512_setzero_si512(),
                _mm512_kunpackb(from_mask_rest, from_mask_white),
                index_vec,
            );
            let piece_idx = _mm_cvtsi128_si32(_mm512_castsi512_si128(piece_vec)) as usize;

            piece_idx
        }
    }

    pub fn move_piece_slow(&mut self, mv: u16) {
        let from_bit: u64 = 1 << (mv & 0x3F);
        let to_bit: u64 = 1 << ((mv >> 6) & 0x3F);

        let from_piece = self.piece_at_slow(from_bit);
        let to_piece = self.piece_at_slow(to_bit);

        println!(
            "From piece: {:?}, To piece: {:?}",
            PieceId::from(from_piece - 1),
            to_piece
        );

        if to_piece != 0 {
            self.board.bitboards[to_piece - 1] &= !to_bit;
        }
        self.board.bitboards[from_piece - 1] ^= to_bit | from_bit;

        self.b_move = !self.b_move;

        // debug_assert!(self.is_valid(), "Board is invalid after move");
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
                        ('r', true) => {
                            self.board.bitboards[PieceId::BlackRook as usize] |= 1 << pos
                        }
                        ('n', true) => {
                            self.board.bitboards[PieceId::BlackKnight as usize] |= 1 << pos
                        }
                        ('b', true) => {
                            self.board.bitboards[PieceId::BlackBishop as usize] |= 1 << pos
                        }
                        ('q', true) => {
                            self.board.bitboards[PieceId::BlackQueen as usize] |= 1 << pos
                        }
                        ('k', true) => {
                            self.board.bitboards[PieceId::BlackKing as usize] |= 1 << pos
                        }
                        ('p', true) => {
                            self.board.bitboards[PieceId::BlackPawn as usize] |= 1 << pos
                        }
                        ('R', true) => {
                            self.board.bitboards[PieceId::WhiteRook as usize] |= 1 << pos
                        }
                        ('N', true) => {
                            self.board.bitboards[PieceId::WhiteKnight as usize] |= 1 << pos
                        }
                        ('B', true) => {
                            self.board.bitboards[PieceId::WhiteBishop as usize] |= 1 << pos
                        }
                        ('Q', true) => {
                            self.board.bitboards[PieceId::WhiteQueen as usize] |= 1 << pos
                        }
                        ('K', true) => {
                            self.board.bitboards[PieceId::WhiteKing as usize] |= 1 << pos
                        }
                        ('P', true) => {
                            self.board.bitboards[PieceId::WhitePawn as usize] |= 1 << pos
                        }
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
                    'w' => self.b_move = false,
                    'b' => self.b_move = true,
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

    pub fn is_valid(&self) -> bool {
        if self.board.bitboards[PieceId::WhiteKing as usize].count_ones() != 1
            || self.board.bitboards[PieceId::BlackKing as usize].count_ones() != 1
        {
            return false;
        }

        for bit_pos in 0..64 {
            let mut sum = 0;
            let bit = 1 << bit_pos;

            for piece_id in PieceId::WhiteKing as usize..PieceId::PieceMax as usize {
                sum += if self.board.bitboards[piece_id] & bit != 0 {
                    1
                } else {
                    0
                };
            }

            if sum > 1 {
                return false;
            }
        }

        true
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_load_fen_start() {
        let mut board = Board::new();
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        assert!(board.load_fen(fen).is_ok());
        assert!(board.is_valid());

        let piece_positions = [
            0x0000000000000010, // w_king
            0x0000000000000008, // w_queen
            0x0000000000000081, // w_rook
            0x0000000000000024, // w_bishop
            0x0000000000000042, // w_knight
            0x000000000000FF00, // w_pawn
            0x1000000000000000, // b_king
            0x0800000000000000, // b_queen
            0x8100000000000000, // b_rook
            0x2400000000000000, // b_bishop
            0x4200000000000000, // b_knight
            0x00FF000000000000, // b_pawn
        ];

        for (i, &pos) in piece_positions.iter().enumerate() {
            assert_eq!(
                board.board.bitboards[i],
                pos,
                "Piece {:?} does not match expected position",
                PieceId::from(i)
            );
        }

        assert!(!board.b_move);
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
        assert!(board.is_valid());

        let piece_positions = [
            0x0000000000000010, // w_king
            0x0000000000000008, // w_queen
            0x0000000000000081, // w_rook
            0x0000000000000024, // w_bishop
            0x0000000000000042, // w_knight
            0x000000001000EF00, // w_pawn
            0x1000000000000000, // b_king
            0x0800000000000000, // b_queen
            0x8100000000000000, // b_rook
            0x2400000000000000, // b_bishop
            0x4200000000000000, // b_knight
            0x00FF000000000000, // b_pawn
        ];

        for (i, &pos) in piece_positions.iter().enumerate() {
            assert_eq!(
                board.board.bitboards[i],
                pos,
                "Piece {:?} does not match expected position",
                PieceId::from(i)
            );
        }

        assert_eq!(board.b_move, true);
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
        assert!(board.is_valid());

        let piece_positions = [
            0x0000000000008000, // w_king
            0x4000000000000000, // w_queen
            0x0000000000000000, // w_rook
            0x0000000000000000, // w_bishop
            0x0000400000000000, // w_knight
            0x0040008000000000, // w_pawn
            0x0000020000000000, // b_king
            0x0000000000000004, // b_queen
            0x0000000020000000, // b_rook
            0x0000000000020021, // b_bishop
            0x0000000400000400, // b_knight
            0x0005081100000000, // b_pawn
        ];

        for (i, &pos) in piece_positions.iter().enumerate() {
            assert_eq!(
                board.board.bitboards[i],
                pos,
                "Piece {:?} does not match expected position",
                PieceId::from(i)
            );
        }

        assert_eq!(board.b_move, true);
        assert_eq!(board.castles, [false, false, false, false]);
        assert_eq!(board.en_passant, None);
        assert_eq!(board.half_moves, 29);
        assert_eq!(board.full_moves, 30);
    }

    #[test]
    fn test_board_validity_dup() {
        let mut board = Board::new();
        board.reset();

        board.board.bitboards[PieceId::WhiteKing as usize] = 0b11;

        assert!(
            !board.is_valid(),
            "Board should be invalid with duplicate kings"
        );

        board.board.bitboards[PieceId::WhiteKing as usize] = 0b1;
        board.board.bitboards[PieceId::WhiteQueen as usize] = 0b1;

        assert!(
            !board.is_valid(),
            "Board should be invalid with duplicate queens"
        );
    }
}
