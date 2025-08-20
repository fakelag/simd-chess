use crate::{
    engine::tables::Tables,
    pop_ls1b,
    util::{self, Align64},
};
use std::{arch::x86_64::*, hint::black_box};

pub const MV_FLAGS: u16 = 0b1111 << 12;
pub const MV_FLAG_PROMOTION: u16 = 0b1000 << 12;

pub const MV_FLAG_DPP: u16 = 0b0001 << 12;

// Exclusive capture flag
pub const MV_FLAG_CAP: u16 = 0b0100 << 12;
pub const MV_FLAG_EPCAP: u16 = MV_FLAG_CAP | (0b0001 << 12);

pub const MV_FLAGS_PR_KNIGHT: u16 = 0b1000 << 12;
pub const MV_FLAGS_PR_BISHOP: u16 = 0b1001 << 12;
pub const MV_FLAGS_PR_ROOK: u16 = 0b1010 << 12;
pub const MV_FLAGS_PR_QUEEN: u16 = 0b1011 << 12;
pub const MV_FLAGS_PR_MASK: u16 = 0b1011 << 12;

pub const MV_FLAGS_CASTLE_KING: u16 = 0b0010 << 12;
pub const MV_FLAGS_CASTLE_QUEEN: u16 = 0b0011 << 12;

const MATERIAL_QUEEN: i32 = 700;
const MATERIAL_ROOK: i32 = 350;
const MATERIAL_BISHOP: i32 = 210;
const MATERIAL_KNIGHT: i32 = 210;
const MATERIAL_PAWN: i32 = 70;
const MATERIAL_TABLE: [u16; 16] = [
    0, // NullPieceWhite
    0, // King has no material value
    MATERIAL_QUEEN as u16,
    MATERIAL_ROOK as u16,
    MATERIAL_BISHOP as u16,
    MATERIAL_KNIGHT as u16,
    MATERIAL_PAWN as u16,
    0, // Pad
    0, // NullPieceBlack
    0, // King has no material value
    MATERIAL_QUEEN as u16,
    MATERIAL_ROOK as u16,
    MATERIAL_BISHOP as u16,
    MATERIAL_KNIGHT as u16,
    MATERIAL_PAWN as u16,
    0, // Pad
];

#[derive(Debug, Copy, Clone)]
enum CaptureMoves {
    PxQ,
    NxQ,
    BxQ,
    RxQ,
    QxQ,
    KxQ,

    PxR,
    NxR,
    BxR,
    RxR,
    QxR,
    KxR,

    PxB,
    NxB,
    BxB,
    RxB,
    QxB,
    KxB,

    PxN,
    NxN,
    BxN,
    RxN,
    QxN,
    KxN,

    PxP,
    NxP,
    BxP,
    RxP,
    QxP,
    KxP,
}

#[derive(Debug)]
#[repr(align(64))]
pub struct MovegenScratch {
    quiet_list: [u16; 218],
    capture_list: [[u16; 32]; 32],
    capture_ptr: [*mut u16; 32],
}

impl MovegenScratch {
    pub fn new() -> Self {
        Self {
            quiet_list: [0; 218],
            capture_list: [[0; 32]; 32],
            capture_ptr: [std::ptr::null_mut(); 32],
        }
    }
}

const CASTLES_SQUARES: [u8; 64] = [
    0b1011, 0b1111, 0b1111, 0b1111, 0b0011, 0b1111, 0b1111, 0b0111, //
    0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, //
    0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, //
    0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, //
    0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, //
    0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, //
    0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, 0b1111, //
    0b1110, 0b1111, 0b1111, 0b1111, 0b1100, 0b1111, 0b1111, 0b1101, //
];

pub enum GameState {
    Ongoing,
    Checkmate(util::Side),
    Stalemate,
    DrawByFiftyMoveRule,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceIndex {
    WhiteNullPiece = 0,
    WhiteKing = 1,
    WhiteQueen = 2,
    WhiteRook = 3,
    WhiteBishop = 4,
    WhiteKnight = 5,
    WhitePawn = 6,
    WhitePad = 7,

    BlackNullPiece = 8,
    BlackKing = 9,
    BlackQueen = 10,
    BlackRook = 11,
    BlackBishop = 12,
    BlackKnight = 13,
    BlackPawn = 14,
    BlackPad = 15,

    PieceIndexMax = 16,
}

impl From<usize> for PieceIndex {
    // @todo perf - mem transmute
    fn from(value: usize) -> Self {
        match value {
            0 => PieceIndex::WhiteNullPiece,
            1 => PieceIndex::WhiteKing,
            2 => PieceIndex::WhiteQueen,
            3 => PieceIndex::WhiteRook,
            4 => PieceIndex::WhiteBishop,
            5 => PieceIndex::WhiteKnight,
            6 => PieceIndex::WhitePawn,
            // 7 => PieceIndex::WhitePad,
            8 => PieceIndex::BlackNullPiece,
            9 => PieceIndex::BlackKing,
            10 => PieceIndex::BlackQueen,
            11 => PieceIndex::BlackRook,
            12 => PieceIndex::BlackBishop,
            13 => PieceIndex::BlackKnight,
            14 => PieceIndex::BlackPawn,
            // 15 => PieceIndex::BlackPad,
            _ => panic!("Invalid PieceIndex for from conversion {}", value),
        }
    }
}

impl From<util::PieceId> for PieceIndex {
    fn from(value: util::PieceId) -> Self {
        match value {
            util::PieceId::WhiteKing => PieceIndex::WhiteKing,
            util::PieceId::WhiteQueen => PieceIndex::WhiteQueen,
            util::PieceId::WhiteRook => PieceIndex::WhiteRook,
            util::PieceId::WhiteBishop => PieceIndex::WhiteBishop,
            util::PieceId::WhiteKnight => PieceIndex::WhiteKnight,
            util::PieceId::WhitePawn => PieceIndex::WhitePawn,
            util::PieceId::BlackKing => PieceIndex::BlackKing,
            util::PieceId::BlackQueen => PieceIndex::BlackQueen,
            util::PieceId::BlackRook => PieceIndex::BlackRook,
            util::PieceId::BlackBishop => PieceIndex::BlackBishop,
            util::PieceId::BlackKnight => PieceIndex::BlackKnight,
            util::PieceId::BlackPawn => PieceIndex::BlackPawn,
            _ => panic!("Invalid PieceIndex for from conversion: {:?}", value),
        }
    }
}

impl Into<usize> for PieceIndex {
    fn into(self) -> usize {
        // self as usize
        match self {
            PieceIndex::WhiteNullPiece => 0,
            PieceIndex::WhiteKing => 1,
            PieceIndex::WhiteQueen => 2,
            PieceIndex::WhiteRook => 3,
            PieceIndex::WhiteBishop => 4,
            PieceIndex::WhiteKnight => 5,
            PieceIndex::WhitePawn => 6,
            // PieceIndex::WhitePad => 7,
            PieceIndex::BlackNullPiece => 8,
            PieceIndex::BlackKing => 9,
            PieceIndex::BlackQueen => 10,
            PieceIndex::BlackRook => 11,
            PieceIndex::BlackBishop => 12,
            PieceIndex::BlackKnight => 13,
            PieceIndex::BlackPawn => 14,
            // PieceIndex::BlackPad => 15,
            _ => panic!("Invalid PieceIndex for usize conversion: {:?}", self),
        }
    }
}

impl Into<char> for PieceIndex {
    fn into(self) -> char {
        match self {
            PieceIndex::WhiteKing => 'K',
            PieceIndex::WhiteQueen => 'Q',
            PieceIndex::WhiteRook => 'R',
            PieceIndex::WhiteBishop => 'B',
            PieceIndex::WhiteKnight => 'N',
            PieceIndex::WhitePawn => 'P',
            PieceIndex::BlackKing => 'k',
            PieceIndex::BlackQueen => 'q',
            PieceIndex::BlackRook => 'r',
            PieceIndex::BlackBishop => 'b',
            PieceIndex::BlackKnight => 'n',
            PieceIndex::BlackPawn => 'p',
            _ => panic!("Invalid PieceIndex for char conversion: {:?}", self),
        }
    }
}

#[repr(C)]
#[repr(align(64))]
#[derive(Default, Debug, Clone, Copy)]
pub struct Bitboards {
    // bitboards in format
    // [0, K, Q, R, B, N, P, 0, 0, k, q, r, b, n, p, 0]
    pub bitboards: [u64; 16],
}
const BITBOARDS_SIZE_ASSERT: [u8; 128] = [0; std::mem::size_of::<Bitboards>()];

#[derive(Debug, Clone, Copy)]
pub struct ChessGame {
    board: Bitboards,
    b_move: bool,
    castles: u8, // 0b[white kingside, white queenside, black kingside, black queenside]
    en_passant: u8, // ep square (or 0 if none)
    half_moves: u32,
    full_moves: u32,
    zobrist_key: u64,
    material: [u16; 2],
    spt: [u8; 64],
}
const CHESS_GAME_SIZE_ASSERT: [u8; 256] = [0; std::mem::size_of::<ChessGame>()];

#[derive(Default, Debug)]
pub struct Stats {
    slider_piece_counts: [u8; 8],
    nonslider_piece_counts: [u8; 8],
    slider_move_counts: [u8; 8],
    nonslider_move_counts: [u8; 8],
}

impl ChessGame {
    pub fn new() -> Self {
        Self {
            board: Bitboards::default(),
            b_move: false,
            castles: 0,
            en_passant: 0,
            half_moves: 0,
            full_moves: 1,
            zobrist_key: 0,
            material: [0; 2],
            spt: [0; 64],
        }
    }

    pub fn gen_moves_slow(&self, tables: &Tables, move_list: &mut [u16]) -> usize {
        let mut move_cursor = 0;

        let bitboards = &self.board.bitboards;
        let friendly_offset = (self.b_move as usize) << 3;
        let opponent_offset = (!self.b_move as usize) << 3;

        let friendly_board = bitboards[friendly_offset..friendly_offset + 8]
            .iter()
            .fold(0, |acc, &bb| acc | bb);

        let opponent_board = bitboards[opponent_offset..opponent_offset + 8]
            .iter()
            .fold(0, |acc, &bb| acc | bb);

        let full_board = friendly_board | opponent_board;

        move_cursor += self.gen_pawn_moves(
            &mut move_list[move_cursor..],
            self.b_move,
            opponent_board,
            full_board,
        );

        move_cursor += self.gen_slider_moves::<true>(
            tables,
            &mut move_list[move_cursor..],
            bitboards[PieceIndex::WhiteRook as usize + friendly_offset],
            friendly_board,
            opponent_board,
            full_board,
        );

        move_cursor += self.gen_slider_moves::<false>(
            tables,
            &mut move_list[move_cursor..],
            bitboards[PieceIndex::WhiteBishop as usize + friendly_offset],
            friendly_board,
            opponent_board,
            full_board,
        );

        move_cursor += self.gen_slider_moves::<false>(
            tables,
            &mut move_list[move_cursor..],
            bitboards[PieceIndex::WhiteQueen as usize + friendly_offset],
            friendly_board,
            opponent_board,
            full_board,
        );
        move_cursor += self.gen_slider_moves::<true>(
            tables,
            &mut move_list[move_cursor..],
            bitboards[PieceIndex::WhiteQueen as usize + friendly_offset],
            friendly_board,
            opponent_board,
            full_board,
        );

        move_cursor += self.gen_knight_moves(
            &mut move_list[move_cursor..],
            self.b_move,
            friendly_board,
            opponent_board,
        );

        move_cursor += self.gen_king_moves(
            &mut move_list[move_cursor..],
            self.b_move,
            friendly_board,
            opponent_board,
            full_board,
        );

        move_cursor
    }

    #[inline(always)]
    pub fn in_check_slow(&self, tables: &Tables, b_move: bool) -> bool {
        let king_sq = self.board.bitboards
            [PieceIndex::WhiteKing as usize + ((b_move as usize) << 3)]
            .trailing_zeros() as u8;

        self.is_king_square_attacked(king_sq, b_move, tables)
    }

    pub fn is_king_square_attacked(&self, sq_index: u8, b_move: bool, tables: &Tables) -> bool {
        let opponent_bitboards = &self.board.bitboards[((!b_move as usize) << 3)..];

        let mut is_attacked = 0u64;

        unsafe {
            let pawn_attack_mask =
                *Tables::LT_PAWN_CAPTURE_MASKS[b_move as usize].get_unchecked(sq_index as usize);

            is_attacked |= opponent_bitboards[PieceIndex::WhitePawn as usize] & pawn_attack_mask;

            let knight_attack_mask = *Tables::LT_KNIGHT_MOVE_MASKS.get_unchecked(sq_index as usize);
            is_attacked |=
                opponent_bitboards[PieceIndex::WhiteKnight as usize] & knight_attack_mask;

            // @todo - King might not be able to attack all squares if pinned -
            // but this does not matter for castling check
            let king_attack_mask = *Tables::LT_KING_MOVE_MASKS.get_unchecked(sq_index as usize);
            is_attacked |= opponent_bitboards[PieceIndex::WhiteKing as usize] & king_attack_mask;

            let full_board = self.board.bitboards.iter().fold(0, |acc, &bb| acc | bb);

            let opponent_rook_board = opponent_bitboards[PieceIndex::WhiteRook as usize];
            let opponent_bishop_board = opponent_bitboards[PieceIndex::WhiteBishop as usize];
            let opponent_queen_board = opponent_bitboards[PieceIndex::WhiteQueen as usize];

            let rook_occupancy_mask =
                *Tables::LT_ROOK_OCCUPANCY_MASKS.get_unchecked(sq_index as usize);
            let rook_blockers = full_board & rook_occupancy_mask;
            let rook_moves =
                tables.get_slider_move_mask_unchecked::<true>(sq_index as usize, rook_blockers);

            let bishop_occupancy_mask =
                *Tables::LT_BISHOP_OCCUPANCY_MASKS.get_unchecked(sq_index as usize);
            let bishop_blockers = full_board & bishop_occupancy_mask;
            let bishop_moves =
                tables.get_slider_move_mask_unchecked::<false>(sq_index as usize, bishop_blockers);

            is_attacked |= (opponent_rook_board | opponent_queen_board) & rook_moves;
            is_attacked |= (opponent_bishop_board | opponent_queen_board) & bishop_moves;

            is_attacked != 0
        }
    }

    pub fn gen_pawn_moves(
        &self,
        move_list: &mut [u16],
        b_move: bool,
        opponent_board: u64,
        full_board: u64,
    ) -> usize {
        let mut move_cursor = 0;
        let mut pawn_bitboard =
            self.board.bitboards[PieceIndex::WhitePawn as usize + ((b_move as usize) << 3)];

        loop {
            let src_sq = pop_ls1b!(pawn_bitboard);

            // Pawn captures
            let pawn_cap_mask = Tables::LT_PAWN_CAPTURE_MASKS[b_move as usize][src_sq as usize];
            let mut pawn_cap_bitboard = pawn_cap_mask & opponent_board;

            loop {
                let dst_sq = pop_ls1b!(pawn_cap_bitboard);

                // Capture can also be a promotion
                if dst_sq / 8 == !b_move as u16 * 7 {
                    for promotion_flag in [
                        MV_FLAGS_PR_KNIGHT,
                        MV_FLAGS_PR_BISHOP,
                        MV_FLAGS_PR_ROOK,
                        MV_FLAGS_PR_QUEEN,
                    ] {
                        move_list[move_cursor] =
                            (dst_sq << 6) | src_sq | promotion_flag | MV_FLAG_CAP;
                        move_cursor += 1;
                    }
                    continue;
                }
                move_list[move_cursor] = (dst_sq << 6) | src_sq | MV_FLAG_CAP;
                move_cursor += 1;
            }

            // En passant
            // @todo - could be branchless
            if self.en_passant != 0 {
                if pawn_cap_mask & (1 << self.en_passant) != 0 {
                    move_list[move_cursor] =
                        ((self.en_passant as u16) << 6) | src_sq | MV_FLAG_EPCAP;
                    move_cursor += 1;
                }
            }

            // Pawn push
            if b_move {
                // println!("src_sq: {}", src_sq);
                let dst_sq = src_sq - 8;

                if (full_board & (1 << dst_sq)) == 0 {
                    match src_sq / 8 {
                        1 => {
                            for promotion_flag in [
                                MV_FLAGS_PR_KNIGHT,
                                MV_FLAGS_PR_BISHOP,
                                MV_FLAGS_PR_ROOK,
                                MV_FLAGS_PR_QUEEN,
                            ] {
                                move_list[move_cursor] = (dst_sq << 6) | src_sq | promotion_flag;
                                move_cursor += 1;
                            }
                        }
                        6 => {
                            move_list[move_cursor] = (dst_sq << 6) | src_sq;
                            move_cursor += 1;

                            let dst_sq = src_sq - 16;
                            if (full_board & (1 << dst_sq)) == 0 {
                                move_list[move_cursor] = (dst_sq << 6) | src_sq | MV_FLAG_DPP;
                                move_cursor += 1;
                            }
                        }
                        _ => {
                            move_list[move_cursor] = (dst_sq << 6) | src_sq;
                            move_cursor += 1;
                        }
                    };
                }
            } else {
                let dst_sq = src_sq + 8;

                if (full_board & (1 << dst_sq)) == 0 {
                    match src_sq / 8 {
                        6 => {
                            for promotion_flag in [
                                MV_FLAGS_PR_KNIGHT,
                                MV_FLAGS_PR_BISHOP,
                                MV_FLAGS_PR_ROOK,
                                MV_FLAGS_PR_QUEEN,
                            ] {
                                move_list[move_cursor] = (dst_sq << 6) | src_sq | promotion_flag;
                                move_cursor += 1;
                            }
                        }
                        1 => {
                            move_list[move_cursor] = (dst_sq << 6) | src_sq;
                            move_cursor += 1;

                            let dst_sq = src_sq + 16;
                            if (full_board & (1 << dst_sq)) == 0 {
                                move_list[move_cursor] = (dst_sq << 6) | src_sq | MV_FLAG_DPP;
                                move_cursor += 1;
                            }
                        }
                        _ => {
                            move_list[move_cursor] = (dst_sq << 6) | src_sq;
                            move_cursor += 1;
                        }
                    };
                }
            }
        }

        move_cursor
    }

    pub fn gen_slider_moves<const IS_ROOK: bool>(
        &self,
        tables: &Tables,
        move_list: &mut [u16],
        piece_board: u64,
        friendly_board: u64,
        opponent_board: u64,
        full_board: u64,
    ) -> usize {
        let mut move_cursor = 0;

        let mut piece_board = piece_board;

        loop {
            let src_sq = pop_ls1b!(piece_board);

            let occupancy_mask = if IS_ROOK {
                Tables::LT_ROOK_OCCUPANCY_MASKS[src_sq as usize]
            } else {
                Tables::LT_BISHOP_OCCUPANCY_MASKS[src_sq as usize]
            };

            let slider_blockers = full_board & occupancy_mask;

            let mut slider_moves = tables
                .get_slider_move_mask::<IS_ROOK>(src_sq as usize, slider_blockers)
                & !friendly_board;

            loop {
                let dst_sq = pop_ls1b!(slider_moves);
                let cap_flag = (((1 << dst_sq) & opponent_board) >> dst_sq << 14) as u16;

                move_list[move_cursor] = (dst_sq << 6) | src_sq | cap_flag;
                move_cursor += 1;
            }
        }

        move_cursor
    }

    pub fn gen_knight_moves(
        &self,
        move_list: &mut [u16],
        b_move: bool,
        friendly_board: u64,
        opponent_board: u64,
    ) -> usize {
        let mut move_cursor = 0;

        let mut knight_bitboard =
            self.board.bitboards[PieceIndex::WhiteKnight as usize + ((b_move as usize) << 3)];

        loop {
            let src_sq = pop_ls1b!(knight_bitboard);

            let mut knight_moves_bitboard =
                Tables::LT_KNIGHT_MOVE_MASKS[src_sq as usize] & !friendly_board;

            loop {
                let dst_sq = pop_ls1b!(knight_moves_bitboard);
                let cap_flag = (((1 << dst_sq) & opponent_board) >> dst_sq << 14) as u16;

                move_list[move_cursor] = (dst_sq << 6) | src_sq | cap_flag;
                move_cursor += 1;
            }
        }

        move_cursor
    }

    pub fn gen_king_moves(
        &self,
        move_list: &mut [u16],
        b_move: bool,
        friendly_board: u64,
        opponent_board: u64,
        full_board: u64,
    ) -> usize {
        let mut move_cursor = 0;

        let king_bitboard =
            self.board.bitboards[PieceIndex::WhiteKing as usize + ((b_move as usize) << 3)];
        let src_sq = king_bitboard.trailing_zeros() as u16;

        let mut king_moves_bitboard = Tables::LT_KING_MOVE_MASKS[src_sq as usize] & !friendly_board;

        loop {
            let dst_sq = pop_ls1b!(king_moves_bitboard);
            let cap_flag: u16 = (((1 << dst_sq) & opponent_board) >> dst_sq << 14) as u16;

            move_list[move_cursor] = (dst_sq << 6) | src_sq | cap_flag;
            move_cursor += 1;
        }

        // Castles
        if self.is_kingside_castle_allowed(b_move) {
            // King side castle
            if (full_board & (0b11 << (src_sq + 1))) == 0 {
                move_list[move_cursor] = ((src_sq + 2) << 6) | src_sq | MV_FLAGS_CASTLE_KING;
                move_cursor += 1;
            }
        }
        if self.is_queenside_castle_allowed(b_move) {
            // Queen side castle
            if (full_board & (0b111 << (src_sq - 3))) == 0 {
                move_list[move_cursor] = ((src_sq - 2) << 6) | src_sq | MV_FLAGS_CASTLE_QUEEN;
                move_cursor += 1;
            }
        }

        move_cursor
    }

    // Returns a PieceIndex as usize
    pub fn piece_at_slow(&self, sq_bit: u64) -> usize {
        for i in PieceIndex::WhiteNullPiece as usize..PieceIndex::PieceIndexMax as usize {
            if self.board.bitboards[i] & sq_bit != 0 {
                return i;
            }
        }

        0
    }

    // Returns 0 if there is no piece, PieceId+1 otherwise
    // Cost estimate: ~7 cycles
    // #[inline(always)]
    // fn piece_at_avx512(&mut self, sq_bit: u64) -> usize {
    //     unsafe {
    //         let select_vec = _mm512_set1_epi64(sq_bit as i64);
    //         let index_vec = _mm512_set_epi32(12, 11, 10, 9, 8, 7, 6, 5, 8, 7, 6, 5, 4, 3, 2, 1);

    //         let white_vec = _mm512_loadu_si512(self.board.bitboards.as_ptr().add(1) as *const _);
    //         let rest_vec = _mm512_loadu_si512(self.board.bitboards.as_ptr().add(5) as *const _);

    //         let from_mask_white = _mm512_test_epi64_mask(white_vec, select_vec) as u16;
    //         let from_mask_rest = _mm512_test_epi64_mask(rest_vec, select_vec) as u16;

    //         let piece_vec = _mm512_mask_compress_epi32(
    //             _mm512_setzero_si512(),
    //             _mm512_kunpackb(from_mask_rest, from_mask_white),
    //             index_vec,
    //         );
    //         let piece_idx = _mm_cvtsi128_si32(_mm512_castsi512_si128(piece_vec)) as usize;

    //         piece_idx
    //     }
    // }

    /// Makes a move on the board. If it returns true, the move was made, otherwise it was not and
    /// no state was changed.
    ///
    /// Safety: the given move must always be pseudovalid, e.g a move that must be valid on the chess board, with
    /// the exception of leaving the king in check or castling without checking threats to the passed squares. In
    /// the latter case, the function returns false. The former case must be handled by the caller.
    pub unsafe fn make_move(&mut self, mv: u16, tables: &Tables) -> bool {
        let zb_keys = &tables.zobrist_hash_keys;

        let from_sq = (mv & 0x3F) as u8;
        let from_bit: u64 = 1 << from_sq;

        let to_sq = ((mv >> 6) & 0x3F) as u8;
        let to_bit: u64 = 1 << to_sq;

        let move_flags = mv & MV_FLAGS;
        let mut next_ep_square = 0;

        let friendly_offset = (self.b_move as usize) << 3;

        match move_flags {
            MV_FLAGS_CASTLE_KING | MV_FLAGS_CASTLE_QUEEN => {
                const CASTLING_OFFSETS: [[i8; 4]; 4] = [
                    [0, 0, 0, 0],
                    [0, 0, 0, 0],
                    [1, 2, 1, -1],   // king
                    [-1, -2, -2, 1], // queen
                ];

                let square_offsets =
                    unsafe { *CASTLING_OFFSETS.get_unchecked((move_flags >> 12) as usize) };

                if [
                    from_sq,
                    from_sq.wrapping_add(square_offsets[0] as u8),
                    from_sq.wrapping_add(square_offsets[1] as u8),
                ]
                .iter()
                .any(|&square| self.is_king_square_attacked(square, self.b_move, tables))
                {
                    return false;
                }

                let full_board = self.board.bitboards.iter().fold(0, |acc, &bb| acc | bb);

                if move_flags == MV_FLAGS_CASTLE_KING && (full_board & (0b11 << (from_sq + 1))) != 0
                {
                    return false;
                } else if move_flags == MV_FLAGS_CASTLE_QUEEN
                    && (full_board & (0b111 << (from_sq - 3))) != 0
                {
                    return false;
                }

                let rook_from_sq = (to_sq as usize).wrapping_add(square_offsets[2] as usize);
                let rook_to_sq = (to_sq as usize).wrapping_add(square_offsets[3] as usize);

                let rook_from_bit = 1 << rook_from_sq;
                let rook_to_bit = 1 << rook_to_sq;

                let rook_piece_id = PieceIndex::WhiteRook as usize + friendly_offset;
                self.board.bitboards[rook_piece_id] ^= rook_to_bit | rook_from_bit;

                unsafe {
                    *self.spt.get_unchecked_mut(rook_from_sq) = 0;
                    *self.spt.get_unchecked_mut(rook_to_sq) = rook_piece_id as u8;
                }

                // Re-add Zobrist key for moved rook
                unsafe {
                    // Safety:
                    //  - rook_piece_id is a valid piece id
                    //  - rook_from_sq and rook_to_sq are guaranteed to be in board range [0, =63]
                    let rook_keys = zb_keys.hash_piece_squares_new.get_unchecked(rook_piece_id);

                    self.zobrist_key ^= rook_keys.get_unchecked(rook_from_sq);
                    self.zobrist_key ^= rook_keys.get_unchecked(rook_to_sq);
                }
            }
            MV_FLAG_EPCAP => {
                let (piece_id, cap_pawn_sq) = if self.b_move {
                    (PieceIndex::WhitePawn as usize, to_sq + 8)
                } else {
                    (PieceIndex::BlackPawn as usize, to_sq - 8)
                };

                debug_assert!(cap_pawn_sq != from_sq);

                self.board.bitboards[piece_id] &= !(1 << cap_pawn_sq);

                unsafe {
                    // Safety: cap_pawn_sq is guaranteed to be in board range [0, =63]
                    *self.spt.get_unchecked_mut(cap_pawn_sq as usize) = 0;
                };

                // Remove captured pawn from Zobrist key
                unsafe {
                    // Safety: cap_pawn_sq is guaranteed to be in board range [0, =63]
                    self.zobrist_key ^= zb_keys
                        .hash_piece_squares_new
                        .get_unchecked(piece_id)
                        .get_unchecked(cap_pawn_sq as usize);
                }

                self.material[!self.b_move as usize] -= MATERIAL_TABLE[piece_id];
            }
            MV_FLAG_DPP => {
                let new_ep_square = if self.b_move { to_sq + 8 } else { to_sq - 8 };

                debug_assert!(new_ep_square != 0);
                next_ep_square = new_ep_square;

                // Add new en passant square to Zobrist key
                unsafe {
                    // Safety: Any pseudovalid move guarantees ep_square to be in range [8, =15] | [48, =55]
                    self.zobrist_key ^= zb_keys
                        .hash_en_passant_squares
                        .get_unchecked(new_ep_square as usize);
                }
            }
            _ => {}
        }

        let from_piece = self.spt[from_sq as usize] as usize;
        let to_piece = self.spt[to_sq as usize] as usize;

        debug_assert!(to_piece == 0 || (mv & MV_FLAG_CAP != 0));

        self.spt[from_sq as usize] = 0;
        self.spt[to_sq as usize] = from_piece as u8;

        unsafe {
            // Safety:
            //  - piece_from is guaranteed to be valid piece id
            //  - piece_to is guaranteed to be a valid piece id OR 0

            // to_piece will write to the white null-piece slot, but due to & it will remain zeros
            *self.board.bitboards.get_unchecked_mut(to_piece) &= !to_bit;
            *self.board.bitboards.get_unchecked_mut(from_piece) ^= to_bit | from_bit;

            self.material[!self.b_move as usize] -= MATERIAL_TABLE.get_unchecked(to_piece);

            // Remove moved piece from from_sq and add it to to_sq
            self.zobrist_key ^=
                zb_keys.hash_piece_squares_new.get_unchecked(from_piece)[from_sq as usize];
            self.zobrist_key ^=
                zb_keys.hash_piece_squares_new.get_unchecked(from_piece)[to_sq as usize];

            // Remove captured piece from Zobrist key
            self.zobrist_key ^=
                zb_keys.hash_piece_squares_new.get_unchecked(to_piece)[to_sq as usize];

            // Remove en passant square from Zobrist key
            self.zobrist_key ^= zb_keys
                .hash_en_passant_squares
                .get_unchecked(self.en_passant as usize);
        }

        if move_flags & MV_FLAG_PROMOTION != 0 {
            let promotion_piece = match move_flags & MV_FLAGS_PR_MASK {
                MV_FLAGS_PR_KNIGHT => PieceIndex::WhiteKnight as usize + friendly_offset,
                MV_FLAGS_PR_BISHOP => PieceIndex::WhiteBishop as usize + friendly_offset,
                MV_FLAGS_PR_ROOK => PieceIndex::WhiteRook as usize + friendly_offset,
                MV_FLAGS_PR_QUEEN => PieceIndex::WhiteQueen as usize + friendly_offset,
                _ => unreachable!(),
            };

            unsafe {
                // Safety: from_piece and promotion_piece are guaranteed to be valid piece ids
                *self.board.bitboards.get_unchecked_mut(from_piece) &= !to_bit;
                *self.board.bitboards.get_unchecked_mut(promotion_piece) |= to_bit;

                // Safety: to_sq is guaranteed to be in board range [0, =63]
                *self.spt.get_unchecked_mut(to_sq as usize) = promotion_piece as u8;

                // Remove Zobrist key for promoted pawn and add it for promoted-to piece
                self.zobrist_key ^= zb_keys
                    .hash_piece_squares_new
                    .get_unchecked(from_piece)
                    .get_unchecked(to_sq as usize);
                self.zobrist_key ^= zb_keys
                    .hash_piece_squares_new
                    .get_unchecked(promotion_piece)
                    .get_unchecked(to_sq as usize);

                self.material[self.b_move as usize] += MATERIAL_TABLE
                    .get_unchecked(promotion_piece)
                    - MATERIAL_TABLE.get_unchecked(from_piece);
            }
        }

        // Remove Zobrist key for castling rights
        unsafe {
            // Safety: self.castles is guaranteed to be in range [0, =15]
            self.zobrist_key ^= zb_keys
                .hash_castling_rights
                .get_unchecked(self.castles as usize);
        }

        self.castles &= CASTLES_SQUARES[from_sq as usize];
        self.castles &= CASTLES_SQUARES[to_sq as usize];

        // Add Zobrist key for castling rights
        unsafe {
            // Safety: self.castles is guaranteed to be in range [0, =15]
            self.zobrist_key ^= zb_keys
                .hash_castling_rights
                .get_unchecked(self.castles as usize);
        }

        self.full_moves += self.b_move as u32;
        self.en_passant = next_ep_square;

        let is_capture = (move_flags & MV_FLAG_CAP) != 0;
        let is_pawn_move = from_piece == (PieceIndex::WhitePawn as usize + friendly_offset);

        self.half_moves = if is_capture || is_pawn_move {
            0
        } else {
            self.half_moves + 1
        };

        self.b_move = !self.b_move;
        self.zobrist_key ^= zb_keys.hash_side_to_move;

        // println!(
        //     "played move {} ({}), b_move: {}, zobrist_key: {:x}",
        //     util::move_string(mv),
        //     mv,
        //     self.b_move,
        //     self.zobrist_key
        // );

        // debug_assert!(self.is_valid(), "Board is invalid after move");
        debug_assert!(
            (self.board.bitboards[PieceIndex::WhiteNullPiece as usize]
                | self.board.bitboards[PieceIndex::BlackNullPiece as usize]
                | self.board.bitboards[PieceIndex::WhitePad as usize]
                | self.board.bitboards[PieceIndex::BlackPad as usize])
                == 0,
            "Empty bitboards are not zero after move {}",
            util::move_string(mv)
        );

        true
    }

    pub fn make_null_move(&mut self, tables: &Tables) -> u8 {
        let zb_keys = &tables.zobrist_hash_keys;

        self.b_move = !self.b_move;
        self.zobrist_key ^= zb_keys.hash_side_to_move;
        self.zobrist_key ^= zb_keys.hash_en_passant_squares[self.en_passant as usize];

        let old_ep = self.en_passant;
        self.en_passant = 0;

        old_ep
    }

    pub fn rollback_null_move(&mut self, ep_square: u8, tables: &Tables) {
        let zb_keys = &tables.zobrist_hash_keys;

        self.b_move = !self.b_move;
        self.zobrist_key ^= zb_keys.hash_en_passant_squares[ep_square as usize];
        self.zobrist_key ^= zb_keys.hash_side_to_move;

        self.en_passant = ep_square;
    }

    pub fn perft_prev<const INTEGRITY_CHECK: bool>(
        &mut self,
        depth: u8,
        tables: &Tables,
        mut moves: Option<&mut Vec<(String, u64)>>,
    ) -> u64 {
        if depth == 0 {
            return 1;
        }

        let mut move_list = [0; 256];
        let move_count = self.gen_moves_slow(tables, &mut move_list);

        let mut node_count = 0;

        let board_copy = self.clone();

        for mv_index in 0..move_count {
            let mv = move_list[mv_index];

            if unsafe { self.make_move(mv, tables) } && !self.in_check_slow(tables, !self.b_move) {
                if INTEGRITY_CHECK {
                    assert!(
                        self.zobrist_key == self.calc_initial_zobrist_key(tables),
                        "Zobrist key mismatch after move {}",
                        util::move_string(mv),
                    );
                    assert!(self.spt == self.calc_spt(), "Spt mismatch after move");
                    assert!(
                        self.material == self.calc_material(),
                        "Material mismatch after move {}: {:?} vs {:?}",
                        util::move_string(mv),
                        self.material,
                        self.calc_material(),
                    );
                }
                let nodes = self.perft_prev::<INTEGRITY_CHECK>(depth - 1, tables, None);
                node_count += nodes;

                if let Some(ref mut moves) = moves {
                    moves.push((util::move_string(mv), nodes));
                }
            }
            *self = board_copy;
        }

        node_count
    }

    pub fn perft<const INTEGRITY_CHECK: bool>(
        &mut self,
        depth: u8,
        tables: &Tables,
        mut moves: Option<&mut Vec<(String, u64)>>,
        scratch: &mut MovegenScratch,
    ) -> u64 {
        if depth == 0 {
            return 1;
        }

        let mut node_count = 0;

        let mut move_list = [0u16; 220];
        let move_count = self.gen_moves_avx512(tables, &mut move_list[2..], scratch, None);

        let board_copy = self.clone();

        let mut i = 2;
        while i < move_count + 2 {
            let mv = move_list[i];
            i += 1;

            // @todo strength - Check queen promotion first
            if mv & MV_FLAGS_PR_MASK == MV_FLAGS_PR_KNIGHT {
                i -= 3;

                let mv_unpromoted = mv & !MV_FLAGS_PR_MASK;
                move_list[i] = mv_unpromoted | MV_FLAGS_PR_BISHOP; // Second promotion to check
                move_list[i + 1] = mv_unpromoted | MV_FLAGS_PR_ROOK; // Third promotion to check
                move_list[i + 2] = mv_unpromoted | MV_FLAGS_PR_QUEEN; // Second promotion to check
            }

            if unsafe { self.make_move(mv, tables) } && !self.in_check_slow(tables, !self.b_move) {
                if INTEGRITY_CHECK {
                    assert!(
                        self.zobrist_key == self.calc_initial_zobrist_key(tables),
                        "Zobrist key mismatch after move {}",
                        util::move_string_dbg(mv),
                    );
                    assert!(
                        self.spt == self.calc_spt(),
                        "Spt mismatch after move {}",
                        util::move_string_dbg(mv),
                    );
                    assert!(
                        self.material == self.calc_material(),
                        "Material mismatch after move {}: {:?} vs {:?}",
                        util::move_string_dbg(mv),
                        self.material,
                        self.calc_material(),
                    );
                }
                let nodes = self.perft::<INTEGRITY_CHECK>(depth - 1, tables, None, scratch);
                node_count += nodes;

                if let Some(ref mut moves) = moves {
                    moves.push((util::move_string(mv), nodes));
                }
            }
            *self = board_copy;
        }

        node_count
    }

    pub fn load_fen(&mut self, fen: &str, tables: &Tables) -> Result<usize, String> {
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
        let mut fen_length = 0;

        for c in fen.chars() {
            fen_length += 1;
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
                    let b = 1u64.wrapping_shl(pos);

                    match (c, file < 8) {
                        ('r', true) => self.board.bitboards[PieceIndex::BlackRook as usize] |= b,
                        ('n', true) => self.board.bitboards[PieceIndex::BlackKnight as usize] |= b,
                        ('b', true) => self.board.bitboards[PieceIndex::BlackBishop as usize] |= b,
                        ('q', true) => self.board.bitboards[PieceIndex::BlackQueen as usize] |= b,
                        ('k', true) => self.board.bitboards[PieceIndex::BlackKing as usize] |= b,
                        ('p', true) => self.board.bitboards[PieceIndex::BlackPawn as usize] |= b,
                        ('R', true) => self.board.bitboards[PieceIndex::WhiteRook as usize] |= b,
                        ('N', true) => self.board.bitboards[PieceIndex::WhiteKnight as usize] |= b,
                        ('B', true) => self.board.bitboards[PieceIndex::WhiteBishop as usize] |= b,
                        ('Q', true) => self.board.bitboards[PieceIndex::WhiteQueen as usize] |= b,
                        ('K', true) => self.board.bitboards[PieceIndex::WhiteKing as usize] |= b,
                        ('P', true) => self.board.bitboards[PieceIndex::WhitePawn as usize] |= b,
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
                    'K' => self.castles |= 0b1000,
                    'Q' => self.castles |= 0b0100,
                    'k' => self.castles |= 0b0010,
                    'q' => self.castles |= 0b0001,
                    '-' => self.castles = 0b0000,
                    _ => return Err(format!("Invalid castling character '{}'", c)),
                },
                FenState::EnPassant => match c {
                    '-' => {
                        self.en_passant = 0;
                        continue;
                    }
                    'a'..='h' => {
                        let file_index = c as u8 - b'a';

                        if file_index > 7 {
                            return Err(format!("Invalid en passant file '{}'", c));
                        }

                        self.en_passant = file_index;
                    }
                    '1'..='8' => {
                        if self.en_passant == 0 {
                            return Err("En passant square specified without file".into());
                        }

                        let rank_index = c as u8 - b'1';

                        if rank_index > 7 {
                            return Err(format!("Invalid en passant rank '{}'", c));
                        }

                        self.en_passant = (rank_index * 8) | self.en_passant;
                    }
                    ' ' => {
                        state = FenState::MovesHalf;
                        continue;
                    }
                    '\n' => break,
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
                    ' ' | '\n' | '\r' => break,
                    '0'..='9' => {
                        self.full_moves = self.full_moves * 10 + (c as u8 - b'0') as u32;
                    }
                    _ => return Err(format!("Invalid full move character '{}'", c)),
                },
            }
        }

        self.zobrist_key = self.calc_initial_zobrist_key(tables);
        self.material = self.calc_material();
        self.spt = self.calc_spt();

        Ok(fen_length)
    }

    pub fn calc_material(&self) -> [u16; 2] {
        let mut material = [0; 2];
        for i in PieceIndex::WhiteNullPiece as usize..PieceIndex::BlackNullPiece as usize {
            material[0] += self.board.bitboards[i].count_ones() as u16 * MATERIAL_TABLE[i];
            material[1] += self.board.bitboards[i + 8].count_ones() as u16 * MATERIAL_TABLE[i];
        }

        material
    }

    fn calc_spt(&self) -> [u8; 64] {
        let mut spt = [0; 64];

        for sq in 0..64 {
            let piece_id = self.piece_at_slow(1 << sq);
            spt[sq] = piece_id as u8;
        }

        spt
    }

    pub fn gen_fen(&self) -> String {
        use std::fmt::Write;

        #[derive(Debug, Clone, Copy)]
        enum RankContent {
            Piece(PieceIndex),
            Empty(u8),
        }

        let mut fen_string = (0..8)
            .rev()
            .flat_map(|rank| (0..8).map(move |file| rank * 8 + file))
            .map(|square_index| (square_index, self.spt[square_index] as usize))
            .array_chunks::<8>()
            .map(|rank_squares| {
                let rank_content =
                    rank_squares
                        .iter()
                        .fold(([None; 8], 0usize), |mut acc, curr| {
                            let previous_slot = acc.0[acc.1];
                            match (previous_slot, curr.1) {
                                (Some(RankContent::Empty(empty_squares)), 0) => {
                                    acc.0[acc.1] = Some(RankContent::Empty(empty_squares + 1))
                                }
                                (
                                    None
                                    | Some(RankContent::Empty(_))
                                    | Some(RankContent::Piece(_)),
                                    piece_id,
                                ) => {
                                    acc.1 += (previous_slot.is_some()) as usize;
                                    acc.0[acc.1] = if piece_id == 0 {
                                        Some(RankContent::Empty(1))
                                    } else {
                                        Some(RankContent::Piece(PieceIndex::from(piece_id)))
                                    }
                                }
                            }

                            acc
                        });

                rank_content.0
            })
            .map(|c| {
                let rank_string = c
                    .iter()
                    .fuse()
                    .filter_map(|content_type| match content_type {
                        Some(r) => Some(r),
                        None => None,
                    })
                    .map(|square_content| match square_content {
                        RankContent::Piece(piece_id) => Into::<char>::into(*piece_id),
                        RankContent::Empty(empty_squares) => (b'0' + *empty_squares as u8) as char,
                    })
                    .collect::<String>();

                rank_string
            })
            .enumerate()
            .fold(
                String::with_capacity(64),
                |mut acc, (rank_index, rank_string)| {
                    write!(
                        &mut acc,
                        "{}{}",
                        if rank_index != 0 { "/" } else { "" },
                        rank_string
                    )
                    .unwrap();
                    acc
                },
            );

        fen_string.push_str(if self.b_move { " b " } else { " w " });

        let castling_string = ['q', 'k', 'Q', 'K']
            .iter()
            .enumerate()
            .filter(|(index, _)| self.castles & (1 << index) != 0)
            .map(|(_, c)| *c)
            .rev()
            .collect::<String>();

        if castling_string.is_empty() {
            fen_string.push('-');
        } else {
            fen_string.push_str(&castling_string);
        }

        fen_string.push(' ');

        if self.en_passant != 0 {
            let file = (self.en_passant % 8) as u8;
            let rank = (self.en_passant / 8) as u8;
            fen_string.push((b'a' + file) as char);
            fen_string.push((b'1' + rank) as char);
        } else {
            fen_string.push('-');
        }

        fen_string.push(' ');
        fen_string.push_str(&self.half_moves.to_string());
        fen_string.push(' ');
        fen_string.push_str(&self.full_moves.to_string());

        fen_string
    }

    pub fn reset(&mut self) {
        *self = Self::new();
    }

    #[inline(always)]
    pub fn check_game_state(
        &self,
        tables: &Tables,
        side_to_move_no_legal_moves: bool,
        b_move: bool,
    ) -> GameState {
        if side_to_move_no_legal_moves {
            if self.in_check_slow(tables, b_move) {
                GameState::Checkmate(util::Side::from(!b_move))
            } else {
                GameState::Stalemate
            }
        } else if self.half_moves >= 100 {
            GameState::DrawByFiftyMoveRule
        } else {
            GameState::Ongoing
        }
    }

    pub fn is_valid(&self) -> bool {
        if self.board.bitboards[PieceIndex::WhiteKing as usize].count_ones() != 1
            || self.board.bitboards[PieceIndex::BlackKing as usize].count_ones() != 1
        {
            return false;
        }

        for bit_pos in 0..64 {
            let mut sum = 0;
            let bit = 1 << bit_pos;

            for piece_id in PieceIndex::WhiteNullPiece as usize..PieceIndex::PieceIndexMax as usize
            {
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

        if self.board.bitboards[PieceIndex::WhiteNullPiece as usize]
            | self.board.bitboards[PieceIndex::BlackNullPiece as usize]
            | self.board.bitboards[PieceIndex::WhitePad as usize]
            | self.board.bitboards[PieceIndex::BlackPad as usize]
            != 0
        {
            return false;
        }

        true
    }

    pub fn calc_initial_zobrist_key(&self, tables: &Tables) -> u64 {
        let mut zobrist_hash = 0u64;
        let zb_keys = &tables.zobrist_hash_keys;

        for square in 0..64 {
            let piece_id = self.piece_at_slow(1 << square);
            zobrist_hash ^= zb_keys.hash_piece_squares_new[piece_id][square as usize];
        }

        zobrist_hash ^= zb_keys.hash_en_passant_squares[self.en_passant as usize];

        zobrist_hash ^= zb_keys.hash_castling_rights[self.castles as usize];

        zobrist_hash ^= if self.b_move {
            zb_keys.hash_side_to_move
        } else {
            0
        };

        zobrist_hash
    }

    // Sets move flags based on board state that can't be derived from move string
    // double pawn push, en passant, castling, capture flag
    pub fn fix_move(&self, mv: u16) -> u16 {
        let mut mv = mv;

        let from_sq = (mv & 0x3F) as u8;
        let to_sq = ((mv >> 6) & 0x3F) as u8;

        let from_rank = from_sq / 8;
        let to_rank = to_sq / 8;

        let from_piece = self.spt[from_sq as usize] as usize;
        let to_piece = self.spt[to_sq as usize] as usize;

        if to_piece != 0 {
            mv |= MV_FLAG_CAP;
        }

        mv |= match (PieceIndex::from(from_piece), from_rank, to_rank) {
            (PieceIndex::WhitePawn, 1, 3) => MV_FLAG_DPP,
            (PieceIndex::BlackPawn, 6, 4) => MV_FLAG_DPP,
            _ => 0,
        };

        if self.en_passant != 0 {
            let side_pawn_piece = PieceIndex::WhitePawn as usize + ((self.b_move as usize) << 3);

            if to_sq == self.en_passant && from_piece == side_pawn_piece {
                mv |= MV_FLAG_EPCAP;
            }
        }

        mv |= match (PieceIndex::from(from_piece), from_sq, to_sq) {
            (PieceIndex::WhiteKing, 4, 6) => MV_FLAGS_CASTLE_KING,
            (PieceIndex::WhiteKing, 4, 2) => MV_FLAGS_CASTLE_QUEEN,
            (PieceIndex::BlackKing, 60, 62) => MV_FLAGS_CASTLE_KING,
            (PieceIndex::BlackKing, 60, 58) => MV_FLAGS_CASTLE_QUEEN,
            _ => 0,
        };

        mv
    }

    #[inline(always)]
    pub fn b_move(&self) -> bool {
        self.b_move
    }

    #[inline(always)]
    pub fn bitboards_new(&self) -> &[u64; 16] {
        &self.board.bitboards
    }

    #[inline(always)]
    pub fn bitboards_new_mut(&mut self) -> &mut [u64; 16] {
        &mut self.board.bitboards
    }

    #[inline(always)]
    pub fn ep_square(&self) -> u8 {
        self.en_passant
    }

    #[inline(always)]
    pub fn half_moves(&self) -> u32 {
        self.half_moves
    }

    #[inline(always)]
    pub fn full_moves(&self) -> u32 {
        self.full_moves
    }

    #[inline(always)]
    pub fn zobrist_key(&self) -> u64 {
        self.zobrist_key
    }

    #[inline(always)]
    pub fn material(&self) -> &[u16; 2] {
        &self.material
    }

    #[inline(always)]
    pub fn spt(&self) -> &[u8; 64] {
        &self.spt
    }

    #[inline(always)]
    pub fn castles(&self) -> u8 {
        self.castles
    }

    #[inline(always)]
    pub fn is_kingside_castle_allowed(&self, b_move: bool) -> bool {
        self.castles & (0b10 << (!b_move as u8 * 2)) != 0
    }

    #[inline(always)]
    pub fn is_queenside_castle_allowed(&self, b_move: bool) -> bool {
        self.castles & (0b1 << (!b_move as u8 * 2)) != 0
    }

    #[inline(always)]
    pub fn gen_moves_avx512(
        &self,
        tables: &Tables,
        move_list: &mut [u16],
        scratch: &mut MovegenScratch,
        mut stats: Option<&mut Stats>,
    ) -> usize {
        for i in 0..32 {
            scratch.capture_ptr[i] = scratch.capture_list[i].as_mut_ptr() as *mut u16;
        }

        let mut quiet_cursor = 0usize;

        unsafe {
            let bitboards = &self.board.bitboards;
            let b_move = self.b_move;

            let friendly_move_offset = (b_move as usize) << 3;
            let opponent_move_offset = (!b_move as usize) << 3;

            let const_nonslider_selector = _mm512_set_epi64(
                PieceIndex::WhitePawn as i64,
                PieceIndex::WhitePawn as i64,
                PieceIndex::WhitePawn as i64,
                PieceIndex::WhitePawn as i64,
                PieceIndex::WhitePawn as i64,
                PieceIndex::WhiteKnight as i64,
                PieceIndex::WhiteKnight as i64,
                PieceIndex::WhiteKing as i64,
            );
            let const_nonslider_split = _mm512_set_epi64(
                0x4040404040404040u64 as i64, // g // 0x8080808080808080u64 as i64, // h file
                0x2020202020202020u64 as i64, // f // 0x4040404040404040u64 as i64, // g file
                0x404040404040404u64 as i64,  // c // 0x3030303030303030u64 as i64, // ef file
                0x1212121212121212u64 as i64, // be // 0xc0c0c0c0c0c0c0cu64 as i64,  // cd file
                0x8989898989898989u64 as i64, // adh // 0x303030303030303u64 as i64,  // ab file
                0xF0F0F0F0F0F0F0F0u64 as i64, // right half
                0x0F0F0F0F0F0F0F0Fu64 as i64, // left half
                0xFFFFFFFF_FFFFFFFFu64 as i64,
            );
            const PAWN_LANES: u8 = 0b11111000;
            const KNIGHT_LANES: u8 = 0b00000110;
            const KING_LANES: u8 = 0b00000001;

            let const_slider_selector = _mm512_set_epi64(
                PieceIndex::WhiteQueen as i64,
                PieceIndex::WhiteQueen as i64,
                PieceIndex::WhiteRook as i64,
                PieceIndex::WhiteRook as i64,
                PieceIndex::WhiteRook as i64,
                PieceIndex::WhiteRook as i64,
                PieceIndex::WhiteBishop as i64,
                PieceIndex::WhiteBishop as i64,
            );

            let const_slider_split = _mm512_set_epi64(
                0xFFFFFFFF_FFFFFFFFu64 as i64,
                0xFFFFFFFF_FFFFFFFFu64 as i64,
                0x050A050A050A050Au64 as i64, // left light squares
                0x0A050A050A050A05u64 as i64, // left black squares
                0x50A050A050A050A0u64 as i64, // right light squares
                0xA050A050A050A050u64 as i64, // right black squares
                0xAA55AA55AA55AA55u64 as i64, // black squares
                0x55AA55AA55AA55AAu64 as i64, // light squares
            );
            // 0x0 = rook, 0x40 = bishop
            let const_slider_gather_magic_masks_offsets_x8 =
                _mm512_set_epi64(0x40, 0, 0, 0, 0, 0, 0x40, 0x40);
            let const_slider_gather_moves_shifts_x8 = _mm512_set_epi64(
                Tables::BISHOP_OCCUPANCY_BITS as i64,
                Tables::ROOK_OCCUPANCY_BITS as i64,
                Tables::ROOK_OCCUPANCY_BITS as i64,
                Tables::ROOK_OCCUPANCY_BITS as i64,
                Tables::ROOK_OCCUPANCY_BITS as i64,
                Tables::ROOK_OCCUPANCY_BITS as i64,
                Tables::BISHOP_OCCUPANCY_BITS as i64,
                Tables::BISHOP_OCCUPANCY_BITS as i64,
            );
            const BISHOP_MV_GATHER_OFFSET: i64 = (64 * Tables::ROOK_OCCUPANCY_MAX) as i64;
            let const_moves_gather_base_offsets_x8 = _mm512_set_epi64(
                BISHOP_MV_GATHER_OFFSET,
                0,
                0,
                0,
                0,
                0,
                BISHOP_MV_GATHER_OFFSET,
                BISHOP_MV_GATHER_OFFSET,
            );
            const BISHOP_LANES: u8 = 0b00000011;
            const ROOK_LANES: u8 = 0b00111100;
            const QUEEN_LANES: u8 = 0b11000000;

            let bitboard_x8 =
                _mm512_load_si512(bitboards.as_ptr().add(friendly_move_offset) as *const __m512i);

            let mut sliders_x8 = _mm512_and_epi64(
                _mm512_permutex2var_epi64(bitboard_x8, const_slider_selector, bitboard_x8),
                const_slider_split,
            );

            let mut non_sliders_x8 = _mm512_and_epi64(
                _mm512_permutex2var_epi64(bitboard_x8, const_nonslider_selector, bitboard_x8),
                const_nonslider_split,
            );

            let full_board = bitboards.iter().fold(0, |acc, &bb| acc | bb);
            let friendly_board = _mm512_reduce_or_epi64(bitboard_x8) as u64;
            let opponent_board = bitboards[opponent_move_offset..opponent_move_offset + 8]
                .iter()
                .fold(0, |acc, &bb| acc | bb);

            let opponent_queen_x8 = _mm512_set1_epi64(
                bitboards[PieceIndex::WhiteQueen as usize + opponent_move_offset] as i64,
            );
            let opponent_rook_x8 = _mm512_set1_epi64(
                bitboards[PieceIndex::WhiteRook as usize + opponent_move_offset] as i64,
            );
            let opponent_bishop_x8 = _mm512_set1_epi64(
                bitboards[PieceIndex::WhiteBishop as usize + opponent_move_offset] as i64,
            );
            let opponent_knight_x8 = _mm512_set1_epi64(
                bitboards[PieceIndex::WhiteKnight as usize + opponent_move_offset] as i64,
            );
            let opponent_pawn_x8 = _mm512_set1_epi64(
                bitboards[PieceIndex::WhitePawn as usize + opponent_move_offset] as i64,
            );
            let opponent_ep_x8 = _mm512_set1_epi64((1u64 << self.en_passant >> 1 << 1) as i64);

            let const_63_x8 = _mm512_set1_epi64(63);
            let const_64_x8 = _mm512_set1_epi64(64);
            let const_1_x8 = _mm512_set1_epi64(1);
            let const_n1_x8 = _mm512_set1_epi64(-1);
            let const_zero_x8 = _mm512_setzero_si512();

            // Flags
            let const_promotion_flag_x8 = _mm512_set1_epi64(MV_FLAGS_PR_KNIGHT as u64 as i64);
            let const_epcap_flag_x8 = _mm512_set1_epi64(MV_FLAG_EPCAP as u64 as i64);
            let const_cap_flag_x8 = _mm512_set1_epi64(MV_FLAG_CAP as u64 as i64);
            let const_dpp_flag_x8 = _mm512_set1_epi64(MV_FLAG_DPP as u64 as i64);

            let b_move_rank_offset = (56 * (b_move as u64)) as u8;

            let friendly_move_offset_x8 = _mm512_set1_epi64(friendly_move_offset as i64);
            let full_board_x8 = _mm512_set1_epi64(full_board as i64);
            let full_board_inv_x8 = _mm512_set1_epi64(!full_board as i64);
            let friendly_board_inv_x8 = _mm512_set1_epi64(!friendly_board as i64);
            let opponent_board_x8 = _mm512_set1_epi64(opponent_board as i64);
            let pawn_promotion_rank_x8 =
                _mm512_set1_epi64((0xFF00000000000000u64 >> b_move_rank_offset) as i64);
            let pawn_double_push_rank_x8 =
                _mm512_set1_epi64((0xFF000000u64 << ((b_move as usize) << 3)) as i64);

            let pawn_push_offset_ranks =
                (opponent_move_offset as u64 + b_move_rank_offset as u64) as i64;
            let pawn_push_rank_rolv_offset_x8 = _mm512_set1_epi64(pawn_push_offset_ranks); // white=8, black=56

            macro_rules! capture_compress {
                ($cap:expr, $src_piece_mask:ident, $cap_piece_mask:ident, $full_move_epi16_x8:ident) => {{
                    let cap_mask = $src_piece_mask & $cap_piece_mask;
                    let cap_mask_popcnt = (cap_mask as u64).count_ones() as usize;

                    // _mm_mask_compressstoreu_epi16(
                    //     scratch.capture_ptr[$cap as usize] as *mut i16,
                    //     cap_mask,
                    //     $full_move_epi16_x8,
                    // );

                    let compressed = _mm_maskz_compress_epi16(cap_mask, $full_move_epi16_x8);
                    _mm_storeu_epi16(scratch.capture_ptr[$cap as usize] as *mut i16, compressed);

                    scratch.capture_ptr[$cap as usize] =
                        scratch.capture_ptr[$cap as usize].add(cap_mask_popcnt);
                }};
            }

            macro_rules! quiet_compress {
                ($mask:expr, $full_move_epi16_x8:ident) => {{
                    let mask = $mask;
                    _mm_mask_compressstoreu_epi16(
                        scratch.quiet_list.as_mut_ptr().add(quiet_cursor) as *mut i16,
                        mask,
                        $full_move_epi16_x8,
                    );
                    quiet_cursor += mask.count_ones() as usize;
                }};
            }

            // @todo - Try sub+and to pop ls1b instead of ms1b
            let mut one_of_each_slider_index_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(sliders_x8));
            let mut one_of_each_non_slider_index_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_sliders_x8));
            let mut active_pieces_non_slider_mask =
                _mm512_cmpneq_epi64_mask(one_of_each_non_slider_index_x8, const_n1_x8);
            let mut active_pieces_slider_mask =
                _mm512_cmpneq_epi64_mask(one_of_each_slider_index_x8, const_n1_x8);

            macro_rules! collect_stats {
                ($stats:ident, $field:ident, $vec:ident) => {
                    let mut counts_arr = [0u64; 8];
                    _mm512_storeu_si512(counts_arr.as_mut_ptr() as *mut __m512i, $vec);
                    $stats.$field = $stats
                        .$field
                        .iter()
                        .enumerate()
                        .map(|(index, &prev)| prev + counts_arr[index] as u8)
                        .collect::<Vec<_>>()
                        .as_slice()
                        .try_into()
                        .unwrap();
                };
            }

            // if let Some(stats) = &mut stats {
            //     let nonslider_piece_counts_x8 = _mm512_popcnt_epi64(non_sliders_x8);
            //     let slider_piece_counts_x8 = _mm512_popcnt_epi64(sliders_x8);
            //     collect_stats!(stats, nonslider_piece_counts, nonslider_piece_counts_x8);
            //     collect_stats!(stats, slider_piece_counts, slider_piece_counts_x8);
            // }

            loop {
                let mut slider_moves_x8 = ChessGame::get_slider_moves_x8(
                    tables,
                    full_board_x8,
                    one_of_each_slider_index_x8,
                    const_slider_gather_magic_masks_offsets_x8,
                    const_moves_gather_base_offsets_x8,
                    const_slider_gather_moves_shifts_x8,
                    active_pieces_slider_mask,
                );

                let pawn_mask = PAWN_LANES & active_pieces_non_slider_mask;
                let knight_mask = KNIGHT_LANES & active_pieces_non_slider_mask;
                let king_mask = KING_LANES & active_pieces_non_slider_mask;

                let bishop_mask = BISHOP_LANES & active_pieces_slider_mask;
                let rook_mask = ROOK_LANES & active_pieces_slider_mask;
                let queen_mask = QUEEN_LANES & active_pieces_slider_mask;

                let mut non_slider_moves_x8 = _mm512_mask_i64gather_epi64(
                    _mm512_setzero_si512(),
                    active_pieces_non_slider_mask,
                    _mm512_add_epi64(
                        _mm512_mullo_epi64(
                            _mm512_add_epi64(const_nonslider_selector, friendly_move_offset_x8),
                            const_64_x8,
                        ),
                        one_of_each_non_slider_index_x8,
                    ),
                    Tables::LT_NON_SLIDER_MASKS_GATHER.0.as_ptr() as *const i64,
                    8,
                );

                slider_moves_x8 = _mm512_and_si512(slider_moves_x8, friendly_board_inv_x8);
                non_slider_moves_x8 = _mm512_and_si512(non_slider_moves_x8, friendly_board_inv_x8);

                // Pawn push moves
                {
                    let src_sq_bit_x8 =
                        _mm512_sllv_epi64(const_1_x8, one_of_each_non_slider_index_x8);
                    let pawn_push_single_bit_x8 = _mm512_maskz_and_epi64(
                        pawn_mask,
                        _mm512_rolv_epi64(src_sq_bit_x8, pawn_push_rank_rolv_offset_x8),
                        full_board_inv_x8,
                    );
                    let promotion_mask =
                        _mm512_test_epi64_mask(pawn_push_single_bit_x8, pawn_promotion_rank_x8);
                    let pawn_push_double_bit_x8 = _mm512_and_epi64(
                        _mm512_rolv_epi64(pawn_push_single_bit_x8, pawn_push_rank_rolv_offset_x8),
                        _mm512_and_epi64(full_board_inv_x8, pawn_double_push_rank_x8),
                    );
                    let pawn_push_single_dst_sq_x8 = _mm512_slli_epi64(
                        _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(pawn_push_single_bit_x8)),
                        6,
                    );
                    let pawn_push_double_dst_sq_x8 = _mm512_slli_epi64(
                        _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(pawn_push_double_bit_x8)),
                        6,
                    );
                    let pawn_push_single_mask = pawn_mask
                        & _mm512_cmpneq_epi64_mask(pawn_push_single_bit_x8, const_zero_x8);
                    let pawn_push_double_mask = pawn_mask
                        & _mm512_cmpneq_epi64_mask(pawn_push_double_bit_x8, const_zero_x8);

                    let mut pawn_push_single_move_x8 = _mm512_or_epi64(
                        pawn_push_single_dst_sq_x8,
                        one_of_each_non_slider_index_x8,
                    );

                    pawn_push_single_move_x8 = _mm512_mask_or_epi64(
                        pawn_push_single_move_x8,
                        promotion_mask,
                        pawn_push_single_move_x8,
                        const_promotion_flag_x8,
                    );

                    let pawn_push_single_move_epi16_x8 =
                        _mm512_cvtepi64_epi16(pawn_push_single_move_x8);
                    let pawn_push_double_move_epi16_x8 = _mm512_cvtepi64_epi16(_mm512_or_epi64(
                        pawn_push_double_dst_sq_x8,
                        _mm512_or_epi64(one_of_each_non_slider_index_x8, const_dpp_flag_x8),
                    ));

                    quiet_compress!(pawn_push_single_mask, pawn_push_single_move_epi16_x8);
                    quiet_compress!(pawn_push_double_mask, pawn_push_double_move_epi16_x8);
                }

                // if let Some(stats) = &mut stats {
                //     let nonslider_move_counts = _mm512_popcnt_epi64(non_slider_moves_x8);
                //     let slider_move_counts = _mm512_popcnt_epi64(slider_moves_x8);
                //     collect_stats!(stats, nonslider_move_counts, nonslider_move_counts);
                //     collect_stats!(stats, slider_move_counts, slider_move_counts);
                // }

                let mut non_slider_dst_sq_x8 =
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_slider_moves_x8));
                let mut slider_dst_sq_x8 =
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(slider_moves_x8));
                loop {
                    // Dst square bits for masking
                    let non_slider_dst_sq_bit_x8 =
                        _mm512_sllv_epi64(const_1_x8, non_slider_dst_sq_x8);

                    let mut non_slider_full_move_x8 = _mm512_or_epi64(
                        _mm512_slli_epi64(non_slider_dst_sq_x8, 6),
                        one_of_each_non_slider_index_x8,
                    );

                    // Promotion flag for pawn moves on the last rank
                    // NOTE: This requires special handling on move maker side to try out other promotions
                    let promotion_mask = pawn_mask
                        & _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, pawn_promotion_rank_x8);

                    // EP flag for en passant captures
                    let ep_mask = pawn_mask
                        & _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_ep_x8);

                    // Capture flag
                    let cap_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_board_x8);

                    non_slider_full_move_x8 = _mm512_mask_or_epi64(
                        non_slider_full_move_x8,
                        promotion_mask,
                        non_slider_full_move_x8,
                        const_promotion_flag_x8,
                    );
                    non_slider_full_move_x8 = _mm512_mask_or_epi64(
                        non_slider_full_move_x8,
                        ep_mask,
                        non_slider_full_move_x8,
                        const_epcap_flag_x8,
                    );
                    non_slider_full_move_x8 = _mm512_mask_or_epi64(
                        non_slider_full_move_x8,
                        cap_mask,
                        non_slider_full_move_x8,
                        const_cap_flag_x8,
                    );

                    // Convert full move to 16-bit format
                    let move_epi16_x8 = _mm512_cvtepi64_epi16(non_slider_full_move_x8);

                    // Create masks for captures
                    let xq_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_queen_x8);
                    let xr_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_rook_x8);
                    let xb_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_bishop_x8);
                    let xn_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_knight_x8);
                    let xp_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_pawn_x8);
                    let quiet_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, full_board_inv_x8);

                    let xp_ep_mask = xp_mask | ep_mask;

                    capture_compress!(CaptureMoves::PxQ, pawn_mask, xq_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::NxQ, knight_mask, xq_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::KxQ, king_mask, xq_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::PxR, pawn_mask, xr_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::NxR, knight_mask, xr_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::KxR, king_mask, xr_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::PxB, pawn_mask, xb_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::NxB, knight_mask, xb_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::KxB, king_mask, xb_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::PxN, pawn_mask, xn_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::NxN, knight_mask, xn_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::KxN, king_mask, xn_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::PxP, pawn_mask, xp_ep_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::NxP, knight_mask, xp_mask, move_epi16_x8);
                    capture_compress!(CaptureMoves::KxP, king_mask, xp_mask, move_epi16_x8);
                    quiet_compress!(knight_mask & quiet_mask, move_epi16_x8);
                    quiet_compress!(king_mask & quiet_mask, move_epi16_x8);

                    let slider_dst_sq_bit_x8 = _mm512_sllv_epi64(const_1_x8, slider_dst_sq_x8);

                    let s_xq_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_queen_x8);
                    let s_xr_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_rook_x8);
                    let s_xb_mask =
                        _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_bishop_x8);
                    let s_xn_mask =
                        _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_knight_x8);
                    let s_xp_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_pawn_x8);
                    let s_quiet_mask =
                        _mm512_test_epi64_mask(slider_dst_sq_bit_x8, full_board_inv_x8);

                    let slider_cap_mask =
                        _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_board_x8);

                    let slider_full_move_x8 = _mm512_or_epi64(
                        _mm512_slli_epi64(slider_dst_sq_x8, 6),
                        _mm512_mask_or_epi64(
                            one_of_each_slider_index_x8,
                            slider_cap_mask,
                            one_of_each_slider_index_x8,
                            const_cap_flag_x8,
                        ),
                    );

                    // Convert full move to 16-bit format
                    let s_mv_epi16_x8 = _mm512_cvtepi64_epi16(slider_full_move_x8);

                    capture_compress!(CaptureMoves::BxQ, bishop_mask, s_xq_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::RxQ, rook_mask, s_xq_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::QxQ, queen_mask, s_xq_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::BxR, bishop_mask, s_xr_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::RxR, rook_mask, s_xr_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::QxR, queen_mask, s_xr_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::BxB, bishop_mask, s_xb_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::RxB, rook_mask, s_xb_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::QxB, queen_mask, s_xb_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::BxN, bishop_mask, s_xn_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::RxN, rook_mask, s_xn_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::QxN, queen_mask, s_xn_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::BxP, bishop_mask, s_xp_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::RxP, rook_mask, s_xp_mask, s_mv_epi16_x8);
                    capture_compress!(CaptureMoves::QxP, queen_mask, s_xp_mask, s_mv_epi16_x8);
                    quiet_compress!(rook_mask & s_quiet_mask, s_mv_epi16_x8);
                    quiet_compress!(bishop_mask & s_quiet_mask, s_mv_epi16_x8);
                    quiet_compress!(queen_mask & s_quiet_mask, s_mv_epi16_x8);

                    non_slider_moves_x8 =
                        _mm512_xor_epi64(non_slider_moves_x8, non_slider_dst_sq_bit_x8);
                    slider_moves_x8 = _mm512_xor_epi64(slider_moves_x8, slider_dst_sq_bit_x8);

                    non_slider_dst_sq_x8 =
                        _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_slider_moves_x8));
                    slider_dst_sq_x8 =
                        _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(slider_moves_x8));

                    let non_slider_dst_sq_mask =
                        _mm512_cmpneq_epi64_mask(non_slider_dst_sq_x8, const_n1_x8);

                    let slider_dst_sq_mask =
                        _mm512_cmpneq_epi64_mask(slider_dst_sq_x8, const_n1_x8);

                    if (non_slider_dst_sq_mask | slider_dst_sq_mask) == 0 {
                        // No more moves left
                        break;
                    }
                }

                // Pop pieces
                non_sliders_x8 = _mm512_xor_epi64(
                    non_sliders_x8,
                    _mm512_sllv_epi64(const_1_x8, one_of_each_non_slider_index_x8),
                );
                sliders_x8 = _mm512_xor_epi64(
                    sliders_x8,
                    _mm512_sllv_epi64(const_1_x8, one_of_each_slider_index_x8),
                );

                one_of_each_slider_index_x8 =
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(sliders_x8));
                one_of_each_non_slider_index_x8 =
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_sliders_x8));
                active_pieces_non_slider_mask =
                    _mm512_cmpneq_epi64_mask(one_of_each_non_slider_index_x8, const_n1_x8);
                active_pieces_slider_mask =
                    _mm512_cmpneq_epi64_mask(one_of_each_slider_index_x8, const_n1_x8);

                if (active_pieces_non_slider_mask | active_pieces_slider_mask) == 0 {
                    break;
                }
            }

            // Castling moves
            let king_bitboard =
                bitboards[PieceIndex::WhiteKing as usize + ((b_move as usize) << 3)];
            let king_square = king_bitboard.trailing_zeros() as u16;

            *scratch.quiet_list.get_unchecked_mut(quiet_cursor as usize) =
                ((king_square.wrapping_add(2)) << 6) | king_square | MV_FLAGS_CASTLE_KING;
            quiet_cursor += self.is_kingside_castle_allowed(b_move) as usize;

            *scratch.quiet_list.get_unchecked_mut(quiet_cursor as usize) =
                ((king_square.wrapping_sub(2)) << 6) | king_square | MV_FLAGS_CASTLE_QUEEN;
            quiet_cursor += self.is_queenside_castle_allowed(b_move) as usize;

            let mut num_captures = 0;
            for i in 0..32 {
                let cap_count = (scratch.capture_ptr[i] as usize
                    - scratch.capture_list[i].as_ptr() as usize)
                    >> 1;
                // capture_list
                //     .get_unchecked_mut(num_captures..num_captures + cap_count)
                //     .copy_from_slice(&scratch[i].get_unchecked(0..cap_count));

                // WIP:
                // *capture_list.get_unchecked_mut(num_captures + 0) = *scratch[i].get_unchecked(0);
                // *capture_list.get_unchecked_mut(num_captures + 1) = *scratch[i].get_unchecked(1);
                // *capture_list.get_unchecked_mut(num_captures + 2) = *scratch[i].get_unchecked(2);
                // *capture_list.get_unchecked_mut(num_captures + 3) = *scratch[i].get_unchecked(3);
                // for j in 4..cap_count {
                //     std::hint::cold_path();
                //     *capture_list.get_unchecked_mut(num_captures + j) = *scratch[i].get_unchecked(j);
                // }

                // Single list
                // move_list
                //     .get_unchecked_mut(num_captures..num_captures + cap_count)
                //     .copy_from_slice(&scratch[i].get_unchecked(0..cap_count));
                *move_list.get_unchecked_mut(num_captures + 0) =
                    *scratch.capture_list[i].get_unchecked(0);
                *move_list.get_unchecked_mut(num_captures + 1) =
                    *scratch.capture_list[i].get_unchecked(1);
                *move_list.get_unchecked_mut(num_captures + 2) =
                    *scratch.capture_list[i].get_unchecked(2);
                *move_list.get_unchecked_mut(num_captures + 3) =
                    *scratch.capture_list[i].get_unchecked(3);
                for j in 4..cap_count {
                    std::hint::cold_path();
                    *move_list.get_unchecked_mut(num_captures + j) =
                        *scratch.capture_list[i].get_unchecked(j);
                }

                num_captures += cap_count;
            }

            move_list
                .get_unchecked_mut(num_captures..num_captures + quiet_cursor)
                .copy_from_slice(&scratch.quiet_list.get_unchecked(0..quiet_cursor));
            // println!("Copied {} captures", num_copied);
            // println!("Capture moves:");
            // for i in 0..num_copied {
            //     println!("[{}]: {}", i, util::move_string_dbg(capture_list[i] as u16));
            // }

            // capture_list[0..capture_cursors[CaptureMoves::PxQ as usize] as usize].copy_from_slice(
            //     &capture_moves[CaptureMoves::PxQ as usize]
            //         [0..capture_cursors[CaptureMoves::PxQ as usize] as usize],
            // );

            // for i in 0..30 {
            //     assert!(scratch2[i] <= 8);
            // }

            // for i in 0..30 {
            //     println!(
            //         "[{:?}]: {:?}",
            //         std::mem::transmute::<u8, CaptureMoves>(i),
            //         capture_moves[i as usize]
            //             .iter()
            //             .map(|mv| {
            //                 if *mv == 0 {
            //                     return "".to_string();
            //                 }
            //                 util::move_string_dbg(*mv as u16)
            //             })
            //             .collect::<Vec<_>>()
            //     );
            // }

            // for mv in quiet_moves {
            //     if mv == 0 {
            //         break;
            //     }
            //     println!("Quiet: {:?} ({})", mv, util::move_string_dbg(mv as u16));
            // }

            // (quiet_cursor as usize, num_captures as usize)
            num_captures + quiet_cursor
        }
    }

    #[inline(always)]
    fn get_slider_moves_x8(
        tables: &Tables,
        full_board_x8: __m512i,
        piece_indices: __m512i,
        gather_offsets_magics_masks_x8: __m512i,
        gather_offsets_moves_x8: __m512i,
        gather_shifts_moves_x8: __m512i,
        mask: u8,
    ) -> __m512i {
        unsafe {
            let masks_x8 = _mm512_mask_i64gather_epi64(
                _mm512_setzero_si512(),
                mask,
                _mm512_add_epi64(piece_indices, gather_offsets_magics_masks_x8),
                Tables::LT_SLIDER_MASKS_GATHER.0.as_ptr() as *const i64,
                8,
            );

            let magics_x8 = _mm512_mask_i64gather_epi64(
                _mm512_setzero_si512(),
                mask,
                _mm512_add_epi64(piece_indices, gather_offsets_magics_masks_x8),
                Tables::LT_SLIDER_MAGICS_GATHER.0.as_ptr() as *const i64,
                8,
            );

            let const_shamt_selector = _mm512_set_epi8(
                0, 0, 0, 0, 0, 0, 0, 63, //
                0, 0, 0, 0, 0, 0, 0, 55, //
                0, 0, 0, 0, 0, 0, 0, 47, //
                0, 0, 0, 0, 0, 0, 0, 39, //
                0, 0, 0, 0, 0, 0, 0, 31, //
                0, 0, 0, 0, 0, 0, 0, 23, //
                0, 0, 0, 0, 0, 0, 0, 15, //
                0, 0, 0, 0, 0, 0, 0, 7, //
            );
            let shamt_x8 =
                _mm512_maskz_shuffle_epi8(0x0101010101010101, magics_x8, const_shamt_selector);

            let occupancy_indices_x8 = _mm512_srlv_epi64(
                _mm512_mullo_epi64(_mm512_and_epi64(full_board_x8, masks_x8), magics_x8),
                shamt_x8,
            );

            let movement_gather_indices_x8 = _mm512_add_epi64(
                _mm512_sllv_epi64(piece_indices, gather_shifts_moves_x8),
                occupancy_indices_x8,
            );

            let slider_movement_mask_x8 = _mm512_mask_i64gather_epi64(
                _mm512_setzero_si512(),
                mask,
                _mm512_add_epi64(movement_gather_indices_x8, gather_offsets_moves_x8),
                tables.slider_combined_move_masks.as_ptr() as *const i64,
                8,
            );

            slider_movement_mask_x8

            // let slider_index_x64 = _mm512_permutexvar_epi8(
            //     _mm512_set_epi8(
            //         56, 56, 56, 56, 56, 56, 56, 56, 48, 48, 48, 48, 48, 48, 48, 48, //
            //         40, 40, 40, 40, 40, 40, 40, 40, 32, 32, 32, 32, 32, 32, 32, 32, //
            //         24, 24, 24, 24, 24, 24, 24, 24, 16, 16, 16, 16, 16, 16, 16, 16, //
            //         8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, //
            //     ),
            //     piece_indices,
            // );

            // let mut permuted = _mm512_permutexvar_epi8(
            //     slider_index_x64,
            //     _mm512_load_si512(
            //         Tables::LT_ROOK_OCCUPANCY_MAGICS_PARTITIONED.0[0].as_ptr() as *const __m512i
            //     ),
            // );
            // permuted = _mm512_mask_permutexvar_epi8(
            //     permuted,
            //     BISHOP_MASK,
            //     slider_index_x64,
            //     _mm512_load_si512(
            //         Tables::LT_BISHOP_OCCUPANCY_MAGICS_PARTITIONED.0[0].as_ptr() as *const __m512i
            //     ),
            // );

            // macro_rules! load_magics_part {
            //     ($base:ident, $mask:expr, $index:expr) => {
            //         _mm512_mask_permutexvar_epi8(
            //             _mm512_mask_permutexvar_epi8(
            //                 $base,
            //                 ($mask) & ROOK_MASK,
            //                 slider_index_x64,
            //                 _mm512_load_si512(
            //                     Tables::LT_ROOK_OCCUPANCY_MAGICS_PARTITIONED.0[$index].as_ptr()
            //                         as *const __m512i,
            //                 ),
            //             ),
            //             ($mask) & BISHOP_MASK,
            //             slider_index_x64,
            //             _mm512_load_si512(
            //                 Tables::LT_BISHOP_OCCUPANCY_MAGICS_PARTITIONED.0[$index].as_ptr()
            //                     as *const __m512i,
            //             ),
            //         )
            //     };
            // }

            // permuted = load_magics_part!(permuted, 0x0202020202020202u64, 1);
            // permuted = load_magics_part!(permuted, 0x0404040404040404u64, 2);
            // permuted = load_magics_part!(permuted, 0x0808080808080808u64, 3);
            // permuted = load_magics_part!(permuted, 0x1010101010101010u64, 4);
            // permuted = load_magics_part!(permuted, 0x2020202020202020u64, 5);
            // permuted = load_magics_part!(permuted, 0x4040404040404040u64, 6);
            // permuted = load_magics_part!(permuted, 0x8080808080808080u64, 7);

            // return permuted;
        }
    }
}

mod tests {
    const PERFT_MOVE_VERIFICATION: bool = true;
    const MOVE_VERIFICATION_ENGINE: &str = "bin/stockfish.exe";

    use std::collections::{BTreeSet, LinkedList};
    use std::io::Write;
    use std::process::{Command, Stdio};

    use super::*;

    fn rdtsc() -> u64 {
        unsafe { std::arch::x86_64::_rdtsc() }
    }

    fn rdtscp() -> u64 {
        unsafe { std::arch::x86_64::__rdtscp(&mut 0u32) }
    }

    fn run_verification_perft(
        depth: u8,
        fen: &'static str,
        moves: &Vec<String>,
    ) -> Vec<(String, u64)> {
        let input = vec![
            format!("position fen {} moves {}", fen, moves.join(" ")),
            format!("go perft {}", depth),
            "quit".to_string(),
        ]
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>();

        let mut engine = Command::new(MOVE_VERIFICATION_ENGINE)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to start engine");

        let mut stdin = engine.stdin.take().expect("Failed to open stdin");

        std::thread::spawn(move || {
            stdin
                .write_all(input.join("\n").as_bytes())
                .expect("Failed to write to stdin");
        });

        let output = engine.wait_with_output().expect("Failed to read stdout");

        let output_string = String::from_utf8_lossy(&output.stdout).to_string();
        let output_lines = output_string.split("\n").collect::<Vec<_>>();

        let square_results = output_lines
            .iter()
            .filter_map(|&line| {
                if line.starts_with("Stockfish") {
                    return None;
                }
                if line.starts_with("info") {
                    return None;
                }
                if !line.contains(": ") {
                    return None;
                }
                if line.starts_with("Nodes searched") {
                    return None;
                }

                let parts: Vec<&str> = line.split(": ").collect();
                if parts.len() != 2 {
                    return None;
                }

                let square = parts[0].trim();
                let count = parts[1].trim().parse::<u64>().ok()?;

                Some((square.to_string(), count))
            })
            .collect::<Vec<_>>();

        square_results
    }

    struct PerftTestContext {
        board: ChessGame,
        tables: Tables,
        depth: u8,
        fen: &'static str,
        moves: Vec<String>,
        state_validation: bool,
    }

    impl PerftTestContext {
        fn new(depth: u8, state_validation: bool, fen: &'static str) -> Self {
            let tables = Tables::new();
            let mut board = ChessGame::new();

            assert!(board.load_fen(fen, &tables).is_ok());

            Self {
                board,
                depth,
                fen,
                tables,
                moves: Vec::new(),
                state_validation,
            }
        }

        fn run(&mut self) -> u64 {
            self.perft_verification(self.depth)
        }

        fn perft_verification(&mut self, depth: u8) -> u64 {
            let mut perft_results = Vec::new();

            let mut scratch = MovegenScratch::new();

            let perft_result = if self.state_validation {
                self.board.perft::<true>(
                    depth,
                    &self.tables,
                    Some(&mut perft_results),
                    &mut scratch,
                )
            } else {
                self.board.perft::<false>(
                    depth,
                    &self.tables,
                    Some(&mut perft_results),
                    &mut scratch,
                )
            };

            if !PERFT_MOVE_VERIFICATION {
                return perft_result;
            }

            let verification_results = run_verification_perft(depth, self.fen, &self.moves);

            let all_moves = BTreeSet::from_iter(
                perft_results
                    .iter()
                    .map(|(mv, _)| mv.clone())
                    .chain(verification_results.iter().map(|(mv, _)| mv.clone())),
            );

            for move_str in all_moves {
                let verify_count = match verification_results
                    .iter()
                    .find(|(s, _)| *s == move_str)
                    .map(|(_, c)| *c)
                {
                    Some(count) => count,
                    None => {
                        assert!(
                            false,
                            "Move {} not found in verification engine results at depth {}. Fen {} moves {}",
                            move_str,
                            depth,
                            self.fen,
                            self.moves.join(" ")
                        );
                        return perft_result;
                    }
                };

                let perft_count = match perft_results
                    .iter()
                    .find(|(s, _)| *s == move_str)
                    .map(|(_, c)| *c)
                {
                    Some(perft_count) => perft_count,
                    None => {
                        assert!(
                            false,
                            "Move {} not found in perft results at depth {}. Fen {} moves {}",
                            move_str,
                            depth,
                            self.fen,
                            self.moves.join(" ")
                        );
                        return perft_result;
                    }
                };

                if verify_count != perft_count {
                    if depth == 1 {
                        assert!(
                            false,
                            "Mismatch at depth {}: {}: {} vs verification engine: {}. Fen {} moves {}",
                            depth,
                            move_str,
                            perft_count,
                            verify_count,
                            self.fen,
                            self.moves.join(" ")
                        );
                        return perft_result;
                    }
                    let board_copy = self.board.clone();

                    let mv = self.board.fix_move(util::create_move(&move_str));
                    assert!(unsafe { self.board.make_move(mv, &self.tables) });

                    self.moves.push(move_str.clone());

                    self.perft_verification(depth - 1);

                    self.board = board_copy;

                    return perft_result;
                }
            }

            perft_result
        }
    }

    #[test]
    fn test_fixmove() {
        let mut board = ChessGame::new();
        let tables = Tables::new();
        [
            "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1",
            "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w - - 0 1",
            "4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1",
            "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1",
            "k2K4/8/p7/1P6/8/8/8/8 w - - 0 1",
            "r7/1P6/8/8/8/8/8/k1K5 w - - 0 1",
        ]
        .iter()
        .for_each(|&fen| {
            assert!(board.load_fen(fen, &tables).is_ok());

            let mut move_list = [0u16; 256];
            let move_count = board.gen_moves_slow(&tables, &mut move_list);

            for i in 0..move_count {
                let mv = move_list[i];

                let mut board_copy = board.clone();

                let is_legal_move = unsafe { board_copy.make_move(mv, &tables) }
                    && !board_copy.in_check_slow(&tables, !board_copy.b_move());

                if !is_legal_move {
                    continue;
                }

                let move_string = util::move_string(mv);
                let recreated_move = board.fix_move(util::create_move(move_string.as_str()));

                assert_eq!(
                    recreated_move, mv,
                    "Fix move failed for move {}. Fen: {} Expected: {:016b}, got: {:016b}",
                    move_string, fen, mv, recreated_move
                );
            }
        });
    }

    #[test]
    fn test_zobrist_hashing() {
        let mut board = ChessGame::new();
        let tables = Tables::new();
        [
            ("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1", 3),
            ("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w - - 0 1", 3),
            ("4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1", 3),
            ("4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1", 3),
            (
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
                4,
            ),
            (
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
                4,
            ),
            (
                "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
                4,
            ),
        ]
        .iter()
        .for_each(|&(fen, depth)| {
            assert!(board.load_fen(fen, &tables).is_ok());

            fn check_keys_to_depth(
                fen: &str,
                depth: usize,
                board: &mut ChessGame,
                tables: &Tables,
            ) {
                if depth == 0 {
                    return;
                }

                let mut move_list = [0u16; 256];
                let move_count = board.gen_moves_slow(&tables, &mut move_list);

                for i in 0..move_count {
                    let mv = move_list[i];

                    let board_copy = board.clone();

                    let is_legal_move = unsafe { board.make_move(mv, &tables) }
                        && !board.in_check_slow(&tables, !board.b_move());

                    if !is_legal_move {
                        *board = board_copy;
                        continue;
                    }

                    let calculated_zobrist_key = board.calc_initial_zobrist_key(&tables);

                    assert_eq!(
                        board.zobrist_key(),
                        calculated_zobrist_key,
                        "Zobrist hash mismatch for move {}. Fen: {} Expected: {:#X}, got: {:#X}",
                        util::move_string(mv),
                        fen,
                        calculated_zobrist_key,
                        board.zobrist_key(),
                    );

                    check_keys_to_depth(fen, depth - 1, board, tables);

                    *board = board_copy;
                }
            }

            check_keys_to_depth(fen, depth as usize, &mut board, &tables);
        });
    }

    #[test]
    fn test_perft_rnbqkbnr_pppppppp_8_8_8_8_PPPPPPPP_RNBQKBNR() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        assert_eq!(PerftTestContext::new(5, true, fen).run(), 4_865_609);
    }

    #[test]
    fn test_perft_r3k2r_p1ppqpb1_bn2pnp1_3PN3_1p2P3_2N2Q1p_PPPBBPPP_R3K2R() {
        let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
        assert_eq!(PerftTestContext::new(5, false, fen).run(), 193_690_690);
    }

    #[test]
    fn test_perft_8_2p5_3p4_KP5r_1R3p1k_8_4P1P1_8() {
        let fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
        assert_eq!(PerftTestContext::new(7, true, fen).run(), 178_633_661);
    }

    #[test]
    fn test_perft_r3k2r_Pppp1ppp_1b3nbN_nP6_BBP1P3_q4N2_Pp1P2PP_R2Q1RK1() {
        let fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
        let mirrored = "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1";
        assert_eq!(PerftTestContext::new(6, false, fen).run(), 706045033);
        assert_eq!(PerftTestContext::new(6, false, mirrored).run(), 706045033);
    }

    #[test]
    fn test_perft_rnbq1k1r_pp1Pbppp_2p5_8_2B5_8_PPP1NnPP_RNBQK2R() {
        let fen = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
        assert_eq!(PerftTestContext::new(5, true, fen).run(), 89941194);
    }

    #[test]
    fn test_perft_r4rk1_1pp1qppp_p1np1n2_2b1p1B1_2B1P1b1_P1NP1N2_1PP1QPPP_R4RK1() {
        let fen = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";
        assert_eq!(PerftTestContext::new(5, true, fen).run(), 164_075_551);
    }

    #[test]
    fn test_perft_8_PPP4k_8_8_8_8_4Kppp_8() {
        let fen = "8/PPP4k/8/8/8/8/4Kppp/8 w - -";
        assert_eq!(PerftTestContext::new(6, true, fen).run(), 34336777);
    }

    #[test]
    fn test_perft_n1n5_PPPk4_8_8_8_8_4Kppp_5N1N() {
        let fen = "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1";
        assert_eq!(PerftTestContext::new(6, true, fen).run(), 71179139);
    }

    #[test]
    fn test_perft_perfectperft() {
        // https://www.chessprogramming.net/perfect-perft/
        [
            ("1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1", 5, 1063513),
            ("3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", 6, 1134888),
            ("8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1", 6, 1015133),
            ("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 6, 1440467),
            ("5k2/8/8/8/8/8/8/4K2R w K - 0 1", 6, 661072),
            ("3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", 6, 803711),
            ("r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", 4, 1274206),
            ("r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", 4, 1720476),
            ("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 6, 3821001),
            ("8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", 5, 1004658),
            ("4k3/1P6/8/8/8/8/K7/8 w - - 0 1", 6, 217342),
            ("8/P1k5/K7/8/8/8/8/8 w - - 0 1", 6, 92683),
            ("K1k5/8/P7/8/8/8/8/8 w - - 0 1", 6, 2217),
            ("8/k1P5/8/1K6/8/8/8/8 w - - 0 1", 7, 567584),
            ("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", 4, 23527),
        ]
        .iter()
        .for_each(|(fen, depth, expected)| {
            assert_eq!(PerftTestContext::new(*depth, true, fen).run(), *expected);
        });
    }

    #[test]
    fn test_generate_fen() {
        let mut board = ChessGame::new();
        let tables = Tables::new();

        let test_fens = [
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",
            "4k3/8/8/8/3pP3/8/8/4K3 b - e3 0 1",
            "5k2/8/8/8/8/8/8/4K2R w K - 0 1",
        ];

        for &fen in &test_fens {
            assert!(board.load_fen(fen, &tables).is_ok());

            let generated_fen = board.gen_fen();
            assert_eq!(generated_fen, fen);
        }
    }

    // #[test]
    fn bench_perft() {
        let fens = [
            ("StartPos", 6, util::FEN_STARTPOS),
            (
                "MidGame",
                5,
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            ),
            ("EndGame", 5, "8/3PPP2/4K3/8/P2qN3/3k4/3N4/1q6 w - - 0 1"),
        ];
        let tables = Tables::new();

        let mut scratch: Box<[[u16; 8]; 30]> =
            vec![[0u16; 8]; 30].into_boxed_slice().try_into().unwrap();

        let bench = |fen: &'static str, depth: u8, it: usize, scratch: &mut [[u16; 8]; 30]| {
            let mut board = ChessGame::new();
            assert!(board.load_fen(std::hint::black_box(fen), &tables).is_ok());

            let start = rdtscp();

            let perft_result = 0; // board.perft::<false>(depth, &tables, None, scratch);

            let end = rdtscp();

            std::hint::black_box(perft_result);

            let cycles = end - start;

            println!("It#{} took {} Mcycles", it, cycles / 1_000_000);

            cycles
        };

        const ITERATIONS: usize = 5;
        let mut total_cycles = 0;

        for fen in fens.iter() {
            println!("{}:", fen.0);
            for it in 0..ITERATIONS {
                total_cycles += bench(fen.2, fen.1, it, &mut scratch);
            }
            println!();
        }

        println!(
            "Average search time over {} iterations: {} Mcycles",
            ITERATIONS,
            total_cycles / ITERATIONS as u64 / 1_000_000
        );
    }

    // #[test]
    fn bench_makemove() {
        use rand::{Rng, SeedableRng};

        let fen = "1r2k2r/p1P1qpb1/b3pnp1/n2pP3/1p4N1/2N2Q1p/PPPBBPPP/R3K2R w KQk d6 0 5";
        let tables = Tables::new();
        let mut board = ChessGame::new();
        assert!(board.load_fen(std::hint::black_box(fen), &tables).is_ok());
        let board_copy = board.clone();

        let moves = [
            "d2h6",  // Bishop quiet
            "f3d5",  // Queen cap
            "c7b8q", // Promotion+Cap
            "c7c8",  // Promotion
            "e5f6",  // Pawn cap
            "e5d6",  // EpCap
            "e1g1",  // Kingside castle
            "e1c1",  // Queenside castle
        ];

        let mut rng = rand::rngs::StdRng::seed_from_u64(std::hint::black_box(42));

        let mvs = moves
            .iter()
            .enumerate()
            .map(|(i, mv)| board.fix_move(util::create_move(mv)))
            .collect::<Vec<_>>();

        let mut bench = |mv_string: &str, mv: u16, b: bool, it: usize| {
            //let mut move_list = [0u16; 256];
            let start = rdtscp();

            std::hint::black_box(unsafe { board.make_move(mv, &tables) });
            std::hint::black_box(board.in_check_slow(&tables, b));
            // std::hint::black_box(board.gen_moves_slow(&tables, &mut move_list));

            let end = rdtscp();

            board = board_copy; // Reset the board state

            let cycles = end - start;

            cycles
        };

        const ITERATIONS: usize = 10000;

        // Warmup
        for it in 0..ITERATIONS {
            for (index, mv_string) in moves.iter().enumerate() {
                bench(mv_string, mvs[index], false, it);
            }
        }

        let mut total_cycles = 0;
        let mut cycle_total_vec = Vec::with_capacity(ITERATIONS);

        let mut min = u64::MAX;
        let mut max = 0;

        for it in 0..ITERATIONS {
            let index = rng.random_range(0..moves.len());
            let b = rng.random_bool(0.5);
            let cycles = bench(moves[index], mvs[index], b, it);

            total_cycles += cycles;
            cycle_total_vec.push(cycles);

            min = min.min(cycles);
            max = max.max(cycles);
        }

        cycle_total_vec.sort();

        let avg = total_cycles / ITERATIONS as u64;
        let median = cycle_total_vec[cycle_total_vec.len() / 2];

        println!(
            "Average cycles: {} (median {}), min: {}, max: {}",
            avg, median, min, max
        );
    }
}
