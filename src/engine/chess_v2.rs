use crate::{
    engine::tables::{self, Tables},
    nnue::nnue::{self, UpdatableNnue},
    pop_ls1b,
    util::{self, print_m512_epi64},
};
use std::arch::x86_64::*;

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

const MATERIAL_QUEEN: u16 = 700;
const MATERIAL_ROOK: u16 = 350;
const MATERIAL_BISHOP: u16 = 210;
const MATERIAL_KNIGHT: u16 = 210;
const MATERIAL_PAWN: u16 = 70;
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GameState {
    Ongoing,
    Checkmate(util::Side),
    Draw,
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
const _ASSERT_BITBOARDS_SIZE: () = assert!(std::mem::size_of::<Bitboards>() == 128);

#[derive(Debug, Clone, Copy)]
pub struct ChessGame {
    board: Bitboards,
    b_move: bool,
    castles: u8, // 0b[white kingside, white queenside, black kingside, black queenside]
    en_passant: u8, // ep square (or 0 if none)
    half_moves: u32,
    full_moves: u16,
    zobrist_key: u64,
    material: [u16; 2],
    spt: [u8; 64],
    pawn_key: u64,
}
const _ASSERT_CHESS_GAME_SIZE: () = assert!(std::mem::size_of::<ChessGame>() == 256);

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
            pawn_key: 0,
        }
    }

    #[inline(always)]
    pub fn gen_moves_avx512<const CAPTURE_ONLY: bool>(&self, move_list: &mut [u16]) -> usize {
        let mut mv_cursor = 0usize;

        unsafe {
            let friendly_move_offset = (self.b_move as usize) << 3;
            let opponent_move_offset = (!self.b_move as usize) << 3;

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
            const KING_LANES: u8 = 0b00000001;
            const KNIGHT_LANES: u8 = 0b00000110;

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
            const BISHOP_LANES: u8 = 0b10000011;
            const ROOK_LANES: u8 = 0b01111100;

            let bitboard_x8 = _mm512_load_si512(
                self.board.bitboards.as_ptr().add(friendly_move_offset) as *const __m512i,
            );

            let mut sliders_x8 = _mm512_and_epi64(
                _mm512_permutex2var_epi64(bitboard_x8, const_slider_selector, bitboard_x8),
                const_slider_split,
            );

            let mut non_sliders_x8 = _mm512_and_epi64(
                _mm512_permutex2var_epi64(bitboard_x8, const_nonslider_selector, bitboard_x8),
                const_nonslider_split,
            );

            let full_board = self.board.bitboards.iter().fold(0, |acc, &bb| acc | bb);
            let friendly_board = _mm512_reduce_or_epi64(bitboard_x8) as u64;
            let opponent_board = self.board.bitboards
                [opponent_move_offset..opponent_move_offset + 8]
                .iter()
                .fold(0, |acc, &bb| acc | bb);

            let opponent_ep_x8 = _mm512_set1_epi64((1u64 << self.en_passant >> 1 << 1) as i64);

            let const_63_x8 = _mm512_set1_epi64(63);
            let const_1_x8 = _mm512_set1_epi64(1);
            let const_n1_x8 = _mm512_set1_epi64(-1);
            let const_zero_x8 = _mm512_setzero_si512();

            // Flags
            let const_promotion_flag_x8 = _mm512_set1_epi64(MV_FLAGS_PR_QUEEN as u64 as i64);
            let const_epcap_flag_x8 = _mm512_set1_epi64(MV_FLAG_EPCAP as u64 as i64);
            let const_cap_flag_x8 = _mm512_set1_epi64(MV_FLAG_CAP as u64 as i64);
            let const_dpp_flag_x8 = _mm512_set1_epi64(MV_FLAG_DPP as u64 as i64);

            let b_move_rank_offset = (56 * (self.b_move as u64)) as u8;

            let full_board_x8 = _mm512_set1_epi64(full_board as i64);
            let full_board_inv_x8 = _mm512_set1_epi64(!full_board as i64);
            let friendly_board_inv_x8 = _mm512_set1_epi64(!friendly_board as i64);
            let opponent_board_x8 = _mm512_set1_epi64(opponent_board as i64);
            let pawn_promotion_rank_x8 =
                _mm512_set1_epi64((0xFF00000000000000u64 >> b_move_rank_offset) as i64);
            let pawn_double_push_rank_x8 =
                _mm512_set1_epi64((0xFF000000u64 << ((self.b_move as usize) << 3)) as i64);

            let pawn_push_offset_ranks =
                (opponent_move_offset as u64 + b_move_rank_offset as u64) as i64;
            let pawn_push_rank_rolv_offset_x8 = _mm512_set1_epi64(pawn_push_offset_ranks); // white=8, black=56

            let mut one_of_each_slider_index_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(sliders_x8));
            let mut one_of_each_non_slider_index_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_sliders_x8));
            let mut active_pieces_non_slider_mask =
                _mm512_cmpneq_epi64_mask(one_of_each_non_slider_index_x8, const_n1_x8);
            let mut active_pieces_slider_mask =
                _mm512_cmpneq_epi64_mask(one_of_each_slider_index_x8, const_n1_x8);

            macro_rules! push_moves {
                ($mask:expr, $moves_epi16_x8:ident) => {{
                    let mask = $mask;
                    _mm_mask_compressstoreu_epi16(
                        move_list.as_mut_ptr().add(mv_cursor) as *mut i16,
                        mask,
                        $moves_epi16_x8,
                    );
                    mv_cursor += mask.count_ones() as usize;
                }};
            }

            loop {
                let pawn_mask = PAWN_LANES & active_pieces_non_slider_mask;

                let one_of_each_non_slider_sq_mask_x8 =
                    _mm512_sllv_epi64(const_1_x8, one_of_each_non_slider_index_x8);

                let mut slider_moves_x8 = Self::calc_slider_moves_avx512_x8(
                    full_board_x8,
                    one_of_each_slider_index_x8,
                    ROOK_LANES & active_pieces_slider_mask,
                    BISHOP_LANES & active_pieces_slider_mask,
                );

                // print_m512("slider_moves_x8", &slider_moves_x8);

                // 0x0 = rook, 0x40 = bishop
                // let const_slider_gather_magic_masks_offsets_x8 =
                //     _mm512_set_epi64(0x40, 0, 0, 0, 0, 0, 0x40, 0x40);
                // let const_slider_gather_moves_shifts_x8 = _mm512_set_epi64(
                //     Tables::BISHOP_OCCUPANCY_BITS as i64,
                //     Tables::ROOK_OCCUPANCY_BITS as i64,
                //     Tables::ROOK_OCCUPANCY_BITS as i64,
                //     Tables::ROOK_OCCUPANCY_BITS as i64,
                //     Tables::ROOK_OCCUPANCY_BITS as i64,
                //     Tables::ROOK_OCCUPANCY_BITS as i64,
                //     Tables::BISHOP_OCCUPANCY_BITS as i64,
                //     Tables::BISHOP_OCCUPANCY_BITS as i64,
                // );
                // const BISHOP_MV_GATHER_OFFSET: i64 = (64 * Tables::ROOK_OCCUPANCY_MAX) as i64;
                // let const_moves_gather_base_offsets_x8 = _mm512_set_epi64(
                //     BISHOP_MV_GATHER_OFFSET,
                //     0,
                //     0,
                //     0,
                //     0,
                //     0,
                //     BISHOP_MV_GATHER_OFFSET,
                //     BISHOP_MV_GATHER_OFFSET,
                // );
                // let mut slider_moves_x8 = Self::gather_slider_moves_avx512_x8(
                //     tables,
                //     full_board_x8,
                //     one_of_each_slider_index_x8,
                //     const_slider_gather_magic_masks_offsets_x8,
                //     const_moves_gather_base_offsets_x8,
                //     const_slider_gather_moves_shifts_x8,
                //     active_pieces_slider_mask,
                // );

                // let friendly_move_offset_x8 = _mm512_set1_epi64(friendly_move_offset as i64);
                // let mut non_slider_moves_x8 = _mm512_mask_i64gather_epi64(
                //     _mm512_setzero_si512(),
                //     active_pieces_non_slider_mask,
                //     _mm512_add_epi64(
                //         _mm512_mullo_epi64(
                //             _mm512_add_epi64(const_nonslider_selector, friendly_move_offset_x8),
                //             const_64_x8,
                //         ),
                //         one_of_each_non_slider_index_x8,
                //     ),
                //     Tables::LT_NON_SLIDER_MASKS_GATHER.0.as_ptr() as *const i64,
                //     8,
                // );

                let mut non_slider_moves_x8 = Self::calc_non_slider_moves_avx512_x8(
                    self.b_move,
                    one_of_each_non_slider_sq_mask_x8,
                    KNIGHT_LANES,
                    KING_LANES,
                    PAWN_LANES,
                    active_pieces_non_slider_mask,
                );

                if CAPTURE_ONLY {
                    // Mask out moves that don't capture opponent pieces
                    slider_moves_x8 = _mm512_and_si512(slider_moves_x8, opponent_board_x8);
                    non_slider_moves_x8 = _mm512_and_si512(non_slider_moves_x8, opponent_board_x8);
                } else {
                    slider_moves_x8 = _mm512_and_si512(slider_moves_x8, friendly_board_inv_x8);
                    non_slider_moves_x8 =
                        _mm512_and_si512(non_slider_moves_x8, friendly_board_inv_x8);
                }

                // Pawn push moves
                if !CAPTURE_ONLY {
                    let pawn_push_single_bit_x8 = _mm512_maskz_and_epi64(
                        pawn_mask,
                        _mm512_rolv_epi64(
                            one_of_each_non_slider_sq_mask_x8,
                            pawn_push_rank_rolv_offset_x8,
                        ),
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

                    push_moves!(pawn_push_single_mask, pawn_push_single_move_epi16_x8);
                    push_moves!(pawn_push_double_mask, pawn_push_double_move_epi16_x8);
                }

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
                    let mv_epi16_x8 = _mm512_cvtepi64_epi16(non_slider_full_move_x8);

                    let quiet_mask = !pawn_mask
                        & _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, full_board_inv_x8);
                    let cap_or_ep_mask = cap_mask | ep_mask;

                    push_moves!(cap_or_ep_mask | quiet_mask, mv_epi16_x8);

                    let slider_dst_sq_bit_x8 = _mm512_sllv_epi64(const_1_x8, slider_dst_sq_x8);

                    let s_all_mask = _mm512_cmpneq_epi64_mask(slider_dst_sq_x8, const_n1_x8);

                    let s_cap_mask =
                        _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_board_x8);

                    let slider_full_move_x8 = _mm512_or_epi64(
                        _mm512_slli_epi64(slider_dst_sq_x8, 6),
                        _mm512_mask_or_epi64(
                            one_of_each_slider_index_x8,
                            s_cap_mask,
                            one_of_each_slider_index_x8,
                            const_cap_flag_x8,
                        ),
                    );

                    // Convert full move to 16-bit format
                    let s_mv_epi16_x8 = _mm512_cvtepi64_epi16(slider_full_move_x8);

                    push_moves!(s_all_mask, s_mv_epi16_x8);

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
            if !CAPTURE_ONLY {
                let king_bitboard =
                    self.board.bitboards[PieceIndex::WhiteKing as usize + friendly_move_offset];
                let king_square = king_bitboard.trailing_zeros() as u16;

                *move_list.get_unchecked_mut(mv_cursor as usize) =
                    ((king_square.wrapping_add(2)) << 6) | king_square | MV_FLAGS_CASTLE_KING;
                mv_cursor += self.is_kingside_castle_allowed(self.b_move) as usize;

                *move_list.get_unchecked_mut(mv_cursor as usize) =
                    ((king_square.wrapping_sub(2)) << 6) | king_square | MV_FLAGS_CASTLE_QUEEN;
                mv_cursor += self.is_queenside_castle_allowed(self.b_move) as usize;
            }

            mv_cursor
        }
    }

    #[inline(always)]
    pub fn estimate_move_count(&mut self, b_move: bool, _tables: &Tables) -> usize {
        let b_move_save = self.b_move;
        self.b_move = b_move;

        let mut move_list = [0u16; 256];
        let move_count = self.gen_moves_avx512::<false>(&mut move_list);

        self.b_move = b_move_save;

        move_count
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
    pub fn in_check(&self, tables: &Tables, b_move: bool) -> bool {
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

            // King might not be able to attack all squares if pinned
            // but it does not matter for castling check
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

    pub fn count_king_square_attackers_slow(
        &self,
        sq_index: u8,
        b_move: bool,
        tables: &Tables,
    ) -> usize {
        let opponent_bitboards = &self.board.bitboards[((!b_move as usize) << 3)..];

        let mut attackers = 0u64;

        unsafe {
            let pawn_attack_mask =
                *Tables::LT_PAWN_CAPTURE_MASKS[b_move as usize].get_unchecked(sq_index as usize);

            attackers |= opponent_bitboards[PieceIndex::WhitePawn as usize] & pawn_attack_mask;

            let knight_attack_mask = *Tables::LT_KNIGHT_MOVE_MASKS.get_unchecked(sq_index as usize);
            attackers |= opponent_bitboards[PieceIndex::WhiteKnight as usize] & knight_attack_mask;

            // King cant threaten another king directly
            // let king_attack_mask = *Tables::LT_KING_MOVE_MASKS.get_unchecked(sq_index as usize);
            // attackers |= opponent_bitboards[PieceIndex::WhiteKing as usize] & king_attack_mask;

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

            attackers |= (opponent_rook_board | opponent_queen_board) & rook_moves;
            attackers |= (opponent_bishop_board | opponent_queen_board) & bishop_moves;

            attackers.count_ones() as usize
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
                    for promotion_flag in [MV_FLAGS_PR_QUEEN] {
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
                            for promotion_flag in [MV_FLAGS_PR_QUEEN] {
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
                            for promotion_flag in [MV_FLAGS_PR_QUEEN] {
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

    #[inline(always)]
    pub fn piece_at(&self, sq_index: u8) -> usize {
        unsafe { *self.spt.get_unchecked(sq_index as usize) as usize }
    }

    /// Makes a move on the board. If it returns true, the move was made, otherwise it was not and
    /// no state was changed.
    ///
    /// Safety: the given move must always be pseudovalid, e.g a move that must be valid on the chess board, with
    /// the exception of leaving the king in check or castling without checking threats to the passed squares. In
    /// the latter case, the function returns false. The former case must be handled by the caller.
    #[inline(always)]
    pub unsafe fn make_move(&mut self, mv: u16, tables: &Tables) -> bool {
        unsafe { self.make_move_nnue(mv, tables).is_some() }
    }

    /// Makes a move on the board. Same as make_move but also updates the given NNUE state if provided.
    #[inline(always)]
    pub unsafe fn make_move_nnue(&mut self, mv: u16, tables: &Tables) -> Option<nnue::NnueUpdate> {
        let zb_keys = &tables.zobrist_hash_keys;

        let from_sq = (mv & 0x3F) as u8;
        let from_bit: u64 = 1 << from_sq;

        let to_sq = ((mv >> 6) & 0x3F) as u8;
        let to_bit: u64 = 1 << to_sq;

        let move_flags = mv & MV_FLAGS;
        let mut next_ep_square = 0;

        let friendly_offset = (self.b_move as usize) << 3;

        let from_piece = self.spt[from_sq as usize] as usize;
        let to_piece = self.spt[to_sq as usize] as usize;
        let is_capture = (move_flags & MV_FLAG_CAP) != 0;
        let mut to_square_piece = from_piece;

        let nnue_update = match move_flags {
            MV_FLAGS_CASTLE_KING | MV_FLAGS_CASTLE_QUEEN => {
                const CASTLING_OFFSETS: [[i8; 3]; 4] = [
                    [0, 0, 0],
                    [0, 0, 0],
                    [1, 1, -1],  // king
                    [-1, -2, 1], // queen
                ];

                let square_offsets =
                    unsafe { *CASTLING_OFFSETS.get_unchecked((move_flags >> 12) as usize) };

                // assert!(from_sq.wrapping_add(square_offsets[1] as u8) == to_sq);

                if [
                    from_sq,
                    from_sq.wrapping_add(square_offsets[0] as u8),
                    to_sq,
                ]
                .iter()
                .any(|&square| self.is_king_square_attacked(square, self.b_move, tables))
                {
                    return None;
                }

                let full_board = self.board.bitboards.iter().fold(0, |acc, &bb| acc | bb);

                if move_flags == MV_FLAGS_CASTLE_KING && (full_board & (0b11 << (from_sq + 1))) != 0
                {
                    return None;
                } else if move_flags == MV_FLAGS_CASTLE_QUEEN
                    && (full_board & (0b111 << (from_sq - 3))) != 0
                {
                    return None;
                }

                let rook_from_sq = (to_sq as usize).wrapping_add(square_offsets[1] as usize);
                let rook_to_sq = (to_sq as usize).wrapping_add(square_offsets[2] as usize);

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

                nnue::NnueUpdate::castle(
                    rook_piece_id as u8,
                    rook_from_sq as u8,
                    rook_to_sq as u8,
                    (PieceIndex::WhiteKing as usize + friendly_offset) as u8,
                    from_sq,
                    to_sq,
                )
            }
            MV_FLAG_EPCAP => {
                let (cap_piece_id, cap_pawn_sq) = if self.b_move {
                    (PieceIndex::WhitePawn as usize, to_sq + 8)
                } else {
                    (PieceIndex::BlackPawn as usize, to_sq - 8)
                };

                debug_assert!(cap_pawn_sq != from_sq);

                self.board.bitboards[cap_piece_id] &= !(1 << cap_pawn_sq);

                unsafe {
                    // Safety: cap_pawn_sq is guaranteed to be in board range [0, =63]
                    *self.spt.get_unchecked_mut(cap_pawn_sq as usize) = 0;
                };

                // Remove captured pawn from Zobrist key
                unsafe {
                    // Safety: cap_pawn_sq is guaranteed to be in board range [0, =63]
                    self.zobrist_key ^= zb_keys
                        .hash_piece_squares_new
                        .get_unchecked(cap_piece_id)
                        .get_unchecked(cap_pawn_sq as usize);

                    self.pawn_key ^= tables
                        .zobrist_hash_keys
                        .hash_pawn_squares
                        .get_unchecked(cap_piece_id)
                        .get_unchecked(cap_pawn_sq as usize);
                }

                self.material[!self.b_move as usize] -= MATERIAL_TABLE[cap_piece_id];

                nnue::NnueUpdate::capture(
                    from_piece as u8,
                    from_piece as u8,
                    from_sq,
                    to_sq,
                    cap_piece_id as u8,
                    cap_pawn_sq,
                )
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

                nnue::NnueUpdate::quiet(from_piece as u8, from_piece as u8, from_sq, to_sq)
            }
            _ => {
                if move_flags & MV_FLAG_PROMOTION != 0 {
                    let promotion_piece = match move_flags & MV_FLAGS_PR_MASK {
                        MV_FLAGS_PR_QUEEN => PieceIndex::WhiteQueen as usize + friendly_offset,
                        MV_FLAGS_PR_KNIGHT => PieceIndex::WhiteKnight as usize + friendly_offset,
                        MV_FLAGS_PR_ROOK => PieceIndex::WhiteRook as usize + friendly_offset,
                        MV_FLAGS_PR_BISHOP => PieceIndex::WhiteBishop as usize + friendly_offset,
                        _ => unreachable!(),
                    };

                    to_square_piece = promotion_piece;

                    unsafe {
                        self.material[self.b_move as usize] += MATERIAL_TABLE
                            .get_unchecked(promotion_piece)
                            - MATERIAL_TABLE.get_unchecked(from_piece);
                    }
                }

                if is_capture {
                    nnue::NnueUpdate::capture(
                        from_piece as u8,
                        to_square_piece as u8,
                        from_sq,
                        to_sq,
                        to_piece as u8,
                        to_sq,
                    )
                } else {
                    nnue::NnueUpdate::quiet(from_piece as u8, to_square_piece as u8, from_sq, to_sq)
                }
            }
        };

        debug_assert!(to_piece == 0 || is_capture);

        self.spt[from_sq as usize] = 0;
        self.spt[to_sq as usize] = to_square_piece as u8;

        unsafe {
            // Safety:
            //  - piece_from is guaranteed to be valid piece id
            //  - piece_to is guaranteed to be a valid piece id OR 0

            // to_piece will write to the white null-piece slot, but due to & it will remain zeros
            *self.board.bitboards.get_unchecked_mut(to_piece) &= !to_bit;
            *self.board.bitboards.get_unchecked_mut(from_piece) ^= from_bit;
            *self.board.bitboards.get_unchecked_mut(to_square_piece) |= to_bit;

            self.material[!self.b_move as usize] -= MATERIAL_TABLE.get_unchecked(to_piece);

            // Remove moved piece from from_sq and add it to to_sq
            self.zobrist_key ^=
                zb_keys.hash_piece_squares_new.get_unchecked(from_piece)[from_sq as usize];
            self.zobrist_key ^= zb_keys
                .hash_piece_squares_new
                .get_unchecked(to_square_piece)[to_sq as usize];
            self.pawn_key ^= tables
                .zobrist_hash_keys
                .hash_pawn_squares
                .get_unchecked(from_piece)[from_sq as usize];
            self.pawn_key ^= tables
                .zobrist_hash_keys
                .hash_pawn_squares
                .get_unchecked(to_square_piece)[to_sq as usize];

            // Remove captured piece from Zobrist key
            self.zobrist_key ^=
                zb_keys.hash_piece_squares_new.get_unchecked(to_piece)[to_sq as usize];
            self.pawn_key ^= tables
                .zobrist_hash_keys
                .hash_pawn_squares
                .get_unchecked(to_piece)[to_sq as usize];

            // Remove en passant square from Zobrist key
            self.zobrist_key ^= zb_keys
                .hash_en_passant_squares
                .get_unchecked(self.en_passant as usize);
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

        self.full_moves += self.b_move as u16;
        self.en_passant = next_ep_square;

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

        Some(nnue_update)
    }

    #[inline(always)]
    pub fn make_null_move(&mut self, tables: &Tables) -> u8 {
        unsafe {
            let zb_keys = &tables.zobrist_hash_keys;

            self.b_move = !self.b_move;
            self.zobrist_key ^= zb_keys.hash_side_to_move;
            self.zobrist_key ^= zb_keys
                .hash_en_passant_squares
                .get_unchecked(self.en_passant as usize);

            let old_ep = self.en_passant;
            self.en_passant = 0;

            old_ep
        }
    }

    #[inline(always)]
    pub fn rollback_null_move(&mut self, ep_square: u8, tables: &Tables) {
        unsafe {
            let zb_keys = &tables.zobrist_hash_keys;

            self.b_move = !self.b_move;
            self.zobrist_key ^= zb_keys
                .hash_en_passant_squares
                .get_unchecked(ep_square as usize);
            self.zobrist_key ^= zb_keys.hash_side_to_move;

            self.en_passant = ep_square;
        }
    }

    pub fn perft(
        &mut self,
        depth: u8,
        tables: &Tables,
        mut moves: Option<&mut Vec<(String, u64)>>,
        mut nnue: Option<&mut impl nnue::UpdatableNnue>,
    ) -> u64 {
        if depth == 0 {
            return 1;
        }

        let mut node_count = 0;

        let mut move_list = [0u16; 220];

        let move_count = self.gen_moves_avx512::<false>(&mut move_list[2..]);

        let board_copy = self.clone();

        let mut i = 2;
        while i < move_count + 2 {
            let mv = move_list[i];
            i += 1;

            if mv & MV_FLAGS_PR_MASK == MV_FLAGS_PR_QUEEN {
                i -= 3;

                let mv_unpromoted = mv & !MV_FLAGS_PR_MASK;
                move_list[i] = mv_unpromoted | MV_FLAGS_PR_KNIGHT; // Second promotion to check
                move_list[i + 1] = mv_unpromoted | MV_FLAGS_PR_ROOK; // Third promotion to check
                move_list[i + 2] = mv_unpromoted | MV_FLAGS_PR_BISHOP; // Fourth promotion to check
            }

            let nnue_update = unsafe { self.make_move_nnue(mv, tables) };

            if nnue_update.is_some() && !self.in_check(tables, !self.b_move) {
                let (board_key, pawn_key) = self.calc_initial_zobrist_key(tables);
                assert!(
                    self.zobrist_key == board_key,
                    "Zobrist key mismatch after move {}",
                    util::move_string_dbg(mv),
                );
                assert!(
                    self.pawn_key == pawn_key,
                    "Pawn key mismatch after move {}",
                    util::move_string_dbg(mv)
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

                if let Some(nnue) = nnue.as_mut() {
                    nnue.make_move(nnue_update.unwrap());
                }
                let nodes = self.perft(depth - 1, tables, None, nnue.as_mut().map(|n| &mut **n));
                node_count += nodes;

                if let Some(ref mut moves) = moves {
                    moves.push((util::move_string(mv), nodes));
                }

                if let Some(nnue) = nnue.as_mut() {
                    nnue.rollback_move();
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

                        self.en_passant = file_index + 1;
                    }
                    '1'..='8' => {
                        if self.en_passant == 0 {
                            return Err("En passant square specified without file".into());
                        }

                        let rank_index = c as u8 - b'1';

                        if rank_index > 7 {
                            return Err(format!("Invalid en passant rank '{}'", c));
                        }

                        self.en_passant = (rank_index * 8) | (self.en_passant - 1);
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
                        self.full_moves = self.full_moves * 10 + (c as u8 - b'0') as u16;
                    }
                    _ => return Err(format!("Invalid full move character '{}'", c)),
                },
            }
        }

        (self.zobrist_key, self.pawn_key) = self.calc_initial_zobrist_key(tables);
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
            if self.in_check(tables, b_move) {
                GameState::Checkmate(util::Side::from(!b_move))
            } else {
                GameState::Draw
            }
        } else if self.half_moves >= 100 {
            GameState::Draw
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

    pub fn calc_initial_zobrist_key(&self, tables: &Tables) -> (u64, u64) {
        let zb_keys = &tables.zobrist_hash_keys;

        let mut board_key = 0u64;
        let mut pawn_key = zb_keys.no_pawn_key;

        for square in 0..64 {
            let piece_id = self.piece_at_slow(1 << square);
            board_key ^= zb_keys.hash_piece_squares_new[piece_id][square as usize];

            match PieceIndex::from(piece_id) {
                PieceIndex::WhitePawn | PieceIndex::BlackPawn => {
                    pawn_key ^= zb_keys.hash_pawn_squares[piece_id][square as usize];
                }
                _ => {}
            }
        }

        board_key ^= zb_keys.hash_en_passant_squares[self.en_passant as usize];

        board_key ^= zb_keys.hash_castling_rights[self.castles as usize];

        board_key ^= if self.b_move {
            zb_keys.hash_side_to_move
        } else {
            0
        };

        (board_key, pawn_key)
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
    pub fn bitboards(&self) -> &[u64; 16] {
        &self.board.bitboards
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
    pub fn ply(&self) -> u16 {
        self.full_moves * 2 + self.b_move as u16 - 2
    }

    #[inline(always)]
    pub fn full_moves(&self) -> u16 {
        self.full_moves
    }

    #[inline(always)]
    pub fn zobrist_key(&self) -> u64 {
        self.zobrist_key
    }

    #[inline(always)]
    pub fn pawn_key(&self) -> u64 {
        self.pawn_key
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
    pub fn occupancy(&self) -> u64 {
        self.board.bitboards.iter().fold(0u64, |acc, &bb| acc | bb)
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
    fn gather_slider_moves_avx512_x8(
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
        }
    }

    #[inline(always)]
    fn byteswap_epi64_x8(input_x8: __m512i) -> __m512i {
        unsafe {
            let swap_x8 = _mm512_set_epi64(
                0x38393a3b3c3d3e3f,
                0x3031323334353637,
                0x28292a2b2c2d2e2f,
                0x2021222324252627,
                0x18191a1b1c1d1e1f,
                0x1011121314151617,
                0x8090a0b0c0d0e0f,
                0x1020304050607,
            );

            _mm512_shuffle_epi8(input_x8, swap_x8)
        }
    }

    fn ray_moves_with_occ_x8(
        active_mask: u8,
        forward_x8: __m512i,
        line_mask_x8: __m512i,
        sq_mask_x8: __m512i,
        sq_mask_bswap_x8: __m512i,
    ) -> __m512i {
        unsafe {
            let mut forward_x8 = forward_x8;
            let mut reverse_x8 = Self::byteswap_epi64_x8(forward_x8);

            forward_x8 = _mm512_sub_epi64(forward_x8, sq_mask_x8);
            reverse_x8 = _mm512_sub_epi64(reverse_x8, sq_mask_bswap_x8);

            forward_x8 = _mm512_xor_epi64(forward_x8, Self::byteswap_epi64_x8(reverse_x8));
            forward_x8 = _mm512_maskz_and_epi64(active_mask, forward_x8, line_mask_x8);

            forward_x8
        }
    }

    #[inline(always)]
    fn calc_slider_moves_avx512_x8(
        full_board_x8: __m512i,
        one_of_each_index_x8: __m512i,
        rook_mask: u8,
        bish_mask: u8,
    ) -> __m512i {
        unsafe {
            const RAY_A2_A8: u64 = 0x101010101010100u64;
            const RAY_A1_A8: u64 = 0x0101010101010101u64;
            const RAY_A1_H8: u64 = 0x8040201008040201u64;
            const RAY_A8_H1: u64 = 0x0102040810204080u64;

            let slider_sq_x8 = _mm512_sllv_epi64(_mm512_set1_epi64(1), one_of_each_index_x8);
            let slider_sq_bswap_x8 = Self::byteswap_epi64_x8(slider_sq_x8);

            let line_mask_x8 =
                _mm512_rolv_epi64(_mm512_set1_epi64(RAY_A2_A8 as i64), one_of_each_index_x8);

            let sq_file_x8 = _mm512_and_epi64(one_of_each_index_x8, _mm512_set1_epi64(7));
            let sq_rank_x8 = _mm512_and_epi64(one_of_each_index_x8, _mm512_set1_epi64(56));

            let diag_rank_x8 = _mm512_slli_epi64(sq_file_x8, 3);

            let diagonal_mask_x8 = {
                let diag_x8 = _mm512_sub_epi64(diag_rank_x8, sq_rank_x8);
                let diag_neg_x8 = _mm512_sub_epi64(_mm512_setzero_si512(), diag_x8);
                let top_x8 = _mm512_and_epi64(diag_neg_x8, _mm512_srai_epi64(diag_x8, 31));
                let bot_x8 = _mm512_and_epi64(diag_x8, _mm512_srai_epi64(diag_neg_x8, 31));

                let base_mask = _mm512_set1_epi64(RAY_A1_H8 as i64);
                _mm512_sllv_epi64(_mm512_srlv_epi64(base_mask, bot_x8), top_x8)
            };

            let antidiag_mask_x8 = {
                let antid_x8 = _mm512_sub_epi64(
                    _mm512_sub_epi64(_mm512_set1_epi64(56), diag_rank_x8),
                    sq_rank_x8,
                );
                let antid_neg_x8 = _mm512_sub_epi64(_mm512_setzero_si512(), antid_x8);
                let top_x8 = _mm512_and_epi64(antid_neg_x8, _mm512_srai_epi64(antid_x8, 31));
                let bot_x8 = _mm512_and_epi64(antid_x8, _mm512_srai_epi64(antid_neg_x8, 31));

                let base_mask = _mm512_set1_epi64(RAY_A8_H1 as i64);
                _mm512_sllv_epi64(_mm512_srlv_epi64(base_mask, bot_x8), top_x8)
            };

            let full_board_without_sq_x8 = _mm512_andnot_epi64(slider_sq_x8, full_board_x8);

            let moves_file_x8 = Self::ray_moves_with_occ_x8(
                rook_mask,
                _mm512_and_epi64(full_board_without_sq_x8, line_mask_x8),
                line_mask_x8,
                slider_sq_x8,
                slider_sq_bswap_x8,
            );
            let moves_diag_x8 = Self::ray_moves_with_occ_x8(
                bish_mask,
                _mm512_and_epi64(full_board_without_sq_x8, diagonal_mask_x8),
                diagonal_mask_x8,
                slider_sq_x8,
                slider_sq_bswap_x8,
            );
            let moves_anti_x8 = Self::ray_moves_with_occ_x8(
                bish_mask,
                _mm512_and_epi64(full_board_without_sq_x8, antidiag_mask_x8),
                antidiag_mask_x8,
                slider_sq_x8,
                slider_sq_bswap_x8,
            );

            // Calculate rank attacks by translating rank to a1-h1 diagonal, calculating attacks and translating back:
            let sq_propagate_x8 = _mm512_and_epi64(
                _mm512_sllv_epi64(_mm512_set1_epi64(0xFFu64 as i64), diag_rank_x8),
                _mm512_set1_epi64(RAY_A1_H8 as i64),
            );

            let occ_bottom_x8 = _mm512_and_epi64(
                _mm512_srlv_epi64(full_board_without_sq_x8, sq_rank_x8),
                _mm512_set1_epi64(0xFFu64 as i64),
            );
            let occ_propagate_x8 = _mm512_maskz_mullo_epi64(
                rook_mask,
                occ_bottom_x8,
                _mm512_set1_epi64(RAY_A1_A8 as i64),
            );

            let diagonal_rank_moves_x8 = Self::ray_moves_with_occ_x8(
                rook_mask,
                _mm512_and_epi64(occ_propagate_x8, _mm512_set1_epi64(RAY_A1_H8 as i64)),
                _mm512_set1_epi64(RAY_A1_H8 as i64),
                sq_propagate_x8,
                Self::byteswap_epi64_x8(sq_propagate_x8),
            );

            let moves_rank_x8 = _mm512_maskz_sllv_epi64(
                rook_mask,
                _mm512_srlv_epi64(
                    _mm512_maskz_mullo_epi64(
                        rook_mask,
                        diagonal_rank_moves_x8,
                        _mm512_set1_epi64(RAY_A1_A8 as i64),
                    ),
                    _mm512_set1_epi64(56),
                ),
                sq_rank_x8,
            );

            let slider_moves_x8 = _mm512_or_epi64(
                _mm512_ternarylogic_epi64(moves_file_x8, moves_diag_x8, moves_anti_x8, 0xFE),
                moves_rank_x8,
            );

            slider_moves_x8
        }
    }

    #[inline(always)]
    fn calc_non_slider_attack_bitboards_avx512(
        from_sq_x8: __m512i,
        knight_mask: u8,
        king_mask: u8,
        white_pawn_mask: u8,
        black_pawn_mask: u8,
    ) -> (__m512i, __m512i, __m512i, __m512i) {
        unsafe {
            let const_ex_file_a_x8 = _mm512_set1_epi64(tables::EX_A_FILE as i64);
            let const_ex_file_ab_x8 =
                _mm512_set1_epi64(tables::EX_A_FILE as i64 & tables::EX_B_FILE as i64);
            let const_ex_file_gh_x8 =
                _mm512_set1_epi64(tables::EX_G_FILE as i64 & tables::EX_H_FILE as i64);
            let const_ex_file_h_x8 = _mm512_set1_epi64(tables::EX_H_FILE as i64);

            let knight_squares_x8 = _mm512_maskz_ternarylogic_epi64(
                knight_mask,
                _mm512_ternarylogic_epi64(
                    _mm512_and_epi64(const_ex_file_h_x8, _mm512_slli_epi64(from_sq_x8, 15)),
                    _mm512_and_epi64(const_ex_file_a_x8, _mm512_slli_epi64(from_sq_x8, 17)),
                    _mm512_and_epi64(const_ex_file_gh_x8, _mm512_slli_epi64(from_sq_x8, 6)),
                    0xFE,
                ),
                _mm512_ternarylogic_epi64(
                    _mm512_and_epi64(const_ex_file_ab_x8, _mm512_srli_epi64(from_sq_x8, 6)),
                    _mm512_and_epi64(const_ex_file_gh_x8, _mm512_srli_epi64(from_sq_x8, 10)),
                    _mm512_and_epi64(const_ex_file_a_x8, _mm512_srli_epi64(from_sq_x8, 15)),
                    0xFE,
                ),
                _mm512_or_epi64(
                    _mm512_and_epi64(const_ex_file_ab_x8, _mm512_slli_epi64(from_sq_x8, 10)),
                    _mm512_and_epi64(const_ex_file_h_x8, _mm512_srli_epi64(from_sq_x8, 17)),
                ),
                0xFE,
            );

            let king_squares_x8 = _mm512_maskz_ternarylogic_epi64(
                king_mask,
                _mm512_and_epi64(
                    const_ex_file_h_x8,
                    _mm512_ternarylogic_epi64(
                        _mm512_srli_epi64(from_sq_x8, 1),
                        _mm512_srli_epi64(from_sq_x8, 9),
                        _mm512_slli_epi64(from_sq_x8, 7),
                        0xFE,
                    ),
                ),
                _mm512_and_epi64(
                    const_ex_file_a_x8,
                    _mm512_ternarylogic_epi64(
                        _mm512_slli_epi64(from_sq_x8, 1),
                        _mm512_slli_epi64(from_sq_x8, 9),
                        _mm512_srli_epi64(from_sq_x8, 7),
                        0xFE,
                    ),
                ),
                _mm512_or_epi64(
                    _mm512_slli_epi64(from_sq_x8, 8),
                    _mm512_srli_epi64(from_sq_x8, 8),
                ),
                0xFE,
            );

            let pawn_squares_white_x8 = _mm512_maskz_or_epi64(
                white_pawn_mask,
                _mm512_and_epi64(const_ex_file_a_x8, _mm512_slli_epi64(from_sq_x8, 9)),
                _mm512_and_epi64(const_ex_file_h_x8, _mm512_slli_epi64(from_sq_x8, 7)),
            );

            let pawn_squares_black_x8 = _mm512_maskz_or_epi64(
                black_pawn_mask,
                _mm512_and_epi64(const_ex_file_h_x8, _mm512_srli_epi64(from_sq_x8, 9)),
                _mm512_and_epi64(const_ex_file_a_x8, _mm512_srli_epi64(from_sq_x8, 7)),
            );

            (
                knight_squares_x8,
                king_squares_x8,
                pawn_squares_white_x8,
                pawn_squares_black_x8,
            )
        }
    }

    #[inline(always)]
    fn calc_non_slider_moves_avx512_x8(
        b_move: bool,
        non_slider_sq_x8: __m512i,
        knight_mask: u8,
        king_mask: u8,
        pawn_mask: u8,
        active_mask: u8,
    ) -> __m512i {
        unsafe {
            let (knight_moves_x8, king_moves_x8, pawn_moves_white_x8, pawn_moves_black_x8) =
                Self::calc_non_slider_attack_bitboards_avx512(
                    non_slider_sq_x8,
                    knight_mask,
                    king_mask,
                    pawn_mask,
                    pawn_mask,
                );

            let b_move_mask = if b_move { 0xFF } else { 0x00 };

            let pawn_moves_x8 =
                _mm512_mask_blend_epi64(b_move_mask, pawn_moves_white_x8, pawn_moves_black_x8);

            let all_moves_x8 = _mm512_maskz_ternarylogic_epi64(
                active_mask,
                knight_moves_x8,
                king_moves_x8,
                pawn_moves_x8,
                0xFE,
            );

            all_moves_x8
        }
    }

    #[inline(always)]
    pub fn in_check_avx512(
        king_sq_index_0: u8,
        king_sq_index_1: u8,
        bitboards: &[u64; 16],
    ) -> (u64, u64) {
        unsafe {
            const RAY_A2_A8: u64 = 0x101010101010100u64;
            const RAY_A1_A8: u64 = 0x0101010101010101u64;
            const RAY_A1_H8: u64 = 0x8040201008040201u64;
            const RAY_A8_H1: u64 = 0x0102040810204080u64;

            let occupancy = bitboards.iter().fold(0u64, |acc, &bb| acc | bb);

            let king_sq_x8 = _mm512_set_epi64(
                1 << king_sq_index_0, // Diagonal
                1 << king_sq_index_0, // Anti-Diag
                1 << king_sq_index_0, // Line
                1 << king_sq_index_0, // Rank
                1 << king_sq_index_1, // Diagonal
                1 << king_sq_index_1, // Anti-Diag
                1 << king_sq_index_1, // Line
                1 << king_sq_index_1, // Rank
            );

            let slider_checkers_x8 = {
                let king_sq_index_x8 = _mm512_set_epi64(
                    king_sq_index_0 as i64,
                    king_sq_index_0 as i64,
                    king_sq_index_0 as i64,
                    king_sq_index_0 as i64,
                    king_sq_index_1 as i64,
                    king_sq_index_1 as i64,
                    king_sq_index_1 as i64,
                    king_sq_index_1 as i64,
                );

                let king_sq_file_x8 = _mm512_and_epi64(king_sq_index_x8, _mm512_set1_epi64(7));
                let king_sq_rank_x8 = _mm512_and_epi64(king_sq_index_x8, _mm512_set1_epi64(56));

                let king_diag_rank_x8 = _mm512_slli_epi64(king_sq_file_x8, 3);

                let diagonal_mask_x8 = {
                    let diag_x8 = _mm512_sub_epi64(king_diag_rank_x8, king_sq_rank_x8);
                    let diag_neg_x8 = _mm512_sub_epi64(_mm512_setzero_si512(), diag_x8);
                    let top_x8 = _mm512_and_epi64(diag_neg_x8, _mm512_srai_epi64(diag_x8, 31));
                    let bot_x8 = _mm512_and_epi64(diag_x8, _mm512_srai_epi64(diag_neg_x8, 31));

                    let base_mask = _mm512_set1_epi64(RAY_A1_H8 as i64);
                    _mm512_maskz_sllv_epi64(
                        0b00010001,
                        _mm512_srlv_epi64(base_mask, bot_x8),
                        top_x8,
                    )
                };

                let antidiag_mask_x8 = {
                    let antid_x8 = _mm512_sub_epi64(
                        _mm512_sub_epi64(_mm512_set1_epi64(56), king_diag_rank_x8),
                        king_sq_rank_x8,
                    );
                    let antid_neg_x8 = _mm512_sub_epi64(_mm512_setzero_si512(), antid_x8);
                    let top_x8 = _mm512_and_epi64(antid_neg_x8, _mm512_srai_epi64(antid_x8, 31));
                    let bot_x8 = _mm512_and_epi64(antid_x8, _mm512_srai_epi64(antid_neg_x8, 31));

                    let base_mask = _mm512_set1_epi64(RAY_A8_H1 as i64);
                    _mm512_maskz_sllv_epi64(
                        0b00100010,
                        _mm512_srlv_epi64(base_mask, bot_x8),
                        top_x8,
                    )
                };

                let line_mask_x8 = _mm512_maskz_rolv_epi64(
                    0b01000100,
                    _mm512_set1_epi64(RAY_A2_A8 as i64),
                    king_sq_index_x8,
                );

                let occupancy_x8 =
                    _mm512_andnot_epi64(king_sq_x8, _mm512_set1_epi64(occupancy as i64));

                // Calculate rank mask by translating via a1-h1 diagonal
                let king_sq_propagate_x8 = _mm512_mask_and_epi64(
                    king_sq_x8,
                    0b10001000,
                    _mm512_sllv_epi64(_mm512_set1_epi64(0xFFu64 as i64), king_diag_rank_x8),
                    _mm512_set1_epi64(RAY_A1_H8 as i64),
                );

                let occ_propagate_x8 = _mm512_mask_mullo_epi64(
                    occupancy_x8,
                    0b10001000,
                    _mm512_and_epi64(
                        _mm512_srlv_epi64(occupancy_x8, king_sq_rank_x8),
                        _mm512_set1_epi64(0xFFu64 as i64),
                    ),
                    _mm512_set1_epi64(RAY_A1_A8 as i64),
                );

                let occupancy_mask_x8 = _mm512_mask_set1_epi64(
                    _mm512_ternarylogic_epi64(
                        diagonal_mask_x8,
                        antidiag_mask_x8,
                        line_mask_x8,
                        0xFE,
                    ),
                    0b10001000,
                    RAY_A1_H8 as i64,
                );

                let ray_results_x8 = Self::ray_moves_with_occ_x8(
                    0xFF,
                    _mm512_and_epi64(occ_propagate_x8, occupancy_mask_x8),
                    occupancy_mask_x8,
                    king_sq_propagate_x8,
                    Self::byteswap_epi64_x8(king_sq_propagate_x8),
                );

                let threat_squares_x8 = _mm512_mask_sllv_epi64(
                    ray_results_x8,
                    0b10001000,
                    _mm512_srlv_epi64(
                        _mm512_mullo_epi64(ray_results_x8, _mm512_set1_epi64(RAY_A1_A8 as i64)),
                        _mm512_set1_epi64(56),
                    ),
                    king_sq_rank_x8,
                );

                let queen_bitboards = (bitboards[PieceIndex::WhiteQueen as usize]
                    | bitboards[PieceIndex::BlackQueen as usize])
                    as i64;
                let rook_bitboards = (bitboards[PieceIndex::WhiteRook as usize]
                    | bitboards[PieceIndex::BlackRook as usize])
                    as i64;
                let bishop_bitboards = (bitboards[PieceIndex::WhiteBishop as usize]
                    | bitboards[PieceIndex::BlackBishop as usize])
                    as i64;

                let sliders_x8 = _mm512_set_epi64(
                    queen_bitboards | rook_bitboards,
                    queen_bitboards | rook_bitboards,
                    queen_bitboards | bishop_bitboards,
                    queen_bitboards | bishop_bitboards,
                    queen_bitboards | rook_bitboards,
                    queen_bitboards | rook_bitboards,
                    queen_bitboards | bishop_bitboards,
                    queen_bitboards | bishop_bitboards,
                );

                _mm512_and_epi64(threat_squares_x8, sliders_x8)
            };
            let non_slider_checkers_x8 = {
                let (
                    knight_squares_x8,
                    king_squares_x8,
                    pawn_squares_white_x8,
                    pawn_squares_black_x8,
                ) = Self::calc_non_slider_attack_bitboards_avx512(
                    king_sq_x8, //
                    0b00010001, //
                    0b00100010, //
                    0b01000100, //
                    0b10001000, //
                );

                let white_pawns_x8 =
                    _mm512_set1_epi64(bitboards[PieceIndex::WhitePawn as usize] as i64);
                let black_pawns_x8 =
                    _mm512_set1_epi64(bitboards[PieceIndex::BlackPawn as usize] as i64);

                // Switch pawn boards for attack patterns
                let pawn_black_x8 = _mm512_and_epi64(pawn_squares_white_x8, black_pawns_x8);
                let pawn_white_x8 = _mm512_and_epi64(pawn_squares_black_x8, white_pawns_x8);

                let threat_squares_x8 = _mm512_ternarylogic_epi64(
                    knight_squares_x8,
                    king_squares_x8,
                    _mm512_or_si512(pawn_white_x8, pawn_black_x8),
                    0xFE,
                );

                let knight_bitboards = (bitboards[PieceIndex::WhiteKnight as usize]
                    | bitboards[PieceIndex::BlackKnight as usize])
                    as i64;
                let king_bitboards = (bitboards[PieceIndex::WhiteKing as usize]
                    | bitboards[PieceIndex::BlackKing as usize])
                    as i64;

                let non_sliders_x8 = _mm512_set_epi64(
                    0xFFFFFFFF_FFFFFFFFu64 as i64,
                    0xFFFFFFFF_FFFFFFFFu64 as i64,
                    king_bitboards,
                    knight_bitboards,
                    0xFFFFFFFF_FFFFFFFFu64 as i64,
                    0xFFFFFFFF_FFFFFFFFu64 as i64,
                    king_bitboards,
                    knight_bitboards,
                );

                _mm512_and_epi64(threat_squares_x8, non_sliders_x8)
            };

            let all_checkers_x8 = _mm512_or_epi64(slider_checkers_x8, non_slider_checkers_x8);

            let checkers_overlap_01_23_x8 = _mm512_or_epi64(
                all_checkers_x8,
                _mm512_castpd_si512(_mm512_permute_pd(
                    _mm512_castsi512_pd(all_checkers_x8),
                    0b0101_0101,
                )),
            );

            let all_checkers_low = _mm512_or_epi64(
                checkers_overlap_01_23_x8,
                _mm512_permutex_epi64(checkers_overlap_01_23_x8, 0b0010_0010),
            );

            let checkers_sq_1 = _mm_cvtsi128_si64(_mm512_castsi512_si128(all_checkers_low)) as u64;
            let checkers_sq_0 =
                _mm_cvtsi128_si64(_mm512_extracti64x2_epi64(all_checkers_low, 2)) as u64;

            (checkers_sq_0, checkers_sq_1)
        }
    }
}

mod tests {
    const PERFT_MOVE_VERIFICATION: bool = true;
    const MOVE_VERIFICATION_ENGINE: &str = "bin/stockfish.exe";

    use std::collections::BTreeSet;
    use std::io::Write;
    use std::process::{Command, Stdio};

    use crate::nnue::nnue;
    use crate::nnue_load;

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
        nnue: Box<nnue::LazyNnue<128, 1>>,
    }

    impl PerftTestContext {
        fn new(depth: u8, fen: &'static str) -> Self {
            let tables = Tables::new();
            let mut board = ChessGame::new();

            assert!(board.load_fen(fen, &tables).is_ok());

            let mut nnue = nnue_load!("../../nnue/x2.bin", 128);
            nnue.load(&board);

            Self {
                board,
                depth,
                fen,
                tables,
                moves: Vec::new(),
                nnue,
            }
        }

        fn run(&mut self) -> u64 {
            self.perft_verification(self.depth)
        }

        fn perft_verification(&mut self, depth: u8) -> u64 {
            let mut perft_results = Vec::new();

            let perft_result = self.board.perft(
                depth,
                &self.tables,
                Some(&mut perft_results),
                Some(&mut *self.nnue),
            );

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
                    let nnue_update = unsafe { self.board.make_move_nnue(mv, &self.tables) };
                    assert!(nnue_update.is_some());
                    self.nnue.make_move(nnue_update.unwrap());

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
                    && !board_copy.in_check(&tables, !board_copy.b_move());

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
                        && !board.in_check(&tables, !board.b_move());

                    if !is_legal_move {
                        *board = board_copy;
                        continue;
                    }

                    let (board_key, pawn_key) = board.calc_initial_zobrist_key(&tables);

                    assert_eq!(
                        board.zobrist_key(),
                        board_key,
                        "Zobrist hash mismatch for move {}. Fen: {} Expected: {:#X}, got: {:#X}",
                        util::move_string(mv),
                        fen,
                        board_key,
                        board.zobrist_key(),
                    );
                    assert_eq!(
                        board.pawn_key(),
                        pawn_key,
                        "Pawn key mismatch for move {}. Fen: {} Expected: {:#X}, got: {:#X}",
                        util::move_string(mv),
                        fen,
                        pawn_key,
                        board.pawn_key(),
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
        assert_eq!(PerftTestContext::new(5, fen).run(), 4_865_609);
    }

    #[test]
    fn test_perft_r3k2r_p1ppqpb1_bn2pnp1_3PN3_1p2P3_2N2Q1p_PPPBBPPP_R3K2R() {
        let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
        assert_eq!(PerftTestContext::new(5, fen).run(), 193_690_690);
    }

    #[test]
    fn test_perft_8_2p5_3p4_KP5r_1R3p1k_8_4P1P1_8() {
        let fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
        assert_eq!(PerftTestContext::new(7, fen).run(), 178_633_661);
    }

    #[test]
    fn test_perft_r3k2r_Pppp1ppp_1b3nbN_nP6_BBP1P3_q4N2_Pp1P2PP_R2Q1RK1() {
        let fen = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
        let mirrored = "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1";
        assert_eq!(PerftTestContext::new(6, fen).run(), 706045033);
        assert_eq!(PerftTestContext::new(6, mirrored).run(), 706045033);
    }

    #[test]
    fn test_perft_rnbq1k1r_pp1Pbppp_2p5_8_2B5_8_PPP1NnPP_RNBQK2R() {
        let fen = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
        assert_eq!(PerftTestContext::new(5, fen).run(), 89941194);
    }

    #[test]
    fn test_perft_r4rk1_1pp1qppp_p1np1n2_2b1p1B1_2B1P1b1_P1NP1N2_1PP1QPPP_R4RK1() {
        let fen = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";
        assert_eq!(PerftTestContext::new(5, fen).run(), 164_075_551);
    }

    #[test]
    fn test_perft_8_PPP4k_8_8_8_8_4Kppp_8() {
        let fen = "8/PPP4k/8/8/8/8/4Kppp/8 w - -";
        assert_eq!(PerftTestContext::new(6, fen).run(), 34336777);
    }

    #[test]
    fn test_perft_n1n5_PPPk4_8_8_8_8_4Kppp_5N1N() {
        let fen = "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1";
        assert_eq!(PerftTestContext::new(6, fen).run(), 71179139);
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
            assert_eq!(PerftTestContext::new(*depth, fen).run(), *expected);
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
}
