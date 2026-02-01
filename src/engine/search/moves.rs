use std::arch::x86_64::*;

use crate::engine::{chess_v2, sorting};

#[cfg_attr(any(), rustfmt::skip)]
const MVV_LVA_SCORES_U8: [[u8; 16]; 16] = [
    /* Ep Cap */      [0, 0, 0, 0, 0, 0, 26, 0, 0, 0, 0, 0, 0, 0, 26, 0],
    /* WhiteKing */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* WhiteQueen */  [0, 0, 0, 0, 0, 0, 0, 0, 0, 07, 06, 05, 04, 03, 02, 0],
    /* WhiteRook */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 12, 11, 10, 09, 08, 0],
    /* WhiteBishop */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 18, 17, 16, 15, 14, 0],
    /* WhiteKnight */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 24, 23, 22, 21, 20, 0],
    /* WhitePawn */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 31, 30, 29, 28, 27, 26, 0],
    /* Pad */         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* Black Null */  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackKing */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackQueen */  [0, 07, 06, 05, 04, 03, 02, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackRook */   [0, 13, 12, 11, 10, 09, 08, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackBishop */ [0, 19, 18, 17, 16, 15, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackKnight */ [0, 25, 24, 23, 22, 21, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackPawn */   [0, 31, 30, 29, 28, 27, 26, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* Pad */         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
];

pub struct FastMvvlva<const HISTORY_MIN: i16, const HISTORY_MAX: i16> {
    tt_index: u8,
    pv_move: u16,
    move_list: [u16; 256],
}

impl<const HISTORY_MIN: i16, const HISTORY_MAX: i16> FastMvvlva<HISTORY_MIN, HISTORY_MAX> {
    /*
        Default (fast) sort layout:

        SORT_QUIET_BASE (0)                     SORT_PVTT_BASE+1 (0xFFFF)
        |                                                               |
        \/                                                             \/
        ================================================================
        |  SORT_QUIET_RANGE                     | SORT_CAPTURE_RANGE |TP|
    */

    const SORT_FAST_CAPTURE_RANGE: u16 = 32;
    const SORT_FAST_QUIET_RANGE: u16 = 65502;

    const SORT_FAST_QUIET_BASE: u16 = 0;
    const SORT_FAST_CAPTURE_BASE: u16 = Self::SORT_FAST_QUIET_BASE + Self::SORT_FAST_QUIET_RANGE;
    const SORT_FAST_PVTT_BASE: u16 = Self::SORT_FAST_CAPTURE_BASE + Self::SORT_FAST_CAPTURE_RANGE;

    const _ASSERT_RANGE: () = assert!(u16::MAX as usize - 1 == Self::SORT_FAST_PVTT_BASE as usize);
    const _ASSERT_SORT_HISTORY_MINMAX_RANGE: () = assert!(
        Self::SORT_FAST_QUIET_RANGE as usize - 1
            == HISTORY_MAX as usize + HISTORY_MIN.abs() as usize
    );

    #[inline(always)]
    pub fn new(pv_move: u16, tt_move_index: u8) -> Self {
        Self {
            pv_move,
            tt_index: tt_move_index,
            move_list: [0; 256],
        }
    }

    #[inline(always)]
    pub fn gen_moves(
        &mut self,
        board: &chess_v2::ChessGame,
        history_moves: &[[i16; 64]; 16],
        move_list: &mut [u32; 256],
    ) -> usize {
        let mut move_count = board.gen_moves_avx512::<false>(&mut self.move_list);

        std::hint::likely(move_count > 8 && move_count < 64);

        unsafe {
            // Safety: maximum number of legal moves in any position is 218.
            // Generated move count is guaranteed to be within bounds of 248 assuming
            // few possible pseudolegal moves like castling or moving into a check
            debug_assert!(move_count < 248);
            std::hint::assert_unchecked(move_count < 248);
        }

        let mut i = 0;
        while i < move_count {
            let mv = self.move_list[i];

            move_list[i] = self.score_move(mv, i as u8, board.spt(), history_moves);

            if (mv & chess_v2::MV_FLAGS_PR_MASK) == chess_v2::MV_FLAGS_PR_QUEEN {
                let mv_unpromoted = mv & !chess_v2::MV_FLAGS_PR_MASK;

                let mv_k = mv_unpromoted | chess_v2::MV_FLAGS_PR_KNIGHT;
                let mv_b = mv_unpromoted | chess_v2::MV_FLAGS_PR_BISHOP;
                let mv_r = mv_unpromoted | chess_v2::MV_FLAGS_PR_ROOK;

                macro_rules! add_move {
                    ($move:expr) => {
                        self.move_list[move_count] = $move;
                        move_count += 1;
                    };
                }

                add_move!(mv_k);
                add_move!(mv_b);
                add_move!(mv_r);
            }

            i += 1;
        }

        unsafe {
            // Safety: move_count is guaranteed to be less than 256
            debug_assert!(move_count < 256);
            std::hint::assert_unchecked(move_count < 256);
        }

        sorting::u32::sort_256u32_desc_avx512(move_list, move_count);

        debug_assert!(
            move_list[self.tt_index as usize] == 0
                || !move_list[0..move_count]
                    .iter()
                    .any(|mv| *mv as u16 == move_list[self.tt_index as usize] as u16)
                || (move_list[0] as u16) == move_list[self.tt_index as usize] as u16
        );
        debug_assert!(self.pv_move == 0 || move_list[0] as u16 == self.pv_move);

        move_count
    }

    pub fn find_move_index_avx512(&self, mv: u16) -> u8 {
        unsafe {
            let mv_list_ptr = self.move_list.as_ptr() as *const __m512i;
            let p0_x32 = _mm512_loadu_si512(mv_list_ptr);
            let p1_x32 = _mm512_loadu_si512(mv_list_ptr.add(1));
            let p2_x32 = _mm512_loadu_si512(mv_list_ptr.add(2));
            let p3_x32 = _mm512_loadu_si512(mv_list_ptr.add(3));

            let mv_x32 = _mm512_set1_epi16(mv as i16);

            let cmp_mask_0 = _mm512_cmpeq_epi16_mask(p0_x32, mv_x32) as u128;
            let cmp_mask_1 = _mm512_cmpeq_epi16_mask(p1_x32, mv_x32) as u128;
            let cmp_mask_2 = _mm512_cmpeq_epi16_mask(p2_x32, mv_x32) as u128;
            let cmp_mask_3 = _mm512_cmpeq_epi16_mask(p3_x32, mv_x32) as u128;

            let lower_mask: u128 =
                cmp_mask_0 | ((cmp_mask_1) << 32) | ((cmp_mask_2) << 64) | ((cmp_mask_3) << 96);

            // if util::should_log(hash) {
            //     println!(
            //         "Scanned for move {} ({}), at index {} in array {:?}",
            //         mv,
            //         util::move_string_dbg(mv),
            //         lower_mask.trailing_zeros(),
            //         move_list
            //     );
            //     println!(
            //         "Masks: {:032b} {:032b} {:032b} {:032b}",
            //         cmp_mask_0, cmp_mask_1, cmp_mask_2, cmp_mask_3
            //     );
            // }

            debug_assert!(lower_mask != 0, "Move {:04x} not found in move list", mv);
            debug_assert!(
                lower_mask.trailing_zeros() < 256,
                "Move {:04x} index overflow",
                mv
            );

            lower_mask.trailing_zeros() as u8
        }
    }

    #[inline(always)]
    fn score_move(
        &self,
        mv: u16,
        mv_index: u8,
        spt: &[u8; 64],
        history_moves: &[[i16; 64]; 16],
    ) -> u32 {
        macro_rules! score {
            ($score:expr) => {
                (mv as u32) | (($score as u32) << 16)
            };
        }

        if mv == self.pv_move {
            return score!(Self::SORT_FAST_PVTT_BASE + 1);
        }

        if mv_index == self.tt_index {
            return score!(Self::SORT_FAST_PVTT_BASE);
        }

        let src_sq = mv & 0x3F;
        let dst_sq = (mv >> 6) & 0x3F;

        if (mv & chess_v2::MV_FLAG_CAP) == 0 {
            unsafe {
                // Safety:
                // - src_sq and dst_sq are always < 64
                // - src_piece is a PieceIndex < 16
                let src_piece = *spt.get_unchecked(src_sq as usize) as usize;

                let history_score = *history_moves
                    .get_unchecked(src_piece)
                    .get_unchecked(dst_sq as usize);

                let history_score = (history_score as i32 + HISTORY_MIN.abs() as i32) as u16
                    + Self::SORT_FAST_QUIET_BASE;

                debug_assert!(
                    history_score >= Self::SORT_FAST_QUIET_BASE,
                    "history score = {}, clamped score = {}",
                    *history_moves
                        .get_unchecked(src_piece)
                        .get_unchecked(dst_sq as usize),
                    history_score
                );
                debug_assert!(
                    history_score <= (Self::SORT_FAST_CAPTURE_BASE - 1),
                    "history score = {}, clamped score = {}",
                    *history_moves
                        .get_unchecked(src_piece)
                        .get_unchecked(dst_sq as usize),
                    history_score
                );

                return score!(history_score);
            }
        }

        // MVV-LVA
        let mvvlva_score = unsafe {
            let dst_piece = *spt.get_unchecked(dst_sq as usize);
            let src_piece = *spt.get_unchecked(src_sq as usize);

            let mvvlva_score = *MVV_LVA_SCORES_U8
                .get_unchecked(dst_piece as usize)
                .get_unchecked(src_piece as usize) as u16;

            31 - mvvlva_score
        };

        debug_assert!(
            Self::SORT_FAST_CAPTURE_BASE + mvvlva_score < Self::SORT_FAST_PVTT_BASE,
            "mvv-lva score = {}, clamped score = {}",
            mvvlva_score,
            Self::SORT_FAST_CAPTURE_BASE + mvvlva_score
        );

        score!(Self::SORT_FAST_CAPTURE_BASE + mvvlva_score)
    }
}

pub struct CaptureMvvlva {
    move_list: [u16; 256],
}

impl CaptureMvvlva {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            move_list: [0; 256],
        }
    }

    #[inline(always)]
    pub fn gen_moves(&mut self, board: &chess_v2::ChessGame, move_list: &mut [u32; 256]) -> usize {
        let mut move_count = board.gen_moves_avx512::<true>(&mut self.move_list);

        std::hint::likely(move_count < 64);

        unsafe {
            // Safety: maximum number of legal moves in any position is 218.
            // Generated move count is guaranteed to be within bounds of 248 assuming
            // few possible pseudolegal moves like castling or moving into a check
            debug_assert!(move_count < 248);
            std::hint::assert_unchecked(move_count < 248);
        }

        let mut i = 0;
        while i < move_count {
            let mv = self.move_list[i];

            move_list[i] = Self::score_move(mv, board.spt());

            if (mv & chess_v2::MV_FLAGS_PR_MASK) == chess_v2::MV_FLAGS_PR_QUEEN {
                let mv_unpromoted = mv & !chess_v2::MV_FLAGS_PR_MASK;

                // Quiescence search can't encounter new captures after queen or knight promotions, so
                // underpromotions to bishop and rook are skipped
                let mv_k = mv_unpromoted | chess_v2::MV_FLAGS_PR_KNIGHT;

                self.move_list[move_count] = mv_k;
                move_count += 1;
            }
            i += 1;
        }

        sorting::u32::sort_256u32_desc_avx512(move_list, move_count);

        move_count
    }

    #[inline(always)]
    fn score_move(mv: u16, spt: &[u8; 64]) -> u32 {
        macro_rules! score {
            ($score:expr) => {
                (mv as u32) | (($score as u32) << 16)
            };
        }

        let src_sq = mv & 0x3F;
        let dst_sq = (mv >> 6) & 0x3F;

        // MVV-LVA
        let mvvlva_score = unsafe {
            let dst_piece = *spt.get_unchecked(dst_sq as usize);
            let src_piece = *spt.get_unchecked(src_sq as usize);

            let mvvlva_score = *MVV_LVA_SCORES_U8
                .get_unchecked(dst_piece as usize)
                .get_unchecked(src_piece as usize) as u16;

            31 - mvvlva_score
        };

        score!(mvvlva_score)
    }
}
