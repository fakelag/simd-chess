use std::{arch::x86_64::*, debug_assert, num::NonZero};

use crate::{
    engine::{
        chess_v2,
        search::{eval, see},
        sorting, tables,
    },
    util,
};

#[cfg_attr(any(), rustfmt::skip)]
const MVV_LVA_SCORES_U8: [[u8; 16]; 16] = [
    /* Ep Cap */      [0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0],
    /* WhiteKing */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* WhiteQueen */  [0, 0, 0, 0, 0, 0, 0, 0, 0, 24, 25, 26, 27, 28, 29, 0],
    /* WhiteRook */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 19, 20, 21, 22, 23, 0],
    /* WhiteBishop */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 13, 14, 15, 16, 17, 0],
    /* WhiteKnight */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 7, 8, 9, 10, 11, 0],
    /* WhitePawn */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 0],
    /* Pad */         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* Black Null */  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackKing */   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackQueen */  [0, 24, 25, 26, 27, 28, 29, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackRook */   [0, 18, 19, 20, 21, 22, 23, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackBishop */ [0, 12, 13, 14, 15, 16, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackKnight */ [0, 6, 7, 8, 9, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* BlackPawn */   [0, 0, 1, 2, 3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /* Pad */         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
];

pub struct ContHistRef<'a> {
    pub ply1: Option<&'a [i16; 768]>,
    pub ply2: Option<&'a [i16; 768]>,
}

#[repr(u8)]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum MovegenPhase {
    MoveTt = 0,
    MoveCapGen = 1,
    MoveGoodCap = 2,
    MoveCut = 3,
    MoveQuietGen = 4,
    MoveQuiet = 5,
    MoveBadCap = 6,
    MoveQuietPromoOnly = 7,
}

pub struct See {
    pub pieces_board: __m512i,
    pub black_board: u64,
    pub white_board: u64,
    pub pins: [see::Pinning; 2],
}

const _: () = assert!(std::mem::align_of::<See>() >= 64);

#[repr(align(64))]
pub struct MoveBuffer {
    move_list: [u16; 256],
    move_list_quiets: [u32; 256],
    move_list_caps: [u32; 128], // Max number of captures is 74
    pub see_info: Option<See>,
}

#[repr(align(64))]
pub struct PhasedMovegen<const QS: bool> {
    phase: MovegenPhase,
    tt_move: u16,
    cut_moves: [u16; 2],

    quiet_index: u8,
    quiet_count: u8,

    cap_index: u8,
    cap_count: u8,

    cut_0: Option<NonZero<u16>>,
    cut_1: Option<NonZero<u16>>,

    move_count: usize,
    see_threshold: eval::Eval,
    bad_cap_count: u8,
    in_check: bool,
}

const _: () = assert!(std::mem::size_of::<PhasedMovegen<true>>() == 64);
const _: () = assert!(std::mem::size_of::<PhasedMovegen<false>>() == 64);

impl Into<u8> for MovegenPhase {
    fn into(self) -> u8 {
        self as u8
    }
}

impl From<u8> for MovegenPhase {
    fn from(value: u8) -> Self {
        unsafe { std::mem::transmute(value) }
    }
}

#[inline(always)]
fn zero_fill_avx512<const CHUNKS: usize>(ptr: *mut u8) {
    unsafe {
        let zero = _mm512_setzero_si512();
        for i in 0..CHUNKS {
            _mm512_storeu_si512(ptr.add(i * 64) as *mut _, zero);
        }
    }
}

#[inline(always)]
fn zero_tail<const N: usize>(buf: &mut [u32; N], n: usize) {
    debug_assert!(n <= N);
    let seg = n.next_power_of_two().min(N).max(16);

    let mut i = n;
    while i < seg {
        let rem = seg - i;
        let mask = if rem >= 16 {
            u16::MAX
        } else {
            ((1u32 << rem) - 1) as u16
        };

        // Safety: i < seg <= N, and the masked store writes only lanes inside
        // [i, seg), so it never touches memory past the buffer.
        unsafe {
            _mm512_mask_storeu_epi32(
                buf.as_mut_ptr().add(i) as *mut i32,
                mask,
                _mm512_setzero_si512(),
            );
        }

        i += 16;
    }
}

impl<const QS: bool> PhasedMovegen<QS> {
    #[inline(always)]
    pub fn new(
        cut_moves: [u16; 2],
        tt_move_index: u8,
        chess: &chess_v2::ChessGame,
        buffer: &mut MoveBuffer,
        in_check: bool,
        see_threshold: eval::Eval,
    ) -> Self {
        let mut s = Self {
            phase: MovegenPhase::MoveTt,
            tt_move: 0,
            cut_moves,
            quiet_count: 0,
            quiet_index: 0,
            cap_count: 0,
            cap_index: 0,
            bad_cap_count: 0,
            cut_0: None,
            cut_1: None,
            move_count: 0,
            in_check,
            see_threshold,
        };

        buffer.see_info = None;

        let move_list_ptr = buffer.move_list.as_mut_ptr();

        zero_fill_avx512::<8>(move_list_ptr as *mut u8);

        match (QS, in_check) {
            (true, true) => {
                std::hint::cold_path();
                s.gen_moves_incheck_noinline(tt_move_index, chess, move_list_ptr);
            }
            (_, _) => s.gen_moves_internal::<QS>(tt_move_index, chess, move_list_ptr),
        }

        s
    }

    #[inline(never)]
    fn gen_moves_incheck_noinline(
        &mut self,
        tt_move_index: u8,
        chess: &chess_v2::ChessGame,
        move_list_ptr: *mut u16,
    ) {
        self.gen_moves_internal::<false>(tt_move_index, chess, move_list_ptr);
    }

    #[inline(always)]
    fn gen_moves_internal<const CAPTURE_ONLY: bool>(
        &mut self,
        tt_move_index: u8,
        chess: &chess_v2::ChessGame,
        move_list_ptr: *mut u16,
    ) {
        self.move_count = chess.gen_moves_avx512::<CAPTURE_ONLY, _>(unsafe {
            std::slice::from_raw_parts_mut(move_list_ptr, 256)
        });

        if tt_move_index < self.move_count as u8 {
            self.tt_move = unsafe { *(move_list_ptr as *const u16).add(tt_move_index as usize) };

            debug_assert!(
                !CAPTURE_ONLY
                    || (self.tt_move & (chess_v2::MV_FLAG_CAP | chess_v2::MV_FLAG_PROMOTION) != 0)
            );
        } else {
            self.phase = MovegenPhase::MoveCapGen;
        }
    }

    #[inline(always)]
    fn score_quiet(
        mv: u32,
        spt: &[u8; 64],
        history_moves: &[[i16; 64]; 16],
        cont_hist: &ContHistRef,
    ) -> u32 {
        macro_rules! score {
            ($score:expr) => {
                (mv as u32 & 0xFFFF) | (($score as u32) << 16)
            };
        }

        let src_sq = mv & 0x3F;
        let dst_sq = (mv >> 6) & 0x3F;

        unsafe {
            // Safety:
            // - src_sq and dst_sq are always < 64
            // - src_piece is a PieceIndex < 16
            let src_piece = *spt.get_unchecked(src_sq as usize) as usize;

            let mut combined = *history_moves
                .get_unchecked(src_piece)
                .get_unchecked(dst_sq as usize) as i32;

            let src_piece_comp = util::compress_piece_index_nonzero(src_piece);
            debug_assert!(src_piece_comp < 12);

            if let Some(ch1) = cont_hist.ply1 {
                let index = src_piece_comp * 64 + dst_sq as usize;
                debug_assert!(index < 768);

                combined += *ch1.get_unchecked(index) as i32 / 2;
            }
            if let Some(ch2) = cont_hist.ply2 {
                let index = src_piece_comp * 64 + dst_sq as usize;
                debug_assert!(index < 768);

                combined += *ch2.get_unchecked(index) as i32 / 2;
            }

            let clamped = combined.clamp(
                crate::engine::search::search::HISTORY_MIN as i32,
                crate::engine::search::search::HISTORY_MAX as i32,
            ) as i16;

            let final_score =
                (clamped as i32 + crate::engine::search::search::HISTORY_MIN.abs() as i32) as u32;

            return score!(final_score);
        }
    }

    #[inline(always)]
    fn calc_see_info(board: &chess_v2::ChessGame, out: &mut MoveBuffer) {
        let bitboards = board.bitboards();

        let (black_board, white_board, pieces_board) = unsafe {
            let white_x8 = _mm512_loadu_epi64(bitboards.as_ptr() as *const i64);
            let black_x8 = _mm512_loadu_epi64(bitboards.as_ptr().add(8) as *const i64);

            (
                _mm512_reduce_or_epi64(black_x8) as u64,
                _mm512_reduce_or_epi64(white_x8) as u64,
                _mm512_or_epi64(white_x8, black_x8),
            )
        };

        let pins = [
            see::calc_pinnings(false, board, black_board, white_board),
            see::calc_pinnings(true, board, black_board, white_board),
        ];

        out.see_info = Some(See {
            black_board,
            white_board,
            pieces_board,
            pins,
        })
    }

    #[inline(always)]
    pub fn next(
        &mut self,
        board: &chess_v2::ChessGame,
        tables: &tables::Tables,
        cont_hist: &ContHistRef,
        history_moves: &[[i16; 64]; 16],
        buffer: &mut MoveBuffer,
    ) -> Option<(u16, MovegenPhase)> {
        loop {
            let phase = self.phase;
            match self.phase {
                MovegenPhase::MoveTt => {
                    self.phase = MovegenPhase::MoveCapGen;
                    return Some((self.tt_move, phase));
                }
                MovegenPhase::MoveCapGen => unsafe {
                    let cut_0 = self.cut_moves[0] as i16;
                    let cut_1 = self.cut_moves[1] as i16;

                    let spt_x64 = _mm512_loadu_epi8(board.spt().as_ptr() as *const i8);

                    // Interleave indices for building the sort keys: each output dword takes its
                    // low u16 from the move vector (index 0..31) and its high u16 from the score
                    // vector (index 32..63), so one permute per half replaces widen+shift+or.
                    // let idx_lo_x32 = _mm512_set_epi16(
                    //     47, 15, 46, 14, 45, 13, 44, 12, 43, 11, 42, 10, 41, 9, 40, 8, 39, 7, 38, 6,
                    //     37, 5, 36, 4, 35, 3, 34, 2, 33, 1, 32, 0,
                    // );
                    // let idx_hi_x32 = _mm512_set_epi16(
                    //     63, 31, 62, 30, 61, 29, 60, 28, 59, 27, 58, 26, 57, 25, 56, 24, 55, 23, 54,
                    //     22, 53, 21, 52, 20, 51, 19, 50, 18, 49, 17, 48, 16,
                    // );

                    if std::hint::likely(!self.in_check) {
                        Self::calc_see_info(board, buffer);
                    }

                    for i in 0..=self.move_count / 32 {
                        let moves_x32 = _mm512_loadu_epi16(
                            (buffer.move_list.as_ptr() as *const i16).add(i * 32),
                        );

                        let skip_mask = _mm512_cmpeq_epi16_mask(
                            moves_x32,
                            _mm512_set1_epi16(self.tt_move as i16),
                        );

                        let nonzero_mask =
                            _mm512_test_epi16_mask(moves_x32, _mm512_set1_epi16(0xFFFFu16 as i16));

                        let cap_emit_mask = _mm512_test_epi16_mask(
                            moves_x32,
                            _mm512_set1_epi16(chess_v2::MV_FLAG_CAP as i16),
                        ) & !skip_mask;

                        let (cut_mask_0, cut_mask_1) = if QS {
                            (0, 0)
                        } else {
                            let cut_mask_0 = nonzero_mask
                                & !skip_mask
                                & _mm512_cmpeq_epi16_mask(moves_x32, _mm512_set1_epi16(cut_0));
                            let cut_mask_1 = nonzero_mask
                                & !skip_mask
                                & _mm512_cmpeq_epi16_mask(moves_x32, _mm512_set1_epi16(cut_1));

                            if cut_mask_0 != 0 {
                                self.cut_0 = Some(NonZero::new_unchecked(cut_0 as u16));
                            }

                            if cut_mask_1 != 0 {
                                self.cut_1 = Some(NonZero::new_unchecked(cut_1 as u16));
                            }

                            debug_assert!(
                                cap_emit_mask & (cut_mask_0 | cut_mask_1) == 0,
                                "Cut moves in capture mask"
                            );

                            (cut_mask_0, cut_mask_1)
                        };

                        let mut moves_x16_0 =
                            _mm512_cvtepu16_epi32(_mm512_castsi512_si256(moves_x32));
                        let mut moves_x16_1 =
                            _mm512_cvtepu16_epi32(_mm512_extracti32x8_epi32(moves_x32, 1));

                        // Use MVV-LVA scoring
                        {
                            // Single lane MVV-LVA
                            // let src_piece_x64 = _mm512_maskz_permutexvar_epi8(
                            //     0x5555555555555555,
                            //     moves_x32,
                            //     spt_x64,
                            // );

                            // let dst_piece_x64 = _mm512_permutexvar_epi8(
                            //     // 0xAAAAAAAAAAAAAAAA,
                            //     _mm512_slli_epi16(moves_x32, 2),
                            //     spt_x64,
                            // );

                            // let dst_piece_offset_x64 =
                            //     _mm512_set1_epi8(chess_v2::PieceIndex::PieceIndexMax as i8);

                            // let dst_piece_inv_x64 = _mm512_maskz_sub_epi8(
                            //     0xAAAAAAAAAAAAAAAA,
                            //     dst_piece_offset_x64,
                            //     dst_piece_x64,
                            // );

                            // let final_scores_x32 =
                            //     _mm512_or_si512(src_piece_x64, dst_piece_inv_x64);

                            // moves_x16_0 =
                            //     _mm512_permutex2var_epi16(moves_x32, idx_lo_x32, final_scores_x32);
                            // moves_x16_1 =
                            //     _mm512_permutex2var_epi16(moves_x32, idx_hi_x32, final_scores_x32);

                            // Dual lane MVV-LVA scoring:
                            let (src_piece_x64_0, src_piece_x64_1) = (
                                _mm512_maskz_permutexvar_epi8(
                                    0x1111111111111111u64,
                                    moves_x16_0,
                                    spt_x64,
                                ),
                                _mm512_maskz_permutexvar_epi8(
                                    0x1111111111111111u64,
                                    moves_x16_1,
                                    spt_x64,
                                ),
                            );

                            let (dst_piece_x64_0, dst_piece_x64_1) = (
                                _mm512_permutexvar_epi8(_mm512_slli_epi16(moves_x16_0, 2), spt_x64),
                                _mm512_permutexvar_epi8(_mm512_slli_epi16(moves_x16_1, 2), spt_x64),
                            );

                            let dst_piece_offset_x64 =
                                _mm512_set1_epi8(chess_v2::PieceIndex::PieceIndexMax as i8);

                            let (dst_piece_inv_x64_0, dst_piece_inv_x64_1) = (
                                _mm512_maskz_sub_epi8(
                                    0x2222222222222222u64,
                                    dst_piece_offset_x64,
                                    dst_piece_x64_0,
                                ),
                                _mm512_maskz_sub_epi8(
                                    0x2222222222222222u64,
                                    dst_piece_offset_x64,
                                    dst_piece_x64_1,
                                ),
                            );

                            (moves_x16_0, moves_x16_1) = (
                                _mm512_or_si512(
                                    _mm512_slli_epi32(
                                        _mm512_or_si512(src_piece_x64_0, dst_piece_inv_x64_0),
                                        16,
                                    ),
                                    moves_x16_0,
                                ),
                                _mm512_or_si512(
                                    _mm512_slli_epi32(
                                        _mm512_or_si512(src_piece_x64_1, dst_piece_inv_x64_1),
                                        16,
                                    ),
                                    moves_x16_1,
                                ),
                            );
                        }

                        let c0_mask = (cap_emit_mask & 0xFFFF) as u16;
                        let c1_mask = (cap_emit_mask >> 16) as u16;
                        let c_list_ptr = buffer.move_list_caps.as_mut_ptr() as *mut i32;

                        _mm512_mask_compressstoreu_epi32(
                            c_list_ptr.add(self.cap_count as usize),
                            c0_mask,
                            moves_x16_0,
                        );
                        _mm512_mask_compressstoreu_epi32(
                            c_list_ptr.add(c0_mask.count_ones() as usize + self.cap_count as usize),
                            c1_mask,
                            moves_x16_1,
                        );
                        self.cap_count += cap_emit_mask.count_ones() as u8;

                        let quiet_emit_mask =
                            !cap_emit_mask & nonzero_mask & !cut_mask_0 & !cut_mask_1 & !skip_mask;
                        let q0_mask = (quiet_emit_mask & 0xFFFF) as u16;
                        let q1_mask = (quiet_emit_mask >> 16) as u16;
                        let q_list_ptr = buffer.move_list_quiets.as_mut_ptr() as *mut i32;

                        _mm512_mask_compressstoreu_epi32(
                            q_list_ptr.add(self.quiet_count as usize),
                            q0_mask,
                            moves_x16_0,
                        );
                        _mm512_mask_compressstoreu_epi32(
                            q_list_ptr
                                .add(q0_mask.count_ones() as usize + self.quiet_count as usize),
                            q1_mask,
                            moves_x16_1,
                        );
                        self.quiet_count += quiet_emit_mask.count_ones() as u8;
                    }

                    zero_tail(&mut buffer.move_list_caps, self.cap_count as usize);

                    sorting::u32::sort_u32_desc_avx512(
                        &mut buffer.move_list_caps,
                        self.cap_count as usize,
                    );

                    self.phase = MovegenPhase::MoveGoodCap;
                    continue;
                },
                MovegenPhase::MoveGoodCap => unsafe {
                    if std::hint::unlikely(self.in_check) {
                        while self.cap_index < self.cap_count {
                            let mv = *buffer.move_list_caps.get_unchecked(self.cap_index as usize);
                            self.cap_index += 1;
                            return Some((mv as u16, phase));
                        }

                        self.cap_index = 0;

                        self.phase = match QS {
                            true => MovegenPhase::MoveQuietGen,
                            false => MovegenPhase::MoveCut,
                        };

                        continue;
                    }

                    debug_assert!(buffer.see_info.is_some());

                    let see_info = buffer.see_info.as_ref().unwrap_unchecked();

                    while self.cap_index < self.cap_count {
                        let mv = *buffer.move_list_caps.get_unchecked(self.cap_index as usize);
                        self.cap_index += 1;

                        let cap_see_threshold = std::hint::unlikely(self.in_check)
                            || see::see_threshold(
                                &eval::WEIGHT_TABLE_ABS,
                                tables,
                                board,
                                mv as u16,
                                self.see_threshold,
                                see_info.black_board,
                                see_info.white_board,
                                see_info.pieces_board,
                                Some(&see_info.pins),
                            );

                        if !cap_see_threshold {
                            // Push the bad cap to the start of the list (retain mvv-lva order)
                            *buffer
                                .move_list_caps
                                .get_unchecked_mut(self.bad_cap_count as usize) = mv;
                            self.bad_cap_count += 1;
                            continue;
                        }

                        return Some((mv as u16, phase));
                    }

                    self.cap_index = 0;

                    self.phase = match (QS, self.in_check) {
                        (true, false) => MovegenPhase::MoveQuietPromoOnly,
                        (true, true) => MovegenPhase::MoveQuietGen,
                        (false, _) => MovegenPhase::MoveCut,
                    };
                    continue;
                },
                MovegenPhase::MoveCut => {
                    debug_assert!(QS == false);

                    if let Some(cut_0) = self.cut_0 {
                        self.cut_0 = None;
                        return Some((cut_0.get(), phase));
                    }

                    if let Some(cut_1) = self.cut_1 {
                        self.cut_1 = None;
                        return Some((cut_1.get(), phase));
                    }

                    self.phase = MovegenPhase::MoveQuietGen;
                    continue;
                }
                MovegenPhase::MoveQuietGen => unsafe {
                    debug_assert!(!QS || self.in_check);

                    for i in 0..self.quiet_count {
                        let mv = buffer.move_list_quiets.get_unchecked_mut(i as usize);
                        *mv = Self::score_quiet(*mv, board.spt(), history_moves, cont_hist);
                    }

                    zero_tail(&mut buffer.move_list_quiets, self.quiet_count as usize);

                    Self::sort_noinline(&mut buffer.move_list_quiets, self.quiet_count as usize);

                    self.phase = MovegenPhase::MoveQuiet;
                    continue;
                },
                MovegenPhase::MoveQuiet => unsafe {
                    if self.quiet_index == self.quiet_count {
                        self.phase = MovegenPhase::MoveBadCap;
                        continue;
                    }

                    let mv_quiet = buffer
                        .move_list_quiets
                        .get_unchecked(self.quiet_index as usize);

                    self.quiet_index += 1;
                    return Some((*mv_quiet as u16, phase));
                },
                MovegenPhase::MoveQuietPromoOnly => unsafe {
                    while self.quiet_index < self.quiet_count {
                        let mv = *buffer
                            .move_list_quiets
                            .get_unchecked(self.quiet_index as usize)
                            as u16;
                        self.quiet_index += 1;

                        if mv & chess_v2::MV_FLAG_PROMOTION != 0 {
                            return Some((mv, phase));
                        }
                    }

                    self.phase = MovegenPhase::MoveBadCap;
                    continue;
                },
                MovegenPhase::MoveBadCap => unsafe {
                    while self.cap_index < self.bad_cap_count {
                        let mv =
                            *buffer.move_list_caps.get_unchecked(self.cap_index as usize) as u16;
                        self.cap_index += 1;
                        return Some((mv, phase));
                    }

                    return None;
                },
            }
        }
    }

    #[inline(always)]
    pub fn set_phase(&mut self, phase: MovegenPhase) {
        debug_assert!(matches!(
            self.phase,
            MovegenPhase::MoveCut | MovegenPhase::MoveQuiet
        ));
        self.phase = phase;
    }

    #[inline(never)]
    fn sort_noinline(buf: &mut [u32; 256], n: usize) {
        sorting::u32::sort_u32_desc_avx512(buf, n);
    }

    #[inline(always)]
    pub fn tt_move(&self) -> u16 {
        self.tt_move
    }

    #[inline(always)]
    pub fn find_move_index_avx512(&self, mv: u16, buffer: &MoveBuffer) -> u8 {
        // Safety: mv is an emitted move, so its index is within the initialized
        // [0, move_count) prefix and the scan can never reach uninitialized data.
        Self::move_index_avx512(mv, &buffer.move_list)
    }

    #[inline(always)]
    fn move_index_avx512(mv: u16, move_list: &[u16; 256]) -> u8 {
        unsafe {
            let mv_list_ptr = move_list.as_ptr() as *const __m512i;
            let mv_x32 = _mm512_set1_epi16(mv as i16);

            let p0_x32 = _mm512_loadu_si512(mv_list_ptr);
            let cmp_mask_0 = _mm512_cmpeq_epi16_mask(p0_x32, mv_x32) as u32;

            if std::hint::likely(cmp_mask_0 != 0) {
                return cmp_mask_0.trailing_zeros() as u8;
            }

            debug_assert!(move_list[32..].iter().any(|&m| m == mv));

            Self::move_index_tail(mv, move_list)
        }
    }

    #[inline(never)]
    fn move_index_tail(mv: u16, move_list: &[u16; 256]) -> u8 {
        // Safety: the caller has established that mv is present past index 32.
        unsafe {
            32 + move_list
                .get_unchecked(32..)
                .iter()
                .position(|&m| m == mv)
                .unwrap_unchecked() as u8
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_move_index_all_positions() {
        for i in 0..256usize {
            let mut move_list = [0xAAAAu16; 256];
            move_list[i] = 0xBBBB;

            let result = PhasedMovegen::<false>::move_index_avx512(0xBBBB, &move_list);
            assert_eq!(result, i as u8, "failed at index {i}");
        }
    }
}
