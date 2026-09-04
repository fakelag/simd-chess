use std::arch::x86_64::*;

use crate::{
    engine::chess_v2::{self, PieceIndex},
    pop_ls1b,
};

pub const QA: i16 = 255;
pub const QB: i16 = 64;
pub const QS: i32 = 400;

const LAZY_NNUE_MAX_PLY: usize = 1024;

type PairFeature = u32;

macro_rules! feature_safety {
    ($feature_id:expr) => {{
        let feat_id = $feature_id;
        debug_assert!(feat_id < 768, "Feature index out of bounds: {}", feat_id);
        unsafe {
            std::hint::assert_unchecked(feat_id < 768);
        }
        feat_id as usize
    }};
}

#[inline(always)]
fn pair_feature_from_piece_square(piece_index: u8, square: u8) -> PairFeature {
    let is_black = (piece_index & 0b1000) != 0;
    let piece_base = (64 * (NNUE_PIECE_INDICES[(piece_index & 7) as usize])) as u16;

    let square = square as u16;
    let white_feature = [0, 0x180][is_black as usize] + piece_base + square;
    let black_feature = [0x180, 0][is_black as usize] + piece_base + (square ^ 56);

    feature_safety!(white_feature);
    feature_safety!(black_feature);

    ((white_feature as u32) << 16) | (black_feature as u32)
}

#[inline]
fn crelu<const QA: i16>(x: i16) -> i32 {
    i32::from(x).clamp(0, i32::from(QA))
}

#[inline]
fn screlu(x: i16) -> i32 {
    let y = i32::from(x).clamp(0, i32::from(QA));
    y * y
}

#[inline(always)]
fn screlu_dp(acc: __m512i, v: __m512i, weights_x32: __m512i) -> __m512i {
    unsafe {
        let c = _mm512_min_epi16(
            _mm512_max_epi16(v, _mm512_setzero_si512()),
            _mm512_set1_epi16(QA),
        );
        _mm512_dpwssd_epi32(acc, _mm512_mullo_epi16(c, weights_x32), c)
    }
}

#[inline(always)]
fn sum4(accs: [__m512i; 4]) -> __m512i {
    unsafe {
        _mm512_add_epi32(
            _mm512_add_epi32(accs[0], accs[1]),
            _mm512_add_epi32(accs[2], accs[3]),
        )
    }
}

#[inline(always)]
unsafe fn apply_block<const NA: usize, const NS: usize>(
    src_p: *const i16,
    add_p: &[*const i16; NA],
    sub_p: &[*const i16; NS],
    off: usize,
) -> __m512i {
    unsafe {
        let mut v = _mm512_load_si512(src_p.add(off) as *const _);
        let mut a = 0;
        while a < NA {
            v = _mm512_add_epi16(v, _mm512_load_si512(add_p[a].add(off) as *const _));
            a += 1;
        }
        let mut s = 0;
        while s < NS {
            v = _mm512_sub_epi16(v, _mm512_load_si512(sub_p[s].add(off) as *const _));
            s += 1;
        }
        v
    }
}

// Maps PieceIndex -> NNUE index
const NNUE_PIECE_INDICES: [usize; 8] = [
    0, // unused
    5, // King
    4, // Queen
    3, // Rook
    2, // Bishop
    1, // Knight
    0, // Pawn
    0, // unused
];

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Network<const HS: usize, const OB: usize>
where
    [(); 2 * HS]:,
{
    feature_weights: [Accumulator<HS, OB>; 768],
    feature_bias: Accumulator<HS, OB>,
    output_weights: [[i16; 2 * HS]; OB],
    output_bias: [i16; OB],
}

impl<const HS: usize, const OB: usize> Network<HS, OB>
where
    [(); 2 * HS]:,
{
    #[inline(always)]
    pub fn evaluate_naive(
        &self,
        us: &Accumulator<HS, OB>,
        them: &Accumulator<HS, OB>,
        bucket: u8,
    ) -> i16 {
        let mut output = 0;

        let weights = &self.output_weights[bucket as usize];

        for (&input, &weight) in us.vals.iter().zip(&weights[..HS]) {
            output += screlu(input) * i32::from(weight);
        }

        for (&input, &weight) in them.vals.iter().zip(&weights[HS..]) {
            output += screlu(input) * i32::from(weight);
        }

        output /= i32::from(QA);
        output += i32::from(self.output_bias[bucket as usize]);
        output *= QS;
        output /= i32::from(QA) * i32::from(QB);

        debug_assert!(
            output >= i32::from(i16::MIN) && output <= i32::from(i16::MAX),
            "NNUE output overflow: {}",
            output,
        );

        output as i16
    }

    #[inline(always)]
    fn finalize(&self, dot: i32, bucket: u8) -> i16 {
        debug_assert!((bucket as usize) < OB);
        let bias = unsafe { *self.output_bias.get_unchecked(bucket as usize) };

        let mut output = dot;
        output /= i32::from(QA);
        output += i32::from(bias);
        output *= QS;
        output /= i32::from(QA) * i32::from(QB);

        debug_assert!(
            output >= i32::from(i16::MIN) && output <= i32::from(i16::MAX),
            "NNUE output overflow: {}",
            output,
        );

        output as i16
    }

    #[inline(always)]
    pub fn evaluate(
        &self,
        stm: &Accumulator<HS, OB>,
        ntm: &Accumulator<HS, OB>,
        bucket: u8,
    ) -> i16 {
        assert!(
            HS % 32 == 0,
            "HS must be a multiple of 32 for SIMD evaluation"
        );

        debug_assert!((bucket as usize) < OB);
        let weights = unsafe { self.output_weights.get_unchecked(bucket as usize) };

        let output = self.finalize(Self::screlu_dot(stm, ntm, weights), bucket);

        debug_assert_eq!(
            self.evaluate_naive(stm, ntm, bucket),
            output,
            "NNUE output mismatch"
        );

        output
    }

    #[inline(always)]
    unsafe fn fused_apply_dot_half<const NA: usize, const NS: usize>(
        &self,
        src: &Accumulator<HS, OB>,
        dst: &mut Accumulator<HS, OB>,
        add_ids: [usize; NA],
        sub_ids: [usize; NS],
        w_half: *const i16,
    ) -> __m512i {
        unsafe {
            let src_p = src.vals.as_ptr();
            let dst_p = dst.vals.as_mut_ptr();
            let add_p = add_ids.map(|id| self.feature_weights[id].vals.as_ptr());
            let sub_p = sub_ids.map(|id| self.feature_weights[id].vals.as_ptr());

            let mut accs = [_mm512_setzero_si512(); 4];

            for i in 0..HS / 128 {
                for chunk in 0..4 {
                    let offset = i * 128 + chunk * 32;

                    let block_x32 = apply_block(src_p, &add_p, &sub_p, offset);
                    _mm512_store_si512(dst_p.add(offset) as *mut _, block_x32);

                    accs[chunk] = screlu_dp(
                        accs[chunk],
                        block_x32,
                        _mm512_load_si512(w_half.add(offset) as *const _),
                    );
                }
            }

            sum4(accs)
        }
    }

    #[inline(always)]
    pub fn evaluate_fused(
        &self,
        src: &AccumulatorPair<HS, OB>,
        dst: &mut AccumulatorPair<HS, OB>,
        update: &NnueUpdate,
        b_move: bool,
        bucket: u8,
    ) -> i16 {
        assert!(
            HS % 128 == 0,
            "HS must be a multiple of 128 for fused SIMD evaluation"
        );

        debug_assert!((bucket as usize) < OB);
        let weights = unsafe { self.output_weights.get_unchecked(bucket as usize).as_ptr() };

        let (w_white, w_black) = if b_move {
            (unsafe { weights.add(HS) }, weights)
        } else {
            (weights, unsafe { weights.add(HS) })
        };

        let dot = unsafe {
            let (wsum, bsum) = match update {
                NnueUpdate::NnueUpdateAddSub((add, sub)) => {
                    let (wa, ba) = (feature_safety!(add >> 16), feature_safety!(add & 0xFFFF));
                    let (ws, bs) = (feature_safety!(sub >> 16), feature_safety!(sub & 0xFFFF));
                    (
                        self.fused_apply_dot_half(&src.white, &mut dst.white, [wa], [ws], w_white),
                        self.fused_apply_dot_half(&src.black, &mut dst.black, [ba], [bs], w_black),
                    )
                }
                NnueUpdate::NnueUpdateAddSubSub((add, sub1, sub2)) => {
                    let (wa, ba) = (feature_safety!(add >> 16), feature_safety!(add & 0xFFFF));
                    let (ws1, bs1) = (feature_safety!(sub1 >> 16), feature_safety!(sub1 & 0xFFFF));
                    let (ws2, bs2) = (feature_safety!(sub2 >> 16), feature_safety!(sub2 & 0xFFFF));
                    (
                        self.fused_apply_dot_half(
                            &src.white,
                            &mut dst.white,
                            [wa],
                            [ws1, ws2],
                            w_white,
                        ),
                        self.fused_apply_dot_half(
                            &src.black,
                            &mut dst.black,
                            [ba],
                            [bs1, bs2],
                            w_black,
                        ),
                    )
                }
                NnueUpdate::NnueUpdateAddAddSubSub((add1, add2, sub1, sub2)) => {
                    std::hint::cold_path();
                    self.add2_sub2_fused_noinline(
                        src, dst, *add1, *add2, *sub1, *sub2, w_white, w_black,
                    )
                }
            };

            _mm512_reduce_add_epi32(_mm512_add_epi32(wsum, bsum))
        };

        let output = self.finalize(dot, bucket);

        debug_assert_eq!(
            {
                let stm = [&dst.white, &dst.black][b_move as usize];
                let ntm = [&dst.black, &dst.white][b_move as usize];
                self.evaluate_naive(stm, ntm, bucket)
            },
            output,
            "NNUE fused output mismatch"
        );

        output
    }

    #[inline(never)]
    fn add2_sub2_fused_noinline(
        &self,
        src: &AccumulatorPair<HS, OB>,
        dst: &mut AccumulatorPair<HS, OB>,
        add1: u32,
        add2: u32,
        sub1: u32,
        sub2: u32,
        w_white: *const i16,
        w_black: *const i16,
    ) -> (__m512i, __m512i) {
        unsafe {
            let (wa1, ba1) = (feature_safety!(add1 >> 16), feature_safety!(add1 & 0xFFFF));
            let (wa2, ba2) = (feature_safety!(add2 >> 16), feature_safety!(add2 & 0xFFFF));
            let (ws1, bs1) = (feature_safety!(sub1 >> 16), feature_safety!(sub1 & 0xFFFF));
            let (ws2, bs2) = (feature_safety!(sub2 >> 16), feature_safety!(sub2 & 0xFFFF));
            (
                self.fused_apply_dot_half(
                    &src.white,
                    &mut dst.white,
                    [wa1, wa2],
                    [ws1, ws2],
                    w_white,
                ),
                self.fused_apply_dot_half(
                    &src.black,
                    &mut dst.black,
                    [ba1, ba2],
                    [bs1, bs2],
                    w_black,
                ),
            )
        }
    }

    #[inline(always)]
    fn screlu_dot(
        stm: &Accumulator<HS, OB>,
        ntm: &Accumulator<HS, OB>,
        weights: &[i16; 2 * HS],
    ) -> i32 {
        debug_assert!(HS % 128 == 0);

        unsafe {
            let stm_p = stm.vals.as_ptr();
            let ntm_p = ntm.vals.as_ptr();
            let w_stm = weights.as_ptr();
            let w_ntm = w_stm.add(HS);

            let mut stm_accs = [_mm512_setzero_si512(); 4];
            let mut ntm_accs = [_mm512_setzero_si512(); 4];

            for i in 0..HS / 128 {
                for chunk in 0..4 {
                    let offset = i * 128 + chunk * 32;
                    stm_accs[chunk] = screlu_dp(
                        stm_accs[chunk],
                        _mm512_load_si512(stm_p.add(offset) as *const _),
                        _mm512_load_si512(w_stm.add(offset) as *const _),
                    );
                    ntm_accs[chunk] = screlu_dp(
                        ntm_accs[chunk],
                        _mm512_load_si512(ntm_p.add(offset) as *const _),
                        _mm512_load_si512(w_ntm.add(offset) as *const _),
                    );
                }
            }

            _mm512_reduce_add_epi32(_mm512_add_epi32(sum4(stm_accs), sum4(ntm_accs)))
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C, align(64))]
pub struct Accumulator<const HS: usize, const OB: usize> {
    vals: [i16; HS],
}

impl<const HS: usize, const OB: usize> Accumulator<HS, OB>
where
    [(); 2 * HS]:,
{
    #[inline(always)]
    pub fn new(net: &Network<HS, OB>) -> Self {
        net.feature_bias
    }

    #[inline(always)]
    pub fn add_feature(&mut self, feature_idx: usize, net: &Network<HS, OB>) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i += *d
        }
    }

    #[inline(always)]
    pub fn remove_feature(&mut self, feature_idx: usize, net: &Network<HS, OB>) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i -= *d
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AccumulatorPair<const HS: usize, const OB: usize> {
    pub white: Accumulator<HS, OB>,
    pub black: Accumulator<HS, OB>,
}

impl<const HS: usize, const OB: usize> AccumulatorPair<HS, OB>
where
    [(); 2 * HS]:,
{
    pub fn new() -> Self {
        Self {
            white: Accumulator { vals: [0; HS] },
            black: Accumulator { vals: [0; HS] },
        }
    }

    pub fn load(&mut self, board: &chess_v2::ChessGame, net: &Network<HS, OB>) {
        let bitboards = board.bitboards();

        self.white = Accumulator::new(net);
        self.black = Accumulator::new(net);

        let white_offset = 0;
        let black_offset = 8;

        for piece_id in PieceIndex::WhiteKing as usize..=PieceIndex::WhitePawn as usize {
            let mut board = bitboards[piece_id + white_offset];

            while board != 0 {
                let sq_index = pop_ls1b!(board) as usize;
                self.add_piece(piece_id + white_offset, sq_index, net);
            }

            let mut board = bitboards[piece_id + black_offset];

            while board != 0 {
                let sq_index = pop_ls1b!(board) as usize;
                self.add_piece(piece_id + black_offset, sq_index, net);
            }
        }
    }

    #[inline(always)]
    pub fn apply_from<const NA: usize, const NS: usize>(
        &mut self,
        src: &AccumulatorPair<HS, OB>,
        adds: [PairFeature; NA],
        subs: [PairFeature; NS],
        net: &Network<HS, OB>,
    ) {
        assert!(HS % 128 == 0, "HS must be a multiple of 128 for SIMD apply");

        let w_add_p = adds.map(|pf| net.feature_weights[feature_safety!(pf >> 16)].vals.as_ptr());
        let b_add_p = adds.map(|pf| {
            net.feature_weights[feature_safety!(pf & 0xFFFF)]
                .vals
                .as_ptr()
        });
        let w_sub_p = subs.map(|pf| net.feature_weights[feature_safety!(pf >> 16)].vals.as_ptr());
        let b_sub_p = subs.map(|pf| {
            net.feature_weights[feature_safety!(pf & 0xFFFF)]
                .vals
                .as_ptr()
        });

        let src_w = src.white.vals.as_ptr();
        let src_b = src.black.vals.as_ptr();
        let dst_w = self.white.vals.as_mut_ptr();
        let dst_b = self.black.vals.as_mut_ptr();

        for i in 0..HS / 128 {
            for chunk in 0..4 {
                let offset = i * 128 + chunk * 32;
                unsafe {
                    _mm512_store_si512(
                        dst_w.add(offset) as *mut _,
                        apply_block(src_w, &w_add_p, &w_sub_p, offset),
                    );
                    _mm512_store_si512(
                        dst_b.add(offset) as *mut _,
                        apply_block(src_b, &b_add_p, &b_sub_p, offset),
                    );
                }
            }
        }
    }

    #[inline(never)]
    fn apply_from_noinline<const NA: usize, const NS: usize>(
        &mut self,
        src: &AccumulatorPair<HS, OB>,
        adds: [PairFeature; NA],
        subs: [PairFeature; NS],
        net: &Network<HS, OB>,
    ) {
        self.apply_from(src, adds, subs, net)
    }

    #[inline(always)]
    pub fn add_piece(&mut self, piece_id: usize, to_sq: usize, net: &Network<HS, OB>) {
        debug_assert!(
            piece_id != PieceIndex::WhiteNullPiece as usize
                && piece_id != PieceIndex::WhitePad as usize
                && piece_id != PieceIndex::BlackNullPiece as usize
                && piece_id != PieceIndex::BlackPad as usize
        );

        let (white_feature, black_feature) = Self::calc_feature_indices(piece_id, to_sq);

        self.white.add_feature(white_feature, net);
        self.black.add_feature(black_feature, net);
    }

    #[inline(always)]
    fn calc_feature_indices(piece_index: usize, square: usize) -> (usize, usize) {
        let is_black = (piece_index & 0b1000) != 0;
        let piece_base = 64 * (NNUE_PIECE_INDICES[piece_index & 7]);

        let white_feature = [0, 0x180][is_black as usize] + piece_base + square;
        let black_feature = [0x180, 0][is_black as usize] + piece_base + (square ^ 56);

        (white_feature, black_feature)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum NnueUpdate {
    NnueUpdateAddSub((u32, u32)),
    NnueUpdateAddSubSub((u32, u32, u32)),
    NnueUpdateAddAddSubSub((u32, u32, u32, u32)),
}

impl NnueUpdate {
    #[inline(always)]
    pub fn quiet(from_piece_id: u8, to_piece_id: u8, from_sq: u8, to_sq: u8) -> Self {
        let sub = pair_feature_from_piece_square(from_piece_id, from_sq);
        let add = pair_feature_from_piece_square(to_piece_id, to_sq);

        NnueUpdate::NnueUpdateAddSub((add, sub))
    }

    #[inline(always)]
    pub fn capture(
        from_piece_id: u8,
        to_piece_id: u8,
        from_sq: u8,
        to_sq: u8,
        captured_piece_id: u8,
        captured_sq: u8,
    ) -> Self {
        let add = pair_feature_from_piece_square(to_piece_id, to_sq);
        let sub1 = pair_feature_from_piece_square(from_piece_id, from_sq);
        let sub2 = pair_feature_from_piece_square(captured_piece_id, captured_sq);
        NnueUpdate::NnueUpdateAddSubSub((add, sub1, sub2))
    }

    #[inline(always)]
    pub fn castle(
        rook_piece_id: u8,
        rook_from_sq: u8,
        rook_to_sq: u8,
        king_piece_id: u8,
        king_from_sq: u8,
        king_to_sq: u8,
    ) -> Self {
        let add1 = pair_feature_from_piece_square(rook_piece_id, rook_to_sq);
        let add2 = pair_feature_from_piece_square(king_piece_id, king_to_sq);
        let sub1 = pair_feature_from_piece_square(rook_piece_id, rook_from_sq);
        let sub2 = pair_feature_from_piece_square(king_piece_id, king_from_sq);
        NnueUpdate::NnueUpdateAddAddSubSub((add1, add2, sub1, sub2))
    }
}

pub trait UpdatableNnue {
    fn make_move(&mut self, mv: NnueUpdate);
    fn rollback_move(&mut self);
}

pub struct LazyNnue<const HS: usize, const OB: usize>
where
    [(); 2 * HS]:,
{
    net: Network<HS, OB>,
    accumulators: Vec<AccumulatorPair<HS, OB>>,

    updates: [NnueUpdate; LAZY_NNUE_MAX_PLY],
    updates_len: usize,
    applied_accumulators: [usize; LAZY_NNUE_MAX_PLY],
    applied_len: usize,
}

impl<const HS: usize, const OB: usize> LazyNnue<HS, OB>
where
    [(); 2 * HS]:,
{
    pub fn heap_alloc(net: &Network<HS, OB>) -> Box<Self> {
        unsafe {
            let layout = std::alloc::Layout::new::<Self>();
            let ptr = std::alloc::alloc(layout) as *mut Self;

            let mut accumulators = Vec::with_capacity(1024);

            for _ in 0..1024 {
                accumulators.push(AccumulatorPair::new());
            }

            (&raw mut (*ptr).net).write(*net);
            (&raw mut (*ptr).accumulators).write(accumulators);
            (&raw mut (*ptr).updates)
                .write([NnueUpdate::NnueUpdateAddSub((0, 0)); LAZY_NNUE_MAX_PLY]);
            (&raw mut (*ptr).updates_len).write(0);
            (&raw mut (*ptr).applied_accumulators).write([0; LAZY_NNUE_MAX_PLY]);
            (&raw mut (*ptr).applied_len).write(0);

            Box::from_raw(ptr)
        }
    }

    pub fn load(&mut self, board: &chess_v2::ChessGame) {
        debug_assert!(self.updates_len == 0);

        self.applied_accumulators[0] = 0;
        self.applied_len = 1;

        self.accumulators[0].load(board, &self.net);
        self.updates_len = 0;
    }

    #[inline(always)]
    pub fn evaluate(&mut self, b_move: bool, bucket: u8) -> i16 {
        debug_assert!(self.applied_len > 0 && self.applied_len <= LAZY_NNUE_MAX_PLY);

        // Safety: load() seeds one entry and rollback never pops below it.
        let start = unsafe {
            *self
                .applied_accumulators
                .get_unchecked(self.applied_len - 1)
        };

        debug_assert!(start <= self.updates_len);
        debug_assert!(start < LAZY_NNUE_MAX_PLY);
        debug_assert!(self.updates_len < LAZY_NNUE_MAX_PLY);
        debug_assert!(self.accumulators.len() == LAZY_NNUE_MAX_PLY);

        unsafe { std::hint::assert_unchecked(self.accumulators.len() == LAZY_NNUE_MAX_PLY) };

        let ply = self.updates_len.min(LAZY_NNUE_MAX_PLY - 1);

        if ply > start {
            for i in start + 1..ply {
                let update = unsafe { self.updates.get_unchecked(i - 1) };

                let (prev, acc) = unsafe { self.accumulators.split_at_mut_unchecked(i) };

                debug_assert!(
                    !prev.is_empty(),
                    "No previous accumulator for ply {}, cannot apply update",
                    i
                );
                debug_assert!(
                    !acc.is_empty(),
                    "No accumulator allocated for ply {}, cannot apply update",
                    i
                );

                let prev = unsafe { prev.last().unwrap_unchecked() };

                // assert!(
                //     acc.first_mut().is_some(),
                //     "Accumulator not allocated for ply {}",
                //     i
                // );

                let acc = unsafe { acc.first_mut().unwrap_unchecked() };

                match update {
                    NnueUpdate::NnueUpdateAddSub((add, sub)) => {
                        acc.apply_from(prev, [*add], [*sub], &self.net);
                    }
                    NnueUpdate::NnueUpdateAddSubSub((add, sub1, sub2)) => {
                        acc.apply_from(prev, [*add], [*sub1, *sub2], &self.net);
                    }
                    NnueUpdate::NnueUpdateAddAddSubSub((add1, add2, sub1, sub2)) => {
                        std::hint::cold_path();
                        acc.apply_from_noinline(prev, [*add1, *add2], [*sub1, *sub2], &self.net);
                    }
                }

                debug_assert!(self.applied_len < LAZY_NNUE_MAX_PLY);
                unsafe {
                    *self
                        .applied_accumulators
                        .get_unchecked_mut(self.applied_len) = i
                };
                self.applied_len += 1;
            }

            let update = unsafe { self.updates.get_unchecked(ply - 1) };
            let (prev, acc) = unsafe { self.accumulators.split_at_mut_unchecked(ply) };

            debug_assert!(!prev.is_empty());
            debug_assert!(!acc.is_empty());

            // Safety: ply > start >= 0 so both sides of the split are non-empty.
            let prev = unsafe { prev.last().unwrap_unchecked() };
            let acc = unsafe { acc.first_mut().unwrap_unchecked() };

            let output = self.net.evaluate_fused(prev, acc, update, b_move, bucket);
            debug_assert!(self.applied_len < LAZY_NNUE_MAX_PLY);
            unsafe {
                *self
                    .applied_accumulators
                    .get_unchecked_mut(self.applied_len) = ply
            };
            self.applied_len += 1;
            output
        } else {
            let acc = unsafe { self.accumulators.get_unchecked(ply) };

            let stm = [&acc.white, &acc.black][b_move as usize];
            let ntm = [&acc.black, &acc.white][b_move as usize];
            self.net.evaluate(stm, ntm, bucket)
        }
    }
}

impl<const HS: usize, const OB: usize> UpdatableNnue for LazyNnue<HS, OB>
where
    [(); 2 * HS]:,
{
    #[inline(always)]
    fn make_move(&mut self, mv: NnueUpdate) {
        debug_assert!(self.updates_len < LAZY_NNUE_MAX_PLY);

        // Safety: search depth is bounded far below LAZY_NNUE_MAX_PLY by
        // PV_DEPTH and the quiescence move-stack guard.
        unsafe { *self.updates.get_unchecked_mut(self.updates_len) = mv };
        self.updates_len += 1;
    }

    #[inline(always)]
    fn rollback_move(&mut self) {
        let ply = self.updates_len;

        debug_assert!(ply > 0, "Cannot rollback move, no moves to rollback");

        self.updates_len = ply - 1;

        if self.applied_len > 0
            && unsafe {
                *self
                    .applied_accumulators
                    .get_unchecked(self.applied_len - 1)
            } == ply
        {
            self.applied_len -= 1;
        }
    }
}

#[macro_export]
macro_rules! nnue_load {
    ($path:expr, $hs:expr,$ob:expr) => {{
        let net: &nnue::Network<$hs, $ob> = unsafe { std::mem::transmute(include_bytes!($path)) };

        nnue::LazyNnue::heap_alloc(net)
    }};
    ($path:expr, $hs:expr) => {{ nnue_load!($path, $hs, 1) }};
}
