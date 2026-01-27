use std::arch::x86_64::*;

use crate::{
    engine::chess_v2::{self, PieceIndex},
    pop_ls1b,
};

pub const QA: i16 = 255;
pub const QB: i16 = 64;
pub const QS: i32 = 400;

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

        let weights = &self.output_weights[bucket as usize];
        let bias = self.output_bias[bucket as usize];

        let output = unsafe {
            let qa_x32 = _mm512_set1_epi16(QA);
            let zero_x32 = _mm512_setzero_si512();
            let mut output_x32 = _mm512_setzero_si512();

            for i in 0..HS / 32 {
                let stm_input_x32 = _mm512_load_si512(stm.vals.as_ptr().add(i * 32) as *const _);
                let ntm_input_x32 = _mm512_load_si512(ntm.vals.as_ptr().add(i * 32) as *const _);
                let stm_weight_x32 =
                    _mm512_load_si512(weights[..HS].as_ptr().add(i * 32) as *const _);
                let ntm_weight_x32 =
                    _mm512_load_si512(weights[HS..].as_ptr().add(i * 32) as *const _);

                let stm_input_clipped_x32 =
                    _mm512_max_epi16(_mm512_min_epi16(stm_input_x32, qa_x32), zero_x32);
                let ntm_input_clipped_x32 =
                    _mm512_max_epi16(_mm512_min_epi16(ntm_input_x32, qa_x32), zero_x32);

                let stm_output_x32 = _mm512_madd_epi16(
                    _mm512_mullo_epi16(stm_input_clipped_x32, stm_weight_x32),
                    stm_input_clipped_x32,
                );
                let ntm_output_x32 = _mm512_madd_epi16(
                    _mm512_mullo_epi16(ntm_input_clipped_x32, ntm_weight_x32),
                    ntm_input_clipped_x32,
                );

                output_x32 = _mm512_add_epi32(output_x32, stm_output_x32);
                output_x32 = _mm512_add_epi32(output_x32, ntm_output_x32);
            }

            let mut output = _mm512_reduce_add_epi32(output_x32);

            output /= i32::from(QA);
            output += i32::from(bias);
            output *= QS;
            output /= i32::from(QA) * i32::from(QB);

            output
        };

        debug_assert!(
            output >= i32::from(i16::MIN) && output <= i32::from(i16::MAX),
            "NNUE output overflow: {}",
            output,
        );
        debug_assert_eq!(
            self.evaluate_naive(stm, ntm, bucket),
            output as i16,
            "NNUE output mismatch"
        );

        output as i16
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
    pub fn acc_add1_sub1_src(
        &mut self,
        src: &AccumulatorPair<HS, OB>,
        add: PairFeature,
        sub: PairFeature,
        net: &Network<HS, OB>,
    ) {
        let white_add_id = feature_safety!(add >> 16);
        let black_add_id = feature_safety!(add & 0xFFFF);
        let white_sub_id = feature_safety!(sub >> 16);
        let black_sub_id = feature_safety!(sub & 0xFFFF);

        let white_add = &net.feature_weights[white_add_id].vals;
        let black_add = &net.feature_weights[black_add_id].vals;
        let white_sub = &net.feature_weights[white_sub_id].vals;
        let black_sub = &net.feature_weights[black_sub_id].vals;

        for i in 0..HS {
            self.white.vals[i] = src.white.vals[i] + white_add[i] - white_sub[i];
            self.black.vals[i] = src.black.vals[i] + black_add[i] - black_sub[i];
        }
    }

    #[inline(always)]
    pub fn acc_add1_sub2_src(
        &mut self,
        src: &AccumulatorPair<HS, OB>,
        add: PairFeature,
        sub1: PairFeature,
        sub2: PairFeature,
        net: &Network<HS, OB>,
    ) {
        let white_add_id = feature_safety!(add >> 16);
        let black_add_id = feature_safety!(add & 0xFFFF);
        let white_sub1_id = feature_safety!(sub1 >> 16);
        let black_sub1_id = feature_safety!(sub1 & 0xFFFF);
        let white_sub2_id = feature_safety!(sub2 >> 16);
        let black_sub2_id = feature_safety!(sub2 & 0xFFFF);

        let white_add = &net.feature_weights[white_add_id].vals;
        let black_add = &net.feature_weights[black_add_id].vals;
        let white_sub1 = &net.feature_weights[white_sub1_id].vals;
        let black_sub1 = &net.feature_weights[black_sub1_id].vals;
        let white_sub2 = &net.feature_weights[white_sub2_id].vals;
        let black_sub2 = &net.feature_weights[black_sub2_id].vals;

        for i in 0..HS {
            self.white.vals[i] = src.white.vals[i] + white_add[i] - white_sub1[i] - white_sub2[i];
            self.black.vals[i] = src.black.vals[i] + black_add[i] - black_sub1[i] - black_sub2[i];
        }
    }

    #[inline(always)]
    pub fn acc_add2_sub2_src(
        &mut self,
        src: &AccumulatorPair<HS, OB>,
        add1: PairFeature,
        add2: PairFeature,
        sub1: PairFeature,
        sub2: PairFeature,
        net: &Network<HS, OB>,
    ) {
        let white_add1 = feature_safety!(add1 >> 16);
        let black_add1 = feature_safety!(add1 & 0xFFFF);
        let white_add2 = feature_safety!(add2 >> 16);
        let black_add2 = feature_safety!(add2 & 0xFFFF);
        let white_sub1 = feature_safety!(sub1 >> 16);
        let black_sub1 = feature_safety!(sub1 & 0xFFFF);
        let white_sub2 = feature_safety!(sub2 >> 16);
        let black_sub2 = feature_safety!(sub2 & 0xFFFF);

        let white_add1 = &net.feature_weights[white_add1].vals;
        let black_add1 = &net.feature_weights[black_add1].vals;
        let white_add2 = &net.feature_weights[white_add2].vals;
        let black_add2 = &net.feature_weights[black_add2].vals;
        let white_sub1 = &net.feature_weights[white_sub1].vals;
        let black_sub1 = &net.feature_weights[black_sub1].vals;
        let white_sub2 = &net.feature_weights[white_sub2].vals;
        let black_sub2 = &net.feature_weights[black_sub2].vals;

        for i in 0..HS {
            self.white.vals[i] =
                src.white.vals[i] + white_add1[i] + white_add2[i] - white_sub1[i] - white_sub2[i];
            self.black.vals[i] =
                src.black.vals[i] + black_add1[i] + black_add2[i] - black_sub1[i] - black_sub2[i];
        }
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

#[derive(Debug)]
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
    updates: Vec<NnueUpdate>,
    applied_accumulators: Vec<usize>,
}

impl<const HS: usize, const OB: usize> LazyNnue<HS, OB>
where
    [(); 2 * HS]:,
{
    pub fn heap_alloc(net: Network<HS, OB>) -> Box<Self> {
        unsafe {
            let layout = std::alloc::Layout::new::<Self>();
            let ptr = std::alloc::alloc(layout) as *mut Self;

            let mut accumulators = Vec::with_capacity(1024);

            for _ in 0..1024 {
                accumulators.push(AccumulatorPair::new());
            }

            (&raw mut (*ptr).net).write(net);
            (&raw mut (*ptr).accumulators).write(accumulators);
            (&raw mut (*ptr).updates).write(Vec::with_capacity(1024));
            (&raw mut (*ptr).applied_accumulators).write(Vec::new());

            Box::from_raw(ptr)
        }
    }

    pub fn load(&mut self, board: &chess_v2::ChessGame) {
        debug_assert!(self.updates.is_empty());

        self.applied_accumulators.clear();
        self.applied_accumulators.push(0);

        self.accumulators[0].load(board, &self.net);
        self.updates.clear();
    }

    #[inline(always)]
    pub fn evaluate(&mut self, b_move: bool, bucket: u8) -> i16 {
        let start = self.applied_accumulators.last().copied().unwrap();

        debug_assert!(start <= self.updates.len());
        debug_assert!(start < self.accumulators.len());
        debug_assert!(self.updates.len() < self.accumulators.len());

        let ply = self.updates.len().min(self.accumulators.len() - 1);

        for i in start + 1..=ply {
            let update = &self.updates[i - 1];

            let (prev, acc) = self.accumulators.split_at_mut(i);

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
                    acc.acc_add1_sub1_src(prev, *add, *sub, &self.net);
                }
                NnueUpdate::NnueUpdateAddSubSub((add, sub1, sub2)) => {
                    acc.acc_add1_sub2_src(prev, *add, *sub1, *sub2, &self.net);
                }
                NnueUpdate::NnueUpdateAddAddSubSub((add1, add2, sub1, sub2)) => {
                    acc.acc_add2_sub2_src(prev, *add1, *add2, *sub1, *sub2, &self.net);
                }
            }

            self.applied_accumulators.push(i);
        }

        let acc = &self.accumulators[ply];

        let stm = [&acc.white, &acc.black][b_move as usize];
        let ntm = [&acc.black, &acc.white][b_move as usize];
        self.net.evaluate(stm, ntm, bucket)
    }
}

impl<const HS: usize, const OB: usize> UpdatableNnue for LazyNnue<HS, OB>
where
    [(); 2 * HS]:,
{
    #[inline(always)]
    fn make_move(&mut self, mv: NnueUpdate) {
        self.updates.push(mv);
    }

    #[inline(always)]
    fn rollback_move(&mut self) {
        let ply = self.updates.len();

        debug_assert!(ply > 0, "Cannot rollback move, no moves to rollback");

        self.updates.pop().unwrap();

        self.applied_accumulators.pop_if(|x| *x == ply);
    }
}

#[macro_export]
macro_rules! nnue_load {
    ($path:expr, $hs:expr,$ob:expr) => {{
        let net: &nnue::Network<$hs, $ob> = &unsafe { std::mem::transmute(*include_bytes!($path)) };

        nnue::LazyNnue::heap_alloc(*net)
    }};
    ($path:expr, $hs:expr) => {{ nnue_load!($path, $hs, 1) }};
}
