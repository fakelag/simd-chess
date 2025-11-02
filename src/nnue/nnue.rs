use super::NnueConfig;
use crate::{
    engine::chess_v2::{self, PieceIndex},
    pop_ls1b,
};

pub const NNUE_CONFIG: NnueConfig = NnueConfig {
    hidden_size: 128,
    quant_scale: 400,
    qa: 255,
    qb: 64,
};

const QA: i16 = 255;
const QB: i16 = 64;
const QS: i32 = 400;

type PairFeature = u32;

fn pair_feature_from_piece_square(piece_index: u8, square: u8) -> PairFeature {
    let is_black = (piece_index & 0b1000) != 0;
    let piece_base = (64 * (NNUE_PIECE_INDICES[(piece_index & 7) as usize])) as u16;

    let square = square as u16;
    let white_feature = [0, 0x180][is_black as usize] + piece_base + square;
    let black_feature = [0x180, 0][is_black as usize] + piece_base + (square ^ 56);

    ((white_feature as u32) << 16) | (black_feature as u32)
}

#[inline]
fn crelu<const QA: i16>(x: i16) -> i32 {
    i32::from(x).clamp(0, i32::from(QA))
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
pub struct Network<const HS: usize>
where
    [(); 2 * HS]:,
{
    feature_weights: [Accumulator<HS>; 768],
    feature_bias: Accumulator<HS>,
    output_weights: [i16; 2 * HS],
    output_bias: i16,
}

impl<const HS: usize> Network<HS>
where
    [(); 2 * HS]:,
{
    #[inline(always)]
    pub fn evaluate(&self, us: &Accumulator<HS>, them: &Accumulator<HS>) -> i16 {
        let mut output = i32::from(self.output_bias);

        for (&input, &weight) in us.vals.iter().zip(&self.output_weights[..HS]) {
            output += crelu::<QA>(input) * i32::from(weight);
        }

        for (&input, &weight) in them.vals.iter().zip(&self.output_weights[HS..]) {
            output += crelu::<QA>(input) * i32::from(weight);
        }

        output *= QS;
        output /= i32::from(QA) * i32::from(QB);

        output as i16
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C, align(64))]
pub struct Accumulator<const HS: usize> {
    vals: [i16; HS],
}

impl<const HS: usize> Accumulator<HS>
where
    [(); 2 * HS]:,
{
    #[inline(always)]
    pub fn new(net: &Network<HS>) -> Self {
        net.feature_bias
    }

    #[inline(always)]
    pub fn add_feature(&mut self, feature_idx: usize, net: &Network<HS>) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i += *d
        }
    }

    #[inline(always)]
    pub fn remove_feature(&mut self, feature_idx: usize, net: &Network<HS>) {
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
pub struct AccumulatorPair<const HS: usize> {
    pub white: Accumulator<HS>,
    pub black: Accumulator<HS>,
}

impl<const HS: usize> AccumulatorPair<HS>
where
    [(); 2 * HS]:,
{
    pub fn new() -> Self {
        Self {
            white: Accumulator { vals: [0; HS] },
            black: Accumulator { vals: [0; HS] },
        }
    }

    pub fn load(&mut self, board: &chess_v2::ChessGame, net: &Network<HS>) {
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
        src: &AccumulatorPair<HS>,
        add: PairFeature,
        sub: PairFeature,
        net: &Network<HS>,
    ) {
        let white_add = &net.feature_weights[(add >> 16) as usize].vals;
        let black_add = &net.feature_weights[(add & 0xFFFF) as usize].vals;
        let white_sub = &net.feature_weights[(sub >> 16) as usize].vals;
        let black_sub = &net.feature_weights[(sub & 0xFFFF) as usize].vals;

        for i in 0..HS {
            self.white.vals[i] = src.white.vals[i] + white_add[i] - white_sub[i];
            self.black.vals[i] = src.black.vals[i] + black_add[i] - black_sub[i];
        }
    }

    #[inline(always)]
    pub fn acc_add1_sub2_src(
        &mut self,
        src: &AccumulatorPair<HS>,
        add: PairFeature,
        sub1: PairFeature,
        sub2: PairFeature,
        net: &Network<HS>,
    ) {
        let white_add = &net.feature_weights[(add >> 16) as usize].vals;
        let black_add = &net.feature_weights[(add & 0xFFFF) as usize].vals;
        let white_sub1 = &net.feature_weights[(sub1 >> 16) as usize].vals;
        let black_sub1 = &net.feature_weights[(sub1 & 0xFFFF) as usize].vals;
        let white_sub2 = &net.feature_weights[(sub2 >> 16) as usize].vals;
        let black_sub2 = &net.feature_weights[(sub2 & 0xFFFF) as usize].vals;

        for i in 0..HS {
            self.white.vals[i] = src.white.vals[i] + white_add[i] - white_sub1[i] - white_sub2[i];
            self.black.vals[i] = src.black.vals[i] + black_add[i] - black_sub1[i] - black_sub2[i];
        }
    }

    #[inline(always)]
    pub fn acc_add2_sub2_src(
        &mut self,
        src: &AccumulatorPair<HS>,
        add1: PairFeature,
        add2: PairFeature,
        sub1: PairFeature,
        sub2: PairFeature,
        net: &Network<HS>,
    ) {
        let white_add1 = &net.feature_weights[(add1 >> 16) as usize].vals;
        let black_add1 = &net.feature_weights[(add1 & 0xFFFF) as usize].vals;
        let white_add2 = &net.feature_weights[(add2 >> 16) as usize].vals;
        let black_add2 = &net.feature_weights[(add2 & 0xFFFF) as usize].vals;
        let white_sub1 = &net.feature_weights[(sub1 >> 16) as usize].vals;
        let black_sub1 = &net.feature_weights[(sub1 & 0xFFFF) as usize].vals;
        let white_sub2 = &net.feature_weights[(sub2 >> 16) as usize].vals;
        let black_sub2 = &net.feature_weights[(sub2 & 0xFFFF) as usize].vals;

        for i in 0..HS {
            self.white.vals[i] =
                src.white.vals[i] + white_add1[i] + white_add2[i] - white_sub1[i] - white_sub2[i];
            self.black.vals[i] =
                src.black.vals[i] + black_add1[i] + black_add2[i] - black_sub1[i] - black_sub2[i];
        }
    }

    #[inline(always)]
    pub fn add_piece(&mut self, piece_id: usize, to_sq: usize, net: &Network<HS>) {
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

pub struct LazyNnue<const HS: usize>
where
    [(); 2 * HS]:,
{
    net: Network<HS>,
    accumulators: Vec<AccumulatorPair<HS>>,
    updates: Vec<NnueUpdate>,
    applied_accumulators: Vec<usize>,
}

impl<const HS: usize> LazyNnue<HS>
where
    [(); 2 * HS]:,
{
    pub fn heap_alloc(net: Network<HS>) -> Box<Self> {
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
    pub fn evaluate(&mut self, b_move: bool) -> i16 {
        let start = self.applied_accumulators.last().copied().unwrap();

        debug_assert!(start <= self.updates.len());
        debug_assert!(start < self.accumulators.len());
        debug_assert!(self.updates.len() < self.accumulators.len());

        let ply = self.updates.len().min(self.accumulators.len() - 1);
        for i in start + 1..=ply {
            let update = &self.updates[i - 1];

            let (prev, acc) = self.accumulators.split_at_mut(i);

            let prev = prev.last().unwrap();

            // assert!(
            //     acc.first_mut().is_some(),
            //     "Accumulator not allocated for ply {}",
            //     i
            // );

            let acc = acc.first_mut().unwrap();

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

        let us = [&acc.white, &acc.black][b_move as usize];
        let them = [&acc.black, &acc.white][b_move as usize];
        self.net.evaluate(us, them)
    }
}

impl<const HS: usize> UpdatableNnue for LazyNnue<HS>
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
    ($path:expr, $hs:expr) => {{
        let net: &nnue::Network<$hs> = &unsafe { std::mem::transmute(*include_bytes!($path)) };

        nnue::LazyNnue::heap_alloc(*net)
    }};
}
