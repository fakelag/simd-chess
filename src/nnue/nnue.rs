use crate::{
    engine::chess_v2::{self, PieceIndex},
    pop_ls1b,
};

const HIDDEN_SIZE: usize = 128;
const SCALE: i32 = 400;
const QA: i16 = 255;
const QB: i16 = 64;

#[inline]
fn crelu(x: i16) -> i32 {
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
pub struct Network {
    feature_weights: [Accumulator; 768],
    feature_bias: Accumulator,
    output_weights: [i16; 2 * HIDDEN_SIZE],
    output_bias: i16,
}

impl Network {
    #[inline(always)]
    pub fn evaluate(&self, us: &Accumulator, them: &Accumulator) -> i32 {
        let mut output = i32::from(self.output_bias);

        for (&input, &weight) in us.vals.iter().zip(&self.output_weights[..HIDDEN_SIZE]) {
            output += crelu(input) * i32::from(weight);
        }

        for (&input, &weight) in them.vals.iter().zip(&self.output_weights[HIDDEN_SIZE..]) {
            output += crelu(input) * i32::from(weight);
        }

        output *= SCALE;
        output /= i32::from(QA) * i32::from(QB);

        output
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C, align(64))]
pub struct Accumulator {
    vals: [i16; HIDDEN_SIZE],
}

impl Accumulator {
    #[inline(always)]
    pub fn new(net: &Network) -> Self {
        net.feature_bias
    }

    #[inline(always)]
    pub fn add_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i += *d
        }
    }

    #[inline(always)]
    pub fn remove_feature(&mut self, feature_idx: usize, net: &Network) {
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
pub struct AccumulatorPair {
    pub white: Accumulator,
    pub black: Accumulator,
}

impl AccumulatorPair {
    pub fn new() -> Self {
        Self {
            white: Accumulator {
                vals: [0; HIDDEN_SIZE],
            },
            black: Accumulator {
                vals: [0; HIDDEN_SIZE],
            },
        }
    }

    pub fn load(&mut self, board: &chess_v2::ChessGame, net: &Network) {
        let bitboards = board.bitboards_new();

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
    pub fn move_piece(&mut self, piece_id: usize, from_sq: usize, to_sq: usize, net: &Network) {
        debug_assert!(
            piece_id != PieceIndex::WhiteNullPiece as usize
                && piece_id != PieceIndex::WhitePad as usize
                && piece_id != PieceIndex::BlackNullPiece as usize
                && piece_id != PieceIndex::BlackPad as usize
        );

        let (white_feature_from, black_feature_from) =
            Self::calc_feature_indices(piece_id, from_sq);
        let (white_feature_to, black_feature_to) = Self::calc_feature_indices(piece_id, to_sq);

        self.white.remove_feature(white_feature_from, net);
        self.white.add_feature(white_feature_to, net);

        self.black.remove_feature(black_feature_from, net);
        self.black.add_feature(black_feature_to, net);
    }

    #[inline(always)]
    pub fn add_piece(&mut self, piece_id: usize, to_sq: usize, net: &Network) {
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
    pub fn remove_piece(&mut self, piece_id_or_null: usize, from_sq: usize, net: &Network) {
        if piece_id_or_null == PieceIndex::WhiteNullPiece as usize {
            return;
        }

        debug_assert!(
            piece_id_or_null != PieceIndex::WhiteNullPiece as usize
                && piece_id_or_null != PieceIndex::WhitePad as usize
                && piece_id_or_null != PieceIndex::BlackNullPiece as usize
                && piece_id_or_null != PieceIndex::BlackPad as usize
        );

        let (white_feature, black_feature) = Self::calc_feature_indices(piece_id_or_null, from_sq);

        self.white.remove_feature(white_feature, net);
        self.black.remove_feature(black_feature, net);
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

pub struct NnueQuietMove {
    from_piece_id: u8,
    to_piece_id: u8,
    from_sq: u8,
    to_sq: u8,
}

pub struct NnueCaptureMove {
    from_piece_id: u8,
    to_piece_id: u8,
    from_sq: u8,
    to_sq: u8,
    captured_piece_id: u8,
    captured_sq: u8,
}

pub struct NnueCastleMove {
    rook_piece_id: u8,
    rook_from_sq: u8,
    rook_to_sq: u8,
    king_piece_id: u8,
    king_from_sq: u8,
    king_to_sq: u8,
}

pub enum NnueUpdate {
    NnueUpdateQuiet(NnueQuietMove),
    NnueUpdateCapture(NnueCaptureMove),
    NnueUpdateCastle(NnueCastleMove),
}

impl NnueUpdate {
    #[inline(always)]
    pub fn quiet(from_piece_id: u8, to_piece_id: u8, from_sq: u8, to_sq: u8) -> Self {
        NnueUpdate::NnueUpdateQuiet(NnueQuietMove {
            from_piece_id,
            to_piece_id,
            from_sq,
            to_sq,
        })
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
        NnueUpdate::NnueUpdateCapture(NnueCaptureMove {
            from_piece_id,
            to_piece_id,
            from_sq,
            to_sq,
            captured_piece_id,
            captured_sq,
        })
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
        NnueUpdate::NnueUpdateCastle(NnueCastleMove {
            rook_piece_id,
            rook_from_sq,
            rook_to_sq,
            king_piece_id,
            king_from_sq,
            king_to_sq,
        })
    }

    #[inline(always)]
    pub fn set_to_piece_id(&mut self, to_piece_id: u8) {
        match self {
            NnueUpdate::NnueUpdateQuiet(mv) => mv.to_piece_id = to_piece_id,
            NnueUpdate::NnueUpdateCapture(mv) => mv.to_piece_id = to_piece_id,
            NnueUpdate::NnueUpdateCastle(_) => {
                unreachable!("Cannot set to_piece_id on castle move")
            }
        }
    }
}

pub trait UpdatableNnue {
    fn update(&mut self, update: &NnueUpdate);
}

pub struct Nnue {
    pub net: Network,
    pub acc: AccumulatorPair,
}

impl Nnue {
    pub fn new(net: Network) -> Self {
        let acc = AccumulatorPair::new();
        Nnue { net, acc }
    }

    pub fn load(&mut self, board: &chess_v2::ChessGame) {
        self.acc.load(board, &self.net);
    }

    #[inline(always)]
    pub fn move_piece(&mut self, piece_id: usize, from_sq: usize, to_sq: usize) {
        self.acc.move_piece(piece_id, from_sq, to_sq, &self.net);
    }

    #[inline(always)]
    pub fn add_piece(&mut self, piece_id: usize, to_sq: usize) {
        self.acc.add_piece(piece_id, to_sq, &self.net);
    }

    #[inline(always)]
    pub fn remove_piece(&mut self, piece_id_or_null: usize, from_sq: usize) {
        self.acc.remove_piece(piece_id_or_null, from_sq, &self.net);
    }

    #[inline(always)]
    pub fn evaluate(&self, b_move: bool) -> i32 {
        let us = [&self.acc.white, &self.acc.black][b_move as usize];
        let them = [&self.acc.black, &self.acc.white][b_move as usize];
        self.net.evaluate(us, them)
    }
}

impl UpdatableNnue for Nnue {
    #[inline(always)]
    fn update(&mut self, update: &NnueUpdate) {
        match update {
            NnueUpdate::NnueUpdateQuiet(mv) => {
                self.remove_piece(mv.from_piece_id as usize, mv.from_sq as usize);
                self.add_piece(mv.to_piece_id as usize, mv.to_sq as usize);
            }
            NnueUpdate::NnueUpdateCapture(mv) => {
                self.remove_piece(mv.from_piece_id as usize, mv.from_sq as usize);
                self.remove_piece(mv.captured_piece_id as usize, mv.captured_sq as usize);
                self.add_piece(mv.to_piece_id as usize, mv.to_sq as usize);
            }
            NnueUpdate::NnueUpdateCastle(mv) => {
                self.move_piece(
                    mv.rook_piece_id as usize,
                    mv.rook_from_sq as usize,
                    mv.rook_to_sq as usize,
                );
                self.move_piece(
                    mv.king_piece_id as usize,
                    mv.king_from_sq as usize,
                    mv.king_to_sq as usize,
                );
            }
        }
    }
}

#[macro_export]
macro_rules! nnue_load {
    ($path:expr) => {{
        let net: &nnue::Network = &unsafe { std::mem::transmute(*include_bytes!($path)) };

        let mut nnue = Box::<nnue::Nnue>::new_uninit();
        let ptr = nnue.as_mut_ptr();

        unsafe {
            (*ptr).acc = nnue::AccumulatorPair::new();
            (*ptr).net = *net;
            nnue.assume_init()
        }
    }};
}
