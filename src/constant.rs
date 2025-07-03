#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    White,
    Black,
    SideMax,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PieceId {
    WhiteKing,
    WhiteQueen,
    WhiteRook,
    WhiteBishop,
    WhiteKnight,
    WhitePawn,

    BlackKing,
    BlackQueen,
    BlackRook,
    BlackBishop,
    BlackKnight,
    BlackPawn,

    PieceMax,
}

impl From<usize> for PieceId {
    fn from(value: usize) -> Self {
        match value {
            0 => PieceId::WhiteKing,
            1 => PieceId::WhiteQueen,
            2 => PieceId::WhiteRook,
            3 => PieceId::WhiteBishop,
            4 => PieceId::WhiteKnight,
            5 => PieceId::WhitePawn,
            6 => PieceId::BlackKing,
            7 => PieceId::BlackQueen,
            8 => PieceId::BlackRook,
            9 => PieceId::BlackBishop,
            10 => PieceId::BlackKnight,
            11 => PieceId::BlackPawn,
            _ => panic!("Invalid piece ID"),
        }
    }
}

impl Into<usize> for PieceId {
    fn into(self) -> usize {
        match self {
            PieceId::WhiteKing => 0,
            PieceId::WhiteQueen => 1,
            PieceId::WhiteRook => 2,
            PieceId::WhiteBishop => 3,
            PieceId::WhiteKnight => 4,
            PieceId::WhitePawn => 5,
            PieceId::BlackKing => 6,
            PieceId::BlackQueen => 7,
            PieceId::BlackRook => 8,
            PieceId::BlackBishop => 9,
            PieceId::BlackKnight => 10,
            PieceId::BlackPawn => 11,
            PieceId::PieceMax => panic!("Invalid piece ID"),
        }
    }
}

pub const PICE_IMAGES: [&str; PieceId::PieceMax as usize] = [
    "assets/w_king.png",
    "assets/w_queen.png",
    "assets/w_rook.png",
    "assets/w_bishop.png",
    "assets/w_knight.png",
    "assets/w_pawn.png",
    "assets/b_king.png",
    "assets/b_queen.png",
    "assets/b_rook.png",
    "assets/b_bishop.png",
    "assets/b_knight.png",
    "assets/b_pawn.png",
];
