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
