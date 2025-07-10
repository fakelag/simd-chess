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
            _ => panic!("Invalid piece ID: {}", value),
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

pub const fn hex_to_f4_color(hex: u32, a: f32) -> [f32; 4] {
    let r = ((hex >> 16) & 0xFF) as f32 / 255.0;
    let g = ((hex >> 8) & 0xFF) as f32 / 255.0;
    let b = ((hex >> 0) & 0xFF) as f32 / 255.0;
    [r, g, b, a]
}

pub fn square_name(index: u8) -> String {
    let file = index % 8;
    let rank = index / 8;
    format!("{}{}", (b'a' + file as u8) as char, rank + 1)
}

pub fn square_index(name: &str) -> u8 {
    let file = name.chars().next().unwrap() as u8 - b'a';
    let rank = name.chars().nth(1).unwrap().to_digit(10).unwrap() as u8 - 1;
    rank * 8 + file
}

pub fn create_move(move_str: &str) -> u16 {
    let from = square_index(&move_str[0..2]);
    let to = square_index(&move_str[2..4]);
    (from as u16) | ((to as u16) << 6)
}
