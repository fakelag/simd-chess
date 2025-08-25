use crate::engine::{
    chess::{self},
    search, tables,
};

pub const FEN_STARTPOS: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[repr(align(64))]
pub struct Align64<T>(pub T);

#[macro_export]
macro_rules! pop_ls1b {
    ($bitboard:ident) => {{
        let ls1b_index = $bitboard.trailing_zeros() as u16;

        if ls1b_index == 64 {
            break;
        }

        $bitboard ^= 1 << ls1b_index;

        ls1b_index
    }};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    White = 0,
    Black = 1,
    SideMax,
}

impl From<bool> for Side {
    fn from(value: bool) -> Self {
        if value { Side::Black } else { Side::White }
    }
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

impl Into<char> for PieceId {
    fn into(self) -> char {
        match self {
            PieceId::WhiteKing => 'K',
            PieceId::WhiteQueen => 'Q',
            PieceId::WhiteRook => 'R',
            PieceId::WhiteBishop => 'B',
            PieceId::WhiteKnight => 'N',
            PieceId::WhitePawn => 'P',
            PieceId::BlackKing => 'k',
            PieceId::BlackQueen => 'q',
            PieceId::BlackRook => 'r',
            PieceId::BlackBishop => 'b',
            PieceId::BlackKnight => 'n',
            PieceId::BlackPawn => 'p',
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

pub const fn table_mirror<T: Copy, const N: usize>(table: [T; N], stride: usize) -> [T; N] {
    let num_rows = N / stride;
    let mut mirrored: [T; N] = [table[0]; N];
    let mut copyindex = 0;
    loop {
        if copyindex == N {
            break;
        }
        mirrored[copyindex] = table[copyindex];
        copyindex += 1;
    }
    let mut row = 0;
    loop {
        if row >= num_rows / 2 {
            break;
        }
        let mut col = 0;
        loop {
            if col >= stride {
                break;
            }
            mirrored[row * stride + col] = table[(num_rows - row - 1) * stride + col];
            mirrored[(num_rows - row - 1) * stride + col] = table[row * stride + col];
            col += 1;
        }

        row += 1;
    }
    mirrored
}

macro_rules! def_table_negate {
    ($name:ident,$ty:ty) => {
        pub const fn $name<const N: usize>(table: [$ty; N]) -> [$ty; N] {
            let mut negated = [table[0]; N];
            let mut i = 0;
            loop {
                if i == N {
                    break;
                }
                negated[i] = -table[i];
                i += 1;
            }
            negated
        }
    };
}

def_table_negate!(table_negate_i8, i8);
def_table_negate!(table_negate_i32, i32);

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
    assert!(
        move_str.len() >= 4 && move_str.len() <= 5,
        "Invalid move string length: {}",
        move_str
    );
    debug_assert!(move_str != "0000", "Null move: {}", move_str);

    let from = square_index(&move_str[0..2]);
    let to = square_index(&move_str[2..4]);

    let flag_bits = if move_str.len() > 4 {
        match &move_str[4..] {
            "b" => chess::MV_FLAGS_PR_BISHOP,
            "n" => chess::MV_FLAGS_PR_KNIGHT,
            "r" => chess::MV_FLAGS_PR_ROOK,
            "q" => chess::MV_FLAGS_PR_QUEEN,
            _ => panic!("Invalid move flag in move string: {}", move_str),
        }
    } else {
        0
    };

    (from as u16) | ((to as u16) << 6) | flag_bits
}

pub fn move_flag_name(mv: u16) -> &'static str {
    match mv & chess::MV_FLAGS_PR_MASK {
        chess::MV_FLAGS_PR_BISHOP => "b",
        chess::MV_FLAGS_PR_KNIGHT => "n",
        chess::MV_FLAGS_PR_ROOK => "r",
        chess::MV_FLAGS_PR_QUEEN => "q",
        _ => "",
    }
}

pub fn move_flag_name_dbg(mv: u16) -> String {
    let promotion = match mv & chess::MV_FLAGS_PR_MASK {
        chess::MV_FLAGS_PR_BISHOP => "+b",
        chess::MV_FLAGS_PR_KNIGHT => "+n",
        chess::MV_FLAGS_PR_ROOK => "+r",
        chess::MV_FLAGS_PR_QUEEN => "+q",
        _ => "",
    };
    let cap = if (mv & chess::MV_FLAGS) == chess::MV_FLAG_EPCAP {
        "+epc"
    } else if mv & chess::MV_FLAG_CAP != 0 {
        "+c"
    } else {
        ""
    };
    let dpp = if (mv & chess::MV_FLAGS) == chess::MV_FLAG_DPP {
        "+dpp"
    } else {
        ""
    };
    let castle = if (mv & chess::MV_FLAGS) == chess::MV_FLAGS_CASTLE_KING {
        "+kc"
    } else if (mv & chess::MV_FLAGS) == chess::MV_FLAGS_CASTLE_QUEEN {
        "+qc"
    } else {
        ""
    };

    format!("{}{}{}{}", promotion, cap, dpp, castle)
}

pub fn move_string(mv: u16) -> String {
    format!(
        "{}{}{}",
        square_name((mv & 0x3F) as u8),
        square_name(((mv >> 6) & 0x3F) as u8),
        move_flag_name(mv)
    )
}

pub fn move_string_dbg(mv: u16) -> String {
    format!(
        "{}{}{}",
        square_name((mv & 0x3F) as u8),
        square_name(((mv >> 6) & 0x3F) as u8),
        move_flag_name_dbg(mv)
    )
}

pub fn time_format(ms: u64) -> String {
    format!("{}:{:02}.{:03}", ms / 60000, (ms / 1000) % 60, ms % 1000,)
}

pub fn parse_position<'a>(
    position_buf: &'a str,
    board: &mut chess::ChessGame,
    tables: &tables::Tables,
    mut repetition_table: Option<&mut search::repetition_v2::RepetitionTable>,
    mut out_moves: Option<&mut Vec<u16>>,
    out_fen: Option<&mut String>,
) -> anyhow::Result<()> {
    let mut fen = FEN_STARTPOS;
    let mut whitespace_it = position_buf.split_whitespace();

    let moves_it = if let Some(position_string) = whitespace_it.next() {
        match position_string {
            "startpos" => {
                board.load_fen(FEN_STARTPOS, tables).unwrap();
                Some(whitespace_it)
            }
            "fen" => {
                let fen_start_index = position_string.as_ptr() as usize
                    - position_buf.as_ptr() as usize
                    + position_string.len()
                    + 1;

                let fen_length = match board.load_fen(&position_buf[fen_start_index..], tables) {
                    Ok(fen_length) => fen_length,
                    Err(e) => return Err(anyhow::anyhow!("Failed to load FEN: {}", e)),
                };

                fen = &position_buf[fen_start_index..fen_start_index + fen_length];

                Some(position_buf[fen_start_index + fen_length..].split_whitespace())
            }
            _ => None,
        }
    } else {
        None
    };

    if let Some(out_fen) = out_fen {
        *out_fen = fen.to_string();
    }

    let mut moves_it = match moves_it {
        Some(it) => it,
        None => {
            return Err(anyhow::anyhow!(
                "Expected 'startpos' or 'fen' in position command"
            ));
        }
    };

    if let Some(rep_table) = &mut repetition_table {
        rep_table.push_position(board.zobrist_key(), true);
    }

    if let Some("moves") = moves_it.next() {
        while let Some(mv_str) = moves_it.next() {
            let mv = board.fix_move(create_move(mv_str));

            if !board.make_move_slow(mv, &tables) {
                return Err(anyhow::anyhow!("Invalid move: {}", mv_str));
            }

            if let Some(out_moves) = &mut out_moves {
                out_moves.push(mv);
            }

            if let Some(rep_table) = &mut repetition_table {
                let is_irreversible = board.half_moves() == 0;
                rep_table.push_position(board.zobrist_key(), is_irreversible);
            }
        }
    }
    // Pop off last move from repetition table to prevent duplicates,
    // the search will start from pushing the current board position
    // into the stack
    if let Some(rep_table) = &mut repetition_table {
        rep_table.pop_position();
    }

    Ok(())
}
