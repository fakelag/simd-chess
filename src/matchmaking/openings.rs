use std::io::BufRead;

use crate::{
    engine::{chess_v2, tables},
    matchmaking::matchmaking::PositionFeeder,
    pgn::moves::parse_moves,
    util,
};

pub struct OpeningFeeder {
    opening_book: OpeningBook,
    tables: tables::Tables,
    startpos: chess_v2::ChessGame,
    fen: String,
    next_op: usize,
}

impl OpeningFeeder {
    pub fn new() -> anyhow::Result<Self> {
        let tables = tables::Tables::new();
        let mut startpos = chess_v2::ChessGame::new();

        startpos
            .load_fen(util::FEN_STARTPOS, &tables)
            .map_err(|e| {
                anyhow::anyhow!(
                    "Failed to load startpos FEN ({}): {}",
                    util::FEN_STARTPOS,
                    e
                )
            })?;

        let mut opening_list = Vec::new();
        load_openings_from_dir(&startpos, &tables, &mut opening_list)?;

        Ok(Self {
            tables,
            startpos,
            opening_book: OpeningBook::new(opening_list),
            fen: util::FEN_STARTPOS.to_string(),
            next_op: 2, // Return startpos for first two calls
        })
    }
}

impl PositionFeeder for OpeningFeeder {
    fn next_position(&mut self) -> Option<String> {
        if self.next_op > 0 {
            self.next_op -= 1;
            return Some(self.fen.clone());
        }

        let opening = match self.opening_book.next() {
            Some(opening) => opening,
            None => return None,
        };

        let mut board = self.startpos.clone();

        for mv in opening.moves {
            if unsafe { !board.make_move(mv, &self.tables) } {
                panic!(
                    "Failed to make opening move {} for opening \"{}\"",
                    mv, opening.name
                );
            }

            if board.in_check(&self.tables, !board.b_move()) {
                panic!(
                    "Opening move {} leaves king in check for opening \"{}\"",
                    mv, opening.name
                );
            }
        }

        self.fen = board.gen_fen();
        self.next_op = 1; // Repeat this position once

        Some(self.fen.clone())
    }
}

#[derive(Debug, Clone)]
pub struct OpeningMoves {
    pub eco: String,
    pub name: String,
    pub moves: Vec<u16>,
}

pub struct OpeningBook {
    opening_index: Vec<(String, Vec<OpeningMoves>)>,
    cursor: usize,
}

impl OpeningBook {
    pub fn new(opening_list: Vec<OpeningMoves>) -> Self {
        let mut openings_by_eco: std::collections::BTreeMap<String, Vec<OpeningMoves>> =
            std::collections::BTreeMap::new();

        for opening in opening_list.into_iter().rev() {
            openings_by_eco
                .entry(opening.eco.clone())
                .or_default()
                .push(opening);
        }

        let opening_index = openings_by_eco
            .into_iter()
            .map(|(eco, openings)| (eco, openings))
            .collect();

        OpeningBook {
            opening_index,
            cursor: 0,
        }
    }
}

impl Iterator for OpeningBook {
    type Item = OpeningMoves;

    fn next(&mut self) -> Option<Self::Item> {
        if self.opening_index.is_empty() {
            return None;
        }

        let eco_index = self.cursor % self.opening_index.len();
        let eco_openings = &mut self.opening_index[eco_index];

        if eco_openings.1.is_empty() {
            self.opening_index.remove(eco_index);
            return self.next();
        } else {
            let opening = eco_openings.1.pop().unwrap();
            self.cursor += 1;
            return Some(opening);
        }
    }
}

pub fn load_openings_from_dir(
    board: &chess_v2::ChessGame,
    tables: &tables::Tables,
    opening_list: &mut Vec<OpeningMoves>,
) -> anyhow::Result<()> {
    opening_list.clear();

    let opening_files = std::fs::read_dir("openings")
        .expect("Failed to read openings directory")
        .filter_map(Result::ok)
        .filter(|entry| {
            entry.metadata().map_or(false, |meta| meta.is_file())
                && entry.path().extension().map_or(false, |ext| ext == "tsv")
        })
        .map(|entry| entry.path());

    let mut moves_buf = Vec::new();

    for file_path in opening_files {
        let file = match std::fs::File::open(&file_path) {
            Ok(file) => file,
            Err(err) => {
                eprintln!("Failed to open file \"{:?}\": {}", file_path, err);
                continue;
            }
        };

        let lines = std::io::BufReader::new(file).lines();

        for (line_num, line) in lines.enumerate() {
            if line_num == 0 {
                // Skip the header line
                continue;
            }

            let line = match line {
                Ok(line) => line,
                Err(e) => {
                    eprintln!(
                        "Failed to read opening line from file {:?}: {}",
                        file_path, e
                    );
                    continue;
                }
            };

            let mut opening_board = board.clone();
            let mut parts = line.split('\t');

            let eco = parts.next().expect("Missing ECO code in opening line");
            let name = parts.next().expect("Missing opening name in opening line");
            let move_str = parts.next().expect("Missing moves in opening line");

            moves_buf.clear();

            let moves = match parse_moves(move_str, &mut opening_board, tables, &mut moves_buf) {
                Ok(_) => &moves_buf,
                Err(e) => {
                    eprintln!(
                        "Failed to parse opening \"{}\" line in file {:?}: {}",
                        name, file_path, e
                    );
                    continue;
                }
            };

            if is_board_checkmated(&opening_board, tables) {
                // Skip checkmate openings
                continue;
            }

            opening_list.push(OpeningMoves {
                eco: eco.to_string(),
                name: name.to_string(),
                moves: moves.clone(),
            });
        }
    }

    Ok(())
}

/// Returns true if side2move is checkmated
fn is_board_checkmated(board: &chess_v2::ChessGame, tables: &tables::Tables) -> bool {
    if !board.in_check(tables, board.b_move()) {
        return false;
    }

    let mut move_list = [0u16; 256];

    // No need to try all promotion variants
    let is_checkmate =
        (0..board.gen_moves_avx512::<false>(tables, &mut move_list)).all(|mv_index| {
            let mv = move_list[mv_index];

            let mut board_copy = board.clone();

            if unsafe { !board_copy.make_move(mv, tables) } {
                return true;
            }

            if board_copy.in_check(tables, !board_copy.b_move()) {
                return true;
            }

            false
        });

    is_checkmate
}
