use std::io::BufRead;

use crate::engine::{chess, tables};

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
    board: &chess::ChessGame,
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
            let moves = match super::pgn::parse_pgn(&mut parts, &mut opening_board, tables) {
                Ok(moves) => moves,
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
                moves,
            });
        }
    }

    Ok(())
}

/// Returns true if side2move is checkmated
fn is_board_checkmated(board: &chess::ChessGame, tables: &tables::Tables) -> bool {
    let mut is_checkmate = false;

    let mut move_list = [0u16; 256];
    if board.in_check_slow(tables, board.b_move) {
        is_checkmate = (0..board.gen_moves_slow(tables, &mut move_list)).all(|mv_index| {
            let mv = move_list[mv_index];

            let mut board_copy = board.clone();

            if !board_copy.make_move_slow(mv, tables) {
                return true;
            }

            if board_copy.in_check_slow(tables, !board_copy.b_move) {
                return true;
            }

            false
        });
    }

    is_checkmate
}
