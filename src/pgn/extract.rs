use std::{
    fs::File,
    io::{BufReader, Write},
};

use rand::{Rng, SeedableRng};

use crate::{
    engine::{
        chess_v2::{self, ChessGame},
        tables,
    },
    pgn::parse::{PgnGame, PgnGameParser, PgnGameTermination, PgnReadBuf},
    util,
};

pub enum MovePick {
    First,
    Random,
}

pub struct PositionExtractParams {
    pub fcompleted_only: bool,
    pub ffrom_ply: usize,
    pub fto_ply: usize,
    pub fmin_ply: usize,
    pub fnum_positions: usize,
    pub fdist_openings: bool,
    pub fno_duplicates: bool,
    pub fpick: MovePick,
    pub fseed: u64,
    pub fattempts_per_position: usize,
}

struct ExtractContext<'a> {
    games: &'a mut Vec<PgnGame>,
    storage: &'a mut PgnReadBuf,
    moves: &'a mut Vec<u16>,
    board: ChessGame,
    tables: tables::Tables,
    rng: rand::rngs::StdRng,
    zobrist_set: std::collections::BTreeSet<u64>,
    opening_map: std::collections::BTreeMap<String, usize>,
    sum_openings: usize,
    count_openings: usize,
    buffer: String,
    game_count: usize,
    writer: std::io::BufWriter<File>,
}

impl ExtractContext<'_> {
    fn process_batch(&mut self, params: &PositionExtractParams) -> bool {
        for game in self.games.drain(..) {
            self.moves.clear();

            if params.fcompleted_only
                && game.termination(&self.storage) != Some(PgnGameTermination::Normal)
            {
                continue;
            }

            if params.fdist_openings {
                let op_entry = match game.opening(&self.storage) {
                    Some(op) => {
                        let key = op.to_string();
                        self.opening_map.entry(key)
                    }
                    None => continue,
                };

                let op_count = op_entry.or_insert(0usize);

                if *op_count == 0 {
                    self.count_openings += 1;
                }

                self.sum_openings += 1;

                if (*op_count as f32) < ((self.sum_openings / self.count_openings).max(1) as f32) {
                    *op_count += 1;
                } else {
                    continue;
                }
            }

            match game.parse_moves(&self.storage, &self.board, &self.tables, self.moves) {
                Some(Ok(())) => {
                    if self.moves.len() < params.fmin_ply || self.moves.len() < params.ffrom_ply {
                        continue;
                    }

                    for _ in 0..params.fattempts_per_position {
                        let to_move = match params.fpick {
                            MovePick::First => params.ffrom_ply,
                            MovePick::Random => {
                                let min = params.ffrom_ply;
                                let max = self.moves.len().min(params.fto_ply);

                                if min >= max {
                                    continue;
                                }

                                self.rng.random_range(min..max)
                            }
                        };

                        let mut game_board = self.board.clone();

                        for mv in self.moves.iter().take(to_move) {
                            unsafe { game_board.make_move(*mv, &self.tables) };
                        }

                        if params.fno_duplicates
                            && !self.zobrist_set.insert(game_board.zobrist_key())
                        {
                            // println!("Skipping duplicate position {}", game_board.gen_fen());
                            continue;
                        }

                        if self.game_count % 50_000 == 0 {
                            println!(
                                "Extracting positions {}/{} ({:.2}%)",
                                self.game_count,
                                params.fnum_positions,
                                (self.game_count as f64 / params.fnum_positions as f64) * 100.0
                            );
                        }

                        self.buffer.push_str(&format!("{}\n", game_board.gen_fen()));
                        self.game_count += 1;
                        break;
                    }
                }
                Some(Err(e)) => {
                    eprintln!(
                        "Failed to parse moves for game {:?}: {}",
                        game.site(&self.storage),
                        e
                    );
                    continue;
                }
                None => continue,
            };

            if self.buffer.len() >= 1024 * 1024 {
                self.writer.write_all(self.buffer.as_bytes()).unwrap();
                self.buffer.clear();
            }

            if self.game_count >= params.fnum_positions {
                self.writer.write_all(self.buffer.as_bytes()).unwrap();
                self.buffer.clear();
                return false;
            }
        }

        true
    }
}

pub fn extract_positions(
    db_path: &str,
    out_path: &str,
    params: PositionExtractParams,
) -> anyhow::Result<()> {
    let tables = tables::Tables::new();

    let mut ctx = ExtractContext {
        games: &mut Vec::new(),
        storage: &mut PgnReadBuf::new(),
        moves: &mut Vec::new(),
        board: {
            let mut board = chess_v2::ChessGame::new();
            assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());
            board
        },
        tables,
        rng: rand::rngs::StdRng::seed_from_u64(params.fseed),
        zobrist_set: std::collections::BTreeSet::new(),
        opening_map: std::collections::BTreeMap::new(),
        sum_openings: 0,
        count_openings: 0,
        buffer: String::new(),
        game_count: 0,
        writer: {
            let file = File::create(out_path)?;
            std::io::BufWriter::new(file)
        },
    };

    let path = std::path::Path::new(db_path);

    match path.try_exists() {
        Ok(true) => {}
        Ok(false) => {
            return Err(anyhow::anyhow!("Pgn file path {} does not exist", db_path));
        }
        Err(e) => {
            return Err(anyhow::anyhow!(
                "Failed to access pgn file path {}: {}",
                db_path,
                e
            ));
        }
    }

    if !path.is_file() {
        return Err(anyhow::anyhow!("Pgn path {} is not a file", db_path));
    }

    let db_extension = path.extension().and_then(std::ffi::OsStr::to_str);

    let mut it = PgnGameParser::new();
    let mut total_games = 0usize;

    match db_extension {
        Some("zst") => {
            let file = File::open(path)?;
            let buf = BufReader::new(file);

            let mut decoder = zstd::Decoder::new(buf)?;

            loop {
                if !it.next_batch(&mut decoder, &mut ctx.storage, &mut ctx.games) {
                    break;
                }

                total_games += ctx.games.len();

                if !ctx.process_batch(&params) {
                    break;
                }
            }
        }
        Some("pgn") => {
            let file = File::open(path)?;
            let mut reader = BufReader::new(file);

            loop {
                if !it.next_batch(&mut reader, &mut ctx.storage, &mut ctx.games) {
                    break;
                }

                total_games += ctx.games.len();

                if !ctx.process_batch(&params) {
                    break;
                }
            }
        }
        _ => {
            return Err(anyhow::anyhow!(
                "Unsupported pgn file extension: {:?}",
                db_extension
            ));
        }
    };

    println!(
        "Extracted {} positions (searched {} games)",
        ctx.game_count, total_games
    );

    Ok(())
}
