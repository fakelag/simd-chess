use std::{
    fs::{self, File},
    io::{BufReader, Read, Write},
};

use rand::{Rng, SeedableRng};

use crate::{
    engine::{
        chess_v2::{self, ChessGame},
        tables,
    },
    pgn, util,
};

pub const CHUNK_SIZE: usize = 1024 * 1024 * 4; // 4 MB
pub const BACKBUF_SIZE: usize = 1024 * 32; // 32 KB

pub enum MovePick {
    First,
    Random,
}

pub struct ExtractParams {
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

#[derive(PartialEq, Eq)]
enum LichessIteratorState {
    Reading,
    End,
}

enum LichessGameState {
    Tags,
    Moves,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LichessGameResult {
    WhiteWin,
    BlackWin,
    Draw,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LichessGameTermination {
    Normal,
    TimeForfeit,
    Abandoned,
    RulesInfraction,
    Unterminated,
}

pub struct LichessDatabaseBuf {
    buf: Vec<u8>,
}

impl LichessDatabaseBuf {
    pub fn new() -> Self {
        LichessDatabaseBuf {
            buf: vec![0u8; CHUNK_SIZE + BACKBUF_SIZE],
        }
    }
}

#[derive(Debug)]
pub struct LichessGame {
    event: Option<(usize, usize)>,
    site: Option<(usize, usize)>,
    white: Option<(usize, usize)>,
    black: Option<(usize, usize)>,
    result: Option<(usize, usize)>,
    utc_date: Option<(usize, usize)>,
    utc_time: Option<(usize, usize)>,
    white_elo: Option<(usize, usize)>,
    black_elo: Option<(usize, usize)>,
    white_rating_diff: Option<(usize, usize)>,
    black_rating_diff: Option<(usize, usize)>,
    eco: Option<(usize, usize)>,
    opening: Option<(usize, usize)>,
    time_control: Option<(usize, usize)>,
    termination: Option<(usize, usize)>,
    moves: Option<(usize, usize)>,
}

macro_rules! game_getter {
    ($name:ident, $ty:ty, $field:ident) => {
        pub fn $name(&self, st: &LichessDatabaseBuf) -> Option<$ty> {
            if let Some((start, end)) = self.$field {
                return Some(unsafe { std::mem::transmute::<&[u8], $ty>(&st.buf[start..end]) });
            }
            None
        }
    };
}

impl LichessGame {
    pub fn new() -> Self {
        LichessGame {
            event: None,
            site: None,
            white: None,
            black: None,
            result: None,
            utc_date: None,
            utc_time: None,
            white_elo: None,
            black_elo: None,
            white_rating_diff: None,
            black_rating_diff: None,
            eco: None,
            opening: None,
            time_control: None,
            termination: None,
            moves: None,
        }
    }

    game_getter!(event, &str, event);
    game_getter!(site, &str, site);
    game_getter!(white, &str, white);
    game_getter!(black, &str, black);
    game_getter!(utc_date, &str, utc_date);
    game_getter!(utc_time, &str, utc_time);
    game_getter!(white_elo, &str, white_elo);
    game_getter!(black_elo, &str, black_elo);
    game_getter!(white_rating_diff, &str, white_rating_diff);
    game_getter!(black_rating_diff, &str, black_rating_diff);
    game_getter!(eco, &str, eco);
    game_getter!(opening, &str, opening);
    game_getter!(time_control, &str, time_control);

    pub fn result(&self, st: &LichessDatabaseBuf) -> Option<LichessGameResult> {
        if let Some((start, end)) = self.result {
            return Some(match &st.buf[start..end] {
                b"1-0" => LichessGameResult::WhiteWin,
                b"0-1" => LichessGameResult::BlackWin,
                b"1/2-1/2" => LichessGameResult::Draw,
                b"*" => LichessGameResult::Draw,
                _ => panic!("Invalid result \"{:?}\"", &st.buf[start..end]),
            });
        }
        None
    }

    pub fn termination(&self, st: &LichessDatabaseBuf) -> Option<LichessGameTermination> {
        if let Some((start, end)) = self.termination {
            return Some(match &st.buf[start..end] {
                b"Normal" => LichessGameTermination::Normal,
                b"Time forfeit" => LichessGameTermination::TimeForfeit,
                b"Abandoned" => LichessGameTermination::Abandoned,
                b"Rules infraction" => LichessGameTermination::RulesInfraction,
                b"Unterminated" => LichessGameTermination::Unterminated,
                _ => panic!(
                    "Invalid termination \"{:?}\"",
                    std::string::String::from_utf8_lossy(&st.buf[start..end])
                ),
            });
        }
        None
    }

    pub fn parse_moves(
        &self,
        st: &LichessDatabaseBuf,
        board: &ChessGame,
        tables: &tables::Tables,
        moves: &mut Vec<u16>,
    ) -> Option<anyhow::Result<()>> {
        let moves_str = if let Some((start, end)) = self.moves {
            unsafe { std::mem::transmute::<&[u8], &str>(&st.buf[start..end]) }
        } else {
            return None;
        };

        let result = pgn::parse_pgn(moves_str, &mut board.clone(), tables, moves);
        Some(result)
    }
}

pub struct LichessGameParser {
    state: LichessIteratorState,
    r_start: usize,
    r_size: usize,
}

impl LichessGameParser {
    pub fn new() -> Self {
        LichessGameParser {
            state: LichessIteratorState::Reading,
            r_start: 0,
            r_size: 0,
        }
    }
}

impl LichessGameParser {
    pub fn next_batch<R>(
        &mut self,
        reader: &mut R,
        storage: &mut LichessDatabaseBuf,
        games: &mut Vec<LichessGame>,
    ) -> bool
    where
        R: Read,
    {
        if self.state == LichessIteratorState::End {
            return false;
        }

        let (ch_start, ch_end, rsize) = match self.next_chunk(
            reader,
            self.r_start,
            self.r_size,
            storage.buf.as_mut_slice(),
        ) {
            Ok(Some((ch_start, ch_end, rsize))) => (ch_start, ch_end, rsize),
            Ok(None) => return false,
            Err(e) => panic!("Error reading chunk: {}", e),
        };

        let buf = storage.buf.as_slice()[ch_start..ch_end].as_ref();
        let mut game_state = LichessGameState::Tags;

        let mut game = LichessGame::new();
        let mut lines = std::str::from_utf8(buf).unwrap().lines();

        for line in &mut lines {
            match game_state {
                LichessGameState::Tags => {
                    if line.is_empty() {
                        game_state = LichessGameState::Moves;
                        continue;
                    }

                    if !line.starts_with('[') || !line.ends_with(']') {
                        panic!("Invalid tag line: {}", line);
                    }

                    let mut parts = line[1..line.len() - 1].splitn(2, ' ');
                    let tag = parts.next().unwrap();
                    let value = parts.next().unwrap_or("").trim_matches('"');

                    let val_start = value.as_ptr() as usize - buf.as_ptr() as usize;
                    let val = Some((ch_start + val_start, ch_start + val_start + value.len()));

                    match tag {
                        "Event" => game.event = val,
                        "Site" => game.site = val,
                        "White" => game.white = val,
                        "Black" => game.black = val,
                        "Result" => game.result = val,
                        "UTCDate" => game.utc_date = val,
                        "UTCTime" => game.utc_time = val,
                        "WhiteElo" => game.white_elo = val,
                        "BlackElo" => game.black_elo = val,
                        "WhiteRatingDiff" => game.white_rating_diff = val,
                        "BlackRatingDiff" => game.black_rating_diff = val,
                        "ECO" => game.eco = val,
                        "Opening" => game.opening = val,
                        "TimeControl" => game.time_control = val,
                        "Termination" => game.termination = val,
                        _ => {}
                    }
                }
                LichessGameState::Moves => {
                    if line.is_empty() {
                        games.push(game);
                        game = LichessGame::new();
                        game_state = LichessGameState::Tags;
                        continue;
                    }

                    if !line.starts_with("1.") {
                        continue;
                    }
                    let val_start = line.as_ptr() as usize - buf.as_ptr() as usize;
                    game.moves = Some((ch_start + val_start, ch_start + val_start + line.len()));
                }
            }
        }

        self.r_start = ch_end;
        self.r_size = rsize;

        true
    }

    fn next_chunk<R>(
        &mut self,
        reader: &mut R,
        remainder_start: usize,
        remainder_size: usize,
        buf: &mut [u8],
    ) -> anyhow::Result<Option<(usize, usize, usize)>>
    where
        R: Read,
    {
        if self.state == LichessIteratorState::End {
            return Ok(None);
        }

        let (pre, post) = buf.split_at_mut(BACKBUF_SIZE);

        if remainder_start > 0 {
            let remainder_start = remainder_start - BACKBUF_SIZE;
            pre[BACKBUF_SIZE - remainder_size..BACKBUF_SIZE]
                .copy_from_slice(&post[remainder_start..remainder_start + remainder_size]);
        }

        let mut ch_start = BACKBUF_SIZE - remainder_size;

        loop {
            let n = reader.read(post);

            let n = match n {
                Ok(0) => {
                    self.state = LichessIteratorState::End;
                    return Ok(None);
                }
                Ok(n) => n,
                Err(e) => panic!("Error reading from file: {}", e),
            };

            let bp = (2..n).rev().find_map(|i| {
                if post[i] == b'\n' && post[i - 1] == b'\n' && post[i - 2] != b']' {
                    return Some(i);
                }
                return None;
            });

            let bp = if let Some(bp) = bp {
                bp + 1
            } else {
                for i in 0..BACKBUF_SIZE - n {
                    pre[i] = pre[i + n];
                }

                for i in 0..n {
                    pre[BACKBUF_SIZE - n + i] = post[i];
                }

                if ch_start < n {
                    return Err(anyhow::anyhow!("Backbuf too small, ch_start={}", ch_start));
                }

                ch_start -= n;
                continue;
            };

            let next_remsize = n - bp;

            if next_remsize > BACKBUF_SIZE {
                return Err(anyhow::anyhow!(
                    "Backbuf too small, remsize={}",
                    next_remsize
                ));
            }

            return Ok(Some((ch_start, BACKBUF_SIZE + bp, next_remsize)));
        }
    }
}

struct ExtractContext<'a> {
    games: &'a mut Vec<LichessGame>,
    storage: &'a mut LichessDatabaseBuf,
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
    fn process_batch(&mut self, params: &ExtractParams) -> bool {
        for game in self.games.drain(..) {
            self.moves.clear();

            if params.fcompleted_only
                && game.termination(&self.storage) != Some(LichessGameTermination::Normal)
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

pub fn lichess_extract(db_path: &str, out_path: &str, params: ExtractParams) -> anyhow::Result<()> {
    let tables = tables::Tables::new();

    let mut ctx = ExtractContext {
        games: &mut Vec::new(),
        storage: &mut LichessDatabaseBuf::new(),
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
            return Err(anyhow::anyhow!(
                "Lichess database path {} does not exist",
                db_path
            ));
        }
        Err(e) => {
            return Err(anyhow::anyhow!(
                "Failed to access lichess database path {}: {}",
                db_path,
                e
            ));
        }
    }

    if !path.is_file() {
        return Err(anyhow::anyhow!(
            "Lichess database path {} is not a file",
            db_path
        ));
    }

    let db_extension = path.extension().and_then(std::ffi::OsStr::to_str);

    let mut it = LichessGameParser::new();
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
                "Unsupported lichess database file extension: {:?}",
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
