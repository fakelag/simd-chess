use std::io::Read;

use crate::{
    engine::{chess_v2::ChessGame, tables},
    pgn::moves,
};

pub const CHUNK_SIZE: usize = 1024 * 1024 * 4; // 4 MB
pub const BACKBUF_SIZE: usize = 1024 * 32; // 32 KB

enum PgnGameState {
    Tags,
    Moves,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PgnGameResult {
    WhiteWin, // 1-0
    BlackWin, // 0-1
    Draw,     // 1/2-1/2
    Other,    // *
}

#[derive(Debug, PartialEq, Eq)]
pub enum PgnGameTermination {
    Normal,
    TimeForfeit,
    Abandoned,
    RulesInfraction,
    Unterminated,
}

pub struct PgnReadBuf {
    buf: Vec<u8>,
}

impl PgnReadBuf {
    pub fn new() -> Self {
        PgnReadBuf {
            buf: vec![0u8; CHUNK_SIZE + BACKBUF_SIZE],
        }
    }
}

#[derive(Debug)]
pub struct PgnGame {
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
        pub fn $name(&self, st: &PgnReadBuf) -> Option<$ty> {
            if let Some((start, end)) = self.$field {
                return Some(unsafe { std::mem::transmute::<&[u8], $ty>(&st.buf[start..end]) });
            }
            None
        }
    };
}

impl PgnGame {
    pub fn new() -> Self {
        PgnGame {
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

    pub fn result(&self, st: &PgnReadBuf) -> Option<PgnGameResult> {
        if let Some((start, end)) = self.result {
            return Some(match &st.buf[start..end] {
                b"1-0" => PgnGameResult::WhiteWin,
                b"0-1" => PgnGameResult::BlackWin,
                b"1/2-1/2" => PgnGameResult::Draw,
                b"*" => PgnGameResult::Other,
                _ => panic!("Invalid result \"{:?}\"", &st.buf[start..end]),
            });
        }
        None
    }

    pub fn termination(&self, st: &PgnReadBuf) -> Option<PgnGameTermination> {
        if let Some((start, end)) = self.termination {
            return Some(match &st.buf[start..end] {
                b"Normal" => PgnGameTermination::Normal,
                b"Time forfeit" => PgnGameTermination::TimeForfeit,
                b"Abandoned" => PgnGameTermination::Abandoned,
                b"Rules infraction" => PgnGameTermination::RulesInfraction,
                b"Unterminated" => PgnGameTermination::Unterminated,
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
        st: &PgnReadBuf,
        board: &ChessGame,
        tables: &tables::Tables,
        moves: &mut Vec<u16>,
    ) -> Option<anyhow::Result<()>> {
        let moves_str = if let Some((start, end)) = self.moves {
            unsafe { std::mem::transmute::<&[u8], &str>(&st.buf[start..end]) }
        } else {
            return None;
        };

        // println!("Parsing moves: {}", moves_str);

        let result = moves::parse_moves(moves_str, &mut board.clone(), tables, moves);
        Some(result)
    }
}

pub struct PgnGameParser {
    is_finished: bool,
    r_start: usize,
    r_size: usize,
}

impl PgnGameParser {
    pub fn new() -> Self {
        PgnGameParser {
            is_finished: false,
            r_start: 0,
            r_size: 0,
        }
    }
}

impl PgnGameParser {
    pub fn next_batch<R>(
        &mut self,
        reader: &mut R,
        storage: &mut PgnReadBuf,
        games: &mut Vec<PgnGame>,
    ) -> bool
    where
        R: Read,
    {
        if self.is_finished {
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
        let mut game_state = PgnGameState::Tags;

        let mut game = PgnGame::new();

        let str = std::str::from_utf8(buf).unwrap();

        let mut lines = str.lines();

        for line in &mut lines {
            match game_state {
                PgnGameState::Tags => {
                    if line.is_empty() {
                        game_state = PgnGameState::Moves;
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
                        "Eco" => game.eco = val,
                        "Opening" => game.opening = val,
                        "TimeControl" => game.time_control = val,
                        "Termination" => game.termination = val,
                        _ => {}
                    }
                }
                PgnGameState::Moves => {
                    if line.is_empty() {
                        games.push(game);
                        game = PgnGame::new();
                        game_state = PgnGameState::Tags;
                        continue;
                    }

                    let val_start = line.as_ptr() as usize - buf.as_ptr() as usize;
                    if let Some(moves) = &mut game.moves {
                        moves.1 = ch_start + val_start + line.len();
                    } else {
                        game.moves =
                            Some((ch_start + val_start, ch_start + val_start + line.len()));
                    }
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
        if self.is_finished {
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
                    self.is_finished = true;
                    return Ok(None);
                }
                Ok(n) => n,
                Err(e) => panic!("Error reading from file: {}", e),
            };

            let bp = (4..n).rev().find_map(|i| {
                let is_lf_ending = &post[i - 1..=i] == b"\n\n" && post[i - 2] != b']';
                let is_crlf_ending = &post[i - 3..=i] == b"\r\n\r\n" && post[i - 4] != b']';

                debug_assert!(
                    (post[i] == b'\n' && post[i - 1] == b'\n' && post[i - 2] != b']')
                        == is_lf_ending
                );
                debug_assert!(
                    (post[i] == b'\n'
                        && post[i - 1] == b'\r'
                        && post[i - 2] == b'\n'
                        && post[i - 3] == b'\r'
                        && post[i - 4] != b']')
                        == is_crlf_ending
                );

                if is_lf_ending || is_crlf_ending {
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
