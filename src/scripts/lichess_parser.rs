use std::{
    fs::{self, File},
    io::{BufReader, Read, Write},
};

pub const CHUNK_SIZE: usize = 1024 * 1024 * 4; // 4 MB
pub const BACKBUF_SIZE: usize = 1024 * 32; // 32 KB

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
pub struct LichessGame<'a> {
    data: &'a [u8],
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
}

macro_rules! game_getter {
    ($name:ident, $ty:ty, $field:ident) => {
        pub fn $name(&self) -> Option<$ty> {
            if let Some((start, end)) = self.$field {
                return Some(unsafe { std::mem::transmute::<&[u8], $ty>(&self.data[start..end]) });
            }
            None
        }
    };
}

impl<'a> LichessGame<'a> {
    pub fn new(d: &'a [u8]) -> Self {
        LichessGame {
            data: d,
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

    pub fn result(&self) -> Option<LichessGameResult> {
        if let Some((start, end)) = self.result {
            return Some(match &self.data[start..end] {
                b"1-0" => LichessGameResult::WhiteWin,
                b"0-1" => LichessGameResult::BlackWin,
                b"1/2-1/2" => LichessGameResult::Draw,
                b"*" => LichessGameResult::Draw,
                _ => panic!("Invalid result \"{:?}\"", &self.data[start..end]),
            });
        }
        None
    }

    pub fn termination(&self) -> Option<LichessGameTermination> {
        if let Some((start, end)) = self.termination {
            return Some(match &self.data[start..end] {
                b"Normal" => LichessGameTermination::Normal,
                b"Time forfeit" => LichessGameTermination::TimeForfeit,
                b"Abandoned" => LichessGameTermination::Abandoned,
                b"Rules infraction" => LichessGameTermination::RulesInfraction,
                b"Unterminated" => LichessGameTermination::Unterminated,
                _ => panic!(
                    "Invalid termination \"{:?}\"",
                    std::string::String::from_utf8_lossy(&self.data[start..end])
                ),
            });
        }
        None
    }
}

pub struct LichessDatabaseReader {
    reader: BufReader<File>,
    state: LichessIteratorState,
}

pub struct LichessGameIterator<'a> {
    reader: LichessDatabaseReader,
    r_start: usize,
    r_size: usize,

    buf: Vec<u8>,
    // games: Vec<LichessGame<'a>>,
    __marker: std::marker::PhantomData<&'a ()>,
}

impl LichessDatabaseReader {
    pub fn new(file: File) -> Self {
        LichessDatabaseReader {
            reader: BufReader::new(file),
            state: LichessIteratorState::Reading,
        }
    }

    pub fn next_chunk(
        &mut self,
        remainder_start: usize,
        remainder_size: usize,
        buf: &mut [u8],
    ) -> anyhow::Result<Option<(usize, usize, usize)>> {
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
            let n = self.reader.read(post);

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

impl<'a> LichessGameIterator<'a> {
    pub fn new(file: File) -> Self {
        LichessGameIterator {
            reader: LichessDatabaseReader::new(file),
            r_start: 0,
            r_size: 0,
            buf: vec![0u8; CHUNK_SIZE + BACKBUF_SIZE],
            // games: Vec::new(),
            __marker: std::marker::PhantomData,
        }
    }
}

impl<'a> LichessGameIterator<'a> {
    pub fn next<'b>(
        &mut self,
        storage: &'b mut LichessDatabaseBuf,
        games: &'b mut Vec<LichessGame<'b>>,
    ) {
        if self.reader.state == LichessIteratorState::End {
            return;
        }

        // if let Some(game) = self.games.pop() {
        //     return Some(game);
        // }

        let (ch_start, ch_end, rsize) =
            match self
                .reader
                .next_chunk(self.r_start, self.r_size, storage.buf.as_mut_slice())
            {
                Ok(Some((ch_start, ch_end, rsize))) => (ch_start, ch_end, rsize),
                Ok(None) => {
                    println!("End of file");
                    return;
                }
                Err(e) => {
                    panic!("Error reading chunk: {}", e);
                }
            };

        // self.buf.split_at()

        // let lol = &self.buf[ch_start..ch_end];

        let mut game_state = LichessGameState::Tags;

        let mut buf = storage.buf.as_slice();

        loop {
            if buf.is_empty() {
                break;
            }

            // &self.buf[ch_start..ch_end];
            let mut game = LichessGame::new(&[]);
            let mut lines = std::str::from_utf8(buf).unwrap().lines();

            let mut num_bytes_read = 0;

            for line in &mut lines {
                num_bytes_read += line.len() + 1;

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

                        let val_start = value.as_ptr() as usize - self.buf.as_ptr() as usize;
                        let val = Some((val_start, val_start + value.len()));

                        // println!("Tag: {} Value: {}", tag, value);

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
                            let (game_slice, rest) = buf.split_at(num_bytes_read);

                            game.data = game_slice;
                            games.push(game);
                            // self.games.push(game);

                            buf = &rest;

                            break;
                        }
                    }
                }
            }
        }

        println!("Read {} games", games.len());

        self.r_start = ch_end;
        self.r_size = rsize;

        // self.games.pop()

        // if self.chunk_range.is_none() {
        //     if !self.next_chunk() {
        //         return None;
        //     }
        // }

        // let mut range = self.chunk_range.unwrap();

        // let mut game_state = LichessGameState::Tags;
        // let mut game = LichessGame::default();
        // game.data = self.buf2[range.0..range.1].to_vec();

        // let mut start_at = 0;
        // let mut num_bytes_read = 0;
        // loop {
        //     let mut lines = std::str::from_utf8(&game.data[start_at..]).unwrap().lines();

        //     for line in &mut lines {
        //         // println!("Line({start_at}): {}", line);
        //         start_at += line.len() + 1;
        //         num_bytes_read += line.len() + 1;
        //         match game_state {
        //             LichessGameState::Tags => {
        //                 if line.is_empty() {
        //                     game_state = LichessGameState::Moves;
        //                     continue;
        //                 }

        //                 if !line.starts_with('[') || !line.ends_with(']') {
        //                     panic!("Invalid tag line: {}", line);
        //                 }

        //                 let mut parts = line[1..line.len() - 1].splitn(2, ' ');
        //                 let tag = parts.next().unwrap();
        //                 let value = parts.next().unwrap_or("").trim_matches('"');

        //                 let val_start = value.as_ptr() as usize - game.data.as_ptr() as usize;
        //                 let val = Some((val_start, val_start + value.len()));

        //                 // println!("Tag: {} Value: {}", tag, value);

        //                 match tag {
        //                     "Event" => game.event = val,
        //                     "Site" => game.site = val,
        //                     "White" => game.white = val,
        //                     "Black" => game.black = val,
        //                     "Result" => game.result = val,
        //                     "UTCDate" => game.utc_date = val,
        //                     "UTCTime" => game.utc_time = val,
        //                     "WhiteElo" => game.white_elo = val,
        //                     "BlackElo" => game.black_elo = val,
        //                     "WhiteRatingDiff" => game.white_rating_diff = val,
        //                     "BlackRatingDiff" => game.black_rating_diff = val,
        //                     "ECO" => game.eco = val,
        //                     "Opening" => game.opening = val,
        //                     "TimeControl" => game.time_control = val,
        //                     "Termination" => game.termination = val,
        //                     _ => {}
        //                 }
        //             }
        //             LichessGameState::Moves => {
        //                 // println!("Moves line: {}", line);
        //                 if line.is_empty() {
        //                     // let range = self.chunk_range.unwrap();
        //                     self.chunk_range = Some((range.0 + num_bytes_read, range.1));
        //                     if self.chunk_range.unwrap().0 >= self.chunk_range.unwrap().1 {
        //                         println!("reset chunk range");
        //                         self.chunk_range = None;
        //                     }
        //                     return Some(game);
        //                 }
        //             }
        //         }
        //     }

        //     println!("foo: ");
        //     if !self.next_chunk() {
        //         panic!("Unexpected end of file");
        //     }
        //     range = self.chunk_range.unwrap();
        //     game.data.extend_from_slice(&self.buf2[range.0..range.1]);
        //     // println!(
        //     //     "next chunk: {}...",
        //     //     std::str::from_utf8(&self.buf[range.0..range.0 + 30]).unwrap()
        //     // );
        //     num_bytes_read = 0;
        // }

        // unreachable!();
    }
}

pub fn parse_lichess_database(path: &str) -> anyhow::Result<LichessGameIterator<'_>> {
    let input = File::open(path)?;

    Ok(LichessGameIterator::new(input))
}
