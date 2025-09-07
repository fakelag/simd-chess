use std::{
    fs::File,
    io::{BufReader, Read},
};

const CHUNK_SIZE: usize = 1024 * 1024 * 2;
const BACKBUF_SIZE: usize = 1024 * 1024 * 2;

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

#[derive(Default, Debug)]
pub struct LichessGame {
    data: Vec<u8>,
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

impl LichessGame {
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

pub struct LichessGameIterator {
    reader: BufReader<File>,
    buf: Vec<u8>,
    chunk_range: Option<(usize, usize)>,
    // remaining_range: Option<(usize, usize)>,
    buf2: Vec<u8>,

    state: LichessIteratorState,
    chunk_start_at: usize,
}

impl LichessGameIterator {
    pub fn new(file: File) -> Self {
        LichessGameIterator {
            reader: BufReader::new(file),
            buf: vec![0u8; CHUNK_SIZE + BACKBUF_SIZE],
            buf2: vec![0u8; CHUNK_SIZE + BACKBUF_SIZE],
            // remaining_range: None,
            chunk_range: None,
            chunk_start_at: BACKBUF_SIZE,
            state: LichessIteratorState::Reading,
        }
    }

    fn next_chunk(&mut self) -> bool {
        if self.state == LichessIteratorState::End {
            return false;
        }
        //  println!("nextchunk");

        // if let Some((start, end)) = self.remaining_range.take() {
        //     println!("rem");
        //     self.chunk_start_at = BACKBUF_SIZE - (end - start);
        //     let (first, second) = self.buf.split_at_mut(BACKBUF_SIZE);
        //     first[self.chunk_start_at..]
        //         .copy_from_slice(&second[start - BACKBUF_SIZE..end - BACKBUF_SIZE]);
        // }

        loop {
            let n = self.reader.read(&mut self.buf[BACKBUF_SIZE..]);

            let chunk = match n {
                Ok(0) => {
                    self.state = LichessIteratorState::End;
                    self.chunk_range = None;

                    println!("over");
                    return false;
                }
                Ok(n) => &mut self.buf[0..BACKBUF_SIZE + n],
                Err(e) => panic!("Error reading from file: {}", e),
            };

            let bp = (self.chunk_start_at..chunk.len()).rev().find_map(|i| {
                if chunk[i] != b'\n' {
                    return None;
                }
                return Some(i);
            });

            let bp = if let Some(bp) = bp {
                bp
            } else {
                println!("Line too long");
                if (chunk.len() - self.chunk_start_at) > BACKBUF_SIZE {
                    panic!("Line too long");
                }
                for i in self.chunk_start_at..chunk.len() {
                    chunk[BACKBUF_SIZE - (chunk.len() - i)] = chunk[i];
                }
                self.chunk_start_at = BACKBUF_SIZE - (chunk.len() - self.chunk_start_at);
                continue;
            };

            let (to_process, remaining) = chunk.split_at_mut(bp + 1);

            self.chunk_range = Some((self.chunk_start_at, bp + 1));
            // self.remaining_range = if remaining.len() > 0 {
            //     Some((bp + 1, bp + 1 + remaining.len()))
            // } else {
            //     None
            // };

            // println!(
            //     "debug: {}",
            //     std::str::from_utf8(
            //         &to_process[self.chunk_range.unwrap().0..self.chunk_range.unwrap().0 + 60]
            //     )
            //     .unwrap()
            // );

            self.buf2[self.chunk_range.unwrap().0..self.chunk_range.unwrap().1].copy_from_slice(
                &to_process[self.chunk_range.unwrap().0..self.chunk_range.unwrap().1],
            );

            self.chunk_start_at = BACKBUF_SIZE - remaining.len();
            to_process[BACKBUF_SIZE - remaining.len()..BACKBUF_SIZE].copy_from_slice(remaining);

            // println!(
            //     "debug2: {}",
            //     std::str::from_utf8(
            //         &to_process[self.chunk_range.unwrap().0..self.chunk_range.unwrap().0 + 60]
            //     )
            //     .unwrap()
            // );

            // let content = &to_process[s..];

            // self.buf_line_iter = Some(std::str::from_utf8(content).unwrap().lines());

            return true;
        }
    }
}

impl Iterator for LichessGameIterator {
    type Item = LichessGame;

    fn next(&mut self) -> Option<Self::Item> {
        if self.state == LichessIteratorState::End {
            return None;
        }

        if self.chunk_range.is_none() {
            if !self.next_chunk() {
                return None;
            }
        }

        let mut range = self.chunk_range.unwrap();

        let mut game_state = LichessGameState::Tags;
        let mut game = LichessGame::default();
        game.data = self.buf2[range.0..range.1].to_vec();

        let mut start_at = 0;
        let mut num_bytes_read = 0;
        loop {
            let mut lines = std::str::from_utf8(&game.data[start_at..]).unwrap().lines();

            for line in &mut lines {
                // println!("Line({start_at}): {}", line);
                start_at += line.len() + 1;
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

                        let val_start = value.as_ptr() as usize - game.data.as_ptr() as usize;
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
                        // println!("Moves line: {}", line);
                        if line.is_empty() {
                            // let range = self.chunk_range.unwrap();
                            self.chunk_range = Some((range.0 + num_bytes_read, range.1));
                            if self.chunk_range.unwrap().0 >= self.chunk_range.unwrap().1 {
                                println!("reset chunk range");
                                self.chunk_range = None;
                            }
                            return Some(game);
                        }
                    }
                }
            }

            println!("foo: ");
            if !self.next_chunk() {
                panic!("Unexpected end of file");
            }
            range = self.chunk_range.unwrap();
            game.data.extend_from_slice(&self.buf2[range.0..range.1]);
            // println!(
            //     "next chunk: {}...",
            //     std::str::from_utf8(&self.buf[range.0..range.0 + 30]).unwrap()
            // );
            num_bytes_read = 0;
        }

        unreachable!();
    }
}

pub fn parse_lichess_database(path: &str) -> anyhow::Result<LichessGameIterator> {
    let input = File::open(path)?;

    Ok(LichessGameIterator::new(input))
}
