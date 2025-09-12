use std::io::Read;

use crate::{matchmaking::*, util};

pub fn selfplay(
    parallel_matches: usize,
    thinktime_ms: u64,
    position_file_path: &str,
) -> anyhow::Result<()> {
    let feeder = Box::new(SharedFenFeeder::new(position_file_path));

    let mut matchmakers = (0..parallel_matches)
        .map(|_i| matchmaking::Matchmaking::new(util::FEN_STARTPOS).unwrap())
        .collect::<Vec<_>>();

    let num_games_per_matchmaker =
        { feeder.inner.lock().unwrap().positions_total / parallel_matches };

    for mm in &mut matchmakers {
        mm.versus_autostart = false;
        mm.versus_start(
            "v12_eval_mgeg_bp_mob.exe",
            "v12_eval_mgeg_bp_mob.exe",
            2,
            false,
            Some(feeder.clone()),
        )?;
        mm.set_go_params(&format!("movetime {}", thinktime_ms));
    }

    let mut active_matchmakings = matchmakers
        .iter()
        .enumerate()
        .map(|(i, _)| i)
        .collect::<Vec<usize>>();

    loop {
        for i in active_matchmakings.clone() {
            let mm = &mut matchmakers[i];

            mm.poll();

            match mm.versus_state {
                matchmaking::VersusState::NextMatch(_, _) => {
                    // println!("fen {} moves: {:?}", mm.fen, mm.moves);
                    // self.collect(fen, &mm.moves);
                    mm.versus_nextgame(false);
                }
                matchmaking::VersusState::Done => {
                    println!("Matchmaking {} done", i);
                    // println!("fen {} moves: {:?}", mm.fen, mm.moves);
                    // self.collect(fen, &mm.moves);
                    active_matchmakings.retain(|&x| x != i);
                }
                matchmaking::VersusState::InProgress => {}
                _ => {
                    println!("Matchmaking {} state {:?}", i, mm.versus_state);
                }
            }
        }

        if active_matchmakings.is_empty() {
            break;
        }
    }

    Ok(())
}

struct SharedFenFeeder {
    inner: std::sync::Arc<std::sync::Mutex<FenFeeder>>,
}

impl Clone for SharedFenFeeder {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl SharedFenFeeder {
    fn new(path: &str) -> Self {
        Self {
            inner: std::sync::Arc::new(std::sync::Mutex::new(FenFeeder::new(path))),
        }
    }
}

impl matchmaking::PositionFeeder for SharedFenFeeder {
    fn next_position(&mut self) -> Option<String> {
        let mut inner = self.inner.lock().unwrap();
        inner.next_position()
    }
}

struct FenFeeder {
    positions_total: usize,
    chunk: Vec<String>,
    overflow: String,
    buf: Vec<u8>,
    reader: std::io::BufReader<std::fs::File>,
}

impl FenFeeder {
    fn new(path: &str) -> Self {
        let mut num_lines = 0;

        let file = std::fs::File::open(path).expect("Failed to open FEN file");
        let mut reader = std::io::BufReader::new(file);

        let mut buf = vec![0; 1024 * 1024 * 4];
        loop {
            let n = reader.read(&mut buf).expect("Failed to read FEN file");

            if n == 0 {
                break;
            }

            num_lines += buf[..n]
                .iter()
                .fold(0, |acc, &b| acc + if b == b'\n' { 1 } else { 0 });
        }

        let file = std::fs::File::open(path).expect("Failed to open FEN file");

        Self {
            positions_total: num_lines,
            chunk: Vec::new(),
            overflow: String::new(),
            reader: std::io::BufReader::new(file),
            buf,
        }
    }

    fn read_chunk(&mut self) {
        loop {
            let n = self
                .reader
                .read(&mut self.buf)
                .expect("Failed to read FEN file");

            if n == 0 {
                break;
            }

            let bp = (0..n).rev().find_map(|i| {
                if self.buf[i] == b'\n' {
                    return Some(i);
                }
                return None;
            });

            let bp = match bp {
                Some(b) => b,
                None => {
                    self.overflow
                        .push_str(&String::from_utf8_lossy(&self.buf[..n]));
                    continue;
                }
            };

            let content = format!(
                "{}{}",
                self.overflow,
                String::from_utf8_lossy(&self.buf[..bp])
            );
            self.overflow = String::from_utf8_lossy(&self.buf[bp + 1..n]).to_string();

            for lines in content.lines() {
                self.chunk.push(lines.to_string());
            }
            break;
        }
    }

    fn next_position(&mut self) -> Option<String> {
        let position = self.chunk.pop();

        match position {
            Some(p) => Some(p),
            None => {
                self.read_chunk();
                self.chunk.pop()
            }
        }
    }
}
