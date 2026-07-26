use std::collections::HashSet;
use std::io::Read;

use super::matchmaking::PositionFeeder;
use crate::engine::{chess_v2::ChessGame, tables};

pub struct SharedFenFeeder {
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
    pub fn new_multi(paths: &[String]) -> Self {
        Self {
            inner: std::sync::Arc::new(std::sync::Mutex::new(FenFeeder::new_multi(paths))),
        }
    }

    pub fn set_max_positions(&self, max: usize) {
        let mut inner = self.inner.lock().unwrap();
        inner.set_max_positions(max);
    }

    pub fn lock(&self) -> std::sync::MutexGuard<'_, FenFeeder> {
        self.inner.lock().unwrap()
    }
}

impl PositionFeeder for SharedFenFeeder {
    fn next_position(&mut self) -> Option<String> {
        let mut inner = self.inner.lock().unwrap();
        inner.next_position()
    }
}

struct FileReader {
    name: String,
    chunk: Vec<String>,
    overflow: String,
    buf: Vec<u8>,
    reader: std::io::BufReader<std::fs::File>,
    lines: usize,
    stride: f64,
    pass: f64,
    active: bool,
    served: u64,
}

impl FileReader {
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
            name: path.to_string(),
            chunk: Vec::new(),
            overflow: String::new(),
            buf,
            reader: std::io::BufReader::new(file),
            lines: num_lines,
            stride: if num_lines > 0 {
                1.0 / num_lines as f64
            } else {
                0.0
            },
            pass: 0.0,
            active: num_lines > 0,
            served: 0,
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

    fn next_line(&mut self) -> Option<String> {
        if let Some(line) = self.chunk.pop() {
            return Some(line);
        }
        self.read_chunk();
        self.chunk.pop()
    }
}

pub struct FenFeeder {
    files: Vec<FileReader>,
    seen: HashSet<u64>,
    board: ChessGame,
    tables: tables::Tables,
    cursor: usize,
    max_positions_to_play: Option<usize>,
    positions_total: usize,
    served_total: u64,
    dup_skips: u64,
    unparsed: u64,
}

impl FenFeeder {
    pub fn new_multi(paths: &[String]) -> Self {
        assert!(!paths.is_empty(), "FenFeeder needs at least one input file");

        let files: Vec<FileReader> = paths.iter().map(|p| FileReader::new(p)).collect();
        let positions_total: usize = files.iter().map(|f| f.lines).sum();

        for f in &files {
            let mix = if positions_total > 0 {
                f.lines as f64 / positions_total as f64 * 100.0
            } else {
                0.0
            };
        }

        Self {
            files,
            seen: HashSet::new(),
            board: ChessGame::new(),
            tables: tables::Tables::new(),
            cursor: 0,
            max_positions_to_play: None,
            positions_total,
            served_total: 0,
            dup_skips: 0,
            unparsed: 0,
        }
    }

    pub fn positions_total(&self) -> usize {
        self.positions_total
    }

    pub fn set_max_positions(&mut self, max: usize) {
        self.max_positions_to_play = Some(max);
        self.cursor = 0;
    }

    /// Canonical seed key of `fen`, or `None` if it fails to parse.
    fn key_of(&mut self, fen: &str) -> Option<u64> {
        self.board.load_fen(fen, &self.tables).ok()?;
        Some(self.board.canonical_seed_key(&self.tables))
    }

    fn log_mix(&self) {
        let parts: Vec<String> = self
            .files
            .iter()
            .map(|f| {
                let share = if self.served_total > 0 {
                    f.served as f64 / self.served_total as f64 * 100.0
                } else {
                    0.0
                };
                format!("{:.1}%", share)
            })
            .collect();
    }

    pub fn next_position(&mut self) -> Option<String> {
        let max = self
            .max_positions_to_play
            .expect("max_positions_to_play not set");

        if self.cursor >= max {
            return None;
        }

        loop {
            let pick = self
                .files
                .iter()
                .enumerate()
                .filter(|(_, f)| f.active)
                .min_by(|(_, a), (_, b)| a.pass.partial_cmp(&b.pass).unwrap())
                .map(|(i, _)| i);

            let Some(i) = pick else {
                return None;
            };

            let mut served_line = None;
            loop {
                match self.files[i].next_line() {
                    None => break,
                    Some(line) => match self.key_of(&line) {
                        None => {
                            self.unparsed += 1;
                            continue;
                        }
                        Some(key) => {
                            if !self.seen.insert(key) {
                                self.dup_skips += 1;
                                continue;
                            }
                            served_line = Some(line);
                            break;
                        }
                    },
                }
            }

            match served_line {
                Some(line) => {
                    self.files[i].pass += self.files[i].stride;
                    self.files[i].served += 1;
                    self.cursor += 1;
                    self.served_total += 1;
                    if self.served_total % 100_000 == 0 {
                        self.log_mix();
                    }
                    return Some(line);
                }
                None => {
                    self.files[i].active = false;
                }
            }
        }
    }
}
