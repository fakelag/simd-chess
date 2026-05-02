use std::io::Read;

use super::matchmaking::PositionFeeder;

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
    pub fn new(path: &str) -> Self {
        Self {
            inner: std::sync::Arc::new(std::sync::Mutex::new(FenFeeder::new(path))),
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

pub struct FenFeeder {
    positions_total: usize,
    cursor: usize,
    max_positions_to_play: Option<usize>,
    chunk: Vec<String>,
    overflow: String,
    buf: Vec<u8>,
    reader: std::io::BufReader<std::fs::File>,
}

impl FenFeeder {
    pub fn new(path: &str) -> Self {
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
            cursor: 0,
            max_positions_to_play: None,
            positions_total: num_lines,
            chunk: Vec::new(),
            overflow: String::new(),
            reader: std::io::BufReader::new(file),
            buf,
        }
    }

    pub fn positions_total(&self) -> usize {
        self.positions_total
    }

    pub fn set_max_positions(&mut self, max: usize) {
        self.max_positions_to_play = Some(max);
        self.cursor = 0;
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

    pub fn next_position(&mut self) -> Option<String> {
        if let Some(max) = self.max_positions_to_play {
            if self.cursor >= max {
                return None;
            }
        } else {
            panic!("max_positions_to_play not set");
        }

        let position = self.chunk.pop();

        match position {
            Some(p) => {
                self.cursor += 1;
                Some(p)
            }
            None => {
                self.read_chunk();
                if let Some(position) = self.chunk.pop() {
                    self.cursor += 1;
                    Some(position)
                } else {
                    None
                }
            }
        }
    }
}
