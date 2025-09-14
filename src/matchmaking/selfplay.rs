use std::io::Read;

use sfbinpack::chess::{coords, r#move, piece};

use crate::{
    engine::chess_v2,
    matchmaking::{matchmaking::GameResult, *},
    util,
};

pub struct SelfplayTrainer {
    binpack_writer: sfbinpack::CompressedTrainingDataEntryWriter,
    stats_num_games_cp: usize,
    stats_num_games_total: usize,
    stats_positions_total: usize,
    stats_flushed_at: std::time::Instant,
    binpack_path: String,
}

impl SelfplayTrainer {
    pub fn new(out_binpack_path: &str) -> Self {
        let binpack_writer =
            sfbinpack::CompressedTrainingDataEntryWriter::new(out_binpack_path, false).unwrap();

        Self {
            binpack_writer,
            stats_num_games_cp: 0,
            stats_num_games_total: 0,
            stats_positions_total: 0,
            stats_flushed_at: std::time::Instant::now(),
            binpack_path: out_binpack_path.to_string(),
        }
    }

    pub fn play(
        &mut self,
        parallel_matches: usize,
        position_file_path: &str,
        from: Option<usize>,
        count: Option<usize>,
    ) -> anyhow::Result<()> {
        let mut matchmakers = (0..parallel_matches)
            .map(|_i| matchmaking::Matchmaking::new(util::FEN_STARTPOS).unwrap())
            .collect::<Vec<_>>();

        let feeder = Box::new(SharedFenFeeder::new(position_file_path));

        if let Some(from) = from {
            let mut lock = feeder.inner.lock().unwrap();
            for _ in 0..from {
                lock.next_position();
            }
        }

        let num_positions_total = if let Some(count) = count {
            let lock = feeder.inner.lock().unwrap();
            lock.positions_total.min(count)
        } else {
            feeder.inner.lock().unwrap().positions_total
        };
        let num_games_per_matchmaker = num_positions_total / parallel_matches;
        let overflow_games = num_positions_total % parallel_matches;

        println!(
            "Starting selfplay with {} parallel matches, {} total positions ({} per matchmaker)",
            parallel_matches, num_positions_total, num_games_per_matchmaker
        );

        // for (index, mm) in matchmakers.iter_mut().enumerate() {
        //     let extra = if index < overflow_games { 1 } else { 0 };
        //     let games_to_play = num_games_per_matchmaker + extra;

        //     mm.versus_autostart = false;
        //     mm.versus_logging = false;
        //     mm.versus_3fr = Some(crate::engine::search::repetition_v2::RepetitionTable::new());
        //     mm.versus_start(
        //         "v12_eval_fixed.exe",
        //         "v12_eval_fixed.exe",
        //         num_games_per_matchmaker + extra,
        //         false,
        //         Some(feeder.clone()),
        //     )?;
        //     // mm.set_go_params(&format!("movetime {}", thinktime_ms));
        //     mm.set_go_params(&format!("depth {}", 8));
        // }

        let mut active_matchmakings = matchmakers
            .iter_mut()
            .enumerate()
            .filter_map(|(i, mm)| {
                let extra = if i < overflow_games { 1 } else { 0 };
                let games_to_play = num_games_per_matchmaker + extra;

                if games_to_play == 0 {
                    return None;
                }

                mm.versus_autostart = false;
                mm.versus_logging = false;
                mm.versus_3fr = Some(crate::engine::search::repetition_v2::RepetitionTable::new());
                mm.versus_start(
                    "v12_eval_fixed.exe",
                    "v12_eval_fixed.exe",
                    games_to_play,
                    false,
                    Some(feeder.clone()),
                )
                .expect("Failed to start matchmaking");
                // mm.set_go_params(&format!("movetime {}", thinktime_ms));
                mm.set_go_params(&format!("depth {}", 8));

                Some(i)
            })
            .collect::<Vec<usize>>();

        self.stats_flushed_at = std::time::Instant::now();
        self.stats_num_games_cp = 0;
        self.stats_num_games_total = 0;

        loop {
            active_matchmakings.retain(|&i| {
                let mm = &mut matchmakers[i];

                mm.poll();

                match mm.versus_state {
                    matchmaking::VersusState::NextMatch(_, _) => {
                        self.collect(&mm);
                        mm.versus_nextgame(false);
                    }
                    matchmaking::VersusState::Done => {
                        self.collect(&mm);
                        return false;
                    }
                    matchmaking::VersusState::InProgress => {}
                    _ => {
                        println!("Matchmaking {} state {:?}", i, mm.versus_state);
                    }
                }

                return true;
            });

            if active_matchmakings.is_empty() {
                break;
            }

            if self.stats_flushed_at.elapsed().as_secs() >= 60 * 1 {
                // self.binpack_writer.flush().unwrap();

                let games_per_minute = self.stats_num_games_cp as f64
                    / (self.stats_flushed_at.elapsed().as_secs_f64() / 60.0);
                println!(
                    "Checkpoint after {} games ({:.02} mins). Games per minute: ~{:.02}. {} total positions so far, ~{:.02} per game avg. Binpack size: {}. ETA: {}",
                    self.stats_num_games_total,
                    self.stats_flushed_at.elapsed().as_secs_f64() / 60.0,
                    games_per_minute,
                    self.stats_positions_total,
                    self.stats_positions_total as f64 / self.stats_num_games_total as f64,
                    util::byte_size_string(self.binpack_size_bytes()),
                    util::time_format(
                        ((num_positions_total - self.stats_num_games_total) as f64
                            / games_per_minute
                            * 60.0
                            * 1000.0) as u64
                    )
                );
                self.stats_flushed_at = std::time::Instant::now();
                self.stats_num_games_cp = 0;
            }
        }

        // self.binpack_writer.flush().unwrap();

        println!(
            "Selfplay done:\n{} total games\n{} total positions\nBinpack size: {}\nPosition reached: {}",
            self.stats_num_games_total,
            self.stats_positions_total,
            util::byte_size_string(self.binpack_size_bytes()),
            feeder.inner.lock().unwrap().cursor
        );

        Ok(())
    }

    fn collect(&mut self, mm: &matchmaking::Matchmaking) {
        if mm.moves_u16.len() < 2 {
            println!(
                "Game too short during collection: {} - {} moves",
                mm.fen,
                mm.moves_u16.len()
            );
            return;
        }

        let mut board = chess_v2::ChessGame::new();

        match board.load_fen(&mm.fen, &mm.tables) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("Failed to load FEN during collection: {} - {:?}", mm.fen, e);
                return;
            }
        }

        if !unsafe { board.make_move(mm.moves_u16[0], &mm.tables, None) } {
            eprintln!(
                "Failed to make first move during collection: {} - {:?}",
                mm.fen, mm.moves_u16[0]
            );
            return;
        }

        if board.in_check(&mm.tables, !board.b_move()) {
            eprintln!(
                "First move leaves player in check during collection: {} - {:?}",
                mm.fen, mm.moves_u16[0]
            );
            return;
        }

        self.stats_num_games_cp += 1;
        self.stats_num_games_total += 1;
        self.stats_positions_total += mm.moves_u16.len();

        // Fen after the first move is played
        let first_fen = board.gen_fen();

        let stats = mm.versus_stats();

        let mut b_move = board.b_move();
        let mut ply = (board.full_moves() * 2 + if b_move { 1 } else { 0 }) as u16;

        let result = match stats.last_game_status {
            Some(GameResult::GameState(chess_v2::GameState::Checkmate(util::Side::White))) => 1,
            Some(GameResult::GameState(chess_v2::GameState::Checkmate(util::Side::Black))) => -1,
            Some(GameResult::GameState(
                chess_v2::GameState::Stalemate | chess_v2::GameState::DrawByFiftyMoveRule,
            )) => 0,
            Some(GameResult::OutOfTime(util::Side::White)) => -1,
            Some(GameResult::OutOfTime(util::Side::Black)) => 1,
            Some(GameResult::ThreeFoldRepetition) => 0,
            st => {
                eprintln!(
                    "Game not finished / invalid state during collection ({:?}): {} - {} moves",
                    st,
                    mm.fen,
                    mm.moves_u16.len()
                );
                return;
            }
        };

        let mut last_entry = sfbinpack::TrainingDataEntry {
            pos: sfbinpack::chess::position::Position::from_fen(&first_fen),
            mv: SelfplayTrainer::convert_move(b_move, mm.moves_u16[1]),
            ply,
            result: if b_move { -result } else { result },
            score: 0,
        };

        self.binpack_writer.write_entry(&last_entry).unwrap();

        for mv in mm.moves_u16.iter().skip(2) {
            b_move = !b_move;
            ply += 1;

            let mv = Self::convert_move(b_move, *mv);

            last_entry = sfbinpack::TrainingDataEntry {
                pos: last_entry.pos.after_move(last_entry.mv),
                mv,
                ply,
                result: if b_move { -result } else { result },
                score: 0,
            };

            self.binpack_writer.write_entry(&last_entry).unwrap();
        }
    }

    fn convert_move(b_move: bool, mv: u16) -> r#move::Move {
        let from_sq = (mv & 0x3F) as u8;
        let mut to_sq = ((mv >> 6) & 0x3F) as u8;

        let mut move_type = r#move::MoveType::Normal;
        let mut promoted_piece = piece::Piece::none();

        match mv & chess_v2::MV_FLAGS_PR_MASK {
            chess_v2::MV_FLAGS_PR_QUEEN => {
                move_type = r#move::MoveType::Promotion;
                promoted_piece = if b_move {
                    piece::Piece::BLACK_QUEEN
                } else {
                    piece::Piece::WHITE_QUEEN
                };
            }
            chess_v2::MV_FLAGS_PR_ROOK => {
                move_type = r#move::MoveType::Promotion;
                promoted_piece = if b_move {
                    piece::Piece::BLACK_ROOK
                } else {
                    piece::Piece::WHITE_ROOK
                };
            }
            chess_v2::MV_FLAGS_PR_BISHOP => {
                move_type = r#move::MoveType::Promotion;
                promoted_piece = if b_move {
                    piece::Piece::BLACK_BISHOP
                } else {
                    piece::Piece::WHITE_BISHOP
                };
            }
            chess_v2::MV_FLAGS_PR_KNIGHT => {
                move_type = r#move::MoveType::Promotion;
                promoted_piece = if b_move {
                    piece::Piece::BLACK_KNIGHT
                } else {
                    piece::Piece::WHITE_KNIGHT
                };
            }
            _ => {
                if (mv & chess_v2::MV_FLAGS) == chess_v2::MV_FLAGS_CASTLE_KING {
                    to_sq += 1;
                    move_type = r#move::MoveType::Castle;
                } else if (mv & chess_v2::MV_FLAGS) == chess_v2::MV_FLAGS_CASTLE_QUEEN {
                    move_type = r#move::MoveType::Castle;
                    to_sq -= 2;
                } else if (mv & chess_v2::MV_FLAGS) == chess_v2::MV_FLAG_EPCAP {
                    move_type = r#move::MoveType::EnPassant;
                }
            }
        }

        r#move::Move::new(
            coords::Square::new(from_sq as u32),
            coords::Square::new(to_sq as u32),
            move_type,
            promoted_piece,
        )
    }

    fn binpack_size_bytes(&self) -> usize {
        std::fs::metadata(&self.binpack_path)
            .map(|m| m.len())
            .unwrap_or(0) as usize
    }
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
    cursor: usize,
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
            cursor: 0,
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
