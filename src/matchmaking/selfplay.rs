use std::io::Read;

use sfbinpack::chess::{coords, r#move, piece};

const DEBUG: bool = false;
const ANNOTATION_DEPTH: u8 = 9;

use crate::{
    engine::{self, chess_v2, search::SearchStrategy, tables},
    matchmaking::{matchmaking::PositionFeeder, *},
    nnue::nnue::UpdatableNnue,
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

    pub fn play_annotated(
        &mut self,
        threads: usize,
        position_file_path: &str,
        from: Option<usize>,
        count: Option<usize>,
    ) -> anyhow::Result<()> {
        let feeder = Box::new(SharedFenFeeder::new(position_file_path));

        if let Some(from) = from {
            let mut lock = feeder.inner.lock().unwrap();
            lock.set_max_positions(from);
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

        feeder.set_max_positions(num_positions_total);

        println!(
            "Starting selfplay with {} threads, depth {} {} total positions ({:.02} per matchmaker)",
            threads,
            ANNOTATION_DEPTH,
            num_positions_total,
            num_positions_total as f64 / threads as f64
        );

        let tables = tables::Tables::new();

        let (tx_entries, rx_entries) = crossbeam::channel::bounded(256);

        std::thread::scope(|s| {
            let start_at = std::time::Instant::now();

            let handles = (0..threads)
                .filter_map(|i| {
                    let tables = &tables;
                    let feeder = feeder.clone();
                    let tx_entries = tx_entries.clone();

                    Some(s.spawn(move || {
                        let core_id = if i % 2 == 0 { i & 31 } else { (i + 16) & 31 };
                        core_affinity::set_for_current(core_affinity::CoreId { id: core_id });
                        Self::play_annotated_thread(i, tx_entries, feeder, tables)
                    }))
                })
                .collect::<Vec<_>>();

            drop(tx_entries);

            self.stats_flushed_at = std::time::Instant::now();
            self.stats_num_games_cp = 0;
            self.stats_num_games_total = 0;

            loop {
                match rx_entries.recv_timeout(std::time::Duration::from_secs(1)) {
                    Ok(entries) => {
                        self.stats_num_games_cp += 1;
                        self.stats_num_games_total += 1;
                        self.stats_positions_total += entries.len();

                        for entry in entries {
                            self.binpack_writer.write_entry(&entry).unwrap();
                        }
                    }
                    Err(crossbeam::channel::RecvTimeoutError::Timeout) => {}
                    Err(crossbeam::channel::RecvTimeoutError::Disconnected) => break,
                }

                if self.stats_flushed_at.elapsed().as_secs() >= 60 * 1 {
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

            for handle in handles {
                let _ = handle.join().unwrap();
            }

            println!(
                "Selfplay finished with {} games in {}. {} total positions annotated, games per minute ~{}. binpack size: ~{}",
                self.stats_num_games_total,
                util::time_format(self.stats_flushed_at.elapsed().as_millis() as u64),
                self.stats_positions_total,
                self.stats_num_games_total as f64 / (start_at.elapsed().as_secs_f64() / 60.0),
                util::byte_size_string(self.binpack_size_bytes()),
            );
        });

        Ok(())
    }

    fn play_annotated_thread(
        thread_id: usize,
        tx: crossbeam::channel::Sender<Vec<sfbinpack::TrainingDataEntry>>,
        mut feeder: Box<dyn PositionFeeder + Send>,
        tables: &tables::Tables,
    ) -> anyhow::Result<()> {
        let search_params = engine::search::search_params::SearchParams::from_iter(
            format!("depth {ANNOTATION_DEPTH}").split_whitespace(),
        );

        // depth 7 -> 2mb, depth 9 -> 4mb
        let mut tt = engine::search::transposition_v2::TranspositionTable::new(4);
        let rt = engine::search::repetition_v2::RepetitionTable::new();

        let mut search_engine =
            engine::search::v12_eval_sp::Search::new(search_params, tables, &mut tt, rt);

        let mut training_entries = Vec::new();

        let mut dbg_tt_hitrates = Vec::new();

        loop {
            let position = match feeder.next_position() {
                Some(p) => p,
                None => break,
            };

            search_engine
                .new_game_from_fen(&position, tables)
                .map_err(|err| {
                    anyhow::anyhow!(
                        "Failed to load FEN \"{}\" during annotated selfplay - {:?}",
                        position,
                        err
                    )
                })?;

            let mut last_entry = sfbinpack::TrainingDataEntry {
                pos: sfbinpack::chess::position::Position::from_fen(&position),
                mv: r#move::Move::null(),
                ply: search_engine.get_board_mut().ply(),
                result: 0,
                score: 0,
            };

            let mut board_zobrist = search_engine.get_board_mut().zobrist_key();
            let initial_b_move = search_engine.get_board_mut().b_move();

            search_engine
                .get_rt_mut()
                .push_position(board_zobrist, true);

            // Main game loop
            let result = loop {
                search_engine.new_search();

                let bestmove = search_engine.search();
                let score = search_engine.search_score();

                let mut board = search_engine.get_board_mut().clone();

                debug_assert!(
                    board_zobrist == board.zobrist_key(),
                    "Board changed during search!"
                );

                last_entry.mv = Self::convert_move(board.b_move(), bestmove);
                last_entry.score = (score as i16).clamp(-10000, 10000);
                training_entries.push(last_entry.clone());
                last_entry.pos = last_entry.pos.after_move(last_entry.mv);
                last_entry.ply += 1;

                let nnue_update = match unsafe { board.make_move_nnue(bestmove, tables) } {
                    Some(nnue_update) => nnue_update,
                    None => {
                        return Err(anyhow::anyhow!(
                            "[Thread {}]: Failed to make move during annotated selfplay, fen=\"{}\": {:?}",
                            thread_id,
                            position,
                            bestmove
                        ));
                    }
                };

                search_engine.get_nnue_mut().make_move(nnue_update);

                if board.in_check(tables, !board.b_move()) {
                    return Err(anyhow::anyhow!(
                        "[Thread {}]: Illegal move (leaves player in check) during annotated selfplay, fen=\"{}\": {:?}",
                        thread_id,
                        position,
                        bestmove
                    ));
                }

                let mut game_state = Self::check_game_state(&board, tables);

                board_zobrist = board.zobrist_key();
                let last_move_irreversible = board.half_moves() == 0;

                let rt = search_engine.get_rt_mut();
                rt.push_position(board_zobrist, last_move_irreversible);

                if game_state == chess_v2::GameState::Ongoing
                    && rt.is_repeated_times(board_zobrist) >= 3
                {
                    game_state = chess_v2::GameState::Draw;
                }

                match game_state {
                    chess_v2::GameState::Ongoing => {}
                    chess_v2::GameState::Checkmate(side) => {
                        break if side == util::Side::White { 1 } else { -1 };
                    }
                    chess_v2::GameState::Draw => {
                        break 0;
                    }
                }

                *search_engine.get_board_mut() = board;
            };

            if DEBUG {
                let stats = search_engine.get_tt_mut().calc_stats();
                dbg_tt_hitrates
                    .push(stats.probe_hit as f64 / (stats.probe_hit + stats.probe_miss) as f64);
                println!(
                    "[Thread {}] Game finished with result {} after {} plies. TT usage: {:.02}%, probe hit rate: {:.02}%, store hit rate: {:.02}%. Collisions: {}",
                    thread_id,
                    result,
                    search_engine.get_board_mut().ply(),
                    stats.fill_percentage * 100.0,
                    stats.probe_hit as f64 / (stats.probe_hit + stats.probe_miss) as f64 * 100.0,
                    stats.store_hit as f64 / (stats.store_hit + stats.store_miss) as f64 * 100.0,
                    stats.collisions,
                );
            }

            // alternate with initia_b_move
            let mut b_move = initial_b_move;
            for entry in training_entries.iter_mut() {
                entry.result = if b_move { -result } else { result };
                b_move = !b_move;
            }

            tx.send(training_entries.clone()).unwrap();
            training_entries.clear();
        }

        if DEBUG {
            dbg_tt_hitrates.sort_by(|a, b| b.partial_cmp(a).unwrap());

            let avg_tt_hitrate: f64 =
                dbg_tt_hitrates.iter().sum::<f64>() / dbg_tt_hitrates.len() as f64;
            let median_tt_hitrate: f64 = dbg_tt_hitrates[dbg_tt_hitrates.len() / 2];
            println!(
                "[Thread {}] Finished annotated selfplay. Avg TT probe hit rate: {:.02}%, median: {:.02}%",
                thread_id,
                avg_tt_hitrate * 100.0,
                median_tt_hitrate * 100.0
            );
        }

        Ok(())
    }

    fn check_game_state(
        board: &chess_v2::ChessGame,
        tables: &tables::Tables,
    ) -> chess_v2::GameState {
        let mut has_legal_moves = false;
        let mut move_list = [0u16; 256];

        for mv_index in 0..board.gen_moves_avx512::<false>(tables, &mut move_list) {
            let mut board_copy = board.clone();

            let is_legal = unsafe { board_copy.make_move(move_list[mv_index], tables) }
                && !board_copy.in_check(tables, !board_copy.b_move());

            if is_legal {
                has_legal_moves = true;
                break;
            }
        }

        board.check_game_state(tables, !has_legal_moves, board.b_move())
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

    fn set_max_positions(&self, max: usize) {
        let mut inner = self.inner.lock().unwrap();
        inner.set_max_positions(max);
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
    max_positions_to_play: Option<usize>,
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
            max_positions_to_play: None,
            positions_total: num_lines,
            chunk: Vec::new(),
            overflow: String::new(),
            reader: std::io::BufReader::new(file),
            buf,
        }
    }

    fn set_max_positions(&mut self, max: usize) {
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

    fn next_position(&mut self) -> Option<String> {
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
