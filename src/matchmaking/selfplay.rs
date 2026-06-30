use sfbinpack::chess::{coords, r#move, piece};

use super::fen_feeder::SharedFenFeeder;

use std::cell::SyncUnsafeCell;

const DEBUG: bool = false;
const ANNOTATION_DEPTH: u8 = 8;
const USE_ACC_SEARCH: bool = false;

const STABILITY_THRESHOLD: f64 = 0.13;

const INSUFFICIENT_MATERIAL_DRAW: bool = true;
const THREE_FOLD_REPETITION_DRAW: bool = true;
const WIN_ADJUDICATION: bool = false;
const WIN_ADJ_SCORE: i32 = 2000;
const WIN_ADJ_PLIES: i32 = 5;
const DRAW_ADJUDICATION: bool = false;
const DRAW_ADJ_SCORE: i32 = 10;
const DRAW_ADJ_PLIES: u32 = 10;
const DRAW_ADJ_MIN_PLY: u32 = 60;

use crate::{
    engine::{
        self,
        chess_v2::{self, GameState, PieceIndex},
        search::{
            EngineForm, SearchStrategy,
            eval::Eval,
            search::Search,
            timeman::{self, TimeManager},
            transposition::TranspositionTable,
        },
        tables,
    },
    matchmaking::matchmaking::PositionFeeder,
    nnue::nnue::UpdatableNnue,
    util,
};

#[derive(Debug, Clone)]
struct TrainingDataEntry {
    entry: sfbinpack::TrainingDataEntry,
    stability: f64,
}

#[derive(Default)]
struct AdjudicationState {
    win_streak: i32,
    draw_streak: u32,
}

struct SelfplayEngine<'a> {
    thread_id: usize,

    tables: &'a tables::Tables,

    search_qlk: engine::search::search::Search<'a, { EngineForm::TacticalB }>,
    search_acc: engine::search::search::Search<'a, { EngineForm::TacticalA }>,

    zobrist_key: u64,
}

pub struct SelfplayTrainer {
    binpack_writer: Option<sfbinpack::CompressedTrainingDataEntryWriter>,
    stats_num_games_cp: usize,
    stats_num_games_total: usize,
    stats_positions_total: usize,
    stat_num_stab_threshold_passed: usize,
    stats_flushed_at: std::time::Instant,
    binpack_path: Option<String>,
}

impl SelfplayTrainer {
    pub fn new(out_binpack_path: Option<&str>) -> Self {
        let binpack_writer = if let Some(out_binpack_path) = out_binpack_path {
            Some(
                sfbinpack::CompressedTrainingDataEntryWriter::new(out_binpack_path, false).unwrap(),
            )
        } else {
            None
        };

        Self {
            binpack_writer,
            stats_num_games_cp: 0,
            stats_num_games_total: 0,
            stats_positions_total: 0,
            stat_num_stab_threshold_passed: 0,
            stats_flushed_at: std::time::Instant::now(),
            binpack_path: out_binpack_path.map(|s| s.to_string()),
        }
    }

    pub fn play_annotated(
        &mut self,
        threads: usize,
        position_file_path: &str,
        from: Option<usize>,
        count: Option<usize>,
        max_positions: Option<usize>,
    ) -> anyhow::Result<()> {
        let feeder = Box::new(SharedFenFeeder::new(position_file_path));

        if let Some(from) = from {
            let mut lock = feeder.lock();
            lock.set_max_positions(from);
            for _ in 0..from {
                lock.next_position();
            }
        }

        let num_positions_total = if let Some(count) = count {
            let lock = feeder.lock();
            lock.positions_total().min(count)
        } else {
            feeder.lock().positions_total()
        };

        feeder.set_max_positions(num_positions_total);

        println!(
            "Starting selfplay with {} threads, depth {} {} total positions ({:.02} per matchmaker){}",
            threads,
            ANNOTATION_DEPTH,
            num_positions_total,
            num_positions_total as f64 / threads as f64,
            if self.binpack_writer.is_some() {
                String::new()
            } else {
                " (dryrun)".to_string()
            }
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
                        util::pin_thread_for_worker(i);
                        Self::play_annotated_thread(i, tx_entries, feeder, tables)
                    }))
                })
                .collect::<Vec<_>>();

            drop(tx_entries);

            self.stats_flushed_at = std::time::Instant::now();
            self.stats_num_games_cp = 0;
            self.stats_num_games_total = 0;
            self.stats_positions_total = 0;
            self.stat_num_stab_threshold_passed = 0;

            'outer: loop {
                match rx_entries.recv_timeout(std::time::Duration::from_secs(1)) {
                    Ok(entries) => {
                        self.stats_num_games_cp += 1;
                        self.stats_num_games_total += 1;

                        for e in entries {
                            self.stat_num_stab_threshold_passed +=
                                (e.stability >= STABILITY_THRESHOLD) as usize;

                            if let Some(writer) = &mut self.binpack_writer {
                                writer.write_entry(&e.entry).unwrap();
                                self.stats_positions_total += 1;
                            }

                            if let Some(max_positions) = max_positions {
                                if self.stats_positions_total >= max_positions {
                                    println!(
                                        "Reached max positions limit of {}. Stopping selfplay.",
                                        max_positions
                                    );
                                    break 'outer;
                                }
                            }
                        }
                    }
                    Err(crossbeam::channel::RecvTimeoutError::Timeout) => {}
                    Err(crossbeam::channel::RecvTimeoutError::Disconnected) => break,
                }

                if self.stats_flushed_at.elapsed().as_secs() >= 60 * 1 {
                    let games_per_minute = self.stats_num_games_cp as f64
                        / (self.stats_flushed_at.elapsed().as_secs_f64() / 60.0);
                    let games_per_minute_stable = self.stats_num_games_total as f64
                        / (start_at.elapsed().as_secs_f64() / 60.0);
                    println!(
                        "Checkpoint after {} games ({:.02} mins). Games per minute: ~{:.02} ({:.02} avg). {} total positions so far, ~{:.02} per game avg. >=stab%: {}, Binpack size: {}. ETA: {}",
                        self.stats_num_games_total,
                        self.stats_flushed_at.elapsed().as_secs_f64() / 60.0,
                        games_per_minute,
                        games_per_minute_stable,
                        self.stats_positions_total,
                        self.stats_positions_total as f64 / self.stats_num_games_total as f64,
                        (self.stat_num_stab_threshold_passed as f64
                            / self.stats_positions_total as f64)
                            * 100.0,
                        util::byte_size_string(self.binpack_size_bytes()),
                        util::time_format(
                            ((num_positions_total - self.stats_num_games_total) as f64
                                / games_per_minute_stable
                                * 60.0
                                * 1000.0) as u64
                        )
                    );
                    self.stats_flushed_at = std::time::Instant::now();
                    self.stats_num_games_cp = 0;
                }
            }

            drop(rx_entries);

            for handle in handles {
                let _ = handle.join().unwrap();
            }

            println!(
                "Selfplay finished with {} games in {}. {} total positions annotated, games per minute ~{}. binpack size: ~{}",
                self.stats_num_games_total,
                util::time_format(start_at.elapsed().as_millis() as u64),
                self.stats_positions_total,
                self.stats_num_games_total as f64 / (start_at.elapsed().as_secs_f64() / 60.0),
                util::byte_size_string(self.binpack_size_bytes()),
            );
        });

        Ok(())
    }

    fn play_annotated_thread(
        thread_id: usize,
        tx: crossbeam::channel::Sender<Vec<TrainingDataEntry>>,
        mut feeder: Box<dyn PositionFeeder + Send>,
        tables: &tables::Tables,
    ) -> anyhow::Result<()> {
        let tt = std::cell::SyncUnsafeCell::new(
            engine::search::transposition::TranspositionTable::new(16),
        );

        let mut tm = std::cell::SyncUnsafeCell::new(timeman::TimeManager::new());
        tm.get_mut().disable();

        let mut engine = SelfplayEngine::new(thread_id, &tt, &tm, tables);

        let mut training_entries = Vec::new();

        // let mut dbg_tt_hitrates = Vec::new();

        loop {
            let position = match feeder.next_position() {
                Some(p) => p,
                None => break,
            };

            engine.new_game(&position)?;

            let mut last_entry = TrainingDataEntry {
                entry: sfbinpack::TrainingDataEntry {
                    pos: sfbinpack::chess::position::Position::from_fen(&position),
                    mv: r#move::Move::null(),
                    ply: engine.ply(),
                    result: 0,
                    score: 0,
                },
                stability: 0.0,
            };

            let initial_b_move = engine.b_move();
            let mut adj = AdjudicationState::default();

            // Main game loop
            let result = loop {
                let (bestmove, score, stability) = engine.new_move(ANNOTATION_DEPTH.into());

                let mover_bmove = engine.b_move();

                last_entry.entry.mv = Self::convert_move(mover_bmove, bestmove);
                last_entry.entry.score = (score as i16).clamp(-10000, 10000);
                last_entry.stability = stability;
                training_entries.push(last_entry.clone());
                last_entry.entry.pos = last_entry.entry.pos.after_move(last_entry.entry.mv);
                last_entry.entry.ply += 1;

                let (mut game_state, rep_count) = engine.make_move(bestmove)?;

                if THREE_FOLD_REPETITION_DRAW && game_state == chess_v2::GameState::Ongoing {
                    if rep_count >= 3 {
                        game_state = chess_v2::GameState::Draw;
                    }
                }

                if INSUFFICIENT_MATERIAL_DRAW && game_state == chess_v2::GameState::Ongoing {
                    let occupancy = engine.occupancy();
                    let bitboards = engine.bitboards();

                    let king_vs_king = occupancy.count_ones() == 2;
                    let king_and_bishop_vs_king = occupancy.count_ones() == 3
                        && (bitboards[PieceIndex::WhiteBishop as usize].count_ones() == 1
                            || bitboards[PieceIndex::BlackBishop as usize].count_ones() == 1);
                    let king_and_knight_vs_king = occupancy.count_ones() == 3
                        && (bitboards[PieceIndex::WhiteKnight as usize].count_ones() == 1
                            || bitboards[PieceIndex::BlackKnight as usize].count_ones() == 1);

                    if king_vs_king || king_and_bishop_vs_king || king_and_knight_vs_king {
                        game_state = chess_v2::GameState::Draw;
                    }
                }

                if game_state == chess_v2::GameState::Ongoing {
                    let white_pov_score = if engine.b_move() { score } else { -score };
                    if let Some(adj_result) = Self::adjudicate(
                        &mut adj,
                        white_pov_score,
                        stability < STABILITY_THRESHOLD,
                        engine.ply() as u32,
                    ) {
                        break adj_result;
                    }
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
            };

            if DEBUG {
                // let stats = search_engine.get_tt_mut().calc_stats();
                // dbg_tt_hitrates
                //     .push(stats.probe_hit as f64 / (stats.probe_hit + stats.probe_miss) as f64);
                // println!(
                //     "[Thread {}] Game finished with result {} after {} plies. TT usage: {:.02}%, probe hit rate: {:.02}%, store hit rate: {:.02}%. Collisions: {}",
                //     thread_id,
                //     result,
                //     search_engine.get_board_mut().ply(),
                //     stats.fill_percentage * 100.0,
                //     stats.probe_hit as f64 / (stats.probe_hit + stats.probe_miss) as f64 * 100.0,
                //     stats.store_hit as f64 / (stats.store_hit + stats.store_miss) as f64 * 100.0,
                //     stats.collisions,
                // );
            }

            // alternate with initia_b_move
            let mut b_move = initial_b_move;
            for e in training_entries.iter_mut() {
                e.entry.result = if b_move { -result } else { result };
                b_move = !b_move;
            }

            match tx.send(training_entries.clone()) {
                Ok(_) => {}
                // Handle disconnect
                Err(_) => break,
            }
            training_entries.clear();
        }

        if DEBUG {
            // dbg_tt_hitrates.sort_by(|a, b| b.partial_cmp(a).unwrap());

            // let avg_tt_hitrate: f64 =
            //     dbg_tt_hitrates.iter().sum::<f64>() / dbg_tt_hitrates.len() as f64;
            // let median_tt_hitrate: f64 = dbg_tt_hitrates[dbg_tt_hitrates.len() / 2];
            // println!(
            //     "[Thread {}] Finished annotated selfplay. Avg TT probe hit rate: {:.02}%, median: {:.02}%",
            //     thread_id,
            //     avg_tt_hitrate * 100.0,
            //     median_tt_hitrate * 100.0
            // );
        }

        Ok(())
    }

    fn check_game_state(
        board: &chess_v2::ChessGame,
        tables: &tables::Tables,
    ) -> chess_v2::GameState {
        let mut has_legal_moves = false;
        let mut move_list = [0u16; 256];

        for mv_index in 0..board.gen_moves_avx512::<false, _>(&mut move_list) {
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
        match self.binpack_path {
            Some(ref path) => std::fs::metadata(path).map(|m| m.len()).unwrap_or(0) as usize,
            None => 0,
        }
    }

    fn search_stability(stats: &[engine::search::search::DepthStat]) -> Option<f64> {
        engine::search::stability::winprob_instability(stats).map(|v| v * 100.0)
    }

    fn adjudicate(
        adj: &mut AdjudicationState,
        white_score: i32,
        stable: bool,
        ply: u32,
    ) -> Option<i16> {
        if WIN_ADJUDICATION {
            if stable && white_score >= WIN_ADJ_SCORE {
                adj.win_streak = adj.win_streak.max(0) + 1;
            } else if stable && white_score <= -WIN_ADJ_SCORE {
                adj.win_streak = adj.win_streak.min(0) - 1;
            } else {
                adj.win_streak = 0;
            }

            if adj.win_streak >= WIN_ADJ_PLIES {
                return Some(1);
            }

            if adj.win_streak <= -WIN_ADJ_PLIES {
                return Some(-1);
            }
        }

        if DRAW_ADJUDICATION {
            if stable && ply >= DRAW_ADJ_MIN_PLY && white_score.abs() <= DRAW_ADJ_SCORE {
                adj.draw_streak += 1;
            } else {
                adj.draw_streak = 0;
            }

            if adj.draw_streak >= DRAW_ADJ_PLIES {
                return Some(0);
            }
        }

        None
    }
}

impl<'a> SelfplayEngine<'a> {
    fn new(
        thread_id: usize,
        tt: &'a SyncUnsafeCell<TranspositionTable>,
        tm: &'a SyncUnsafeCell<TimeManager>,
        tables: &'a tables::Tables,
    ) -> Self {
        let search_qlk = Search::<{ EngineForm::TacticalB }>::new(
            tables,
            &tt,
            &tm,
            engine::search::repetition::RepetitionTable::new(),
        );
        let search_acc = Search::<{ EngineForm::TacticalA }>::new(
            tables,
            &tt,
            &tm,
            engine::search::repetition::RepetitionTable::new(),
        );

        Self {
            thread_id,
            tables,
            search_qlk,
            search_acc,
            zobrist_key: 0,
        }
    }

    fn new_game(&mut self, fen: &str) -> anyhow::Result<()> {
        self.search_qlk.new_game();

        self.search_qlk
            .load_from_fen(fen, self.tables)
            .map_err(|err| {
                anyhow::anyhow!(
                    "Failed to load FEN \"{}\" during annotated selfplay - {:?}",
                    fen,
                    err
                )
            })?;

        if USE_ACC_SEARCH {
            self.search_acc.new_game();
            self.search_acc
                .load_from_fen(fen, self.tables)
                .map_err(|err| {
                    anyhow::anyhow!(
                        "Failed to load FEN \"{}\" during annotated selfplay - {:?}",
                        fen,
                        err
                    )
                })?;
        }

        let board_zobrist = self.search_qlk.get_board_mut().zobrist_key();

        self.search_qlk
            .get_rt_mut()
            .push_position(board_zobrist, true);

        if USE_ACC_SEARCH {
            self.search_acc
                .get_rt_mut()
                .push_position(board_zobrist, true);
        }

        self.zobrist_key = board_zobrist;

        Ok(())
    }

    fn new_move(&mut self, search_depth: u8) -> (u16, i32, f64) {
        self.search_qlk.new_search();

        let qlk_bestmove = self.search_qlk.search(search_depth.into());

        let stability =
            SelfplayTrainer::search_stability(self.search_qlk.depth_stats()).unwrap_or(1.0);

        let use_acc = stability >= STABILITY_THRESHOLD;

        let is_mate = engine::search::eval::is_mate(self.search_qlk.search_score() as Eval);

        let (bestmove, score) = if USE_ACC_SEARCH && use_acc && !is_mate {
            self.search_acc.new_search();
            self.search_acc.tt_mut().zero_depth_entries();

            let acc_bestmove = self.search_acc.search(search_depth.into());
            (acc_bestmove, self.search_acc.search_score())
        } else {
            (qlk_bestmove, self.search_qlk.search_score())
        };

        debug_assert!(
            self.search_qlk.get_board_mut().zobrist_key() == self.zobrist_key,
            "Qlk board changed during search!"
        );
        debug_assert!(
            !USE_ACC_SEARCH || self.search_acc.get_board_mut().zobrist_key() == self.zobrist_key,
            "Acc board changed during search!"
        );

        (bestmove, score, stability)
    }

    fn make_move(&mut self, mv: u16) -> anyhow::Result<(GameState, u32)> {
        let mut board = self.search_qlk.get_board_mut().clone();

        let nnue_update = match unsafe { board.make_move_nnue(mv, self.tables) } {
            Some(nnue_update) => nnue_update,
            None => {
                return Err(anyhow::anyhow!(
                    "[Thread {}]: Failed to make move during annotated selfplay, fen=\"{}\": {}",
                    self.thread_id,
                    board.gen_fen(),
                    util::move_string_dbg(mv),
                ));
            }
        };

        if board.in_check(self.tables, !board.b_move()) {
            return Err(anyhow::anyhow!(
                "[Thread {}]: Illegal move (leaves player in check) during annotated selfplay, fen=\"{}\": {:?}",
                self.thread_id,
                board.gen_fen(),
                util::move_string_dbg(mv),
            ));
        }

        let last_move_irreversible = board.half_moves() == 0;

        self.search_qlk
            .get_nnue_mut()
            .make_move(nnue_update.clone());
        self.search_qlk
            .get_rt_mut()
            .push_position(board.zobrist_key(), last_move_irreversible);
        self.search_qlk.get_board_mut().clone_from(&board);

        if USE_ACC_SEARCH {
            self.search_acc.get_nnue_mut().make_move(nnue_update);

            self.search_acc
                .get_rt_mut()
                .push_position(board.zobrist_key(), last_move_irreversible);

            self.search_acc.get_board_mut().clone_from(&board);
        }

        self.zobrist_key = board.zobrist_key();

        Ok((
            SelfplayTrainer::check_game_state(&board, self.tables),
            self.search_qlk.get_rt().is_repeated_times(self.zobrist_key),
        ))
    }

    fn ply(&mut self) -> u16 {
        self.search_qlk.get_board_mut().ply()
    }

    fn b_move(&mut self) -> bool {
        self.search_qlk.get_board_mut().b_move()
    }

    fn occupancy(&mut self) -> u64 {
        self.search_qlk.get_board_mut().occupancy()
    }

    fn bitboards(&mut self) -> &[u64; 16] {
        self.search_qlk.get_board_mut().bitboards()
    }
}
