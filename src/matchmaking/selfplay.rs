use sfbinpack::chess::{coords, r#move, piece};

use super::fen_feeder::SharedFenFeeder;

use std::cell::SyncUnsafeCell;

const DEBUG: bool = false;
const ANNOTATION_DEPTH: u8 = 9;

const LABELING_HARD_NODES: u64 = 100_000_000;

const INSUFFICIENT_MATERIAL_DRAW: bool = true;
const THREE_FOLD_REPETITION_DRAW: bool = true;

const WIN_ADJ_SCORE: i32 = 2000;
const WIN_ADJ_PLIES: i32 = 7;
const DRAW_ADJ_SCORE: i32 = 7;
const DRAW_ADJ_PLIES: u32 = 17;
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
}

#[derive(Default)]
struct AdjudicationState {
    win_streak: i32,
    draw_streak: u32,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum GameEnding {
    Natural,
    WinAdjudication,
    DrawAdjudication,
    Terminated, // Node budget exceeded, cant guarantee optimal play
}

struct ShadowPly {
    ply: u32,
    white_score: i32,
    instab_x100: f64,
}

struct ShadowFire {
    fire_ply: u32,
    gate: &'static str,
    declared: i16,
}

struct ShadowGame {
    seed_fen: String,
    natural_result: i16,
    plies: Vec<ShadowPly>,
    would_fire: Option<ShadowFire>,
}

struct GameResult {
    entries: Vec<TrainingDataEntry>,
    ending: GameEnding,
    shadow: Option<ShadowGame>,
}

struct SelfplayEngine<'a> {
    thread_id: usize,
    tables: &'a tables::Tables,
    search: engine::search::search::Search<'a, { EngineForm::TacticalB }>,
    zobrist_key: u64,
}

pub struct SelfplayTrainer {
    binpack_writer: Option<sfbinpack::CompressedTrainingDataEntryWriter>,
    stats_num_games_cp: usize,
    stats_num_games_total: usize,
    stats_positions_total: usize,
    stats_win_adj: usize,
    stats_draw_adj: usize,
    stats_terminated: usize,
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
            stats_win_adj: 0,
            stats_draw_adj: 0,
            stats_terminated: 0,
            stats_flushed_at: std::time::Instant::now(),
            binpack_path: out_binpack_path.map(|s| s.to_string()),
        }
    }

    pub fn play_annotated(
        &mut self,
        threads: usize,
        position_file_paths: &[String],
        from: Option<usize>,
        count: Option<usize>,
        max_positions: Option<usize>,
        win_adj: bool,
        draw_adj: bool,
        adj_shadow_log: Option<&str>,
    ) -> anyhow::Result<()> {
        if adj_shadow_log.is_some() && (win_adj || draw_adj) {
            return Err(anyhow::anyhow!(
                "--adj-shadow-log requires adjudication off; drop --win-adj / --draw-adj"
            ));
        }
        let shadow = adj_shadow_log.is_some();
        let mut shadow_writer = match adj_shadow_log {
            Some(path) => Some(std::io::BufWriter::new(std::fs::File::create(path)?)),
            None => None,
        };

        let feeder = Box::new(SharedFenFeeder::new_multi(position_file_paths));

        if let Some(from) = from {
            let mut lock = feeder.lock();
            lock.set_max_positions(from);
            for _ in 0..from {
                lock.next_position();
            }
        }

        let num_games_total = if let Some(count) = count {
            let lock = feeder.lock();
            lock.positions_total().min(count)
        } else {
            feeder.lock().positions_total()
        };

        feeder.set_max_positions(num_games_total);

        println!(
            "Starting selfplay with {} threads, depth {} {} total games ({:.02} per matchmaker){}",
            threads,
            ANNOTATION_DEPTH,
            num_games_total,
            num_games_total as f64 / threads as f64,
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
                        Self::play_annotated_thread(
                            i, tx_entries, feeder, tables, win_adj, draw_adj, shadow,
                        )
                    }))
                })
                .collect::<Vec<_>>();

            drop(tx_entries);

            self.stats_flushed_at = std::time::Instant::now();
            self.stats_num_games_cp = 0;
            self.stats_num_games_total = 0;
            self.stats_positions_total = 0;
            self.stats_win_adj = 0;
            self.stats_draw_adj = 0;

            'outer: loop {
                match rx_entries.recv_timeout(std::time::Duration::from_secs(1)) {
                    Ok(game) => {
                        self.stats_num_games_cp += 1;
                        self.stats_num_games_total += 1;

                        match game.ending {
                            GameEnding::WinAdjudication => self.stats_win_adj += 1,
                            GameEnding::DrawAdjudication => self.stats_draw_adj += 1,
                            GameEnding::Terminated => {
                                self.stats_terminated += 1;
                                continue;
                            }
                            GameEnding::Natural => {}
                        }

                        if let (Some(shadow), Some(w)) = (&game.shadow, shadow_writer.as_mut()) {
                            Self::write_shadow_record(w, shadow).expect("shadow log write failed");
                        }

                        for e in game.entries {
                            if let Some(writer) = &mut self.binpack_writer {
                                writer.write_entry(&e.entry).unwrap();
                            }
                            self.stats_positions_total += 1;

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

                    let time_to_complete_games_ms = ((num_games_total - self.stats_num_games_total)
                        as f64
                        / games_per_minute_stable
                        * 60.0
                        * 1000.0) as u64;

                    let eta_ms = if let Some(max_positions) = max_positions {
                        let positions_per_minute_stable = self.stats_positions_total as f64
                            / (start_at.elapsed().as_secs_f64() / 60.0);

                        let time_to_complete_positions_ms =
                            ((max_positions - self.stats_positions_total) as f64
                                / positions_per_minute_stable
                                * 60.0
                                * 1000.0) as u64;

                        time_to_complete_games_ms.min(time_to_complete_positions_ms)
                    } else {
                        time_to_complete_games_ms
                    };

                    println!(
                        "Checkpoint after {} games ({:.02} mins). Games per minute: ~{:.02} ({:.02} avg). {} total positions, ~{:.02} per game avg. win-adj%: {:.02}, draw-adj%: {:.02}, term: {}, BP size: {}. ETA: {}",
                        self.stats_num_games_total,
                        self.stats_flushed_at.elapsed().as_secs_f64() / 60.0,
                        games_per_minute,
                        games_per_minute_stable,
                        self.stats_positions_total,
                        self.stats_positions_total as f64 / self.stats_num_games_total as f64,
                        self.stats_win_adj as f64 / self.stats_num_games_total as f64 * 100.0,
                        self.stats_draw_adj as f64 / self.stats_num_games_total as f64 * 100.0,
                        self.stats_terminated,
                        util::byte_size_string(self.binpack_size_bytes()),
                        util::time_format(eta_ms)
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
                "Selfplay finished with {} games in {}. {} total positions annotated, games per minute ~{}. win-adj%: {:.02}, draw-adj%: {:.02}. binpack size: ~{}",
                self.stats_num_games_total,
                util::time_format(start_at.elapsed().as_millis() as u64),
                self.stats_positions_total,
                self.stats_num_games_total as f64 / (start_at.elapsed().as_secs_f64() / 60.0),
                self.stats_win_adj as f64 / self.stats_num_games_total as f64 * 100.0,
                self.stats_draw_adj as f64 / self.stats_num_games_total as f64 * 100.0,
                util::byte_size_string(self.binpack_size_bytes()),
            );
        });

        if let Some(mut w) = shadow_writer.take() {
            use std::io::Write;
            w.flush()?;
        }

        Ok(())
    }

    fn play_annotated_thread(
        thread_id: usize,
        tx: crossbeam::channel::Sender<GameResult>,
        mut feeder: Box<dyn PositionFeeder + Send>,
        tables: &tables::Tables,
        win_adj: bool,
        draw_adj: bool,
        shadow: bool,
    ) -> anyhow::Result<()> {
        let tt = std::cell::SyncUnsafeCell::new(
            engine::search::transposition::TranspositionTable::new(8),
        );

        let mut tm = std::cell::SyncUnsafeCell::new(timeman::TimeManager::new());
        tm.get_mut().disable();
        tm.get_mut().set_nodes(LABELING_HARD_NODES);

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
            };

            let initial_b_move = engine.b_move();
            let mut adj = AdjudicationState::default();

            let mut shadow_adj = AdjudicationState::default();
            let mut shadow_plies: Vec<ShadowPly> = Vec::new();
            let mut shadow_would_fire: Option<ShadowFire> = None;

            // Main game loop
            let (result, ending) = loop {
                let (bestmove, score, stability, node_aborted) =
                    engine.new_move(ANNOTATION_DEPTH.into());

                if node_aborted {
                    break (0, GameEnding::Terminated);
                }

                let mover_bmove = engine.b_move();

                last_entry.entry.mv = Self::convert_move(mover_bmove, bestmove);
                last_entry.entry.score = (score as i16).clamp(-10000, 10000);
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

                    if shadow {
                        if shadow_would_fire.is_none() {
                            if let Some((declared, fired)) = Self::adjudicate(
                                &mut shadow_adj,
                                white_pov_score,
                                engine.ply() as u32,
                                true,
                                true,
                            ) {
                                shadow_would_fire = Some(ShadowFire {
                                    fire_ply: engine.ply() as u32,
                                    gate: if fired == GameEnding::WinAdjudication {
                                        "win"
                                    } else {
                                        "draw"
                                    },
                                    declared,
                                });
                            }
                        }
                        shadow_plies.push(ShadowPly {
                            ply: engine.ply() as u32,
                            white_score: white_pov_score,
                            instab_x100: stability,
                        });
                    }

                    if let Some(adj_result) = Self::adjudicate(
                        &mut adj,
                        white_pov_score,
                        engine.ply() as u32,
                        win_adj,
                        draw_adj,
                    ) {
                        break adj_result;
                    }
                }

                match game_state {
                    chess_v2::GameState::Ongoing => {}
                    chess_v2::GameState::Checkmate(side) => {
                        break (
                            if side == util::Side::White { 1 } else { -1 },
                            GameEnding::Natural,
                        );
                    }
                    chess_v2::GameState::Draw => {
                        break (0, GameEnding::Natural);
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

            let shadow_game = if shadow {
                Some(ShadowGame {
                    seed_fen: position.trim().to_string(),
                    natural_result: result,
                    plies: std::mem::take(&mut shadow_plies),
                    would_fire: shadow_would_fire.take(),
                })
            } else {
                None
            };

            match tx.send(GameResult {
                entries: training_entries.clone(),
                ending,
                shadow: shadow_game,
            }) {
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

    // FEN carries no '"' or '\', so it needs no JSON escaping.
    fn write_shadow_record(w: &mut impl std::io::Write, g: &ShadowGame) -> std::io::Result<()> {
        use std::io::Write;
        write!(
            w,
            "{{\"seed_fen\":\"{}\",\"natural_result\":{},\"plies\":[",
            g.seed_fen, g.natural_result
        )?;
        for (i, p) in g.plies.iter().enumerate() {
            if i > 0 {
                w.write_all(b",")?;
            }
            write!(w, "[{},{},{:.6}]", p.ply, p.white_score, p.instab_x100)?;
        }
        w.write_all(b"],\"would_fire\":")?;
        match &g.would_fire {
            Some(f) => write!(
                w,
                "{{\"fire_ply\":{},\"gate\":\"{}\",\"declared\":{}}}",
                f.fire_ply, f.gate, f.declared
            )?,
            None => w.write_all(b"null")?,
        }
        w.write_all(b"}\n")
    }

    fn adjudicate(
        adj: &mut AdjudicationState,
        white_score: i32,
        ply: u32,
        win_adj: bool,
        draw_adj: bool,
    ) -> Option<(i16, GameEnding)> {
        if win_adj {
            if white_score >= WIN_ADJ_SCORE {
                adj.win_streak = adj.win_streak.max(0) + 1;
            } else if white_score <= -WIN_ADJ_SCORE {
                adj.win_streak = adj.win_streak.min(0) - 1;
            } else {
                adj.win_streak = 0;
            }

            if adj.win_streak >= WIN_ADJ_PLIES {
                return Some((1, GameEnding::WinAdjudication));
            }

            if adj.win_streak <= -WIN_ADJ_PLIES {
                return Some((-1, GameEnding::WinAdjudication));
            }
        }

        if draw_adj {
            if ply >= DRAW_ADJ_MIN_PLY && white_score.abs() <= DRAW_ADJ_SCORE {
                adj.draw_streak += 1;
            } else {
                adj.draw_streak = 0;
            }

            if adj.draw_streak >= DRAW_ADJ_PLIES {
                return Some((0, GameEnding::DrawAdjudication));
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
        let search = Search::<{ EngineForm::TacticalB }>::new(
            tables,
            &tt,
            &tm,
            engine::search::repetition::RepetitionTable::new(),
        );

        Self {
            thread_id,
            tables,
            search,
            zobrist_key: 0,
        }
    }

    fn new_game(&mut self, fen: &str) -> anyhow::Result<()> {
        self.search.new_game();

        self.search.load_from_fen(fen, self.tables).map_err(|err| {
            anyhow::anyhow!(
                "Failed to load FEN \"{}\" during annotated selfplay - {:?}",
                fen,
                err
            )
        })?;

        let board_zobrist = self.search.get_board_mut().zobrist_key();

        self.search.get_rt_mut().push_hash(board_zobrist);

        self.zobrist_key = board_zobrist;

        Ok(())
    }

    fn new_move(&mut self, search_depth: u8) -> (u16, i32, f64, bool) {
        self.search.new_search();

        self.search.get_rt_mut().pop_position();

        let bestmove = self.search.search(search_depth.into());

        self.search.get_rt_mut().push_hash(self.zobrist_key);

        let node_aborted = self.search.was_node_aborted() || self.search.depth_stats().len() < 2;

        let stability = if node_aborted {
            0.0
        } else {
            SelfplayTrainer::search_stability(self.search.depth_stats())
                .expect("winprob_instability None: <2 completed depths")
        };

        let score = self.search.search_score();

        debug_assert!(
            self.search.get_board_mut().zobrist_key() == self.zobrist_key,
            "Qlk board changed during search!"
        );

        (bestmove, score, stability, node_aborted)
    }

    fn make_move(&mut self, mv: u16) -> anyhow::Result<(GameState, u32)> {
        let mut board = self.search.get_board_mut().clone();

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

        self.search.get_nnue_mut().make_move(nnue_update.clone());
        self.search.get_rt_mut().push_hash(board.zobrist_key());
        self.search.get_board_mut().clone_from(&board);

        self.zobrist_key = board.zobrist_key();

        Ok((
            SelfplayTrainer::check_game_state(&board, self.tables),
            self.search
                .get_rt()
                .is_repeated_times(self.zobrist_key, board.half_moves() as usize),
        ))
    }

    fn ply(&mut self) -> u16 {
        self.search.get_board_mut().ply()
    }

    fn b_move(&mut self) -> bool {
        self.search.get_board_mut().b_move()
    }

    fn occupancy(&mut self) -> u64 {
        self.search.get_board_mut().occupancy()
    }

    fn bitboards(&mut self) -> &[u64; 16] {
        self.search.get_board_mut().bitboards()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const REP_FEN_LOSING: &str = "4k1n1/8/8/8/8/8/8/3QK1N1 b - - 10 1";
    const REP_FEN_WINNING: &str = "4k1n1/8/8/8/8/8/8/3QK1N1 w - - 10 1";
    const TEST_DEPTH: u8 = 8;

    const REPRO_GAME: &str = "d2d4 e7e6 g1f3 c7c5 g2g3 c5d4 f3d4 d7d5 f1g2 b8c6 e1g1 f8c5 d4b3 c5b6 c2c4 g8e7 c4d5 e6d5 b1c3 d5d4 c3a4 e8g8 a4b6 d8b6 e2e3 c8e6 b3d4 f8d8 b2b3 e7f5 c1b2 f5d4 e3d4 c6d4 d1e1 a8c8 e1e4 d4c6 a1d1 h7h6 f1e1 d8d1 e1d1 b6c5 e4e1 c8c7 h2h4 c5e7 d1d2 c7d7 b2c3 d7d2 e1d2 e7c7 b3b4 b7b6 a2a4 c6e7 a4a5 c7c8 c3e5 f7f6 e5f4 g8f7 f4e3 c8c7 g1h2 e7f5 e3f4 c7c8 d2b2 g7g5 h4g5 h6g5 f4d2 f5h6 d2c3 e6c4 f2f3 c8e6 b2c2 e6f5 c2d1 g5g4 h2g1 f5e6 d1d2 h6f5 a5b6 e6b6 g1h2 b6e6 d2f4 f5e3 f3g4 e3g2 h2g2 a7a6 f4f5 c4b5 f5e6 f7e6 c3d2 e6d5 g4g5 f6g5 d2g5 d5c4 g5e7 b5c6 g2h3 c6d7 h3h4 d7e6 e7d6 c4b5 h4h5 e6d7 h5g5 d7h3 d6f8 b5c4 g5h5 h3e6 h5h6 e6c8 h6g7 c4b5 g7g6 c8g4 g6g5 g4h3 g5h5 h3d7 h5h4 b5c4 f8d6 c4b5 d6e7 b5a4 h4g5 d7h3 g5f4 a4b5 e7f8 b5c4 f8d6 h3e6 d6e7 e6c8 f4e5 c8h3 e5e4 h3c8 e4f4 c8h3 f4g5 c4b5 g5f4 b5c4 e7f8 c4b5 f8d6 b5c4 f4e5 h3g4 d6f8 g4h3 e5f6 c4b5 f8d6 b5c4 d6e7 c4b5 e7f8 b5c4 f6e5 h3g4";

    struct Harness {
        tt: SyncUnsafeCell<TranspositionTable>,
        tm: SyncUnsafeCell<TimeManager>,
        tables: tables::Tables,
    }

    impl Harness {
        fn new() -> Self {
            let mut tm = SyncUnsafeCell::new(TimeManager::new());
            tm.get_mut().disable();
            tm.get_mut().set_nodes(LABELING_HARD_NODES);

            Self {
                tt: SyncUnsafeCell::new(TranspositionTable::new(8)),
                tm,
                tables: tables::Tables::new(),
            }
        }

        fn engine(&self) -> SelfplayEngine<'_> {
            SelfplayEngine::new(0, &self.tt, &self.tm, &self.tables)
        }
    }

    fn mv_of(engine: &mut SelfplayEngine, mv: &str) -> u16 {
        let board = engine.search.get_board_mut();
        board.fix_move(util::create_move(mv))
    }

    fn play(engine: &mut SelfplayEngine, mv: &str) -> (GameState, u32) {
        let mv = mv_of(engine, mv);
        engine.make_move(mv).expect("legal move")
    }

    fn extended_pv(engine: &mut SelfplayEngine) -> Vec<u16> {
        engine.search.get_rt_mut().pop_position();

        let pv = engine.search.get_pv();

        let key = engine.zobrist_key;
        engine.search.get_rt_mut().push_hash(key);

        pv
    }

    fn drive_shuffle(engine: &mut SelfplayEngine, fen: &str, shuffle: [&str; 4]) {
        engine.new_game(fen).unwrap();
        let start_key = engine.zobrist_key;

        for mv in shuffle {
            play(engine, mv);
        }

        assert_eq!(
            engine.zobrist_key, start_key,
            "the shuffle must return to the loaded position"
        );
    }

    #[test]
    fn test_selfplay_rep_count_sequence() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        let moves = [
            ("e2e4", 1),
            ("e7e5", 1),
            ("g1f3", 1),
            ("g8f6", 1),
            ("f3h4", 1),
            ("f6g8", 1),
            ("h4f3", 2),
            ("g8f6", 2),
            ("f3h4", 2),
            ("f6g8", 2),
            ("h4f3", 3),
            ("g8f6", 3),
            ("f3h4", 3),
            ("f6g8", 3),
            ("h4f3", 4),
            ("g8f6", 4),
            ("f3h4", 4),
            ("f6g8", 4),
            ("h4f3", 5),
            ("g8f6", 5),
            ("f3h4", 5),
            ("f6g8", 5),
            ("h4g6", 1),
            ("g8f6", 1),
            ("g6h4", 6),
            ("f6e4", 1),
            ("h4f3", 1),
            ("e4f6", 1),
            ("f3h4", 1),
        ];

        let mut first_threefold = None;

        for (index, (mv, expected)) in moves.iter().enumerate() {
            let (state, rep_count) = play(&mut engine, mv);

            assert_eq!(state, GameState::Ongoing, "move {} ended the game", mv);
            assert_eq!(rep_count, *expected, "move {} rep count mismatch", mv);

            if rep_count >= 3 && first_threefold.is_none() {
                first_threefold = Some(index);
            }
        }

        assert_eq!(
            first_threefold,
            Some(10),
            "three-fold must first trigger on the 11th move"
        );
    }

    #[test]
    fn test_selfplay_rt_balanced_across_search() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        for mv in ["e2e4", "e7e5", "g1f3", "b8c6"] {
            play(&mut engine, mv);
        }

        for _ in 0..3 {
            let cursor_before = engine.search.get_rt().cursor;
            let key_before = engine.zobrist_key;

            let (bestmove, _, _, aborted) = engine.new_move(TEST_DEPTH);

            assert!(!aborted);
            assert_eq!(engine.search.get_rt().cursor, cursor_before);
            assert_eq!(engine.zobrist_key, key_before);
            assert_eq!(
                engine.search.get_rt().hashes[cursor_before - 1],
                key_before,
                "the current position must be the table top between searches"
            );

            engine.make_move(bestmove).unwrap();
        }
    }

    #[test]
    fn test_selfplay_finds_pre_root_repetition() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        drive_shuffle(
            &mut engine,
            REP_FEN_LOSING,
            ["g8f6", "g1f3", "f6g8", "f3g1"],
        );

        let repeating = mv_of(&mut engine, "g8f6");
        let (bestmove, score, _, aborted) = engine.new_move(TEST_DEPTH);

        assert!(!aborted);
        assert_eq!(
            bestmove, repeating,
            "the losing side must repeat a pre-root position"
        );
        assert_eq!(score, 0, "repeating must score as a draw");
    }

    #[test]
    fn test_selfplay_avoids_pre_root_repetition() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        drive_shuffle(
            &mut engine,
            REP_FEN_WINNING,
            ["g1f3", "g8f6", "f3g1", "f6g8"],
        );

        let repeating = mv_of(&mut engine, "g1f3");
        let (bestmove, score, _, aborted) = engine.new_move(TEST_DEPTH);

        assert!(!aborted);
        assert_ne!(
            bestmove, repeating,
            "the winning side must not repeat a pre-root position"
        );
        assert!(score > 1000, "winning side scored {}", score);
    }

    #[test]
    #[ignore]
    fn tmp_find_fortress() {
        let fens = [
            "8/8/1p6/pPp5/PpP5/1P6/8/K1k5 w - - 0 1",
            "8/8/1p6/pPp5/PpP5/1P6/8/K1k5 b - - 0 1",
            "8/8/8/3k4/3p4/3P4/8/3K4 w - - 0 1",
            "8/8/4k3/8/8/4K3/8/8 w - - 0 1",
            "4k1n1/8/8/8/8/8/8/4K1N1 w - - 10 1",
        ];

        for fen in fens {
            let harness = Harness::new();
            let mut engine = harness.engine();
            if engine.new_game(fen).is_err() {
                println!("{} -> FEN rejected", fen);
                continue;
            }

            let root_key = engine.zobrist_key;

            for depth in [8u8, 12, 16] {
                let (_, score, _, _) = engine.new_move(depth);
                let pv = extended_pv(&mut engine);
                let mut board = *engine.search.get_board_mut();

                let mut desc = String::new();
                let mut revisit = None;

                for (index, mv) in pv.iter().enumerate() {
                    assert!(unsafe { board.make_move(*mv, &harness.tables) });
                    let hit = board.zobrist_key() == root_key;
                    desc.push_str(&format!(
                        "{}{} ",
                        util::move_string(*mv),
                        if hit { "*" } else { "" }
                    ));
                    if hit && index + 1 != pv.len() && revisit.is_none() {
                        revisit = Some(index);
                    }
                }

                println!(
                    "{} d{} score {} len {} : {} {}",
                    fen,
                    depth,
                    score,
                    pv.len(),
                    desc,
                    if revisit.is_some() {
                        "<<< ROOT REVISIT"
                    } else {
                        ""
                    }
                );
            }
        }
    }

    #[test]
    fn test_pv_extension_stops_at_root_revisit() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine
            .new_game("4k1n1/8/8/8/8/8/8/4K1N1 w - - 10 1")
            .unwrap();

        engine.new_move(10);

        let root_key = engine.zobrist_key;
        let mut board = *engine.search.get_board_mut();
        let mut cycle = Vec::new();

        for mv in ["g1f3", "g8f6", "f3g1", "f6g8"] {
            let mv = board.fix_move(util::create_move(mv));
            assert!(
                unsafe { board.make_move(mv, &harness.tables) },
                "{} must be legal",
                util::move_string_dbg(mv)
            );
            cycle.push(mv);
        }

        assert_eq!(
            board.zobrist_key(),
            root_key,
            "the shuffle must return to the root position"
        );

        engine.search.get_rt_mut().pop_position();

        let mut extended = cycle.clone();
        engine.search.extend_pv_from_tt(&mut extended);

        engine.search.get_rt_mut().push_hash(root_key);

        assert_eq!(
            extended.len(),
            cycle.len(),
            "PV ran {} ply past a return to the root: {:?}",
            extended.len().saturating_sub(cycle.len()),
            extended
                .iter()
                .map(|mv| util::move_string(*mv))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_pv_never_extends_past_repetition_in_repro_game() {
        const REPLAY_DEPTH: u8 = 14;

        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        let game: Vec<&str> = REPRO_GAME.split_whitespace().collect();
        let warm_from = game.len().saturating_sub(40);

        for (ply, mv) in game.iter().enumerate() {
            if ply >= warm_from {
                engine.new_move(REPLAY_DEPTH);

                let pv = extended_pv(&mut engine);
                let root_key = engine.zobrist_key;
                let rt = engine.search.get_rt();

                let mut history: Vec<u64> = rt.hashes[..rt.cursor].to_vec();
                let mut board = *engine.search.get_board_mut();

                for (index, pv_move) in pv.iter().enumerate() {
                    assert!(
                        unsafe { board.make_move(*pv_move, &harness.tables) },
                        "ply {}: PV move {} is not makeable",
                        ply,
                        index
                    );

                    let key = board.zobrist_key();
                    let window = (board.half_moves() as usize).min(history.len());
                    let repeats =
                        history[history.len() - window..].contains(&key) || key == root_key;

                    assert!(
                        !repeats || index + 1 == pv.len(),
                        "ply {}: PV extends past a repetition at move {}: {:?}",
                        ply,
                        index,
                        pv.iter().map(|m| util::move_string(*m)).collect::<Vec<_>>()
                    );

                    history.push(key);
                }
            }

            play(&mut engine, mv);
        }
    }

    #[test]
    fn test_selfplay_pv_terminates_at_repetition() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        for mv in ["e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "f8c5"] {
            play(&mut engine, mv);
        }

        for _ in 0..4 {
            let (bestmove, _, _, aborted) = engine.new_move(TEST_DEPTH);
            assert!(!aborted);

            let pv = extended_pv(&mut engine);
            assert!(!pv.is_empty());
            assert_eq!(pv[0], bestmove);

            let rt = engine.search.get_rt();
            let mut history: Vec<u64> = rt.hashes[..rt.cursor].to_vec();
            let mut board = *engine.search.get_board_mut();

            for (index, mv) in pv.iter().enumerate() {
                assert!(
                    unsafe { board.make_move(*mv, &harness.tables) },
                    "PV move {} is not makeable",
                    index
                );
                assert!(
                    !board.in_check(&harness.tables, !board.b_move()),
                    "PV move {} leaves the mover in check",
                    index
                );

                let key = board.zobrist_key();
                let window = (board.half_moves() as usize).min(history.len());
                let repeats = history[history.len() - window..].contains(&key);

                assert!(
                    !repeats || index + 1 == pv.len(),
                    "PV extends past a repetition at move {}",
                    index
                );

                history.push(key);
            }

            engine.make_move(bestmove).unwrap();
        }
    }
}
