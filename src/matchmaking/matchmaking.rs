use std::collections::VecDeque;

use crate::{
    engine::{self, chess_v2, tables},
    matchmaking::process::{EngineProcess, EngineState},
    util::{self},
};

pub const NEXT_MATCH_DELAY_SECONDS: u64 = 1;

pub trait PositionFeeder {
    fn next_position(&mut self) -> Option<String>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VersusState {
    Idle,
    InProgress,
    Paused,
    NextMatch(std::time::Instant, bool),
    Done,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EnginePollResult {
    MoveMade,
    NoAction,
    OutOfTime,
    EngineShutdown(i32, usize), // exit_code, engine_index
}

#[derive(Debug, Copy, Clone)]
pub enum GameResult {
    GameState(chess_v2::GameState),
    OutOfTime(util::Side),
    ThreeFoldRepetition,
}

#[derive(Debug, Clone)]
pub struct VersusStats {
    pub engine1_name: String,
    pub engine2_name: String,
    pub engine1_wins: usize,
    pub engine2_wins: usize,
    pub draws: usize,
    pub num_matches: usize,
    pub last_game_status: Option<GameResult>,
}

pub struct VersusMatch {
    pub stats: VersusStats,
    pub start_paused: bool,
    pub feeder: Option<Box<dyn PositionFeeder>>,
    pub is_ongoing: bool,
}

pub struct Matchmaking {
    pub fen: String,
    pub board: chess_v2::ChessGame,
    pub tables: tables::Tables,
    pub legal_moves: Vec<u16>,

    pub moves_u16: Vec<u16>,
    pub moves: Vec<String>,
    engines: [Option<EngineProcess>; 2],
    engine_white: usize,
    is_startpos: bool,
    engine_command_buf: String,

    pub versus_state: VersusState,
    pub versus_matches_left: usize,
    pub versus_draws: usize,
    pub versus_btime_ms: usize,
    pub versus_wtime_ms: usize,
    pub versus_move_start_time: Option<std::time::Instant>,
    pub versus_autostart: bool,
    pub versus_logging: bool,
    pub versus_3fr: Option<engine::search::repetition_v2::RepetitionTable>,

    pub versus_queue: VecDeque<VersusMatch>,
    pub versus_matches: VecDeque<VersusMatch>,
}

impl Matchmaking {
    pub fn new(fen: &str) -> anyhow::Result<Self> {
        let mut mm = Self {
            fen: fen.to_string(),
            is_startpos: fen == util::FEN_STARTPOS,
            board: chess_v2::ChessGame::new(),
            tables: tables::Tables::new(),
            moves: Vec::new(),
            moves_u16: Vec::new(),
            legal_moves: Vec::new(),
            engines: [None, None],
            engine_white: 0,
            engine_command_buf: String::new(),

            versus_state: VersusState::Idle,
            versus_matches_left: 0,
            versus_draws: 0,
            versus_btime_ms: 0,
            versus_wtime_ms: 0,
            versus_move_start_time: None,
            versus_autostart: true,
            versus_logging: true,
            versus_3fr: None,

            versus_queue: VecDeque::new(),
            versus_matches: VecDeque::new(),
        };

        mm.load_fen(fen)?;

        Ok(mm)
    }

    pub fn load_fen(&mut self, fen: &str) -> anyhow::Result<()> {
        self.board
            .load_fen(fen, &self.tables)
            .map_err(|e| anyhow::anyhow!("Failed to load FEN '{}': {}", fen, e))?;
        self.fen = fen.to_string();
        self.is_startpos = fen == util::FEN_STARTPOS;
        self.moves.clear();
        self.moves_u16.clear();
        self.update_legal_moves();

        if let Some(repetition_table) = &mut self.versus_3fr {
            repetition_table.clear();
            repetition_table.push_position(self.board.zobrist_key(), true);
        }

        Ok(())
    }

    pub fn update_legal_moves(&mut self) {
        self.legal_moves.clear();

        let mut move_list = [0u16; 256];
        let move_count = self
            .board
            .gen_moves_avx512::<false>(&self.tables, &mut move_list);

        for i in 0..move_count {
            let mv = move_list[i];

            let mut board_copy = self.board.clone();

            let move_ok = unsafe { board_copy.make_move(mv, &self.tables, None) };

            if move_ok && !board_copy.in_check(&self.tables, !board_copy.b_move()) {
                self.legal_moves.push(mv);

                if (mv & chess_v2::MV_FLAGS_PR_MASK) == chess_v2::MV_FLAGS_PR_QUEEN {
                    // Add underpromotions as legal moves too
                    let mv_unpromoted = mv & !chess_v2::MV_FLAGS_PR_MASK;
                    self.legal_moves
                        .push(mv_unpromoted | chess_v2::MV_FLAGS_PR_ROOK);
                    self.legal_moves
                        .push(mv_unpromoted | chess_v2::MV_FLAGS_PR_BISHOP);
                    self.legal_moves
                        .push(mv_unpromoted | chess_v2::MV_FLAGS_PR_KNIGHT);
                }
            }
        }
    }

    pub fn make_move_with_validation(&mut self, mv: u16) -> anyhow::Result<bool> {
        let mv_string = util::move_string(mv);

        if self.update_versus_timers() {
            // Force game over due to out of time
            self.versus_check_game_over(self.versus_state == VersusState::Paused);
            return Ok(false);
        }

        let is_legal_move = self.legal_moves.contains(&mv);

        if !is_legal_move {
            return Err(anyhow::anyhow!("Move {} is not a legal move", mv_string));
        }

        if unsafe { !self.board.make_move(mv, &self.tables, None) } {
            // Should not happen unless legal_moves is out of sync
            panic!("Failed to make move {}: Invalid move", mv_string);
        }

        self.moves.push(mv_string);
        self.moves_u16.push(mv);
        self.update_legal_moves();

        if let Some(repetition_table) = &mut self.versus_3fr {
            repetition_table.push_position(self.board.zobrist_key(), self.board.half_moves() == 0);
        }

        Ok(true)
    }

    pub fn poll(&mut self) {
        match self.versus_state {
            VersusState::NextMatch(start_time, start_paused) => {
                if self.versus_autostart
                    && start_time.elapsed().as_secs() >= NEXT_MATCH_DELAY_SECONDS
                {
                    self.versus_nextgame(start_paused);
                }

                return;
            }
            _ => {}
        }

        // Poll thinking engine (if any)
        let poll_result = self.uci_poll();

        match poll_result {
            EnginePollResult::EngineShutdown(exit_code, engine_index) => {
                let engine = match self.engines[engine_index].as_mut() {
                    Some(engine) => engine,
                    None => {
                        eprintln!("Engine at index {} is not initialized", engine_index);
                        return;
                    }
                };
                eprintln!(
                    "[{}] Engine exited unexpectedly with code {}",
                    engine.path, exit_code,
                );

                self.redeploy_engines()
                    .expect("Failed to redeploy engines after crash");

                println!("Moving to next match if available...");
                self.versus_state = VersusState::NextMatch(
                    std::time::Instant::now(),
                    self.versus_state == VersusState::Paused,
                );
            }
            EnginePollResult::MoveMade | EnginePollResult::OutOfTime => {}
            EnginePollResult::NoAction => return,
        }

        match self.versus_state {
            VersusState::Idle
            | VersusState::Paused
            | VersusState::Done
            | VersusState::NextMatch(_, _) => return,
            VersusState::InProgress => {}
        }

        if poll_result != EnginePollResult::OutOfTime
            && !self.versus_check_game_over(self.versus_state == VersusState::Paused)
        {
            // Advance game loop
            self.uci_nextmove();
            return;
        }
    }

    fn versus_check_game_over(&mut self, start_paused: bool) -> bool {
        let game_end_status = if self.check_versus_timer() {
            let engine = self
                .get_engine_for_side_mut(util::Side::from(!self.board.b_move()))
                .unwrap();
            engine.versus_wins += 1;
            Some(GameResult::OutOfTime(util::Side::from(self.board.b_move())))
        } else {
            let is_3fr = if let Some(repetition_table) = &mut self.versus_3fr {
                repetition_table.is_repeated_times(self.board.zobrist_key()) >= 3
            } else {
                false
            };

            if is_3fr {
                self.versus_draws += 1;
                Some(GameResult::ThreeFoldRepetition)
            } else {
                let game_state = self.board.check_game_state(
                    &self.tables,
                    self.legal_moves.is_empty(),
                    self.board.b_move(),
                );

                match game_state {
                    chess_v2::GameState::Checkmate(side) => {
                        let engine = self.get_engine_for_side_mut(side).unwrap();
                        engine.versus_wins += 1;
                    }
                    chess_v2::GameState::Stalemate => {
                        self.versus_draws += 1;
                    }
                    chess_v2::GameState::DrawByFiftyMoveRule => {
                        self.versus_draws += 1;
                    }
                    chess_v2::GameState::Ongoing => {
                        return false;
                    }
                }

                Some(GameResult::GameState(game_state))
            }
        };

        self.reset_timers();
        self.versus_matches_left -= 1;

        let engine1 = self.engines[0].as_ref().unwrap();
        let engine2 = self.engines[1].as_ref().unwrap();

        let current_match = self.versus_matches.front_mut().unwrap();
        current_match.stats.engine1_wins = engine1.versus_wins;
        current_match.stats.engine2_wins = engine2.versus_wins;
        current_match.stats.draws = self.versus_draws;
        current_match.stats.last_game_status = game_end_status;

        if self.versus_matches_left > 0 {
            self.versus_state = VersusState::NextMatch(std::time::Instant::now(), start_paused);
            return true;
        }

        self.versus_state = VersusState::Done;
        println!(
            "Versus match ended: {} {} wins, {} {} wins, {} draws",
            engine1.versus_wins, engine1.path, engine2.versus_wins, engine2.path, self.versus_draws
        );

        self.next_versus().unwrap();

        return true;
    }

    pub fn versus_start(
        &mut self,
        engine_white: &str,
        engine_black: &str,
        num_matches: usize,
        start_paused: bool,
        feeder: Option<Box<dyn PositionFeeder>>,
    ) -> anyhow::Result<()> {
        self.versus_queue.push_back(VersusMatch {
            stats: VersusStats {
                engine1_name: engine_white.to_string(),
                engine2_name: engine_black.to_string(),
                engine1_wins: 0,
                engine2_wins: 0,
                draws: 0,
                num_matches,
                last_game_status: None,
            },
            feeder,
            start_paused,
            is_ongoing: false,
        });

        if self.versus_state == VersusState::Idle || self.versus_state == VersusState::Done {
            self.next_versus()?;
        } else {
            println!("Versus match {} vs {} queued", engine_white, engine_black);
        }

        Ok(())
    }

    pub fn set_go_params(&mut self, params: &str) {
        self.engines[0].as_mut().unwrap().go_params = params.to_string();
        self.engines[1].as_mut().unwrap().go_params = params.to_string();
    }

    pub fn next_versus(&mut self) -> anyhow::Result<()> {
        let mut next_versus = match self.versus_queue.pop_front() {
            Some(v) => v,
            None => {
                self.versus_matches.front_mut().and_then(|m| {
                    m.is_ongoing = false;
                    Some(m)
                });
                println!("No more matches in the queue");
                return Ok(());
            }
        };

        self.versus_reset();

        self.engines[0] = Some(EngineProcess::new(&next_versus.stats.engine1_name)?);
        self.engines[1] = Some(EngineProcess::new(&next_versus.stats.engine2_name)?);

        self.set_go_params("movetime 100");

        self.engine_white = 0;
        self.versus_matches_left = next_versus.stats.num_matches;

        let start_paused = next_versus.start_paused;
        next_versus.is_ongoing = true;
        self.versus_matches.front_mut().and_then(|m| {
            m.is_ongoing = false;
            Some(m)
        });
        self.versus_matches.push_front(next_versus);

        if !self.on_new_match() {
            self.versus_matches_left = 0;
            self.versus_matches[0].is_ongoing = false;
            self.versus_state = VersusState::Done;
        } else {
            if start_paused {
                self.versus_state = VersusState::Paused;
            } else {
                self.versus_state = VersusState::InProgress;
                self.uci_nextmove();
            }
        }

        Ok(())
    }

    pub fn versus_nextgame(&mut self, start_paused: bool) {
        // Reset board and swap sides
        let fen_copy = self.fen.clone();
        self.load_fen(&fen_copy)
            .expect("Failed to reset board after match end");
        self.engine_white = (self.engine_white + 1) % 2;
        self.versus_state = if start_paused {
            VersusState::Paused
        } else {
            VersusState::InProgress
        };

        if self.versus_matches_left > 0 {
            if self.on_new_match() {
                if self.versus_state == VersusState::InProgress {
                    self.uci_nextmove();
                }
            } else {
                self.versus_matches_left = 0;
                self.versus_state = VersusState::Done;
            }
        } else {
            eprintln!("No matches left, ending versus mode");
            self.versus_state = VersusState::Done;
        }
    }

    pub fn redeploy_engines(&mut self) -> anyhow::Result<()> {
        for engine in &mut self.engines {
            if let Some(engine_process) = engine {
                engine_process.redeploy()?;
            }
        }

        Ok(())
    }

    pub fn versus_pause(&mut self) {
        if self.versus_state != VersusState::InProgress {
            panic!("Cannot pause versus mode when not in progress");
        }

        self.redeploy_engines()
            .expect("Failed to redeploy engines during pause");

        self.versus_move_start_time = None;

        if self.versus_check_game_over(true) {
            return;
        }

        self.versus_state = VersusState::Paused;
    }

    pub fn versus_resume(&mut self) {
        if self.versus_state != VersusState::Paused {
            panic!("Cannot resume versus mode when not paused");
        }

        self.redeploy_engines()
            .expect("Failed to redeploy engines during pause");

        self.update_versus_timers();
        if self.versus_check_game_over(false) {
            return;
        }

        self.versus_state = VersusState::InProgress;
        self.uci_nextmove();
    }

    pub fn versus_step(&mut self) {
        self.update_versus_timers();
        if self.versus_check_game_over(self.versus_state == VersusState::Paused) {
            return;
        }

        if self.versus_matches_left > 0 {
            self.uci_nextmove();
        }
    }

    pub fn versus_reset(&mut self) {
        self.versus_state = VersusState::Idle;
        self.versus_matches_left = 0;
        self.versus_draws = 0;
        self.engines[0] = None;
        self.engines[1] = None;
        self.engine_white = 0;
        self.reset_timers();

        let fen_copy = self.fen.clone();
        self.load_fen(&fen_copy)
            .expect("Failed to reset board after match end");
    }

    pub fn engine_index(&self, side: util::Side) -> usize {
        self.engine_white ^ side as usize
    }

    pub fn get_engine_for_side(&self, side: util::Side) -> Option<&EngineProcess> {
        let engine_white_index = self.engine_index(side);
        self.engines[engine_white_index].as_ref()
    }

    pub fn get_engine_for_side_mut(&mut self, side: util::Side) -> Option<&mut EngineProcess> {
        let engine_white_index = self.engine_index(side);
        self.engines[engine_white_index].as_mut()
    }

    pub fn versus_stats(&self) -> VersusStats {
        self.versus_matches.front().map_or(
            VersusStats {
                engine1_name: "<none>".to_string(),
                engine2_name: "<none>".to_string(),
                engine1_wins: 0,
                engine2_wins: 0,
                draws: 0,
                num_matches: 0,
                last_game_status: None,
            },
            |match_info| match_info.stats.clone(),
        )
    }

    fn on_new_match(&mut self) -> bool {
        let current_match = self.versus_matches.front_mut().unwrap();

        if self.versus_logging {
            println!(
                "Starting a new match match between {} ({}) vs {} ({})",
                current_match.stats.engine1_name,
                current_match.stats.engine1_wins,
                current_match.stats.engine2_name,
                current_match.stats.engine2_wins
            );
        }

        let feeder = match &mut current_match.feeder {
            Some(feeder) => feeder,
            None => return true,
        };

        let next_position = match feeder.next_position() {
            Some(fen) => fen,
            None => {
                println!("Feeder has no more positions, ending versus mode");
                return false;
            }
        };

        match self.load_fen(&next_position) {
            Ok(()) => {}
            Err(e) => {
                println!(
                    "Failed to load position from feeder: {}, ending versus mode",
                    e
                );
                return false;
            }
        }

        true
    }

    fn check_versus_timer(&mut self) -> bool {
        if self.versus_state == VersusState::Idle {
            return false;
        }

        let side_timer = if self.board.b_move() {
            self.versus_btime_ms
        } else {
            self.versus_wtime_ms
        };

        let elapsed_ms = if let Some(start_time) = &mut self.versus_move_start_time {
            start_time.elapsed().as_millis() as usize
        } else {
            0
        };

        side_timer.saturating_sub(elapsed_ms) == 0
    }

    fn update_versus_timers(&mut self) -> bool {
        if self.versus_state == VersusState::Idle {
            return false;
        }

        let side_timer = if self.board.b_move() {
            &mut self.versus_btime_ms
        } else {
            &mut self.versus_wtime_ms
        };

        if let Some(start_time) = &mut self.versus_move_start_time {
            let elapsed_ms = start_time.elapsed().as_millis();
            *side_timer = side_timer.saturating_sub(elapsed_ms as usize);
        }

        self.versus_move_start_time = None;

        *side_timer == 0
    }

    fn uci_nextmove(&mut self) {
        let engine_index = self.engine_index(util::Side::from(self.board.b_move()));

        let engine = self.engines[engine_index]
            .as_mut()
            .expect("Engine not initialized");

        while engine.get_state() != EngineState::ReadyOk {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        self.engine_command_buf.clear();

        if self.is_startpos {
            self.engine_command_buf.push_str("position startpos");
        } else {
            self.engine_command_buf
                .push_str(&format!("position fen {}", self.fen));
        }

        if !self.moves.is_empty() {
            self.engine_command_buf.push_str(" moves");
        }

        for mv_string in &self.moves {
            self.engine_command_buf.push_str(&format!(" {}", mv_string));
        }

        self.engine_command_buf.push_str("\n");

        self.engine_command_buf.push_str(
            format!(
                "go wtime {} btime {} {}\n",
                self.versus_wtime_ms, self.versus_btime_ms, engine.go_params
            )
            .as_str(),
        );

        // Start move timer
        self.versus_move_start_time = Some(std::time::Instant::now());

        engine
            .send_position_and_go(&self.engine_command_buf)
            .expect("Failed to send position and go command to engine");
    }

    fn uci_poll(&mut self) -> EnginePollResult {
        let engine_index = self.engine_index(util::Side::from(self.board.b_move()));

        let engine = match self.engines[engine_index].as_mut() {
            Some(engine) => engine,
            None => return EnginePollResult::NoAction,
        };

        match engine.get_state() {
            EngineState::Shutdown => {
                return EnginePollResult::EngineShutdown(0, engine_index);
            }
            EngineState::Exited(code) => {
                return EnginePollResult::EngineShutdown(code, engine_index);
            }
            EngineState::Thinking | EngineState::CheckUci | EngineState::WaitReadyOk => {
                return EnginePollResult::NoAction;
            }
            _ => {}
        }

        let bestmove = if let Some(bestmove) = engine.take_bestmove() {
            bestmove
        } else {
            return EnginePollResult::NoAction;
        };

        let mv = self.board.fix_move(util::create_move(&bestmove));

        match self.make_move_with_validation(mv) {
            Ok(true) => return EnginePollResult::MoveMade,
            Ok(false) => return EnginePollResult::OutOfTime,
            Err(e) => {
                panic!("Failed to make move {}: {}", bestmove, e);
            }
        }
    }

    fn reset_timers(&mut self) {
        self.versus_wtime_ms = 10 * 60 * 1000;
        self.versus_btime_ms = 10 * 60 * 1000;
        self.versus_move_start_time = None;
    }
}
