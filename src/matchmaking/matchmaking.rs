use crate::matchmaking::openings::OpeningBook;
use crate::matchmaking::openings::OpeningMoves;
use crate::matchmaking::openings::load_openings_from_dir;
use crate::{
    engine::{chess, tables},
    matchmaking::process::{EngineProcess, EngineState},
    util::{self},
};

pub const NEXT_MATCH_DELAY_SECONDS: u64 = 1;

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
}

pub struct VersusStats {
    pub engine1_name: String,
    pub engine2_name: String,
    pub engine1_wins: usize,
    pub engine2_wins: usize,
    pub draws: usize,
}

pub struct Matchmaking {
    pub fen: String,
    pub board: chess::ChessGame,
    pub tables: tables::Tables,
    pub legal_moves: Vec<u16>,
    moves: Vec<String>,
    engines: [Option<EngineProcess>; 2],
    engine_white: usize,
    is_startpos: bool,
    engine_command_buf: String,

    opening_book: OpeningBook,

    pub versus_current_opening: Option<OpeningMoves>,

    pub versus_state: VersusState,
    pub versus_matches_left: usize,
    pub versus_draws: usize,
    pub versus_btime_ms: usize,
    pub versus_wtime_ms: usize,
    pub versus_move_start_time: Option<std::time::Instant>,
    pub versus_opening_book_after_matches: Option<u8>,
}

impl Matchmaking {
    pub fn new(fen: &str) -> anyhow::Result<Self> {
        let mut mm = Self {
            fen: fen.to_string(),
            is_startpos: fen == util::FEN_STARTPOS,
            board: chess::ChessGame::new(),
            tables: tables::Tables::new(),
            moves: Vec::new(),
            legal_moves: Vec::new(),
            engines: [None, None],
            engine_white: 0,
            engine_command_buf: String::new(),
            opening_book: OpeningBook::new(Vec::new()),
            versus_current_opening: None,

            versus_state: VersusState::Idle,
            versus_matches_left: 0,
            versus_draws: 0,
            versus_btime_ms: 0,
            versus_wtime_ms: 0,
            versus_move_start_time: None,
            versus_opening_book_after_matches: None,
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
        self.update_legal_moves();
        Ok(())
    }

    pub fn update_legal_moves(&mut self) {
        self.legal_moves.clear();

        let mut move_list = [0u16; 256];
        let move_count = self.board.gen_moves_slow(&self.tables, &mut move_list);

        for i in 0..move_count {
            let mv = move_list[i];

            let mut board_copy = self.board.clone();

            if board_copy.make_move_slow(mv, &self.tables)
                && !board_copy.in_check_slow(&self.tables, !board_copy.b_move())
            {
                self.legal_moves.push(mv);
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

        if !self.board.make_move_slow(mv, &self.tables) {
            // Should not happen unless legal_moves is out of sync
            panic!("Failed to make move {}: Invalid move", mv_string);
        }

        self.moves.push(mv_string);
        self.update_legal_moves();

        Ok(true)
    }

    pub fn poll(&mut self) {
        match self.versus_state {
            VersusState::NextMatch(start_time, start_paused) => {
                if start_time.elapsed().as_secs() >= NEXT_MATCH_DELAY_SECONDS {
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

                return;
            }
            _ => {}
        }

        // Poll thinking engine (if any)
        let poll_result = self.uci_poll();

        match poll_result {
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
        if self.check_versus_timer() {
            let engine = self
                .get_engine_for_side_mut(util::Side::from(!self.board.b_move()))
                .unwrap();
            engine.versus_wins += 1;
        } else {
            match self.board.check_game_state(
                &self.tables,
                self.legal_moves.is_empty(),
                self.board.b_move(),
            ) {
                chess::GameState::Checkmate(side) => {
                    let engine = self.get_engine_for_side_mut(side).unwrap();
                    engine.versus_wins += 1;
                }
                chess::GameState::Stalemate => {
                    self.versus_draws += 1;
                }
                chess::GameState::DrawByFiftyMoveRule => {
                    self.versus_draws += 1;
                }
                chess::GameState::Ongoing => {
                    return false;
                }
            }
        }

        self.versus_matches_left -= 1;
        self.reset_timers();

        if self.versus_matches_left == 0 {
            self.versus_state = VersusState::Done;
            let engine1 = self.engines[0].as_ref().unwrap();
            let engine2 = self.engines[1].as_ref().unwrap();

            println!(
                "Versus match ended: {} {} wins, {} {} wins, {} draws",
                engine1.versus_wins,
                engine1.path,
                engine2.versus_wins,
                engine2.path,
                self.versus_draws
            );
            return true;
        }

        self.versus_state = VersusState::NextMatch(std::time::Instant::now(), start_paused);
        return true;
    }

    pub fn versus_start(
        &mut self,
        engine_white: &str,
        engine_black: &str,
        num_matches: usize,
        start_paused: bool,
        use_opening_book: bool,
    ) -> anyhow::Result<()> {
        if self.versus_state != VersusState::Idle {
            panic!("Cannot start versus mode when already in progress");
        }

        self.versus_reset();

        self.engines[0] = Some(EngineProcess::new(engine_white)?);
        self.engines[1] = Some(EngineProcess::new(engine_black)?);
        self.engine_white = 0;
        self.versus_opening_book_after_matches = if use_opening_book && self.is_startpos {
            // Use openings after playing 2 games from startpos
            Some(2)
        } else {
            None
        };
        self.versus_matches_left = num_matches;

        self.load_openings()?;

        if !self.on_new_match() {
            self.versus_matches_left = 0;
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
        self.versus_opening_book_after_matches = None;
        self.versus_current_opening = None;
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
        let engine1 = self.engines[0].as_ref().unwrap();
        let engine2 = self.engines[1].as_ref().unwrap();

        VersusStats {
            engine1_name: engine1.path.clone(),
            engine2_name: engine2.path.clone(),
            engine1_wins: engine1.versus_wins,
            engine2_wins: engine2.versus_wins,
            draws: self.versus_draws,
        }
    }

    fn load_openings(&mut self) -> anyhow::Result<()> {
        if self.is_startpos {
            let mut opening_list = Vec::new();
            load_openings_from_dir(&self.board, &self.tables, &mut opening_list)?;

            self.opening_book = OpeningBook::new(opening_list);
        } else {
            println!(
                "Not loading openings for a non-startpos FEN \"{}\"",
                self.fen
            );
            self.opening_book = OpeningBook::new(Vec::new());
        }

        Ok(())
    }

    fn on_new_match(&mut self) -> bool {
        let stats = self.versus_stats();
        let match_num = stats.draws + stats.engine1_wins + stats.engine2_wins + 1;
        println!(
            "Starting match {}: {} {} wins, {} {} wins, draws: {}",
            match_num,
            stats.engine1_name,
            stats.engine1_wins,
            stats.engine2_name,
            stats.engine2_wins,
            stats.draws
        );

        match self.versus_opening_book_after_matches {
            Some(0) => {}
            Some(matches_left) => {
                self.versus_opening_book_after_matches = Some(matches_left - 1);
                return true;
            }
            None => {
                self.versus_current_opening = None;
                return true;
            }
        }

        if self.versus_current_opening.is_none() || self.versus_matches_left % 2 == 0 {
            let next_opening = self.opening_book.next();

            match next_opening {
                Some(opening) => {
                    println!("Using opening: {}", opening.name);
                    self.versus_current_opening = Some(opening);
                }
                None => {
                    println!("Opening book exhausted, signaling versus mode to end");
                    return false;
                }
            }
        }

        let opening = self.versus_current_opening.as_ref().unwrap().clone();

        for mv in &opening.moves {
            if self.make_move_with_validation(*mv).is_err() {
                panic!(
                    "Failed to make move {} from opening book",
                    util::move_string(*mv)
                );
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
                "go wtime {} btime {}\n",
                self.versus_wtime_ms, self.versus_btime_ms
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
