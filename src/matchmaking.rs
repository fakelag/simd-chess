use std::io::{BufRead, BufReader, Write};

use crate::{
    constant,
    engine::{chess, tables},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EngineState {
    Idle,
    Thinking,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VersusState {
    Idle,
    InProgress,
    Paused,
    Done,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EnginePollResult {
    MoveMade,
    EngineCrash,
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

pub struct EngineProcess {
    pub process: std::process::Child,
    pub state: EngineState,
    pub path: String,
    pub versus_wins: usize,
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

    pub versus_state: VersusState,
    pub versus_matches: usize,
    pub versus_draws: usize,
    pub versus_btime_ms: usize,
    pub versus_wtime_ms: usize,
    pub versus_move_start_time: Option<std::time::Instant>,
}

impl Matchmaking {
    pub fn new(fen: &str) -> anyhow::Result<Self> {
        let mut board = chess::ChessGame::new();

        board
            .load_fen(fen)
            .map_err(|e| anyhow::anyhow!("Failed to load FEN '{}': {}", fen, e))?;

        let mut mm = Self {
            fen: fen.to_string(),
            is_startpos: fen == constant::FEN_STARTPOS,
            board,
            tables: tables::Tables::new(),
            moves: Vec::new(),
            legal_moves: Vec::new(),
            engines: [None, None],
            engine_white: 0,
            engine_command_buf: String::new(),

            versus_state: VersusState::Idle,
            versus_matches: 0,
            versus_draws: 0,
            versus_btime_ms: 0,
            versus_wtime_ms: 0,
            versus_move_start_time: None,
        };

        mm.update_legal_moves();

        Ok(mm)
    }

    pub fn load_fen(&mut self, fen: &str) -> anyhow::Result<()> {
        self.board
            .load_fen(fen)
            .map_err(|e| anyhow::anyhow!("Failed to load FEN '{}': {}", fen, e))?;
        self.fen = fen.to_string();
        self.is_startpos = fen == constant::FEN_STARTPOS;
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
                && !board_copy.in_check_slow(&self.tables, !board_copy.b_move)
            {
                self.legal_moves.push(mv);
            }
        }
    }

    pub fn make_move_with_validation(&mut self, mv: u16) -> anyhow::Result<bool> {
        let mv_string = constant::move_string(mv);

        if self.update_timers() {
            // Force game over due to out of time
            self.versus_check_game_over();
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
        // Poll thinking engine (if any)
        let poll_result = self.uci_poll();

        match poll_result {
            EnginePollResult::MoveMade | EnginePollResult::OutOfTime => {}
            EnginePollResult::EngineCrash => {
                eprintln!("Engine process has crashed, pausing versus mode");
                self.versus_state = VersusState::Paused;
                self.versus_move_start_time = None;
                return;
            }
            EnginePollResult::NoAction => return,
        }

        match self.versus_state {
            VersusState::Idle | VersusState::Paused | VersusState::Done => return,
            VersusState::InProgress => {}
        }

        if poll_result != EnginePollResult::OutOfTime && !self.versus_check_game_over() {
            // Advance game loop
            self.uci_nextmove();
            return;
        }

        if self.versus_matches > 0 {
            self.uci_nextmove();
        }
    }

    fn versus_check_game_over(&mut self) -> bool {
        if self.check_timer() {
            let engine = self
                .get_engine_for_side_mut(constant::Side::from(!self.board.b_move))
                .unwrap();
            engine.versus_wins += 1;
        } else {
            match self.board.check_game_state(
                &self.tables,
                self.legal_moves.is_empty(),
                self.board.b_move,
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

        self.versus_matches -= 1;
        self.reset_timers();

        if self.versus_matches == 0 {
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

        // Reset board and swap sides
        let fen_copy = self.fen.clone();
        self.load_fen(&fen_copy)
            .expect("Failed to reset board after match end");

        debug_assert!(self.engines.iter().all(|e| {
            e.as_ref()
                .and_then(|e| Some(e.state == EngineState::Idle))
                .unwrap_or(false)
        }),);

        self.engine_white = (self.engine_white + 1) % 2;
        return true;
    }

    pub fn versus_start(
        &mut self,
        engine_white: &str,
        engine_black: &str,
        num_matches: usize,
        start_paused: bool,
    ) -> anyhow::Result<()> {
        if self.versus_state != VersusState::Idle {
            panic!("Cannot start versus mode when already in progress");
        }

        self.versus_reset();

        self.engines[0] = Some(self.spawn_engine(engine_white)?);
        self.engines[1] = Some(self.spawn_engine(engine_black)?);
        self.engine_white = 0;

        self.versus_matches = num_matches;

        if start_paused {
            self.versus_state = VersusState::Paused;
        } else {
            self.versus_state = VersusState::InProgress;
            self.uci_nextmove();
        }

        Ok(())
    }

    pub fn versus_pause(&mut self) {
        if self.versus_state != VersusState::InProgress {
            panic!("Cannot pause versus mode when not in progress");
        }

        self.versus_state = VersusState::Paused;
        self.versus_move_start_time = None;
    }

    pub fn versus_resume(&mut self) {
        if self.versus_state != VersusState::Paused {
            panic!("Cannot resume versus mode when not paused");
        }

        self.versus_state = VersusState::InProgress;
        self.uci_nextmove();
    }

    pub fn versus_step(&mut self) {
        if self.versus_check_game_over() {
            return;
        }

        if self.versus_matches > 0 {
            self.uci_nextmove();
        }
    }

    pub fn versus_reset(&mut self) {
        self.versus_state = VersusState::Idle;
        self.versus_matches = 0;
        self.versus_draws = 0;
        self.engines[0] = None;
        self.engines[1] = None;
        self.engine_white = 0;
        self.reset_timers();

        let fen_copy = self.fen.clone();
        self.load_fen(&fen_copy)
            .expect("Failed to reset board after match end");
    }

    pub fn engine_index(&self, side: constant::Side) -> usize {
        self.engine_white ^ side as usize
    }

    pub fn get_engine_for_side(&self, side: constant::Side) -> Option<&EngineProcess> {
        let engine_white_index = self.engine_index(side);
        self.engines[engine_white_index].as_ref()
    }

    pub fn get_engine_for_side_mut(&mut self, side: constant::Side) -> Option<&mut EngineProcess> {
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

    fn check_timer(&mut self) -> bool {
        let side_timer = if self.board.b_move {
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

    fn update_timers(&mut self) -> bool {
        let side_timer = if self.board.b_move {
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
        let engine_index = self.engine_index(constant::Side::from(self.board.b_move));
        let engine = self.engines[engine_index]
            .as_mut()
            .expect("Engine not initialized");

        if engine.state != EngineState::Idle {
            panic!("Engine is already thinking");
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
                "go depth 4 wtime {} btime {}\n",
                self.versus_wtime_ms, self.versus_btime_ms
            )
            .as_str(),
        );

        // Start move timer
        self.versus_move_start_time = Some(std::time::Instant::now());

        engine
            .process
            .stdin
            .as_mut()
            .expect("Failed to open engine stdin")
            .write_all(self.engine_command_buf.as_bytes())
            .expect("Failed to write to engine stdin");

        engine.state = EngineState::Thinking;
    }

    fn uci_poll(&mut self) -> EnginePollResult {
        let engine_index = self.engine_index(constant::Side::from(self.board.b_move));

        let engine = match self.engines[engine_index].as_mut() {
            Some(engine) => engine,
            None => return EnginePollResult::NoAction,
        };

        if engine.state != EngineState::Thinking {
            return EnginePollResult::NoAction;
        }

        if let Ok(exit_code) = engine.process.try_wait() {
            if let Some(code) = exit_code {
                eprintln!("Engine process has exited unexpectedly with code: {}", code);

                for line in BufReader::new(
                    engine
                        .process
                        .stderr
                        .as_mut()
                        .expect("Failed to open engine stderr"),
                )
                .lines()
                {
                    println!("Engine stderr: {}", line.unwrap());
                }

                engine.state = EngineState::Idle;
                return EnginePollResult::EngineCrash;
            }
        } else {
            return EnginePollResult::NoAction;
        }

        let stdout = engine
            .process
            .stdout
            .as_mut()
            .expect("Failed to open engine stdout");

        for line in BufReader::new(stdout).lines() {
            match line {
                Ok(line) => {
                    let mut parts = line.split_whitespace();

                    if parts.next() != Some("bestmove") {
                        println!("Engine stdout: {}", line);
                        continue;
                    }

                    let bestmove = match parts.next() {
                        Some(mv) => mv,
                        None => {
                            eprintln!("No bestmove found in engine output: {}", line);
                            continue;
                        }
                    };

                    engine.state = EngineState::Idle;

                    let mv = constant::fix_move(&self.board, constant::create_move(bestmove));

                    match self.make_move_with_validation(mv) {
                        Ok(true) => return EnginePollResult::MoveMade,
                        Ok(false) => return EnginePollResult::OutOfTime,
                        Err(e) => {
                            panic!("Failed to make move {}: {}", bestmove, e);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error reading from engine stdout: {}", e);
                    break;
                }
            }
        }

        EnginePollResult::NoAction
    }

    fn reset_timers(&mut self) {
        self.versus_wtime_ms = 10 * 60 * 1000;
        self.versus_btime_ms = 10 * 60 * 1000;
        self.versus_move_start_time = None;
    }

    fn kill_engines(&mut self) {
        for engine in self.engines.iter_mut().filter_map(|e| e.as_mut()) {
            if engine.process.kill().is_err() {
                eprintln!("Failed to kill engine process");
            }
            engine.state = EngineState::Idle;
        }

        self.engines = [None, None];
    }

    fn spawn_engine(&self, engine_path: &str) -> anyhow::Result<EngineProcess> {
        let mut child_process = std::process::Command::new(format!("bin/{}", engine_path))
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .expect("Failed to start engine chess process");

        child_process
            .stdin
            .as_mut()
            .expect("Failed to open engine stdin")
            .write_all("uci\n".as_bytes())
            .expect("Failed to write to engine stdin");

        'outer: loop {
            let stdout = child_process
                .stdout
                .as_mut()
                .expect("Failed to open engine stdout");

            for line in BufReader::new(stdout).lines() {
                match line {
                    Ok(line) => {
                        if line == "uciok" {
                            break 'outer;
                        } else {
                            println!("Ignored line: {}", line);
                        }
                    }
                    Err(e) => {
                        return Err(anyhow::anyhow!("Error reading from engine stdout: {}", e));
                    }
                }
            }
        }

        Ok(EngineProcess {
            process: child_process,
            state: EngineState::Idle,
            path: engine_path.to_string(),
            versus_wins: 0,
        })
    }
}
