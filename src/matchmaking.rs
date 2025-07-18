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

pub struct EngineProcess {
    pub process: std::process::Child,
    pub state: EngineState,
    pub path: String,
}

pub struct Matchmaking {
    pub fen: String,
    pub board: chess::Board,
    pub tables: tables::Tables,
    pub legal_moves: Vec<u16>,
    moves: Vec<String>,
    engine_black: Option<EngineProcess>,
    engine_white: Option<EngineProcess>,
    is_startpos: bool,
}

impl Matchmaking {
    pub fn new(fen: &str) -> anyhow::Result<Self> {
        let mut board = chess::Board::new();

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
            engine_black: None,
            engine_white: None,
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

    pub fn make_move_with_validation(&mut self, mv: u16) -> anyhow::Result<()> {
        let mv_string = constant::move_string(mv);
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

        // Check win/draw conditions
        if self.legal_moves.is_empty() {
            if self.board.in_check_slow(&self.tables, self.board.b_move) {
                eprintln!("Checkmate!");
            } else {
                eprintln!("Stalemate!");
            }
        } else if self.board.half_moves >= 100 {
            eprintln!("Draw by fifty-move rule!");
        }

        Ok(())
    }

    pub fn respawn_engines(&mut self, black: &str, white: &str) -> anyhow::Result<()> {
        for engine in self
            .engine_white
            .iter_mut()
            .chain(self.engine_black.iter_mut())
        {
            if engine.process.kill().is_err() {
                eprintln!("Failed to kill engine process");
            }
            engine.state = EngineState::Idle;
        }

        self.engine_black = Some(self.spawn_engine(black)?);
        self.engine_white = Some(self.spawn_engine(white)?);
        Ok(())
    }

    pub fn uci_nextmove(&mut self) {
        let engine = match self.board.b_move {
            true => &mut self.engine_black.as_mut().unwrap(),
            false => &mut self.engine_white.as_mut().unwrap(),
        };

        if engine.state != EngineState::Idle {
            panic!("Engine is already thinking");
        }

        let position_string = if self.is_startpos {
            format!(
                "position startpos moves {}\ngo depth 5\n",
                self.moves.join(" ")
            )
        } else {
            format!(
                "position fen {} moves {}\ngo depth 5\n",
                self.fen,
                self.moves.join(" ")
            )
        };

        engine
            .process
            .stdin
            .as_mut()
            .expect("Failed to open engine stdin")
            .write_all(position_string.as_bytes())
            .expect("Failed to write to engine stdin");

        engine.state = EngineState::Thinking;
    }

    pub fn uci_query(&mut self) -> bool {
        let engine = match self.board.b_move {
            true => &mut self.engine_black.as_mut(),
            false => &mut self.engine_white.as_mut(),
        };

        let engine = match engine {
            Some(p) => p,
            None => return false,
        };

        if engine.state != EngineState::Thinking {
            return false;
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
                return false;
            }
        } else {
            eprintln!("Failed to check engine process status");
            return false;
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
                        println!("Unexpected line from engine process: {}", line);
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

                    if let Err(err) = self.make_move_with_validation(mv) {
                        panic!("Invalid move from engine: {}: {}", bestmove, err);
                    }
                    return true;
                }
                Err(e) => {
                    eprintln!("Error reading from engine stdout: {}", e);
                    break;
                }
            }
        }

        false
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

        let stdout = child_process
            .stdout
            .as_mut()
            .expect("Failed to open engine stdout");

        for line in BufReader::new(stdout).lines() {
            match line {
                Ok(line) => {
                    if line == "uciok" {
                        break;
                    } else {
                        println!("Ignored line: {}", line);
                    }
                }
                Err(e) => {
                    return Err(anyhow::anyhow!("Error reading from engine stdout: {}", e));
                }
            }
        }

        Ok(EngineProcess {
            process: child_process,
            state: EngineState::Idle,
            path: engine_path.to_string(),
        })
    }
}
