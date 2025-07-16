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
    moves: Vec<String>,
    engine_black: Option<EngineProcess>,
    engine_white: Option<EngineProcess>,
}

impl Matchmaking {
    pub fn new(fen: &str) -> anyhow::Result<Self> {
        let mut board = chess::Board::new();

        board
            .load_fen(fen)
            .map_err(|e| anyhow::anyhow!("Failed to load FEN '{}': {}", fen, e))?;

        Ok(Self {
            fen: fen.to_string(),
            board,
            tables: tables::Tables::new(),
            moves: Vec::new(),
            engine_black: None,
            engine_white: None,
        })
    }

    pub fn load_fen(&mut self, fen: &str) -> anyhow::Result<()> {
        self.board
            .load_fen(fen)
            .map_err(|e| anyhow::anyhow!("Failed to load FEN '{}': {}", fen, e))?;
        self.fen = fen.to_string();
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

        let position_string = format!(
            "position startpos moves {}\ngo depth 5\n",
            self.moves.join(" ")
        );

        engine
            .process
            .stdin
            .as_mut()
            .expect("Failed to open opponent stdin")
            .write_all(position_string.as_bytes())
            .expect("Failed to write to opponent stdin");

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
                    if let Some(bestmove) = line.strip_prefix("bestmove ") {
                        println!("Bestmove: {}", bestmove);

                        let mv = constant::create_move(bestmove);

                        if !self.board.make_move_slow(mv, &self.tables) {
                            panic!("Invalid move from engine: {}", bestmove);
                        }

                        self.moves.push(bestmove.to_string());

                        engine.state = EngineState::Idle;
                        return true;
                    } else {
                        println!("Unexpected line from engine process: {}", line);
                    }
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
            .arg("uci")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .expect("Failed to start opponent chess process");

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
