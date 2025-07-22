use std::io::{BufRead, BufReader, Write};
use std::process::ExitStatus;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EngineState {
    CheckUci,
    WaitReadyOk,
    ReadyOk,
    Thinking,
    Shutdown,
}

struct EngineInternal {
    name: String,
    process: std::process::Child,
    state: EngineState,
    bestmove: Option<String>,
}

pub struct EngineProcess {
    pub path: String,
    pub versus_wins: usize,

    read_handle: Option<std::thread::JoinHandle<()>>,
    int: std::sync::Arc<std::sync::Mutex<EngineInternal>>,
}

impl EngineInternal {
    fn process_line(&mut self, line: &str) -> anyhow::Result<()> {
        let mut parts = line.split_whitespace();

        match (self.state, parts.next()) {
            (EngineState::CheckUci, Some("uciok")) => {
                self.state = EngineState::WaitReadyOk;
                self.write_stdin("debug on\nisready\n")?;
            }
            (EngineState::WaitReadyOk, Some("readyok")) => {
                self.state = EngineState::ReadyOk;
            }
            (EngineState::Thinking, Some("bestmove")) => {
                let bestmove = match parts.next() {
                    Some(mv) => mv,
                    None => {
                        return Err(anyhow::anyhow!("No bestmove found in engine output"));
                    }
                };

                self.state = EngineState::ReadyOk;
                self.bestmove = Some(bestmove.to_string());
            }
            _ => {
                eprintln!("[{}] stdout: {}", self.name, line);
            }
        }

        Ok(())
    }

    fn wait(&mut self) -> Result<ExitStatus, std::io::Error> {
        self.process.wait()
    }

    fn write_stdin(&mut self, buf: &str) -> anyhow::Result<()> {
        self.process
            .stdin
            .as_mut()
            .expect("Failed to open engine stdin")
            .write_all(buf.as_bytes())?;

        Ok(())
    }
}

impl EngineProcess {
    pub fn new(path: &str) -> anyhow::Result<Self> {
        let child_process = std::process::Command::new(format!("bin/{}", path))
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .expect("Failed to start engine chess process");

        let int = std::sync::Arc::new(std::sync::Mutex::new(EngineInternal {
            name: path.to_string(),
            process: child_process,
            bestmove: None,
            state: EngineState::CheckUci,
        }));

        EngineProcess::write_stdin(int.clone(), "uci\n")?;

        let int_clone = int.clone();

        let read_handle = std::thread::spawn(|| {
            EngineProcess::read_thread(int_clone);
        });

        let engine = EngineProcess {
            int,
            path: path.to_string(),
            read_handle: Some(read_handle),
            versus_wins: 0,
        };

        Ok(engine)
    }

    pub fn take_bestmove(&self) -> Option<String> {
        let mut int = self.int.lock().unwrap();
        int.bestmove.take()
    }

    pub fn get_state(&self) -> EngineState {
        self.int.lock().unwrap().state
    }

    pub fn send_position_and_go(&self, position: &str) -> anyhow::Result<()> {
        let mut int = self.int.lock().unwrap();
        int.state = EngineState::Thinking;
        int.bestmove = None;
        int.write_stdin(position)?;
        Ok(())
    }

    pub fn redeploy(&mut self) -> anyhow::Result<()> {
        println!("Redeploying engine: {}", self.path);
        self.shutdown();
        println!("Redeploying engine process: {}", self.path);

        *self = EngineProcess::new(&self.path)?;
        println!("Engine redeployed: {}", self.path);

        Ok(())
    }

    fn write_stdin(
        int: std::sync::Arc<std::sync::Mutex<EngineInternal>>,
        buf: &str,
    ) -> anyhow::Result<()> {
        int.lock().unwrap().write_stdin(buf)?;
        Ok(())
    }

    fn read_thread(int: std::sync::Arc<std::sync::Mutex<EngineInternal>>) {
        let stdout = {
            let mut int = int.lock().unwrap();

            let stdout = int
                .process
                .stdout
                .take()
                .expect("Failed to open engine stdout for reading");

            stdout
        };

        for line in BufReader::new(stdout).lines() {
            match line {
                Ok(line) => {
                    if let Err(err) = int.lock().unwrap().process_line(&line) {
                        eprintln!("Error processing engine output: {}", err);
                    }
                }
                Err(e) => panic!("Error reading from engine stdout: {}", e),
            }
        }

        let (exit_result, status) = {
            let mut lock = int.lock().unwrap();
            (lock.wait(), lock.state)
        };

        if let Ok(exit_code) = exit_result {
            if !exit_code.success() && status != EngineState::Shutdown {
                eprintln!(
                    "[{}] Engine process exited with code: {:?}",
                    int.lock().unwrap().name,
                    exit_code.code()
                );
            }
        }
    }

    fn shutdown(&mut self) {
        if let Ok(mut lock) = self.int.lock() {
            lock.state = EngineState::Shutdown;

            if let Err(err) = lock.process.kill() {
                eprintln!("Failed to kill engine process: {}", err);
            }
        } else {
            panic!("Failed to lock engine internal state for shutdown");
        }

        if let Some(handle) = self.read_handle.take() {
            handle.join().expect("Failed to join read thread");
        }
    }
}

impl Drop for EngineProcess {
    fn drop(&mut self) {
        self.shutdown();
    }
}
