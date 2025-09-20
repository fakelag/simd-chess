use std::io::{BufRead, BufReader, Write};
use std::process::ExitStatus;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EngineState {
    CheckUci,
    WaitReadyOk,
    ReadyOk,
    Thinking,
    Shutdown,
    Exited(i32),
}

struct EngineInternal {
    name: String,
    process: std::process::Child,
    state: EngineState,
    bestmove: Option<String>,

    last_command: Option<String>,
}

pub struct EngineProcess {
    pub path: String,
    pub go_params: String,
    pub versus_wins: usize,

    read_handle: Option<std::thread::JoinHandle<()>>,
    int: std::sync::Arc<std::sync::Mutex<EngineInternal>>,
}

impl EngineInternal {
    fn process_line(&mut self, line: &str) -> anyhow::Result<()> {
        // if self.stdout_lines.len() >= MAX_STDOUT_LINES {
        //     let remove_count = MAX_STDOUT_LINES / 5;
        //     self.stdout_lines.drain(0..remove_count);
        // }
        // self.stdout_lines.push(line.to_string());

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
        self.last_command = Some(buf.to_string());
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
            last_command: None,
            // stdout_lines: Vec::with_capacity(MAX_STDOUT_LINES),
        }));

        EngineProcess::write_stdin(int.clone(), "uci\n")?;

        let int_clone = int.clone();

        let read_handle = std::thread::spawn(|| {
            EngineProcess::read_thread(int_clone);
        });

        let engine = EngineProcess {
            int,
            go_params: String::new(),
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

    pub fn get_last_command(&self) -> Option<String> {
        self.int.lock().unwrap().last_command.clone()
    }
    // pub fn get_stdout_buffer(&self) -> Option<Vec<String>> {
    //     self.int.lock().unwrap().stdout_lines.clone().into()
    // }

    pub fn send_newgame(&self) -> anyhow::Result<()> {
        let mut int = self.int.lock().unwrap();
        int.state = EngineState::WaitReadyOk;
        int.bestmove = None;
        int.write_stdin("ucinewgame\nisready\n")?;
        Ok(())
    }

    pub fn send_position_and_go(&self, position: &str) -> anyhow::Result<()> {
        let mut int = self.int.lock().unwrap();
        int.state = EngineState::Thinking;
        int.bestmove = None;
        int.write_stdin(position)?;
        Ok(())
    }

    pub fn redeploy(&mut self) -> anyhow::Result<()> {
        self.shutdown();

        // @todo - Hack to preserve wins status, move wins outside of process.rs eventually
        let wins = self.versus_wins;
        let args = self.go_params.clone();

        *self = EngineProcess::new(&self.path)?;

        self.versus_wins = wins;
        self.go_params = args;

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
            if status != EngineState::Shutdown {
                let mut lock = int.lock().unwrap();
                lock.state = EngineState::Exited(exit_code.code().unwrap_or(-1));

                if !exit_code.success() {
                    lock.process.stderr.take().map(|mut stderr| {
                        let mut err_output = String::new();
                        use std::io::Read;
                        stderr.read_to_string(&mut err_output).ok();
                        eprintln!(
                            "[{}] Engine exited unexpectedly with code {}. Last command: {:?}\nSTDERR:\n{}\n",
                            lock.name,
                            exit_code.code().unwrap_or(-1),
                            lock.last_command.as_ref().map(|x| x.clone()).unwrap_or_else(|| "<None>".to_string()),
                            err_output,
                        );
                    });
                }
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
