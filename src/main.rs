use winit::event_loop::{ControlFlow, EventLoop};

use crate::{
    engine::{chess, tables},
    ui::chess_ui::ChessUi,
};

mod clipb;
mod constant;
mod engine;
mod ui;
mod window;

fn chess_ui() -> anyhow::Result<()> {
    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    let chess_ui = ChessUi::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    // "8/PPPPPPPP/7k/8/8/7K/pppppppp/8 w KQkq - 0 1");

    let mut app = window::App::new(chess_ui);
    event_loop.run_app(&mut app)?;

    Ok(())
}

fn chess_uci() -> anyhow::Result<()> {
    let mut debug = false;
    let mut board = chess::Board::new();
    let tables = tables::Tables::new();

    loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer)?;

        let mut input = buffer.split_whitespace(); // .collect::<Vec<&str>>().join(" ");

        match input.next() {
            Some("uci") => {
                println!("id name chess\nuciok")
            }
            Some("debug") => {
                if let Some(arg) = input.next() {
                    debug = arg == "on";
                } else {
                    println!("Usage: debug <on|off>");
                }
            }
            Some("isready") => println!("readyok"),
            Some("position") => {
                let fen = match input.next() {
                    Some("startpos") => "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
                    Some("fen") => match input.next() {
                        Some(fen) => fen,
                        None => return Err(anyhow::anyhow!("No FEN provided in position command")),
                    },
                    val => {
                        return Err(anyhow::anyhow!(
                            "Expected 'startpos' or 'fen' in position command, got: {}",
                            val.unwrap_or("<None>")
                        ));
                    }
                };

                match board.load_fen(fen) {
                    Ok(_) => {}
                    Err(e) => return Err(anyhow::anyhow!("Failed to load position: {}", e)),
                };

                if let Some("moves") = input.next() {
                    for mv_str in input {
                        let mv = constant::create_move(mv_str);

                        if !board.make_move_slow(mv, &tables) {
                            return Err(anyhow::anyhow!("Invalid move: {}", mv_str));
                        }
                    }
                }
            }
            Some("go") => loop {
                let command_str = match input.next() {
                    Some(cmd) => cmd,
                    None => break,
                };
                match command_str {
                    "depth" => {
                        let depth = input.next().and_then(|s| s.parse::<u8>().ok()).unwrap_or(1);

                        // println!("Calculating best move with depth {}", depth);

                        // @todo - Search

                        // @todo - return bestmove

                        println!("bestmove e2e4");
                    }
                    _ => return Err(anyhow::anyhow!("Unknown command in go: {}", command_str)),
                }
            },
            Some("stop") => return Err(anyhow::anyhow!("Stop command received")),
            Some("quit") => {
                return Ok(());
            }
            Some(arg) => return Err(anyhow::anyhow!("Unknown command: {}", arg)),
            None => return Err(anyhow::anyhow!("No command provided")),
        }
    }

    Ok(())
}

fn main() {
    let mode = std::env::args().nth(1).expect("no pattern given");

    let result = match mode.as_str() {
        "gui" => chess_ui(),
        "uci" => chess_uci(),
        _ => panic!("Unknown mode: {}", mode),
    };

    match result {
        Ok(_) => println!("Exited successfully"),
        Err(e) => eprintln!("Error: {}", e),
    };
}
