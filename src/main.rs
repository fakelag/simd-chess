use winit::event_loop::{ControlFlow, EventLoop};

use crate::{
    engine::{chess, search, tables},
    ui::chess_ui::ChessUi,
};

mod clipb;
mod constant;
mod engine;
mod matchmaking;
mod ui;
mod uicomponents;
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
    let mut board = chess::ChessGame::new();
    let tables = tables::Tables::new();

    'next_cmd: loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer)?;

        let mut input = buffer.split_whitespace();

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
                let moves_it = if let Some(position_string) = input.next() {
                    match position_string {
                        "startpos" => {
                            board.load_fen(constant::FEN_STARTPOS).unwrap();
                            Some(input)
                        }
                        "fen" => {
                            let fen_start_index = position_string.as_ptr() as usize
                                - buffer.as_ptr() as usize
                                + position_string.len()
                                + 1;

                            let fen_length = match board.load_fen(&buffer[fen_start_index..]) {
                                Ok(fen_length) => fen_length,
                                Err(e) => return Err(anyhow::anyhow!("Failed to load FEN: {}", e)),
                            };

                            Some(buffer[fen_start_index + fen_length..].split_whitespace())
                        }
                        _ => None,
                    }
                } else {
                    None
                };

                let mut moves_it = match moves_it {
                    Some(it) => it,
                    None => {
                        return Err(anyhow::anyhow!(
                            "Expected 'startpos' or 'fen' in position command"
                        ));
                    }
                };

                if let Some("moves") = moves_it.next() {
                    while let Some(mv_str) = moves_it.next() {
                        let mv = constant::fix_move(&board, constant::create_move(mv_str));

                        if !board.make_move_slow(mv, &tables) {
                            return Err(anyhow::anyhow!("Invalid move: {}", mv_str));
                        }
                    }
                }
            }
            Some("go") => {
                let command_str = match input.next() {
                    Some(cmd) => cmd,
                    None => break,
                };
                match command_str {
                    "depth" => {
                        let depth = input.next().and_then(|s| s.parse::<u8>().ok()).unwrap_or(1);

                        let best_move = search::Search::new(&mut board, &tables).search(depth);

                        println!(
                            "bestmove {}",
                            if best_move != 0 {
                                constant::move_string(best_move)
                            } else {
                                "0000".to_string()
                            }
                        );
                    }
                    _ => return Err(anyhow::anyhow!("Unknown command in go: {}", command_str)),
                }
            }
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
    let mode = std::env::args().nth(1).unwrap_or("uci".to_string());

    let result = match mode.as_str() {
        "gui" => chess_ui(),
        "uci" => chess_uci(),
        _ => panic!("Unknown mode: {}", mode),
    };

    match result {
        Ok(_) => println!("Exited successfully"),
        Err(e) => println!("Error: {}", e),
    };
}
