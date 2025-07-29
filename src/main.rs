#![feature(sync_unsafe_cell)]

use std::{cell::SyncUnsafeCell, thread::JoinHandle};

use crossbeam::channel;
use winit::event_loop::{ControlFlow, EventLoop};

use crate::{
    engine::{
        chess,
        search::{self, AbortSignal, SearchStrategy, SigAbort, search_params, transposition},
        tables,
    },
    ui::chess_ui::ChessUi,
};

mod clipb;
mod engine;
mod matchmaking;
mod ui;
mod uicomponents;
mod util;
mod window;

struct GoCommand {
    start_time: std::time::Instant,
    params: search_params::SearchParams,
    chess: chess::ChessGame,
    sig: AbortSignal,
    debug: bool,
}

fn chess_ui() -> anyhow::Result<()> {
    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    let chess_ui = ChessUi::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    // "8/PPPPPPPP/7k/8/8/7K/pppppppp/8 w KQkq - 0 1");

    let mut app = window::App::new(chess_ui);
    event_loop.run_app(&mut app)?;

    Ok(())
}

fn search_thread(
    rx_search: channel::Receiver<GoCommand>,
    tables: &tables::Tables,
    tt: &SyncUnsafeCell<transposition::TranspositionTable>,
) {
    let tt = unsafe { &mut *tt.get() };
    loop {
        match rx_search.recv() {
            Ok(go) => {
                let mut search_engine =
                    search::v5_tt::Search::new(go.params, go.chess, tables, tt, &go.sig);

                // let mut search_engine =
                //     search::v4_pv::Search::new(go.params, go.chess, tables, &go.sig);

                // let mut search_engine =
                //     search::v3_itdep::IterativeDeepening::new(go.params, go.chess, tables, &go.sig);

                // let mut search_engine =
                //     search::v2_alphabeta::Alphabeta::new(go.params, go.chess, tables, &go.sig);

                let best_move = search_engine.search();

                if go.debug {
                    let elapsed = go.start_time.elapsed();
                    println!(
                        "info searched {} nodes in {} with bestmove {} ({:016b}) score {}",
                        search_engine.num_nodes_searched(),
                        util::time_format(elapsed.as_millis() as u64),
                        util::move_string(best_move),
                        best_move,
                        search_engine.search_score()
                    );
                }

                println!(
                    "bestmove {}",
                    if best_move != 0 {
                        util::move_string(best_move)
                    } else {
                        "0000".to_string()
                    }
                );
            }
            Err(_) => {
                println!("Search thread terminated");
                break;
            }
        }
    }
}

fn chess_uci(
    tx_search: channel::Sender<GoCommand>,
    tables: &tables::Tables,
    tt: &SyncUnsafeCell<transposition::TranspositionTable>,
) -> anyhow::Result<()> {
    let mut debug = false;
    let mut game_board: Option<chess::ChessGame> = None;

    struct GoContext {
        tx_abort: channel::Sender<SigAbort>,
        tx_stop: channel::Sender<SigAbort>,
        timeout_handle: Option<JoinHandle<()>>,
    }

    let mut context: Option<GoContext> = None;

    fn abort_search_uci(context: &mut Option<GoContext>) {
        if let Some(context) = context.take() {
            let _ = context.tx_abort.try_send(SigAbort {});

            if let Some(handle) = context.timeout_handle {
                // Exit timeout thread
                if !handle.is_finished() {
                    context.tx_stop.try_send(SigAbort {}).unwrap();
                    handle.join().unwrap();
                }
            }
        }
    }

    'next_cmd: loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer)?;

        let mut input = buffer.split_whitespace();

        match input.next() {
            Some("uci") => {
                println!("id name chess\nuciok")
            }
            Some("debug") => match input.next() {
                Some("on") => debug = true,
                Some("off") => debug = false,
                _ => panic!("Expected 'on' or 'off' after debug command"),
            },
            Some("stop") => abort_search_uci(&mut context),
            Some("isready") => println!("readyok"),
            Some("position") => {
                let mut board = chess::ChessGame::new();

                // Safety: Engine should not be calculating when receiving a position command
                let tt = unsafe { &mut *tt.get() };

                tt.clear();

                let moves_it = if let Some(position_string) = input.next() {
                    match position_string {
                        "startpos" => {
                            board.load_fen(util::FEN_STARTPOS, tables).unwrap();
                            Some(input)
                        }
                        "fen" => {
                            let fen_start_index = position_string.as_ptr() as usize
                                - buffer.as_ptr() as usize
                                + position_string.len()
                                + 1;

                            let fen_length = match board
                                .load_fen(&buffer[fen_start_index..], tables)
                            {
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
                        let mv = util::fix_move(&board, util::create_move(mv_str));

                        if !board.make_move_slow(mv, &tables) {
                            return Err(anyhow::anyhow!("Invalid move: {}", mv_str));
                        }

                        // let is_irreversible = board.half_moves == 0;
                    }
                }

                game_board = Some(board);
            }
            Some("go") => {
                let start_time = std::time::Instant::now();

                abort_search_uci(&mut context);

                let (tx_abort, rx_abort) = channel::bounded(1);
                let (tx_stop, rx_stop) = channel::bounded(1);

                let mut new_context = GoContext {
                    tx_abort,
                    tx_stop,
                    timeout_handle: None,
                };

                let search_params = search_params::SearchParams::from_iter(input);

                tx_search.send(GoCommand {
                    start_time,
                    params: search_params,
                    chess: game_board
                        .take()
                        .expect("Expected position command to be sent before go"),
                    sig: rx_abort,
                    debug,
                })?;

                // @todo - Calc thinking time
                let think_time = std::time::Duration::from_millis(100);

                // Timeout thread
                let tx_abort = new_context.tx_abort.clone();

                new_context.timeout_handle = Some(std::thread::spawn(move || {
                    match rx_stop.recv_timeout(think_time) {
                        Ok(_) => {}
                        Err(channel::RecvTimeoutError::Timeout) => {
                            let _ = tx_abort.try_send(SigAbort {});
                        }
                        Err(channel::RecvTimeoutError::Disconnected) => {
                            panic!("Timeout thread disconnected")
                        }
                    }
                }));

                context = Some(new_context);
            }
            Some("quit") => break,
            Some(arg) => return Err(anyhow::anyhow!("Unknown command: \"{}\"", arg)),
            None => return Err(anyhow::anyhow!("No command provided")),
        }
    }

    abort_search_uci(&mut context);

    Ok(())
}

// fn test() {
//     use std::arch::x86_64::*;

//     let mut bb = [0u64; 12];
//     let b_move = false;

//     bb[0] = 0b100;
//     bb[1] = 0b010;
//     bb[2] = 0b001;

//     struct T {
//         tables: [[u64; 64]; 6],
//     }

//     let mut t = T {
//         tables: [
//             [0b10; 64],
//             [0b111; 64],
//             [0b1111; 64],
//             [0; 64],
//             [0; 64],
//             [0; 64],
//         ],
//     };

//     t.tables[1][1] = 0b101;

//     unsafe {
//         let most_sig_bit_1 = _mm512_set1_epi64(1 << 63);
//         let sixtythree = _mm512_set1_epi64(63);

//         // Getting piece squares and popping ms1b for next it
//         let pieces_vec = _mm512_loadu_si512(bb.as_ptr().add(b_move as usize * 6) as *const _);
//         let ms1b_vec = _mm512_lzcnt_epi64(pieces_vec);
//         let shft = _mm512_srlv_epi64(most_sig_bit_1, ms1b_vec);
//         let popped = _mm512_xor_si512(pieces_vec, shft);
//         let realindexes = _mm512_sub_epi64(sixtythree, ms1b_vec);

//         println!("Pieces: {:?}", pieces_vec);
//         println!("MS1B: {:?}", ms1b_vec);
//         println!("shft: {:?}", shft);
//         println!("Popped: {:?}", popped);
//         println!("Real indexes: {:?}", realindexes);

//         // Gathering tables from mem
//         let vindex = _mm256_set_epi32(0, 0, 0, 0, 0, 64 * 2, 64 + 1, 0);
//         let lol = _mm512_i32gather_epi64(vindex, t.tables.as_ptr() as *const i64, 8);

//         println!("Gathered tables: {:?}", lol);
//     }
// }

fn main() {
    let mode = std::env::args().nth(1).unwrap_or("uci".to_string());

    let result = match mode.as_str() {
        "gui" => chess_ui(),
        "uci" => {
            let (tx_search, rx_search) = channel::bounded(1);
            let tables = tables::Tables::new();

            // Safety: TranspositionTable is Send+Sync, it is up to the table implementation itself to
            // implement thread safety correctly. This has a few benefits:
            // 1. TranspositionTable can be accessed without runtime costs such as locks or refcounters
            // 2. The table can potentially be shared between threads efficiently, even unsoundly if races are deemed to be acceptable
            let tt = SyncUnsafeCell::new(transposition::TranspositionTable::new(8));

            let result = std::thread::scope(|s| {
                let st = s.spawn(|| {
                    search_thread(rx_search, &tables, &tt);
                });

                let result = chess_uci(tx_search, &tables, &tt);

                st.join().unwrap();

                result
            });

            result
        }
        _ => panic!("Unknown mode: {}", mode),
    };

    match result {
        Ok(_) => println!("Exited successfully"),
        Err(e) => println!("Error: {}", e),
    };
}
