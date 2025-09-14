#![feature(sync_unsafe_cell)]
#![feature(iter_array_chunks)]
#![feature(likely_unlikely)]
#![feature(cold_path)]
#![feature(slice_swap_unchecked)]

use std::{cell::SyncUnsafeCell, thread::JoinHandle};

use crossbeam::channel;
use winit::event_loop::{ControlFlow, EventLoop};

use crate::engine::search::{SearchStrategy, transposition_v2};
use crate::scripts::lichess_parser::{self};
use crate::{
    engine::{
        chess, chess_v2,
        search::{self, AbortSignal, SigAbort, search_params},
        tables,
    },
    ui::chess_ui::ChessUi,
};

mod clipb;
mod engine;
mod matchmaking;
mod nnue;
mod pgn;
mod scripts;
mod ui;
mod uicomponents;
mod util;
mod window;

struct GoCommand {
    start_time: std::time::Instant,
    params: search_params::SearchParams,
    chess: chess::ChessGame,
    sig: AbortSignal,
    repetition_table: search::repetition_v2::RepetitionTable,
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
    transposition_table: &SyncUnsafeCell<transposition_v2::TranspositionTable>,
) {
    let tt = unsafe { &mut *transposition_table.get() };
    loop {
        match rx_search.recv() {
            Ok(go) => {
                let debug = go.params.debug;

                let chess = chess_v2::ChessGame::from(go.chess);

                assert!(chess.zobrist_key() == chess.calc_initial_zobrist_key(tables));

                // let mut search_engine = search::v13_nnue::Search::new(
                //     go.params,
                //     chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );

                let mut search_engine = search::v12_eval::Search::new(
                    go.params,
                    chess,
                    tables,
                    tt,
                    go.repetition_table,
                    &go.sig,
                );
                // let mut search_engine = search::v10_mvcache::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );
                // let mut search_engine = search::v9_prune::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );
                // let mut search_engine = search::v8_quiesc::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );
                // let mut search_engine = search::v7_mvvlva::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );

                // let mut search_engine = search::v6_psquare::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );

                // let mut search_engine = search::v5_tt::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );

                // let mut search_engine =
                //     search::v4_pv::Search::new(go.params, go.chess, tables, &go.sig);

                // let mut search_engine =
                //     search::v3_itdep::IterativeDeepening::new(go.params, go.chess, tables, &go.sig);

                // let mut search_engine =
                //     search::v2_alphabeta::Alphabeta::new(go.params, go.chess, tables, &go.sig);

                let best_move = search_engine.search();

                println!(
                    "bestmove {}",
                    if best_move != 0 {
                        util::move_string(best_move)
                    } else {
                        "0000".to_string()
                    }
                );

                if debug {
                    let elapsed = go.start_time.elapsed();

                    let search_nodes = search_engine.num_nodes_searched();
                    let search_depth = search_engine.get_depth();
                    let search_score = search_engine.search_score();
                    let tt_stats = unsafe { &mut *transposition_table.get() }.calc_stats();

                    println!(
                        "info searched {} nodes in {} with depth {} bestmove {} ({:016b}) score {} ({:.02}% tt occupancy)",
                        search_nodes,
                        util::time_format(elapsed.as_millis() as u64),
                        search_depth,
                        util::move_string(best_move),
                        best_move,
                        search_score,
                        (tt_stats.1 + tt_stats.2 + tt_stats.3) as f64 / (tt_stats.0 as f64) * 100.0
                    );

                    // println!(
                    //     "info pv {:?}",
                    //     search_engine
                    //         .get_pv()
                    //         .iter()
                    //         .map(|mv| util::move_string_dbg(*mv))
                    //         .collect::<Vec<String>>()
                    // );
                }
            }
            Err(_) => {
                println!("info search thread terminated");
                break;
            }
        }
    }
}

fn chess_uci(
    tx_search: channel::Sender<GoCommand>,
    tables: &tables::Tables,
    tt: &SyncUnsafeCell<transposition_v2::TranspositionTable>,
) -> anyhow::Result<()> {
    let mut debug = true;
    let mut game_board: Option<chess::ChessGame> = None;
    let mut repetition_table: Option<search::repetition_v2::RepetitionTable> = None;

    struct GoContext {
        tx_abort: channel::Sender<SigAbort>,
        tx_stop: channel::Sender<SigAbort>,
        timeout_handle: Option<JoinHandle<()>>,
    }

    let mut context: Option<GoContext> = None;

    fn abort_search_uci(_debug: bool, context: &mut Option<GoContext>) {
        if let Some(context) = context.take() {
            let _ = context.tx_abort.try_send(SigAbort {});

            if let Some(handle) = context.timeout_handle {
                // Exit timeout thread
                if !handle.is_finished() {
                    let _ = context.tx_stop.try_send(SigAbort {});
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
            Some("ucinewgame") => {}
            Some("stop") => abort_search_uci(debug, &mut context),
            Some("isready") => println!("readyok"),
            Some("position") => {
                let mut board = chess::ChessGame::new();
                let mut rep_table = search::repetition_v2::RepetitionTable::new();

                // Safety: Engine should not be calculating when receiving a position command
                let tt = unsafe { &mut *tt.get() };

                tt.clear();

                let position_start_index = input
                    .next()
                    .expect("Expected \"startpos\" or \"fen\" after position")
                    .as_ptr() as usize
                    - buffer.as_ptr() as usize;

                util::parse_position(
                    &buffer[position_start_index..],
                    &mut board,
                    tables,
                    Some(&mut rep_table),
                    None,
                    None,
                )?;

                game_board = Some(board);
                repetition_table = Some(rep_table);
            }
            Some("go") => {
                let start_time = std::time::Instant::now();

                abort_search_uci(debug, &mut context);

                let (tx_abort, rx_abort) = channel::bounded(1);
                let (tx_stop, rx_stop) = channel::bounded(1);

                let mut new_context = GoContext {
                    tx_abort,
                    tx_stop,
                    timeout_handle: None,
                };

                let chess = game_board
                    .take()
                    .expect("Expected position command to be sent before go");

                let mut search_params = search_params::SearchParams::from_iter(input);
                search_params.debug = debug;

                let infinite = search_params.infinite;
                let movetime = search_params.movetime;
                // let movestogo = search_params.movestogo;
                let time = if chess.b_move {
                    search_params.btime
                } else {
                    search_params.wtime
                };
                let inc = if chess.b_move {
                    search_params.binc
                } else {
                    search_params.winc
                };

                tx_search.send(GoCommand {
                    start_time,
                    params: search_params,
                    chess,
                    repetition_table: repetition_table
                        .take()
                        .expect("Expected position command to be sent before go"),
                    sig: rx_abort,
                })?;

                if !infinite {
                    let movetime_ms = if let Some(movetime) = movetime {
                        Some(movetime)
                    } else {
                        // base / 20 + inc / 2
                        time.and_then(|t| Some(t / 20 + inc.unwrap_or(0) / 2))
                    };

                    /*
                        position startpos moves d2d4 g8f6 c2c4 e7e6 b1c3 f8b4 f2f3 d7d5 a2a3 b4c3 b2c3 e8g8 c4d5 e6d5 e2e3 c7c5 f1d3                                                                                                                                  engine.py:950
                        go wtime 187750 btime 126970 winc 1000 binc 1000
                    */

                    if let Some(mut movetime_ms) = movetime_ms {
                        movetime_ms = movetime_ms.max(1);

                        let think_time = std::time::Duration::from_millis(movetime_ms as u64);

                        // Timeout thread
                        let tx_abort = new_context.tx_abort.clone();

                        new_context.timeout_handle =
                            Some(std::thread::spawn(move || {
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

                        if debug {
                            println!("info movetime ms {}", movetime_ms);
                        }
                    }
                }

                context = Some(new_context);
            }
            Some("quit") => break,
            Some(arg) => return Err(anyhow::anyhow!("Unknown command: \"{}\"", arg)),
            None => return Err(anyhow::anyhow!("No command provided")),
        }
    }

    if debug {
        println!("info exiting UCI mode");
    }

    abort_search_uci(debug, &mut context);

    Ok(())
}

fn main() {
    let mode = std::env::args().nth(1).unwrap_or("uci".to_string());

    let result = match mode.as_str() {
        "annotate" => {
            let mut arg_it = std::env::args().skip(2);
            let mut binpack_path = None;

            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--binpack" => binpack_path = Some(arg_it.next().unwrap()),
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            let binpack_path = binpack_path.expect("Expected path to binpack");

            let mut annotation = nnue::annotation::Annotator::new(binpack_path);

            annotation.annotate()
        }
        "selfplay" => {
            let mut arg_it = std::env::args().skip(2);
            let mut from = None;
            let mut count = None;
            let mut threads = None;
            let mut out_path = None;

            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--from" => from = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--count" => count = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--threads" => threads = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--out" => out_path = Some(arg_it.next().unwrap()),
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            let mut selfplay = matchmaking::selfplay::SelfplayTrainer::new(
                &out_path.expect("Expected output path"),
            );
            selfplay.play(
                threads.unwrap_or(1),
                ".\\data\\positions-comp-8to70-2017-1m.txt",
                from,
                count,
            )
        }
        "lext" => {
            let mut params = lichess_parser::ExtractParams {
                ffrom_ply: 0,
                fto_ply: 300,
                fmin_ply: 0,
                fnum_positions: 100,
                fcompleted_only: false,
                fdist_openings: false,
                fno_duplicates: false,
                fpick: lichess_parser::MovePick::Random,
            };

            let mut arg_it = std::env::args().skip(2);

            let mut in_path = None;
            let mut out_path = None;
            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--from-ply" => params.ffrom_ply = arg_it.next().unwrap().parse().unwrap(),
                    "--to-ply" => params.fto_ply = arg_it.next().unwrap().parse().unwrap(),
                    "--min-ply" => params.fmin_ply = arg_it.next().unwrap().parse().unwrap(),
                    "--count" => params.fnum_positions = arg_it.next().unwrap().parse().unwrap(),
                    "--no-duplicates" => params.fno_duplicates = true,
                    "--dist-openings" => params.fdist_openings = true,
                    "--completed-only" => params.fcompleted_only = true,
                    "--db" => in_path = Some(arg_it.next().unwrap()),
                    "--out" => out_path = Some(arg_it.next().unwrap()),
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            let in_path = in_path.expect("Expected path to Lichess database");
            let out_path = out_path.expect("Expected output path");

            lichess_parser::lichess_extract(&in_path, &out_path, params);

            Ok(())
        }
        "gui" => chess_ui(),
        "uci" => {
            // Register a panic hook to stop the process if any thread panics.
            // @todo - Restructure code to make parent threads handle panics for their children
            let panic_hook = std::panic::take_hook();
            std::panic::set_hook(Box::new(move |panic_info| {
                panic_hook(panic_info);
                std::process::exit(1);
            }));

            let (tx_search, rx_search) = channel::bounded(1);
            let tables = tables::Tables::new();

            // Safety: TranspositionTable is Send+Sync, it is up to the table implementation itself to
            // implement thread safety correctly. This has a few benefits:
            // 1. TranspositionTable can be accessed without runtime costs such as locks or refcounters
            // 2. The table can potentially be shared between threads efficiently, even unsoundly if races are deemed to be acceptable
            let tt = SyncUnsafeCell::new(transposition_v2::TranspositionTable::new(2));

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
