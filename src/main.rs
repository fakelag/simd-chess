#![feature(sync_unsafe_cell)]
#![feature(iter_array_chunks)]
#![feature(likely_unlikely)]
#![feature(cold_path)]
#![feature(slice_swap_unchecked)]
#![feature(isolate_most_least_significant_one)]
#![feature(adt_const_params)]
#![feature(generic_const_exprs)]

use std::cell::SyncUnsafeCell;

use crossbeam::channel;
use winit::event_loop::{ControlFlow, EventLoop};

use crate::engine::ownbook::OwnBook;
use crate::engine::search::search_params::SearchParams;
use crate::engine::search::{SearchStrategy, repetition_v2, transposition_v2};
use crate::uci::uci::{UciCommand, chess_uci};
use crate::{
    engine::{
        chess_v2,
        search::{self},
        tables,
    },
    ui::chess_ui::ChessUi,
};

mod clipb;
mod engine;
mod matchmaking;
mod nnue;
mod pgn;
mod uci;
mod ui;
mod uicomponents;
mod util;
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

fn get_opening_book(
    uci_context: &uci::context::UciContext<uci::uci::UciOptions>,
    tables: &tables::Tables,
) -> Option<OwnBook> {
    if uci_context
        .lock()
        .get_by_id(uci::uci::UciOptions::OwnBook)
        .val_bool()
    {
        let book_path = uci_context
            .lock()
            .get_by_id(uci::uci::UciOptions::OwnBookPath)
            .val_string()
            .to_string();
        Some(OwnBook::from_pgn(&tables, &book_path, |_, _, _| true).unwrap())
    } else {
        None
    }
}

fn search_thread(
    uci_context: uci::context::UciContext<uci::uci::UciOptions>,
    rx_search: channel::Receiver<UciCommand>,
    tables: &tables::Tables,
    transposition_table: &SyncUnsafeCell<transposition_v2::TranspositionTable>,
) {
    let tt = unsafe { &mut *transposition_table.get() };

    let mut search_engine = search::v12_eval_sp::Search::new(
        SearchParams::new(),
        tables,
        tt,
        repetition_v2::RepetitionTable::new(),
    );

    let mut used_book = get_opening_book(&uci_context, tables);

    loop {
        match rx_search.recv() {
            Ok(UciCommand::Go(go)) => {
                let debug = go.params.debug;

                let chess = chess_v2::ChessGame::from_v1(go.chess, tables);

                let (board_key, pawn_key) = chess.calc_initial_zobrist_key(tables);
                assert!(chess.zobrist_key() == board_key);
                assert!(chess.pawn_key() == pawn_key);

                search_engine.load_from_board(&chess);
                search_engine.new_search();
                search_engine.set_sig(go.sig);
                search_engine.set_rt(go.repetition_table);
                search_engine.set_search_params(go.params);

                let book_move = match used_book {
                    Some(ref own_book) if chess.ply() < 16 => own_book
                        .probe(chess.zobrist_key())
                        .map(|entry| entry.bestmove),
                    _ => None,
                };

                let best_move = book_move.unwrap_or_else(|| search_engine.search());

                if debug {
                    if book_move.is_some() {
                        println!("info depth 0 score cp 0 (book)");
                    } else {
                        println!(
                            "info depth {} score cp {}",
                            search_engine.get_depth(),
                            search_engine.search_score()
                        );
                    }

                    // let search_nodes = search_engine.num_nodes_searched();
                    // let search_depth = search_engine.get_depth();
                    // let search_score = search_engine.search_score();
                    // let tt_stats = unsafe { &mut *transposition_table.get() }.calc_stats();

                    // let elapsed = go.start_time.elapsed();

                    // println!(
                    //     "info string searched {} nodes in {} with depth {} bestmove {} ({:016b}) score {} ({:.02}% tt occupancy, {:.02} probe hit rate)",
                    //     search_nodes,
                    //     util::time_format(elapsed.as_millis() as u64),
                    //     search_depth,
                    //     util::move_string(best_move),
                    //     best_move,
                    //     search_score,
                    //     tt_stats.fill_percentage * 100.0,
                    //     tt_stats.probe_hit as f64
                    //         / (tt_stats.probe_hit as f64 + tt_stats.probe_miss as f64)
                    //         * 100.0
                    // );

                    // println!(
                    //     "info pv {:?}",
                    //     search_engine
                    //         .get_pv()
                    //         .iter()
                    //         .map(|mv| util::move_string_dbg(*mv))
                    //         .collect::<Vec<String>>()
                    // );
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
            Ok(UciCommand::OptionChange(changed_option)) => match changed_option {
                uci::uci::UciOptions::OwnBook | uci::uci::UciOptions::OwnBookPath => {
                    // Reload opening book
                    used_book = get_opening_book(&uci_context, tables);
                    println!("info string reloaded opening book");
                }
            },
            Ok(UciCommand::NewGame) => {
                search_engine.new_game();
            }
            Err(_) => {
                println!("info search thread terminated");
                break;
            }
        }
    }
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

            let mut annotation = nnue::annotation::Annotator::new(16, binpack_path);

            annotation.annotate()
        }
        "selfplay" => {
            let mut arg_it = std::env::args().skip(2);
            let mut from = None;
            let mut count = None;
            let mut threads = None;
            let mut out_path = None;
            let mut positions_path = None;
            let mut annotated = false;

            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--from" => from = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--count" => count = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--threads" => threads = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--positions" => positions_path = Some(arg_it.next().unwrap()),
                    "--annotated" => annotated = true,
                    "--out" => out_path = Some(arg_it.next().unwrap()),
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            let mut selfplay = matchmaking::selfplay::SelfplayTrainer::new(
                &out_path.expect("Expected output path"),
            );

            if annotated {
                let positions_file_path = positions_path.expect("Expected positions path");
                selfplay.play_annotated(threads.unwrap_or(1), &positions_file_path, from, count)
            } else {
                todo!("Unannotated selfplay not implemented");
            }
        }
        "train" => {
            let mut arg_it = std::env::args().skip(2);
            let name = arg_it.next().expect("Expected training name");

            let mut out_path = None;
            let mut valid_path = None;
            let mut bp_paths: Vec<String> = vec![];

            let mut hidden_size = 128;
            let mut output_size = 1;

            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--paths" => bp_paths.push(arg_it.next().unwrap()),
                    "--validate" => valid_path = Some(arg_it.next().unwrap()),
                    "--out" => out_path = Some(arg_it.next().unwrap()),
                    "--hs" => hidden_size = arg_it.next().unwrap().parse().unwrap(),
                    "--os" => output_size = arg_it.next().unwrap().parse().unwrap(),
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            if bp_paths.is_empty() {
                panic!("Expected at least one binpack path");
            }

            let path_refs = bp_paths.iter().map(|s| s.as_str()).collect::<Vec<&str>>();

            macro_rules! train {
                ($os:expr) => {{
                    nnue::training::train::<$os>(
                        &name,
                        path_refs.as_slice(),
                        &out_path.expect("Expected output path"),
                        valid_path.as_deref(),
                        hidden_size,
                    );
                }};
            }

            match output_size {
                1 => train!(1),
                2 => train!(2),
                4 => train!(4),
                6 => train!(6),
                8 => train!(8),
                _ => panic!("Unsupported output size: {}", output_size),
            }

            Ok(())
        }
        "pgnextract" => {
            let mut params = pgn::extract::PositionExtractParams {
                ffrom_ply: 0,
                fto_ply: 300,
                fmin_ply: 0,
                fseed: 0,
                fnum_positions: 100,
                fcompleted_only: false,
                fdist_openings: false,
                fno_duplicates: false,
                fpick: pgn::extract::MovePick::Random,
                fattempts_per_position: 1,
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
                    "--seed" => params.fseed = arg_it.next().unwrap().parse().unwrap(),
                    "--no-duplicates" => params.fno_duplicates = true,
                    "--dist-openings" => params.fdist_openings = true,
                    "--completed-only" => params.fcompleted_only = true,
                    "--db" => in_path = Some(arg_it.next().unwrap()),
                    "--core" => {
                        core_affinity::set_for_current(core_affinity::CoreId {
                            id: arg_it.next().unwrap().parse().unwrap(),
                        });
                    }
                    "--out" => out_path = Some(arg_it.next().unwrap()),
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            let in_path = in_path.expect("Expected path to pgn input file");
            let out_path = out_path.expect("Expected output path");

            pgn::extract::extract_positions(&in_path, &out_path, params)
        }
        "gui" => {
            let mut arg_it = std::env::args().skip(2);
            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--pin" => {
                        let core_id: usize = arg_it.next().unwrap().parse().unwrap();
                        core_affinity::set_for_current(core_affinity::CoreId { id: core_id });
                        println!("Pinned GUI thread to core {}", core_id);
                    }
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            chess_ui()
        }
        "uci" => {
            let mut arg_it = std::env::args().skip(2);
            let mut pin_core_id = None;
            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--pin" => {
                        pin_core_id = Some(arg_it.next().unwrap().parse().unwrap());
                    }
                    _ => panic!("Unknown argument: {}", arg),
                }
            }
            // Register a panic hook to stop the process if any thread panics.
            // @todo - Restructure code to make parent threads handle panics for their children
            let panic_hook = std::panic::take_hook();
            std::panic::set_hook(Box::new(move |panic_info| {
                panic_hook(panic_info);
                std::process::exit(1);
            }));

            let (tx_search, rx_search) = channel::bounded(1);
            let tables = tables::Tables::new();

            let uci_context = uci::uci::create_context();

            // Safety: TranspositionTable is Send+Sync, it is up to the table implementation itself to
            // implement thread safety correctly. This has a few benefits:
            // 1. TranspositionTable can be accessed without runtime costs such as locks or refcounters
            // 2. The table can potentially be shared between threads efficiently, even unsoundly if races are deemed to be acceptable
            let tt = SyncUnsafeCell::new(transposition_v2::TranspositionTable::new(1));

            let result = std::thread::scope(|s| {
                let st = s.spawn(|| {
                    if let Some(core_id) = pin_core_id {
                        core_affinity::set_for_current(core_affinity::CoreId { id: core_id });
                        println!("Pinned search thread to core {}", core_id);
                    }

                    search_thread(uci_context.clone(), rx_search, &tables, &tt);
                });

                let result = chess_uci(uci_context.clone(), tx_search, &tables, &tt);

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
