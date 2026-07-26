#![feature(sync_unsafe_cell)]
#![feature(iter_array_chunks)]
#![feature(likely_unlikely)]
#![feature(cold_path)]
#![feature(slice_swap_unchecked)]
#![feature(isolate_most_least_significant_one)]
#![feature(adt_const_params)]
#![feature(iter_collect_into)]
#![feature(generic_const_exprs)]

use std::cell::SyncUnsafeCell;

use crossbeam::channel;
use winit::event_loop::{ControlFlow, EventLoop};

use crate::engine::ownbook::OwnBook;
use crate::engine::search::search_params::SearchParams;
use crate::engine::search::{EngineForm, SearchStrategy, repetition};
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
    tm: &SyncUnsafeCell<search::timeman::TimeManager>,
    tables: &tables::Tables,
) {
    let tt = std::cell::SyncUnsafeCell::new(search::transposition::TranspositionTable::new(8));
    let mut search_engine = search::search::Search::<{ EngineForm::Strategy }>::new(
        tables,
        &tt,
        tm,
        repetition::RepetitionTable::new(),
    );
    search_engine.set_print_info(true);

    let mut used_book = get_opening_book(&uci_context, tables);

    loop {
        match rx_search.recv() {
            Ok(UciCommand::Go(go)) => {
                let debug = go.params.debug;
                let chess = &go.chess;

                let (board_key, pawn_key, non_pawn_key) = chess.calc_initial_zobrist_key(tables);
                assert!(chess.zobrist_key() == board_key);
                assert!(chess.pawn_key() == pawn_key);
                assert!([chess.non_pawn_key(0), chess.non_pawn_key(1)] == non_pawn_key);

                search_engine.load_from_board(chess);
                search_engine.new_search();
                search_engine.set_rt(go.repetition_table);

                match go.limits {
                    Some(limits) => search_engine.tm_mut().enable(limits, go.start_time),
                    None => search_engine.tm_mut().disable(),
                }

                search_engine
                    .tm_mut()
                    .set_nodes(go.params.nodes.unwrap_or(0));

                let book_move = match used_book {
                    Some(ref own_book) if chess.ply() < 16 => own_book
                        .probe(chess.zobrist_key())
                        .map(|entry| entry.bestmove),
                    _ => None,
                };

                let best_move = book_move.unwrap_or_else(|| search_engine.search(go.params.depth));

                if debug && book_move.is_some() {
                        println!("info depth 0 score cp 0 (book)");
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
            Ok(UciCommand::Ping) => {}
            Err(_) => {
                println!("info search thread terminated");
                break;
            }
        }
    }
}

fn main() {
    // let mut b = chess_v2::ChessGame::new();
    // b.load_fen(
    //     "rnbqkbnr/1ppppppp/p7/8/8/P7/1PPPPPPP/RNBQKBNR w KQkq - 0 2",
    //     &tables::Tables::new(),
    // )
    // .unwrap();
    // chess_v2::ChessGame::in_check_avx512(1, 1, b.bitboards());
    // return;
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
            let mut games = None;
            let mut threads = None;
            let mut out_path = None;
            let mut max_positions = None;
            let mut positions_paths: Vec<String> = Vec::new();
            let mut win_adj = false;
            let mut draw_adj = false;
            let mut adj_shadow_log = None;

            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--from" => from = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--count" => games = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--max-positions" => {
                        max_positions = Some(arg_it.next().unwrap().parse().unwrap())
                    }
                    "--threads" => threads = Some(arg_it.next().unwrap().parse().unwrap()),
                    "--positions" => positions_paths.push(arg_it.next().unwrap()),
                    "--out" => out_path = Some(arg_it.next().unwrap()),
                    "--win-adj" => win_adj = true,
                    "--draw-adj" => draw_adj = true,
                    "--adj-shadow-log" => adj_shadow_log = Some(arg_it.next().unwrap()),
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            let mut selfplay = matchmaking::selfplay::SelfplayTrainer::new(out_path.as_deref());

            assert!(
                !positions_paths.is_empty(),
                "Expected at least one --positions path"
            );
            selfplay.play_annotated(
                threads.unwrap_or(1),
                &positions_paths,
                from,
                games,
                max_positions,
                win_adj,
                draw_adj,
                adj_shadow_log.as_deref(),
            )
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
                fcp_threshold: None,
                fnum_positions: 100,
                fcompleted_only: false,
                fno_duplicates: false,
                fpick: pgn::extract::MovePick::Random,
                fattempts_per_position: 1,
                fsharpness_top_percent: None,
                fsharpness_depth: 7,
                fout_shard_size: 0,
            };

            let mut arg_it = std::env::args().skip(2);

            let mut in_paths: Vec<String> = Vec::new();
            let mut out_path = None;
            let mut threads = 1usize;
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
                    "--cp-threshold" => {
                        params.fcp_threshold = Some(arg_it.next().unwrap().parse().unwrap())
                    }
                    "--attempts" => {
                        params.fattempts_per_position = arg_it.next().unwrap().parse().unwrap()
                    }
                    "--sharpness-top-percent" => {
                        params.fsharpness_top_percent =
                            Some(arg_it.next().unwrap().parse().unwrap())
                    }
                    "--sharpness-depth" => {
                        params.fsharpness_depth = arg_it.next().unwrap().parse().unwrap()
                    }
                    "--threads" => threads = arg_it.next().unwrap().parse().unwrap(),
                    "--no-duplicates" => params.fno_duplicates = true,
                    "--completed-only" => params.fcompleted_only = true,
                    "--db" => in_paths.push(arg_it.next().unwrap()),
                    "--core" => {
                        core_affinity::set_for_current(core_affinity::CoreId {
                            id: arg_it.next().unwrap().parse().unwrap(),
                        });
                    }
                    "--out" => out_path = Some(arg_it.next().unwrap()),
                    "--out-shard-size" => {
                        params.fout_shard_size = arg_it.next().unwrap().parse().unwrap()
                    }
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            assert!(
                !in_paths.is_empty(),
                "Expected at least one --db input file"
            );
            let out_path = out_path.expect("Expected output path");

            pgn::extract::extract_positions(&in_paths, &out_path, params, threads)
        }
        "tune" => {
            let mut arg_it = std::env::args().skip(2);

            let mut fen_path = None;
            let mut depth = 8u8;
            let mut quantiles: Vec<f64> = vec![];

            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--fen" => fen_path = Some(arg_it.next().unwrap()),
                    "--depth" => depth = arg_it.next().unwrap().parse().unwrap(),
                    "--quantiles" => {
                        quantiles = arg_it
                            .next()
                            .unwrap()
                            .split(',')
                            .map(|s| s.parse().unwrap())
                            .collect()
                    }
                    "--core" => {
                        core_affinity::set_for_current(core_affinity::CoreId {
                            id: arg_it.next().unwrap().parse().unwrap(),
                        });
                    }
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            let fen_path = fen_path.expect("Expected --fen <file>");
            if quantiles.is_empty() {
                quantiles = vec![0.1, 0.25, 0.5, 0.75, 0.9];
            }

            let tables = tables::Tables::new();
            let file = std::fs::File::open(&fen_path)
                .unwrap_or_else(|e| panic!("failed to open FEN file {}: {}", fen_path, e));

            let mut boards = Vec::new();
            for line in std::io::BufRead::lines(std::io::BufReader::new(file)) {
                let line = line.unwrap();
                let fen = line.trim();
                if fen.is_empty() {
                    continue;
                }
                let mut board = chess_v2::ChessGame::new();
                if board.load_fen(fen, &tables).is_ok() {
                    boards.push(board);
                }
            }

            println!(
                "tuning over {} positions at depth {} (raw winprob_instability)",
                boards.len(),
                depth
            );
            let cuts =
                pgn::tuner::tune_thresholds(boards.into_iter(), 16, depth, &tables, &quantiles);
            for (q, cut) in quantiles.iter().zip(cuts.iter()) {
                println!("q{:.4} -> raw instability {:.6}", q, cut);
            }

            Ok(())
        }
        "metrics" => {
            let mut arg_it = std::env::args().skip(2);

            let mut fen_path = None;
            let mut binpack_paths: Vec<String> = vec![];
            let mut lineage_keys_path = None;
            let mut out_prefix = "scratch/tmp_metrics".to_string();

            loop {
                let arg = match arg_it.next() {
                    Some(a) => a,
                    None => break,
                };

                match arg.as_str() {
                    "--fen" => fen_path = Some(arg_it.next().unwrap()),
                    "--binpack" => binpack_paths.push(arg_it.next().unwrap()),
                    "--lineage-keys" => lineage_keys_path = Some(arg_it.next().unwrap()),
                    "--out" => out_prefix = arg_it.next().unwrap(),
                    _ => panic!("Unknown argument: {}", arg),
                }
            }

            match (fen_path, binpack_paths.is_empty()) {
                (Some(fen), true) => pgn::metrics::run_fen_stats(&fen, &out_prefix),
                (None, false) => {
                    let path_refs = binpack_paths.iter().map(|s| s.as_str()).collect::<Vec<_>>();
                    pgn::metrics::run_binpack_metrics(
                        &path_refs,
                        lineage_keys_path.as_deref(),
                        &out_prefix,
                    )
                }
                (Some(_), false) => panic!("Pass exactly one of --fen / --binpack, not both"),
                (None, true) => panic!("Expected --fen <file> or --binpack <file> (repeatable)"),
            }
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

            // chess_ui()
            Ok(())
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

            let uci_context = uci::uci::create_context();

            let tables = tables::Tables::new();
            let time_manager = SyncUnsafeCell::new(search::timeman::TimeManager::new());

            let result = std::thread::scope(|s| {
                let st = s.spawn(|| {
                    if let Some(core_id) = pin_core_id {
                        core_affinity::set_for_current(core_affinity::CoreId { id: core_id });
                        println!("Pinned search thread to core {}", core_id);
                    }

                    search_thread(uci_context.clone(), rx_search, &time_manager, &tables);
                });

                let result = chess_uci(uci_context.clone(), tx_search, &time_manager, &tables);

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
