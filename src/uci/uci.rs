use std::{cell::SyncUnsafeCell, thread::JoinHandle};

use crossbeam::channel;

use crate::{
    engine::{
        chess,
        search::{self, AbortSignal, SigAbort, search_params, transposition_v2},
        tables,
    },
    uci::{context::UciContext, option::*},
    util,
};

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum UciOptions {
    OwnBook,
    OwnBookPath,
}

pub fn create_context() -> UciContext<UciOptions> {
    let uci_context = UciContext::<UciOptions>::new();

    uci_context.lock_add(
        UciOptions::OwnBook,
        Box::new(UciOptionType::<bool>::new("OwnBook", false)),
    );

    uci_context.lock_add(
        UciOptions::OwnBookPath,
        Box::new(UciOptionType::<FilePathString>::new(
            "OwnBookPath",
            "openings/8moves_v3.pgn".to_string(),
        )),
    );

    uci_context
}

pub struct GoCommand {
    pub start_time: std::time::Instant,
    pub params: search_params::SearchParams,
    pub chess: chess::ChessGame,
    pub sig: AbortSignal,
    pub repetition_table: search::repetition_v2::RepetitionTable,
}

pub enum UciCommand {
    Go(GoCommand),
    OptionChange(UciOptions),
    NewGame,
}

pub fn chess_uci(
    uci_context: UciContext<UciOptions>,
    tx_search: channel::Sender<UciCommand>,
    tables: &tables::Tables,
    tt: &SyncUnsafeCell<transposition_v2::TranspositionTable>,
) -> anyhow::Result<()> {
    let mut debug_enabled = true;

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

    loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer)?;

        let mut input = buffer.split_whitespace();

        match input.next() {
            Some("uci") => {
                println!("id name simd-chess");
                for option in uci_context.lock().iter() {
                    option.print();
                }
                println!("uciok");
            }
            Some("debug") => match input.next() {
                Some("on") => debug_enabled = true,
                Some("off") => debug_enabled = false,
                _ => panic!("Expected 'on' or 'off' after debug command"),
            },
            Some("ucinewgame") => {
                // Safety: Engine should not be calculating when receiving a ucinewgame command
                unsafe { &mut *tt.get() }.clear();

                // @todo - Clear TT on the other end
                tx_search.send(UciCommand::NewGame)?;
            }
            Some("stop") => abort_search_uci(debug_enabled, &mut context),
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

                abort_search_uci(debug_enabled, &mut context);

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
                search_params.debug = debug_enabled;

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

                tx_search.send(UciCommand::Go(GoCommand {
                    start_time,
                    params: search_params,
                    chess,
                    repetition_table: repetition_table
                        .take()
                        .expect("Expected position command to be sent before go"),
                    sig: rx_abort,
                }))?;

                if !infinite {
                    let movetime_ms = if let Some(movetime) = movetime {
                        Some(movetime)
                    } else {
                        // base / 20 + inc / 2
                        time.and_then(|t| Some(t / 20 + inc.unwrap_or(0) / 2))
                    };

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

                        if debug_enabled {
                            println!("info movetime ms {}", movetime_ms);
                        }
                    }
                }

                context = Some(new_context);
            }
            Some("setoption") => {
                if input.next() != Some("name") {
                    return Err(anyhow::anyhow!("Expected 'name' after setoption"));
                }

                let opt_name = input
                    .next()
                    .expect("Expected option name after setoption name ...");

                let mut lock = uci_context.lock();
                let option = lock.get_by_name_mut(opt_name);

                let (oid, option) = match option {
                    Some(val) => val,
                    None => {
                        return Err(anyhow::anyhow!(
                            "Option with name \"{}\" not found",
                            opt_name
                        ));
                    }
                };

                match input.next() {
                    Some("value") => {
                        let opt_value = match input.next() {
                            Some(v) => v,
                            None => {
                                return Err(anyhow::anyhow!(
                                    "Expected value after setoption name {} value",
                                    opt_name
                                ));
                            }
                        };

                        option.set_value_from_str(opt_value)?;
                    }
                    Some(v) => {
                        return Err(anyhow::anyhow!(
                            "Expected 'value' after setoption name {}, got \"{}\"",
                            opt_name,
                            v
                        ));
                    }
                    None => {
                        return Err(anyhow::anyhow!(
                            "Expected 'value' after setoption name {}",
                            opt_name
                        ));
                    }
                }

                drop(lock);

                tx_search.send(UciCommand::OptionChange(oid))?;
            }
            Some("quit") => break,
            Some(arg) => return Err(anyhow::anyhow!("Unknown command: \"{}\"", arg)),
            None => return Err(anyhow::anyhow!("No command provided")),
        }
    }

    if debug_enabled {
        println!("info exiting UCI mode");
    }

    abort_search_uci(debug_enabled, &mut context);

    Ok(())
}
