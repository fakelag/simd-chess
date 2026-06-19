use std::cell::SyncUnsafeCell;

use crossbeam::channel;

use crate::{
    engine::{
        chess_v2,
        search::{self, search_params, timeman},
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
    pub chess: chess_v2::ChessGame,
    pub repetition_table: search::repetition::RepetitionTable,
    pub limits: Option<timeman::TimeLimits>,
}

pub enum UciCommand {
    Go(GoCommand),
    OptionChange(UciOptions),
    Ping,
    NewGame,
}

pub fn chess_uci(
    uci_context: UciContext<UciOptions>,
    tx_search: channel::Sender<UciCommand>,
    tm: &SyncUnsafeCell<timeman::TimeManager>,
    tables: &tables::Tables,
) -> anyhow::Result<()> {
    let mut debug_enabled = true;

    let mut game_board: Option<chess_v2::ChessGame> = None;
    let mut repetition_table: Option<search::repetition::RepetitionTable> = None;

    let sync_stop = || {
        let mut pings = 0;
        unsafe { &mut *tm.get() }.stop();
        loop {
            match tx_search.try_send(UciCommand::Ping) {
                Ok(_) => pings += 1,
                Err(channel::TrySendError::Full(_)) => unsafe { &mut *tm.get() }.stop(),
                Err(channel::TrySendError::Disconnected(_)) => break,
            }
            if pings > 1 {
                break;
            }
        }
    };

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
                tx_search.send(UciCommand::NewGame)?;
            }
            Some("stop") => sync_stop(),
            Some("isready") => println!("readyok"),
            Some("position") => {
                let mut board = chess_v2::ChessGame::new();
                let mut rep_table = search::repetition::RepetitionTable::new();

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

                let chess = game_board
                    .take()
                    .expect("Expected position command to be sent before go");

                let mut search_params = search_params::SearchParams::from_iter(input);
                search_params.debug = debug_enabled;

                let limits = timeman::compute_limits(&search_params, chess.b_move());

                tx_search.send(UciCommand::Go(GoCommand {
                    start_time,
                    params: search_params,
                    chess,
                    repetition_table: repetition_table
                        .take()
                        .expect("Expected position command to be sent before go"),
                    limits,
                }))?;

                if let Some(limits) = limits
                    && debug_enabled
                {
                    println!(
                        "info string soft {} hard {}",
                        limits.soft_ms, limits.hard_ms
                    );
                }
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
            Some("quit") => {
                sync_stop();
                break;
            }
            Some(arg) => return Err(anyhow::anyhow!("Unknown command: \"{}\"", arg)),
            None => return Err(anyhow::anyhow!("No command provided")),
        }
    }

    Ok(())
}
