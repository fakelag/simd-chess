use std::collections::HashSet;
use std::sync::Mutex;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

use crossbeam::channel::Sender;
use rand::{Rng, SeedableRng};

use crate::{
    engine::{
        chess_v2::ChessGame,
        search::{
            EngineForm, SearchStrategy, eval::WEIGHT_TABLE_ABS, repetition, search::Search, see,
            stability, timeman, transposition,
        },
        tables,
    },
    pgn::fen_shard::ShardedFenWriter,
    tools::tuner,
    util,
};

const TT_SIZE_MB: usize = 16;
const CALIBRATION_SAMPLES: usize = 10_000;

pub struct GenOpeningsParams {
    pub count: usize,
    pub plies: usize,
    pub seed: u64,
    pub see_floor: Option<i16>,
    pub eval_bound: i32,
    pub screen_depth: u8,
    pub out_shard_size: usize,
    pub threads: usize,
    pub sharpness_top_percent: Option<f64>,
}

fn legal_moves_screened(
    board: &ChessGame,
    tables: &tables::Tables,
    see_floor: Option<i16>,
) -> Vec<u16> {
    let mut move_list = [0u16; 256];
    let n = board.gen_moves_avx512::<false, _>(&mut move_list);

    let mut legal: Vec<u16> = Vec::new();
    for mv in &move_list[..n] {
        let mut copy = board.clone();
        if unsafe { copy.make_move(*mv, tables) } && !copy.in_check(tables, !copy.b_move()) {
            legal.push(*mv);
        }
    }

    let Some(floor) = see_floor else {
        return legal;
    };

    let bitboards = board.bitboards();
    let black_board = bitboards.iter().skip(8).fold(0, |acc, &bb| acc | bb);
    let white_board = bitboards.iter().take(8).fold(0, |acc, &bb| acc | bb);

    let piece_board = unsafe {
        use std::arch::x86_64::*;
        _mm512_or_epi64(
            _mm512_loadu_epi64(bitboards.as_ptr() as *const i64),
            _mm512_loadu_epi64(bitboards.as_ptr().add(8) as *const i64),
        )
    };
    let pins = [
        see::calc_pinnings(false, board, black_board, white_board),
        see::calc_pinnings(true, board, black_board, white_board),
    ];

    let filtered: Vec<u16> = legal
        .iter()
        .copied()
        .filter(|&mv| {
            see::see_threshold(
                &WEIGHT_TABLE_ABS,
                tables,
                board,
                mv,
                floor,
                black_board,
                white_board,
                piece_board,
                Some(&pins),
            )
        })
        .collect();

    if filtered.is_empty() { legal } else { filtered }
}

fn play_random_plies(
    board: &mut ChessGame,
    tables: &tables::Tables,
    rng: &mut rand::rngs::StdRng,
    k: usize,
    see_floor: Option<i16>,
) {
    for _ in 0..k {
        let legal = legal_moves_screened(board, tables, see_floor);
        if legal.is_empty() {
            break;
        }
        let mv = legal[rng.random_range(0..legal.len())];
        unsafe { board.make_move(mv, tables) };
    }
}

#[derive(Default)]
struct GenStats {
    accepted: AtomicUsize,
    attempts: AtomicU64,
    rej_dedup: AtomicU64,
    rej_eval_bound: AtomicU64,
    rej_sharpness: AtomicU64,
}

fn calibrate_sharpness_cut(
    params: &GenOpeningsParams,
    tables: &tables::Tables,
    top_percent: f64,
) -> anyhow::Result<f64> {
    println!("Calibrating sharpness for top {:.2}%...", top_percent);

    let mut samples = Vec::with_capacity(CALIBRATION_SAMPLES);
    for i in 0..CALIBRATION_SAMPLES as u64 {
        let mut rng = rand::rngs::StdRng::seed_from_u64(params.seed ^ 0x5ca1ab1e ^ i);
        let mut board = ChessGame::new();
        board.load_fen(util::FEN_STARTPOS, tables).unwrap();
        let k = params.plies + rng.random_range(0..=1);
        play_random_plies(&mut board, tables, &mut rng, k, params.see_floor);
        samples.push(board);
    }

    let mut dist = tuner::instability_distribution(
        samples.into_iter(),
        TT_SIZE_MB,
        params.screen_depth,
        tables,
    );
    if dist.is_empty() {
        return Err(anyhow::anyhow!(
            "genopenings: sharpness calibration produced no instability samples"
        ));
    }
    tuner::sort_ascending(&mut dist);
    let cut = tuner::top_percent_cut(&dist, top_percent);

    println!(
        "Sharpness calibration complete: keeping top {:.2}% of {} candidates at depth {} -> raw instability cut {:.6}",
        top_percent,
        dist.len(),
        params.screen_depth,
        cut
    );

    Ok(cut)
}

pub fn run_genopenings(out_path: &str, params: GenOpeningsParams) -> anyhow::Result<()> {
    let tables = tables::Tables::new();

    let sharpness_cut = match params.sharpness_top_percent {
        Some(top_percent) => Some(calibrate_sharpness_cut(&params, &tables, top_percent)?),
        None => None,
    };

    let mut writer = ShardedFenWriter::new(out_path, params.out_shard_size)?;
    let dedup: Mutex<HashSet<u64>> = Mutex::new(HashSet::new());
    let stats = GenStats::default();

    let max_attempts = (params.count.saturating_mul(1000).max(100_000)) as u64;

    let n_threads = params.threads.max(1);
    let (tx_fen, rx_fen) = crossbeam::channel::bounded::<String>(256);
    let start_time = std::time::Instant::now();

    let params = &params;
    let tables = &tables;
    let dedup = &dedup;
    let stats = &stats;

    std::thread::scope(|scope| -> anyhow::Result<()> {
        for i in 0..n_threads {
            let tx_fen = tx_fen.clone();
            scope.spawn(move || {
                util::pin_thread_for_worker(i);
                worker(
                    params,
                    tables,
                    dedup,
                    stats,
                    max_attempts,
                    sharpness_cut,
                    &tx_fen,
                );
            });
        }
        drop(tx_fen);

        let mut last_log_time = start_time;
        let mut last_accepted = 0usize;
        let mut last_attempts = 0u64;
        while let Ok(fen) = rx_fen.recv() {
            if stats.accepted.load(Ordering::Relaxed) >= params.count {
                break;
            }
            writer.write_line(&fen)?;
            let accepted = stats.accepted.fetch_add(1, Ordering::Relaxed) + 1;

            if last_log_time.elapsed().as_secs() >= 60 {
                let now = std::time::Instant::now();
                let interval = now.duration_since(last_log_time).as_secs_f64();
                let attempts = stats.attempts.load(Ordering::Relaxed);

                let acc_rate = (accepted - last_accepted) as f64 / interval;
                let att_rate = (attempts - last_attempts) as f64 / interval;

                let [sharp_pct, eval_pct, dup_pct, acceptance_pct] = if attempts > 0 {
                    [
                        stats.rej_sharpness.load(Ordering::Relaxed) as f64,
                        stats.rej_eval_bound.load(Ordering::Relaxed) as f64,
                        stats.rej_dedup.load(Ordering::Relaxed) as f64,
                        accepted as f64,
                    ]
                    .map(|c| c / attempts as f64 * 100.0)
                } else {
                    [0.0; 4]
                };

                let remaining = params.count.saturating_sub(accepted);
                let eta = if acc_rate > 0.0 {
                    util::time_format((remaining as f64 / acc_rate * 1000.0) as u64)
                } else {
                    "?".to_string()
                };

                println!(
                    "Generating {}/{} ({:.2}%) | {:.0} acc/s | {:.0} att/s | sharp-rej {:.2}% | eval-rej {:.2}% | dup-rej {:.2}% | acceptance {:.2}% | ETA {} | elapsed {}",
                    accepted,
                    params.count,
                    accepted as f64 / params.count as f64 * 100.0,
                    acc_rate,
                    att_rate,
                    sharp_pct,
                    eval_pct,
                    dup_pct,
                    acceptance_pct,
                    eta,
                    util::time_format(now.duration_since(start_time).as_millis() as u64),
                );

                last_log_time = now;
                last_accepted = accepted;
                last_attempts = attempts;
            }
        }

        writer.finish()?;
        drop(rx_fen);
        Ok(())
    })?;

    let accepted = stats.accepted.load(Ordering::Relaxed);
    if accepted < params.count {
        return Err(anyhow::anyhow!(
            "genopenings: {} attempts without reaching --count {} (screens too strict?)",
            stats.attempts.load(Ordering::Relaxed),
            params.count
        ));
    }

    println!(
        "Generated {} openings from {} attempts (dedup-rejected {}, eval-bound-rejected {}, sharpness-rejected {})",
        accepted,
        stats.attempts.load(Ordering::Relaxed),
        stats.rej_dedup.load(Ordering::Relaxed),
        stats.rej_eval_bound.load(Ordering::Relaxed),
        stats.rej_sharpness.load(Ordering::Relaxed),
    );

    Ok(())
}

fn worker(
    params: &GenOpeningsParams,
    tables: &tables::Tables,
    dedup: &Mutex<HashSet<u64>>,
    stats: &GenStats,
    max_attempts: u64,
    sharpness_cut: Option<f64>,
    tx_fen: &Sender<String>,
) {
    let tt = std::cell::SyncUnsafeCell::new(transposition::TranspositionTable::new(TT_SIZE_MB));
    let mut tm = std::cell::SyncUnsafeCell::new(timeman::TimeManager::new());
    tm.get_mut().disable();
    let mut search = Search::<{ EngineForm::TacticalB }>::new(
        tables,
        &tt,
        &tm,
        repetition::RepetitionTable::new(),
    );

    while stats.accepted.load(Ordering::Relaxed) < params.count {
        let attempt = stats.attempts.fetch_add(1, Ordering::Relaxed);
        if attempt >= max_attempts {
            break;
        }

        let mut rng = rand::rngs::StdRng::seed_from_u64(params.seed ^ attempt);

        let mut board = ChessGame::new();
        board.load_fen(util::FEN_STARTPOS, tables).unwrap();

        let k = params.plies + rng.random_range(0..=1);
        play_random_plies(&mut board, tables, &mut rng, k, params.see_floor);

        let key = board.canonical_seed_key(tables);
        if dedup.lock().unwrap().contains(&key) {
            stats.rej_dedup.fetch_add(1, Ordering::Relaxed);
            continue;
        }

        search.new_game();
        search.load_from_board(&board);
        search.new_search();
        search.search(Some(params.screen_depth));
        let score = search.search_score();

        if let Some(cut) = sharpness_cut {
            let instability = stability::winprob_instability(search.depth_stats());
            if !instability.is_some_and(|i| i >= cut) {
                stats.rej_sharpness.fetch_add(1, Ordering::Relaxed);
                continue;
            }
        }

        if score.abs() > params.eval_bound {
            stats.rej_eval_bound.fetch_add(1, Ordering::Relaxed);
            continue;
        }

        if !dedup.lock().unwrap().insert(key) {
            stats.rej_dedup.fetch_add(1, Ordering::Relaxed);
            continue;
        }

        if tx_fen.send(board.gen_fen()).is_err() {
            break;
        }
    }
}
