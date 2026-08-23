use std::cell::SyncUnsafeCell;
use std::collections::VecDeque;
use std::fmt::Write as FmtWrite;
use std::fs::File;
use std::io::Write;
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use rand::{Rng, SeedableRng};
use sfbinpack::chess::r#move::MoveType;
use sfbinpack::chess::piecetype::PieceType;

use crate::engine::chess_v2::ChessGame;
use crate::engine::search::search::Search;
use crate::engine::search::{
    EngineForm, SearchStrategy, repetition, stability, timeman, transposition,
};
use crate::engine::tables::Tables;
use crate::util;

const STUDY_TT_MB: usize = 32;

pub struct LabelStudyParams {
    pub count: usize,
    pub seed: u64,
    pub min_ply: u16,
    pub max_abs_score: i32,
    pub fixed_depth: u8,
    pub ref_depth: u8,
    pub soft_budgets: Vec<u64>,
    pub max_scan: usize,
    pub threads: usize,

    pub training_filters: bool,
    pub ref_max_nodes: u64,
    pub warm_tt: bool,
    pub soft_max_depth: u8,
    pub warm_tt_mb: usize,
    pub warmup_plies: usize,
    pub print_sample: bool,
    pub fen_file: Option<String>,
    pub only_idx: Vec<usize>,
    pub hard_node_cap: u64,
    pub core_list: Vec<usize>,
}

struct Sampled {
    fen: String,
    ply: u16,
    prefix: Vec<String>,
}

struct ArmResult {
    score_cp: i32,
    winprob: f64,
    nodes: u64,
    completed_depth: u8,
    instab: Option<f64>,
}

fn sample_positions(
    binpacks: &[&str],
    p: &LabelStudyParams,
    tables: &Tables,
) -> anyhow::Result<Vec<Sampled>> {
    let mut rng = rand::rngs::StdRng::seed_from_u64(p.seed);
    let mut board = ChessGame::new();

    let mut reservoir: Vec<Sampled> = Vec::with_capacity(p.count);
    let mut qualifying = 0usize;
    let mut scanned = 0usize;

    let mut game_window: VecDeque<String> = VecDeque::new();

    'outer: for binpack in binpacks {
        let mut reader = sfbinpack::CompressedTrainingDataEntryReader::new(binpack)
            .map_err(|e| anyhow::anyhow!("failed to open binpack {}: {}", binpack, e))?;

        // A game never spans part files, so each part starts a fresh game.
        let mut prev_entry: Option<sfbinpack::TrainingDataEntry> = None;

        while reader.has_next() {
            let entry = reader.next();
            scanned += 1;
            if p.max_scan != 0 && scanned > p.max_scan {
                break 'outer;
            }

            if p.warm_tt {
                let is_game_start = match &prev_entry {
                    None => true,
                    Some(prev) => !prev.is_continuation(&entry),
                };
                if is_game_start {
                    game_window.clear();
                }
            }

            let passes_entry_filters = entry.ply >= p.min_ply
                && (entry.score as i32).abs() <= p.max_abs_score
                && (!p.training_filters
                    || (entry.mv.mtype() == MoveType::Normal
                        && entry.pos.piece_at(entry.mv.to()).piece_type() == PieceType::None));

            // The FEN is needed for the in-check filter, and under warm_tt for the window too.
            if passes_entry_filters || p.warm_tt {
                let fen = entry.pos.fen();

                let eligible = passes_entry_filters
                    && board.load_fen(&fen, tables).is_ok()
                    && !board.in_check(tables, board.b_move());

                if eligible {
                    qualifying += 1;
                    let slot = if reservoir.len() < p.count {
                        Some(reservoir.len())
                    } else {
                        let j = rng.random_range(0..qualifying);
                        (j < p.count).then_some(j)
                    };
                    if let Some(slot) = slot {
                        let sampled = Sampled {
                            fen: fen.clone(),
                            ply: entry.ply,
                            prefix: if p.warm_tt {
                                game_window.iter().cloned().collect()
                            } else {
                                Vec::new()
                            },
                        };
                        if slot == reservoir.len() {
                            reservoir.push(sampled);
                        } else {
                            reservoir[slot] = sampled;
                        }
                    }
                }

                if p.warm_tt {
                    game_window.push_back(fen);
                    while game_window.len() > p.warmup_plies {
                        game_window.pop_front();
                    }
                }
            }

            if p.warm_tt {
                prev_entry = Some(entry);
            }
        }
    }

    println!(
        "scanned {} entries, {} qualifying, sampled {}",
        scanned,
        qualifying,
        reservoir.len()
    );
    Ok(reservoir)
}

fn load_fens_from_file(path: &str) -> anyhow::Result<Vec<Sampled>> {
    let text = std::fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("failed to read fen file {}: {}", path, e))?;
    let mut out = Vec::new();
    for line in text.lines() {
        let fen = line.trim();
        if !fen.is_empty() {
            out.push(Sampled {
                ply: ply_from_fen(fen),
                fen: fen.to_string(),
                prefix: Vec::new(),
            });
        }
    }
    Ok(out)
}

fn ply_from_fen(fen: &str) -> u16 {
    let mut it = fen.split_whitespace();
    let stm = it.nth(1).unwrap_or("w");
    let fullmove: u32 = it.nth(3).and_then(|s| s.parse().ok()).unwrap_or(1);
    (2 * fullmove.saturating_sub(1) + (stm == "b") as u32).min(u16::MAX as u32) as u16
}

fn label(
    search: &mut Search<'_, { EngineForm::TacticalB }>,
    s: &Sampled,
    tables: &Tables,
    depth: Option<u8>,
    soft_nodes: u64,
    max_nodes: u64,
    warm_prefix: bool,
) -> ArmResult {
    search.new_game();

    if warm_prefix {
        for prefix_fen in &s.prefix {
            search.load_from_fen(prefix_fen, tables).unwrap();
            search.new_search();
            search.tm_mut().set_soft_nodes(soft_nodes);
            search.tm_mut().set_nodes(max_nodes);
            search.search(depth);
        }
    }

    search.load_from_fen(&s.fen, tables).unwrap();
    search.new_search();

    search.tm_mut().set_soft_nodes(soft_nodes);
    search.tm_mut().set_nodes(max_nodes);
    search.search(depth);

    let score_cp = search.search_score();
    let stats = search.depth_stats();
    ArmResult {
        score_cp,
        winprob: stability::win_prob(score_cp),
        nodes: search.num_nodes_searched(),
        completed_depth: stats.last().map(|s| s.depth).unwrap_or(0),
        instab: stability::winprob_instability(stats),
    }
}

fn write_row(
    out: &mut String,
    idx: usize,
    ply: u16,
    instab_d8: f64,
    arm: &str,
    budget: u64,
    r: &ArmResult,
    fen: &str,
) {
    let _ = writeln!(
        out,
        "{},{},{:.8},{},{},{},{:.6},{},{},{}",
        idx, ply, instab_d8, arm, budget, r.score_cp, r.winprob, r.nodes, r.completed_depth, fen
    );
}

fn label_position(
    search_meas: &mut Search<'_, { EngineForm::TacticalB }>,
    search_ref: &mut Search<'_, { EngineForm::TacticalB }>,
    tables: &Tables,
    p: &LabelStudyParams,
    idx: usize,
    s: &Sampled,
    depth_arm: &str,
    ref_arm: &str,
) -> (String, bool, usize) {
    let mut rows = String::new();

    let d_fixed = label(
        search_meas,
        s,
        tables,
        Some(p.fixed_depth),
        0,
        p.hard_node_cap,
        p.warm_tt,
    );
    let instab_d8 = d_fixed.instab.unwrap_or(f64::NAN);
    write_row(
        &mut rows,
        idx,
        s.ply,
        instab_d8,
        depth_arm,
        p.fixed_depth as u64,
        &d_fixed,
        &s.fen,
    );

    // Soft arms run to the depth backstop unless a ceiling is configured (soft_max_depth).
    let soft_target = (p.soft_max_depth > 0).then_some(p.soft_max_depth);
    let mut soft_capped = 0usize;
    for &b in &p.soft_budgets {
        let r = label(
            search_meas,
            s,
            tables,
            soft_target,
            b,
            p.hard_node_cap,
            p.warm_tt,
        );
        soft_capped += (p.soft_max_depth > 0 && r.completed_depth >= p.soft_max_depth) as usize;
        write_row(&mut rows, idx, s.ply, instab_d8, "soft", b, &r, &s.fen);
    }

    // Reference: always cold and never prefix-warmed -> identical across arms.
    let r_ref = label(
        search_ref,
        s,
        tables,
        Some(p.ref_depth),
        0,
        p.ref_max_nodes,
        false,
    );
    write_row(
        &mut rows,
        idx,
        s.ply,
        instab_d8,
        ref_arm,
        p.ref_depth as u64,
        &r_ref,
        &s.fen,
    );

    (rows, r_ref.completed_depth < p.ref_depth, soft_capped)
}

pub fn run_label_study(
    binpacks: &[&str],
    out_csv: &str,
    params: LabelStudyParams,
) -> anyhow::Result<()> {
    let tables = Tables::new();

    let sampled = match &params.fen_file {
        Some(path) => {
            println!(
                "loading FENs from {} (single-position lab; cold, empty prefix)...",
                path
            );
            load_fens_from_file(path)?
        }
        None => {
            println!(
                "sampling up to {} positions from {} binpack(s) (min_ply {}, |score| <= {}, \
                 training_filters {})...",
                params.count,
                binpacks.len(),
                params.min_ply,
                params.max_abs_score,
                if params.training_filters {
                    "on: move Normal + non-capture, matching training.rs"
                } else {
                    "off: move-selection population"
                }
            );
            sample_positions(binpacks, &params, &tables)?
        }
    };
    if sampled.is_empty() {
        return Err(anyhow::anyhow!("no positions to label"));
    }

    if params.print_sample {
        for (idx, s) in sampled.iter().enumerate() {
            println!("{}\t{}\t{}", idx, s.ply, s.fen);
        }
        return Ok(());
    }

    if let Some(parent) = std::path::Path::new(out_csv).parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent)?;
        }
    }

    let depth_arm = format!("depth{}", params.fixed_depth);
    let ref_arm = format!("ref{}", params.ref_depth);
    let threads = if params.core_list.is_empty() {
        params.threads.max(1)
    } else {
        params.core_list.len()
    };

    let worklist: Vec<usize> = if params.only_idx.is_empty() {
        (0..sampled.len()).collect()
    } else {
        let mut w: Vec<usize> = params
            .only_idx
            .iter()
            .copied()
            .filter(|&i| i < sampled.len())
            .collect();
        w.sort_unstable();
        w.dedup();
        if w.len() != params.only_idx.len() {
            println!(
                "WARNING: {} --only-idx value(s) were out of range (>= {}) or duplicated and dropped",
                params.only_idx.len() - w.len(),
                sampled.len()
            );
        }
        w
    };
    if worklist.is_empty() {
        return Err(anyhow::anyhow!(
            "no positions to label (--only-idx filtered everything out)"
        ));
    }

    let next_idx = AtomicUsize::new(0);
    let completed = AtomicUsize::new(0);
    let truncated = AtomicUsize::new(0);
    let soft_capped = AtomicUsize::new(0);
    let collected: Mutex<Vec<(usize, String)>> = Mutex::new(Vec::with_capacity(sampled.len()));

    let live: Mutex<std::io::BufWriter<File>> =
        Mutex::new(std::io::BufWriter::new(File::create(out_csv)?));
    writeln!(
        live.lock().unwrap(),
        "idx,ply,instab_d8,arm,budget,score_cp,winprob,nodes,completed_depth,fen"
    )?;

    println!(
        "labeling {} positions on {} thread(s)...",
        worklist.len(),
        threads
    );
    if params.warm_tt {
        let mean_prefix =
            sampled.iter().map(|s| s.prefix.len()).sum::<usize>() as f64 / sampled.len() as f64;
        println!(
            "warm TT: measured arms use a {} MB table cleared once per game and retained across \
             moves; mean replayed prefix {:.1} plies (cap {}). Reference stays cold at {} MB.",
            params.warm_tt_mb, mean_prefix, params.warmup_plies, STUDY_TT_MB
        );
    }
    if params.ref_max_nodes > 0 {
        println!(
            "reference arm capped at {} nodes; truncated refs are reported at the end",
            params.ref_max_nodes
        );
    }
    let st = std::time::Instant::now();

    std::thread::scope(|scope| {
        for t in 0..threads {
            let (next_idx, completed, collected) = (&next_idx, &completed, &collected);
            let (truncated, soft_capped, live) = (&truncated, &soft_capped, &live);
            let (sampled, params, tables) = (&sampled, &params, &tables);
            let (depth_arm, ref_arm) = (depth_arm.as_str(), ref_arm.as_str());
            let worklist = &worklist;

            scope.spawn(move || {
                if !params.core_list.is_empty() {
                    util::pin_thread_to(params.core_list[t]);
                } else if threads > 1 {
                    util::pin_thread_for_worker(t);
                }

                let meas_mb = if params.warm_tt {
                    params.warm_tt_mb
                } else {
                    STUDY_TT_MB
                };
                let tt_meas = SyncUnsafeCell::new(transposition::TranspositionTable::new(meas_mb));
                let mut tm_meas = SyncUnsafeCell::new(timeman::TimeManager::new());
                tm_meas.get_mut().disable();
                let mut search_meas = Search::<{ EngineForm::TacticalB }>::new(
                    tables,
                    &tt_meas,
                    &tm_meas,
                    repetition::RepetitionTable::new(),
                );

                let tt_ref =
                    SyncUnsafeCell::new(transposition::TranspositionTable::new(STUDY_TT_MB));
                let mut tm_ref = SyncUnsafeCell::new(timeman::TimeManager::new());
                tm_ref.get_mut().disable();
                let mut search_ref = Search::<{ EngineForm::TacticalB }>::new(
                    tables,
                    &tt_ref,
                    &tm_ref,
                    repetition::RepetitionTable::new(),
                );

                let mut local: Vec<(usize, String)> = Vec::new();

                loop {
                    let work_i = next_idx.fetch_add(1, Ordering::Relaxed);
                    if work_i >= worklist.len() {
                        break;
                    }
                    let idx = worklist[work_i];

                    let (rows, ref_truncated, capped) = label_position(
                        &mut search_meas,
                        &mut search_ref,
                        tables,
                        params,
                        idx,
                        &sampled[idx],
                        depth_arm,
                        ref_arm,
                    );
                    let _ = live.lock().unwrap().write_all(rows.as_bytes());
                    local.push((idx, rows));
                    if ref_truncated {
                        truncated.fetch_add(1, Ordering::Relaxed);
                    }
                    soft_capped.fetch_add(capped, Ordering::Relaxed);

                    let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
                    if done % 100 == 0 {
                        let rate = done as f64 / st.elapsed().as_secs_f64();
                        println!("labeled {}/{} ({:.1} pos/s)", done, worklist.len(), rate);
                    }
                }

                collected.lock().unwrap().extend(local);
            });
        }
    });

    live.into_inner().unwrap().flush()?;
    let mut collected = collected.into_inner().unwrap();
    collected.sort_unstable_by_key(|(idx, _)| *idx);

    let mut out = File::create(out_csv)?;
    writeln!(
        out,
        "idx,ply,instab_d8,arm,budget,score_cp,winprob,nodes,completed_depth,fen"
    )?;
    for (_, rows) in &collected {
        out.write_all(rows.as_bytes())?;
    }

    let soft_capped = soft_capped.load(Ordering::Relaxed);
    if params.soft_max_depth > 0 {
        let soft_total = worklist.len() * params.soft_budgets.len();
        println!(
            "soft depth ceiling {}: bound on {}/{} soft labels ({:.1}%). Where it binds the arm is a \
             hybrid (depth {} OR budget), not pure fixed-nodes — if this is high the node budget is \
             inert and the arm is really measuring depth {}.",
            params.soft_max_depth,
            soft_capped,
            soft_total,
            soft_capped as f64 / soft_total.max(1) as f64 * 100.0,
            params.soft_max_depth,
            params.soft_max_depth
        );
    }

    let truncated = truncated.load(Ordering::Relaxed);
    if truncated > 0 {
        println!(
            "WARNING: {}/{} references ({:.2}%) hit the {}-node cap and did not reach depth {} — \
             their rows carry completed_depth < {}. Truncation correlates with material, so check \
             the low-material strata before trusting them.",
            truncated,
            worklist.len(),
            truncated as f64 / worklist.len() as f64 * 100.0,
            params.ref_max_nodes,
            params.ref_depth,
            params.ref_depth
        );
    }

    println!(
        "wrote {} ({:?}, {} thread(s))",
        out_csv,
        st.elapsed(),
        threads
    );
    Ok(())
}
