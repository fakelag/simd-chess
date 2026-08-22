use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::sync::Mutex;

use sfbinpack::TrainingDataEntry;
use sfbinpack::chess::castling_rights::CastlingRights;
use sfbinpack::chess::color::Color;
use sfbinpack::chess::coords::Square;
use sfbinpack::chess::piecetype::PieceType;
use sfbinpack::chess::position::Position;
use sfbinpack::chess::r#move::MoveType;

use crate::engine::chess_v2::{ChessGame, PieceIndex};
use crate::engine::search::search::NET_OSIZE;
use crate::engine::tables::Tables;

fn output_bucket(piece_count: u32) -> usize {
    if NET_OSIZE == 1 {
        return 0;
    }
    let divisor = 32usize.div_ceil(NET_OSIZE);
    ((piece_count.saturating_sub(2) as usize) / divisor).min(NET_OSIZE - 1)
}

const FORM_LOW: u8 = 0b01; // own_key == canonical
const FORM_HIGH: u8 = 0b10; // own_key is the mirror of the canonical

#[derive(Default)]
struct UniquenessTally {
    total: u64,
    parsed: u64,
    parse_failures: u64,
    unique: u64,
    mirror_hits: u64,
    exact_dups: u64,
}

impl UniquenessTally {
    fn observe(&mut self, canonical: u64, own_key: u64, seen: &mut HashMap<u64, u8>) -> bool {
        self.parsed += 1;
        let form = if own_key == canonical {
            FORM_LOW
        } else {
            FORM_HIGH
        };
        let mask = seen.entry(canonical).or_insert(0);

        if *mask == 0 {
            *mask = form;
            self.unique += 1;
            true
        } else {
            if *mask & form != 0 {
                self.exact_dups += 1;
            } else {
                self.mirror_hits += 1;
                *mask |= form;
            }
            false
        }
    }

    fn print(&self, label: &str) {
        let pct = |n: u64| {
            if self.parsed == 0 {
                0.0
            } else {
                n as f64 / self.parsed as f64 * 100.0
            }
        };
        println!("--- {label} ---");
        println!("total lines/entries : {}", self.total);
        println!("parse failures      : {}", self.parse_failures);
        println!("parsed positions    : {}", self.parsed);
        println!(
            "unique              : {} ({:.3}%)",
            self.unique,
            pct(self.unique)
        );
        println!(
            "mirror hits          : {} ({:.3}%)",
            self.mirror_hits,
            pct(self.mirror_hits)
        );
        println!(
            "exact duplicates    : {} ({:.3}%)",
            self.exact_dups,
            pct(self.exact_dups)
        );
    }

    fn write_summary_rows(&self, w: &mut impl Write) -> std::io::Result<()> {
        writeln!(w, "metric,value")?;
        writeln!(w, "total,{}", self.total)?;
        writeln!(w, "parse_failures,{}", self.parse_failures)?;
        writeln!(w, "parsed,{}", self.parsed)?;
        writeln!(w, "unique,{}", self.unique)?;
        writeln!(w, "mirror_hits,{}", self.mirror_hits)?;
        writeln!(w, "exact_dups,{}", self.exact_dups)?;
        Ok(())
    }
}

fn ensure_parent_dir(out_prefix: &str) -> std::io::Result<()> {
    if let Some(parent) = std::path::Path::new(out_prefix).parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent)?;
        }
    }
    Ok(())
}

pub fn run_fen_stats(fen_path: &str, out_prefix: &str) -> anyhow::Result<()> {
    let tables = Tables::new();
    let mut board = ChessGame::new();
    let mut seen_forms: HashMap<u64, u8> = HashMap::new();
    let mut tally = UniquenessTally::default();

    let file = File::open(fen_path)
        .map_err(|e| anyhow::anyhow!("failed to open FEN file {}: {}", fen_path, e))?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line?;
        let fen = line.trim();
        if fen.is_empty() {
            continue;
        }
        tally.total += 1;

        if board.load_fen(fen, &tables).is_err() {
            tally.parse_failures += 1;
            continue;
        }

        let (own_key, flipped) = board.canonical_seed_key_parts(&tables);
        tally.observe(own_key.min(flipped), own_key, &mut seen_forms);
    }

    tally.print("FEN seed-file uniqueness");

    ensure_parent_dir(out_prefix)?;
    let summary_path = format!("{out_prefix}_fen_summary.csv");
    let mut summary = File::create(&summary_path)?;
    tally.write_summary_rows(&mut summary)?;
    println!("wrote {summary_path}");

    Ok(())
}

const NSHARDS: usize = 1024;
const BATCH_TARGET: usize = 16 * 1024;
const FLAG_NONLINEAGE: u8 = 0b01;
const FLAG_LINEAGE: u8 = 0b10;

#[derive(Default)]
struct Shard {
    seen: HashSet<u64>,
    lineage_flags: HashMap<u64, u8>,
    unique: u64,
    mirror_hits: u64,
    exact_dups: u64,
}

impl Shard {
    fn observe(&mut self, own_key: u64, flipped: u64) {
        if !self.seen.insert(own_key) {
            self.exact_dups += 1;
        } else if own_key != flipped && self.seen.contains(&flipped) {
            self.mirror_hits += 1;
        } else {
            self.unique += 1;
        }
    }
}

fn loader_eligible(entry: &TrainingDataEntry) -> bool {
    crate::loader_filter!(entry, MoveType, PieceType)
}

const SCORE_BANDS: [i32; 6] = [50, 100, 200, 400, 800, i32::MAX];
const N_SCORE_BUCKETS: usize = SCORE_BANDS.len() * 2;

fn score_bucket(score: i32) -> usize {
    let mag = SCORE_BANDS.iter().position(|b| score.abs() < *b).unwrap();
    if score < 0 {
        SCORE_BANDS.len() - 1 - mag
    } else {
        SCORE_BANDS.len() + mag
    }
}

#[derive(Default)]
struct Locals {
    total: u64,
    games: u64,
    eligible: u64,
    bucket_counts: [u64; NET_OSIZE],
    ply_counts: Vec<u64>,
    result_all: [u64; 3],
    result_lineage: [u64; 3],
    wdl_abs_sum: f64,
    wdl_sq_sum: f64,
    score_result: [[u64; 3]; N_SCORE_BUCKETS],
}

impl Locals {
    fn bump_ply(&mut self, ply: usize) {
        if ply >= self.ply_counts.len() {
            self.ply_counts.resize(ply + 1, 0);
        }
        self.ply_counts[ply] += 1;
    }
}

struct Batch {
    entries: Vec<TrainingDataEntry>,
    game_starts: Vec<usize>,
}

fn board_from_sf(pos: &Position, tables: &Tables) -> ChessGame {
    let mut bb = [0u64; 16];
    for (color, off) in [(Color::White, 0usize), (Color::Black, 8usize)] {
        bb[PieceIndex::WhiteKing as usize + off] = pos.pieces_bb_color(color, PieceType::King).bits();
        bb[PieceIndex::WhiteQueen as usize + off] =
            pos.pieces_bb_color(color, PieceType::Queen).bits();
        bb[PieceIndex::WhiteRook as usize + off] = pos.pieces_bb_color(color, PieceType::Rook).bits();
        bb[PieceIndex::WhiteBishop as usize + off] =
            pos.pieces_bb_color(color, PieceType::Bishop).bits();
        bb[PieceIndex::WhiteKnight as usize + off] =
            pos.pieces_bb_color(color, PieceType::Knight).bits();
        bb[PieceIndex::WhitePawn as usize + off] = pos.pieces_bb_color(color, PieceType::Pawn).bits();
    }

    let cr = pos.castling_rights();
    let castles = ((cr.contains(CastlingRights::WHITE_KING_SIDE) as u8) << 3)
        | ((cr.contains(CastlingRights::WHITE_QUEEN_SIDE) as u8) << 2)
        | ((cr.contains(CastlingRights::BLACK_KING_SIDE) as u8) << 1)
        | (cr.contains(CastlingRights::BLACK_QUEEN_SIDE) as u8);

    let ep = pos.ep_square();
    let en_passant = if ep == Square::NONE { 0 } else { ep.index() as u8 };

    ChessGame::from_position_parts(
        bb,
        pos.side_to_move() == Color::Black,
        castles,
        en_passant,
        pos.rule50_counter() as u32,
        tables,
    )
}

pub fn run_binpack_metrics(
    binpack_paths: &[&str],
    lineage_keys_path: Option<&str>,
    out_prefix: &str,
    threads: Option<usize>,
    track_uniqueness: bool,
) -> anyhow::Result<()> {
    let tables = Tables::new();

    let lineage_keys: Option<HashSet<u64>> = match lineage_keys_path {
        Some(path) => {
            let (keys, failures) = load_canonical_key_set(path, &tables)?;
            println!(
                "loaded {} lineage canonical keys from {} ({} parse failures)",
                keys.len(),
                path,
                failures
            );
            Some(keys)
        }
        None => None,
    };

    let lineage_keys = lineage_keys.as_ref();

    let num_threads = threads
        .or_else(|| std::thread::available_parallelism().ok().map(|n| n.get()))
        .unwrap_or(1)
        .max(1);
    println!("metrics: {num_threads} worker threads, {NSHARDS} shards");

    let shards: Vec<Mutex<Shard>> = (0..NSHARDS).map(|_| Mutex::new(Shard::default())).collect();
    let worker_locals: Mutex<Vec<Locals>> = Mutex::new(Vec::new());

    let (tx, rx) = crossbeam::channel::bounded::<Batch>(num_threads * 4);

    std::thread::scope(|s| {
        for _ in 0..num_threads {
            let rx = rx.clone();
            let shards = &shards;
            let tables = &tables;
            let worker_locals = &worker_locals;
            s.spawn(move || {
                let mut local = Locals::default();
                for batch in rx.iter() {
                    for gi in 0..batch.game_starts.len() {
                        let start = batch.game_starts[gi];
                        let end = batch
                            .game_starts
                            .get(gi + 1)
                            .copied()
                            .unwrap_or(batch.entries.len());

                        let mut game_lineage = false;
                        local.games += 1;
                        for (j, entry) in batch.entries[start..end].iter().enumerate() {
                            let g = board_from_sf(&entry.pos, tables);
                            let (own_key, flipped) = g.canonical_seed_key_parts(tables);
                            let canonical = own_key.min(flipped);

                            if j == 0 {
                                game_lineage = lineage_keys.is_some_and(|k| k.contains(&canonical));
                            }

                            let eligible = loader_eligible(entry);
                            local.total += 1;
                            local.eligible += eligible as u64;
                            if eligible {
                                let bin = (entry.result.clamp(-1, 1) + 1) as usize;
                                let disagree = bin as f64 * 0.5
                                    - crate::engine::search::stability::win_prob(entry.score as i32);
                                local.wdl_abs_sum += disagree.abs();
                                local.wdl_sq_sum += disagree * disagree;
                                local.score_result[score_bucket(entry.score as i32)][bin] += 1;
                            }
                            local.bucket_counts[output_bucket(g.occupancy().count_ones())] += 1;
                            local.bump_ply(entry.ply as usize);
                            let bin = (entry.result.clamp(-1, 1) + 1) as usize;
                            local.result_all[bin] += 1;
                            if game_lineage {
                                local.result_lineage[bin] += 1;
                            }

                            if track_uniqueness || lineage_keys.is_some() {
                                let mut sh =
                                    shards[(canonical as usize) & (NSHARDS - 1)].lock().unwrap();
                                if track_uniqueness {
                                    sh.observe(own_key, flipped);
                                }
                                if lineage_keys.is_some() {
                                    let flag = if game_lineage {
                                        FLAG_LINEAGE
                                    } else {
                                        FLAG_NONLINEAGE
                                    };
                                    *sh.lineage_flags.entry(canonical).or_insert(0) |= flag;
                                }
                            }
                        }
                    }
                }
                worker_locals.lock().unwrap().push(local);
            });
        }
        drop(rx);

        let mut counter: u64 = 0;
        let mut last_report_time = std::time::Instant::now();
        let mut batch = Batch {
            entries: Vec::with_capacity(BATCH_TARGET),
            game_starts: Vec::new(),
        };
        let mut prev: Option<TrainingDataEntry> = None;

        for binpack_path in binpack_paths {
            let mut reader = match sfbinpack::CompressedTrainingDataEntryReader::new(binpack_path) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("failed to open binpack {binpack_path}: {e}");
                    continue;
                }
            };
            println!("reading binpack {binpack_path}...");

            while reader.has_next() {
                let entry = reader.next();
                counter += 1;

                let is_start = match &prev {
                    None => true,
                    Some(p) => !p.is_continuation(&entry),
                };

                if is_start && batch.entries.len() >= BATCH_TARGET {
                    let full = std::mem::replace(
                        &mut batch,
                        Batch {
                            entries: Vec::with_capacity(BATCH_TARGET),
                            game_starts: Vec::new(),
                        },
                    );
                    tx.send(full).unwrap();
                }
                if is_start {
                    batch.game_starts.push(batch.entries.len());
                }
                batch.entries.push(entry);
                prev = Some(entry);

                if last_report_time.elapsed().as_secs() >= 5 {
                    let fs = reader.file_size().max(1);
                    println!(
                        "read {} entries (file {:.1}%)",
                        counter,
                        reader.read_bytes() as f64 / fs as f64 * 100.0
                    );
                    last_report_time = std::time::Instant::now();
                }
            }

            prev = None;
        }

        if !batch.entries.is_empty() {
            tx.send(batch).unwrap();
        }
        drop(tx);
    });

    let mut tally = UniquenessTally::default();
    let mut games = 0u64;
    let mut eligible = 0u64;
    let mut wdl_abs_sum = 0.0f64;
    let mut wdl_sq_sum = 0.0f64;
    let mut score_result = [[0u64; 3]; N_SCORE_BUCKETS];
    let mut bucket_counts = [0u64; NET_OSIZE];
    let mut ply_counts: Vec<u64> = Vec::new();
    let mut result_all = [0u64; 3];
    let mut result_lineage = [0u64; 3];

    for local in worker_locals.into_inner().unwrap() {
        tally.total += local.total;
        tally.parsed += local.total;
        games += local.games;
        eligible += local.eligible;
        wdl_abs_sum += local.wdl_abs_sum;
        wdl_sq_sum += local.wdl_sq_sum;
        for (b, counts) in local.score_result.iter().enumerate() {
            for (i, c) in counts.iter().enumerate() {
                score_result[b][i] += c;
            }
        }
        for (b, c) in local.bucket_counts.iter().enumerate() {
            bucket_counts[b] += c;
        }
        if local.ply_counts.len() > ply_counts.len() {
            ply_counts.resize(local.ply_counts.len(), 0);
        }
        for (i, c) in local.ply_counts.iter().enumerate() {
            ply_counts[i] += c;
        }
        for i in 0..3 {
            result_all[i] += local.result_all[i];
            result_lineage[i] += local.result_lineage[i];
        }
    }

    if track_uniqueness {
        for sh in &shards {
            let sh = sh.lock().unwrap();
            tally.unique += sh.unique;
            tally.mirror_hits += sh.mirror_hits;
            tally.exact_dups += sh.exact_dups;
        }
    }

    if track_uniqueness {
        tally.print("corpus uniqueness");
    } else {
        println!("--- corpus uniqueness ---");
        println!("total lines/entries : {}", tally.total);
        println!("parsed positions    : {}", tally.parsed);
        println!("(uniqueness tracking disabled via --no-uniqueness)");
    }

    println!(
        "games               : {} ({:.2} positions/game)",
        games,
        if games == 0 {
            0.0
        } else {
            tally.parsed as f64 / games as f64
        }
    );
    println!(
        "loader eligible     : {} ({:.3}%)",
        eligible,
        if tally.parsed == 0 {
            0.0
        } else {
            eligible as f64 / tally.parsed as f64 * 100.0
        }
    );

    if eligible > 0 {
        let n = eligible as f64;
        println!("--- result vs eval disagreement (loader-eligible, STM-relative) ---");
        println!(
            "mean |result_wp - wp(score)| : {:.5}   mean squared: {:.5}",
            wdl_abs_sum / n,
            wdl_sq_sum / n
        );
        println!("score_band          count      loss%    draw%     win%");
        for (b, counts) in score_result.iter().enumerate() {
            let tot: u64 = counts.iter().sum();
            if tot == 0 {
                continue;
            }
            let lo = if b < SCORE_BANDS.len() {
                SCORE_BANDS.len() - 1 - b
            } else {
                b - SCORE_BANDS.len()
            };
            let label = if b < SCORE_BANDS.len() {
                format!("-{}", SCORE_BANDS[lo])
            } else {
                format!("+{}", SCORE_BANDS[lo])
            };
            println!(
                "{label:>10} {tot:>14}  {:>7.2}  {:>7.2}  {:>7.2}",
                counts[0] as f64 / tot as f64 * 100.0,
                counts[1] as f64 / tot as f64 * 100.0,
                counts[2] as f64 / tot as f64 * 100.0
            );
        }
    }

    println!("--- output-bucket histogram (material count) ---");
    for (b, count) in bucket_counts.iter().enumerate() {
        println!("bucket {b}: {count}");
    }

    let (lineage_unique, overlap_unique) = if lineage_keys.is_some() {
        let mut lineage_unique = 0u64;
        let mut overlap_unique = 0u64;
        for sh in &shards {
            let sh = sh.lock().unwrap();
            for f in sh.lineage_flags.values() {
                lineage_unique += (f & FLAG_LINEAGE != 0) as u64;
                overlap_unique += (*f == (FLAG_LINEAGE | FLAG_NONLINEAGE)) as u64;
            }
        }
        let rate = if lineage_unique == 0 {
            0.0
        } else {
            overlap_unique as f64 / lineage_unique as f64 * 100.0
        };
        println!("--- seed lineage ---");
        println!("lineage unique positions              : {lineage_unique}");
        println!("...also present in non-lineage games   : {overlap_unique} ({rate:.3}%)");
        (lineage_unique, overlap_unique)
    } else {
        (0, 0)
    };

    ensure_parent_dir(out_prefix)?;

    let buckets_path = format!("{out_prefix}_buckets.csv");
    let mut f = File::create(&buckets_path)?;
    writeln!(f, "bucket,count")?;
    for (b, count) in bucket_counts.iter().enumerate() {
        writeln!(f, "{b},{count}")?;
    }
    println!("wrote {buckets_path}");

    let ply_path = format!("{out_prefix}_ply.csv");
    let mut f = File::create(&ply_path)?;
    writeln!(f, "ply,count")?;
    for (ply, count) in ply_counts.iter().enumerate() {
        writeln!(f, "{ply},{count}")?;
    }
    println!("wrote {ply_path}");

    let results_path = format!("{out_prefix}_results.csv");
    let mut f = File::create(&results_path)?;
    writeln!(f, "frame,stm_loss,draw,stm_win")?;
    writeln!(
        f,
        "all,{},{},{}",
        result_all[0], result_all[1], result_all[2]
    )?;
    writeln!(
        f,
        "lineage,{},{},{}",
        result_lineage[0], result_lineage[1], result_lineage[2]
    )?;
    println!("wrote {results_path}");

    let summary_path = format!("{out_prefix}_summary.csv");
    let mut f = File::create(&summary_path)?;
    tally.write_summary_rows(&mut f)?;
    writeln!(f, "games,{games}")?;
    writeln!(f, "loader_eligible,{eligible}")?;
    writeln!(f, "lineage_unique,{lineage_unique}")?;
    writeln!(f, "lineage_overlap_unique,{overlap_unique}")?;
    println!("wrote {summary_path}");

    Ok(())
}

fn load_canonical_key_set(
    fen_path: &str,
    tables: &Tables,
) -> anyhow::Result<(HashSet<u64>, usize)> {
    let mut board = ChessGame::new();
    let mut keys = HashSet::new();
    let mut failures = 0usize;

    let file = File::open(fen_path)
        .map_err(|e| anyhow::anyhow!("failed to open lineage key file {}: {}", fen_path, e))?;
    for line in BufReader::new(file).lines() {
        let line = line?;
        let fen = line.trim();
        if fen.is_empty() {
            continue;
        }
        if board.load_fen(fen, tables).is_err() {
            failures += 1;
            continue;
        }
        keys.insert(board.canonical_seed_key(tables));
    }

    Ok((keys, failures))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_position_parts_matches_load_fen() {
        let tables = Tables::new();

        let fens = [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            "rnbqkbnr/ppp1pppp/8/8/3pP3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 3",
            "rnbqkbnr/pppp1ppp/8/4p3/8/8/PPPPPPPP/RNBQKBNR w KQkq e6 0 2",
            "r3k2r/8/8/8/8/8/8/R3K2R w Kq - 5 20",
            "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 b - - 0 1",
            "8/8/8/4k3/8/2K5/8/8 w - - 0 1",
        ];

        let mut lf = ChessGame::new();
        for fen in fens {
            let sf = Position::from_fen(fen);
            let direct = board_from_sf(&sf, &tables);
            lf.load_fen(fen, &tables).unwrap();

            assert_eq!(
                direct.canonical_seed_key_parts(&tables),
                lf.canonical_seed_key_parts(&tables),
                "canonical key mismatch for {fen}"
            );
            assert_eq!(
                direct.occupancy(),
                lf.occupancy(),
                "occupancy mismatch for {fen}"
            );
            assert_eq!(
                direct.bitboards(),
                lf.bitboards(),
                "bitboard mismatch for {fen}"
            );
            assert_eq!(direct.b_move(), lf.b_move(), "stm mismatch for {fen}");
        }
    }
}
