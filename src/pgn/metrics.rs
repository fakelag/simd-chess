use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};

use crate::engine::chess_v2::ChessGame;
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

pub fn run_binpack_metrics(
    binpack_paths: &[&str],
    lineage_keys_path: Option<&str>,
    out_prefix: &str,
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

    const FLAG_NONLINEAGE: u8 = 0b01;
    const FLAG_LINEAGE: u8 = 0b10;

    let mut seen_forms: HashMap<u64, u8> = HashMap::new();
    let mut lineage_flags: HashMap<u64, u8> = HashMap::new();
    let mut tally = UniquenessTally::default();

    let mut bucket_counts = [0u64; NET_OSIZE];
    let mut ply_counts: Vec<u64> = Vec::new();

    let mut result_all = [0u64; 3];
    let mut result_lineage = [0u64; 3];

    let mut board = ChessGame::new();
    let mut current_game_lineage = false;
    let mut counter: u64 = 0;

    let mut last_report_time = std::time::Instant::now();

    for binpack_path in binpack_paths {
        let mut reader = sfbinpack::CompressedTrainingDataEntryReader::new(binpack_path)
            .map_err(|e| anyhow::anyhow!("failed to open binpack {}: {}", binpack_path, e))?;

        let mut prev_entry: Option<sfbinpack::TrainingDataEntry> = None;

        println!("reading binpack {}...", binpack_path);

        while reader.has_next() {
            let entry = reader.next();
            tally.total += 1;
            counter += 1;

            if last_report_time.elapsed().as_secs() >= 5 {
                let fs = reader.file_size().max(1);
                println!(
                    "read {} entries (file {:.1}%)",
                    counter,
                    reader.read_bytes() as f64 / fs as f64 * 100.0
                );

                last_report_time = std::time::Instant::now();
            }

            let fen = entry.pos.fen();
            if board.load_fen(&fen, &tables).is_err() {
                tally.parse_failures += 1;
                prev_entry = Some(entry);
                continue;
            }

            let (own_key, flipped) = board.canonical_seed_key_parts(&tables);
            let canonical = own_key.min(flipped);

            let is_game_start = match &prev_entry {
                None => true,
                Some(prev) => !prev.is_continuation(&entry),
            };
            if is_game_start {
                current_game_lineage = lineage_keys
                    .as_ref()
                    .is_some_and(|keys| keys.contains(&canonical));
            }

            tally.observe(canonical, own_key, &mut seen_forms);

            let bucket = output_bucket(board.occupancy().count_ones());
            bucket_counts[bucket] += 1;

            let ply = entry.ply as usize;
            if ply >= ply_counts.len() {
                ply_counts.resize(ply + 1, 0);
            }
            ply_counts[ply] += 1;

            let result_bin = (entry.result.clamp(-1, 1) + 1) as usize;
            result_all[result_bin] += 1;

            if lineage_keys.is_some() {
                let flag = if current_game_lineage {
                    FLAG_LINEAGE
                } else {
                    FLAG_NONLINEAGE
                };
                *lineage_flags.entry(canonical).or_insert(0) |= flag;
                if current_game_lineage {
                    result_lineage[result_bin] += 1;
                }
            }

            prev_entry = Some(entry);
        }
    }

    tally.print("corpus uniqueness");

    println!("--- output-bucket histogram (material count) ---");
    for (b, count) in bucket_counts.iter().enumerate() {
        println!("bucket {b}: {count}");
    }

    let (lineage_unique, overlap_unique) = if lineage_keys.is_some() {
        let lineage_unique = lineage_flags
            .values()
            .filter(|f| *f & FLAG_LINEAGE != 0)
            .count() as u64;
        let overlap_unique = lineage_flags
            .values()
            .filter(|f| **f == (FLAG_LINEAGE | FLAG_NONLINEAGE))
            .count() as u64;
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
