use std::collections::HashSet;

use sfbinpack::{
    CompressedTrainingDataEntryReader, CompressedTrainingDataEntryWriter, TrainingDataEntry,
};

use crate::engine::tables::Tables;
use crate::tools::metrics::board_from_sf;

const DUP_SENTINEL_SCORE: i16 = i16::MAX;

struct FileStats {
    idx: usize,
    path: String,
    games: u64,
    positions: u64,
    error: Option<String>,
}

fn count_file(idx: usize, path: &str) -> FileStats {
    let mut reader = match CompressedTrainingDataEntryReader::new(path) {
        Ok(r) => r,
        Err(e) => {
            return FileStats {
                idx,
                path: path.to_string(),
                games: 0,
                positions: 0,
                error: Some(e.to_string()),
            };
        }
    };

    let mut games = 0u64;
    let mut positions = 0u64;
    let mut prev: Option<TrainingDataEntry> = None;

    while reader.has_next() {
        let entry = reader.next();
        positions += 1;
        let is_start = match &prev {
            None => true,
            Some(p) => !p.is_continuation(&entry),
        };
        if is_start {
            games += 1;
        }
        prev = Some(entry);
    }

    FileStats {
        idx,
        path: path.to_string(),
        games,
        positions,
        error: None,
    }
}

struct GameSource {
    reader: CompressedTrainingDataEntryReader,
    lookahead: Option<TrainingDataEntry>,
    stride: f64,
    pass: f64,
    active: bool,
}

impl GameSource {
    fn open(path: &str, games: u64) -> Option<Self> {
        let reader = match CompressedTrainingDataEntryReader::new(path) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("shuffle: failed to reopen {path}: {e} (skipping)");
                return None;
            }
        };
        Some(Self {
            reader,
            lookahead: None,
            stride: if games > 0 { 1.0 / games as f64 } else { 0.0 },
            pass: 0.0,
            active: games > 0,
        })
    }

    fn next_game(&mut self) -> Option<Vec<TrainingDataEntry>> {
        let first = match self.lookahead.take() {
            Some(e) => e,
            None => {
                if !self.reader.has_next() {
                    return None;
                }
                self.reader.next()
            }
        };

        let mut game = vec![first];
        let mut prev = first;
        while self.reader.has_next() {
            let entry = self.reader.next();
            if prev.is_continuation(&entry) {
                game.push(entry);
                prev = entry;
            } else {
                self.lookahead = Some(entry);
                break;
            }
        }

        Some(game)
    }
}

pub fn run_shuffle(
    paths: &[String],
    out_path: &str,
    threads: usize,
    no_duplicates: bool,
) -> anyhow::Result<()> {
    if paths.is_empty() {
        return Err(anyhow::anyhow!("shuffle: no input binpacks provided"));
    }

    let num_threads = threads.max(1).min(paths.len());
    println!(
        "shuffle: {} input file(s), counting with {} thread(s){}",
        paths.len(),
        num_threads,
        if no_duplicates {
            ", dedup ON (duplicates score-sentineled)"
        } else {
            ""
        }
    );

    let (path_tx, path_rx) = crossbeam::channel::unbounded::<(usize, String)>();
    for (i, p) in paths.iter().enumerate() {
        path_tx.send((i, p.clone())).unwrap();
    }
    drop(path_tx);

    let (res_tx, res_rx) = crossbeam::channel::unbounded::<FileStats>();

    let mut stats: Vec<FileStats> = std::thread::scope(|s| {
        for _ in 0..num_threads {
            let path_rx = path_rx.clone();
            let res_tx = res_tx.clone();
            s.spawn(move || {
                while let Ok((idx, path)) = path_rx.recv() {
                    res_tx.send(count_file(idx, &path)).unwrap();
                }
            });
        }
        drop(res_tx);
        res_rx.iter().collect()
    });

    stats.sort_by_key(|s| s.idx);

    let mut total_games = 0u64;
    let mut total_positions = 0u64;
    println!("--- per-file counts ---");
    for st in &stats {
        match &st.error {
            Some(e) => println!("  {} : FAILED ({e})", st.path),
            None => {
                println!(
                    "  {} : {} games, {} positions",
                    st.path, st.games, st.positions
                );
                total_games += st.games;
                total_positions += st.positions;
            }
        }
    }
    println!(
        "--- totals: {} games, {} positions across {} readable file(s) ---",
        total_games,
        total_positions,
        stats.iter().filter(|s| s.error.is_none()).count()
    );

    if total_games == 0 {
        return Err(anyhow::anyhow!("shuffle: no games found in any input"));
    }

    let mut sources: Vec<GameSource> = stats
        .iter()
        .filter(|s| s.error.is_none() && s.games > 0)
        .filter_map(|s| GameSource::open(&s.path, s.games))
        .collect();

    let mut writer = CompressedTrainingDataEntryWriter::new(out_path, false)
        .map_err(|e| anyhow::anyhow!("shuffle: failed to open output {out_path}: {e}"))?;

    let tables = Tables::new();
    let mut seen: HashSet<u64> = HashSet::new();

    let mut written_games = 0u64;
    let mut written_positions = 0u64;
    let mut dup_positions = 0u64;
    let mut last_report = std::time::Instant::now();

    loop {
        let pick = sources
            .iter()
            .enumerate()
            .filter(|(_, src)| src.active)
            .min_by(|(_, a), (_, b)| a.pass.partial_cmp(&b.pass).unwrap())
            .map(|(i, _)| i);

        let Some(i) = pick else {
            break;
        };

        match sources[i].next_game() {
            Some(game) => {
                sources[i].pass += sources[i].stride;

                for mut entry in game {
                    if no_duplicates {
                        let key = board_from_sf(&entry.pos, &tables).zobrist_key();
                        if !seen.insert(key) {
                            entry.score = DUP_SENTINEL_SCORE;
                            dup_positions += 1;
                        }
                    }
                    writer
                        .write_entry(&entry)
                        .map_err(|e| anyhow::anyhow!("shuffle: write failed: {e}"))?;
                    written_positions += 1;
                }
                written_games += 1;

                if last_report.elapsed().as_secs() >= 5 {
                    println!(
                        "shuffle: wrote {}/{} games ({:.1}%), {} positions",
                        written_games,
                        total_games,
                        written_games as f64 / total_games as f64 * 100.0,
                        written_positions
                    );
                    last_report = std::time::Instant::now();
                }
            }
            None => sources[i].active = false,
        }
    }

    writer
        .flush()
        .map_err(|e| anyhow::anyhow!("shuffle: final flush failed: {e}"))?;

    println!("--- shuffle done ---");
    println!("  output: {out_path}");
    println!("  games written: {written_games}");
    println!("  positions written: {written_positions}");
    if no_duplicates {
        println!(
            "  duplicate positions sentineled: {} ({:.2}%)",
            dup_positions,
            if written_positions > 0 {
                dup_positions as f64 / written_positions as f64 * 100.0
            } else {
                0.0
            }
        );
    }

    Ok(())
}
