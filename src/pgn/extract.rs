use std::{
    cell::SyncUnsafeCell,
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufReader, Read},
    sync::{
        Mutex,
        atomic::{AtomicU64, AtomicUsize, Ordering},
    },
};

use crossbeam::channel::{Receiver, Sender};
use rand::{Rng, SeedableRng};

use crate::{
    engine::{
        chess_v2::{self, ChessGame},
        search::{
            EngineForm, SearchStrategy, repetition, search::Search, stability, timeman,
            transposition,
        },
        tables,
    },
    pgn::fen_shard::ShardedFenWriter,
    pgn::parse::{self, PgnGame, PgnGameTermination, PgnReadBuf},
    pgn::tuner,
    util,
};

const TT_SIZE_MB: usize = 8;

pub enum MovePick {
    First,
    Random,
}

struct CountingReader<'a, R> {
    inner: R,
    bytes: &'a AtomicU64,
}

impl<R: Read> Read for CountingReader<'_, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let n = self.inner.read(buf)?;
        self.bytes.fetch_add(n as u64, Ordering::Relaxed);
        Ok(n)
    }
}

pub struct PositionExtractParams {
    pub fcompleted_only: bool,
    pub ffrom_ply: usize,
    pub fto_ply: usize,
    pub fmin_ply: usize,
    pub fnum_positions: usize,
    pub fno_duplicates: bool,
    pub fcp_threshold: Option<i32>,
    pub fpick: MovePick,
    pub fseed: u64,
    pub fattempts_per_position: usize,
    pub fsharpness_top_percent: Option<f64>,
    pub fsharpness_depth: u8,
    pub fout_shard_size: usize,
}

const CALIBRATION_SAMPLES: usize = 10_000;

struct WorkerChunkInput {
    chunk_index: u64,
    buf: Vec<u8>,
    len: usize,
}

struct WorkerChunkResult {
    accepted: Vec<String>,
    visited: usize,
    rejected_duplicates: usize,
    rejected_filters: usize,
    games: usize,
}

fn pick_candidate(
    board: &ChessGame,
    tables: &tables::Tables,
    params: &PositionExtractParams,
    moves: &[u16],
    rng: &mut rand::rngs::StdRng,
) -> Option<ChessGame> {
    let to_move = match params.fpick {
        MovePick::First => params.ffrom_ply,
        MovePick::Random => {
            let min = params.ffrom_ply;
            let max = moves.len().min(params.fto_ply);

            if min >= max {
                return None;
            }

            rng.random_range(min..max)
        }
    };

    let mut game_board = board.clone();
    for mv in moves.iter().take(to_move) {
        unsafe { game_board.make_move(*mv, tables) };
    }

    Some(game_board)
}

fn open_pgn_source<'a>(
    path: &std::path::Path,
    bytes_read: &'a AtomicU64,
) -> anyhow::Result<Box<dyn Read + Send + 'a>> {
    let counted = CountingReader {
        inner: File::open(path)?,
        bytes: bytes_read,
    };

    match path.extension().and_then(std::ffi::OsStr::to_str) {
        Some("zst") => Ok(Box::new(zstd::Decoder::new(BufReader::new(counted))?)),
        Some("pgn") => Ok(Box::new(BufReader::new(counted))),
        other => Err(anyhow::anyhow!(
            "Unsupported pgn file extension: {:?}",
            other
        )),
    }
}

fn calibrate_sharpness_cut(
    db_paths: &[String],
    params: &PositionExtractParams,
    board: &ChessGame,
    tables: &tables::Tables,
    top_percent: f64,
    depth: u8,
) -> anyhow::Result<f64> {
    println!("Calibrating sharpness for top {:.2}%...", top_percent);

    let bytes_read = AtomicU64::new(0);
    let mut sources = build_sources(db_paths, &bytes_read)?;

    let mut buf = vec![0u8; parse::CHUNK_SIZE + parse::BACKBUF_SIZE];
    let mut chunk_index = 0u64;
    let mut games: Vec<PgnGame> = Vec::new();
    let mut moves: Vec<u16> = Vec::new();
    let mut samples: Vec<ChessGame> = Vec::new();

    while samples.len() < CALIBRATION_SAMPLES {
        let len = match next_strided_chunk(&mut sources, &mut buf) {
            Some(len) => len,
            None => break,
        };

        let mut rng = rand::rngs::StdRng::seed_from_u64(params.fseed ^ chunk_index);
        chunk_index += 1;

        games.clear();
        parse::parse_games(&buf[..len], &mut games);
        let storage = PgnReadBuf::from_buf(buf.into());

        for game in games.drain(..) {
            if samples.len() >= CALIBRATION_SAMPLES {
                break;
            }

            if params.fcompleted_only
                && game.termination(&storage) != Some(PgnGameTermination::Normal)
            {
                continue;
            }

            moves.clear();
            if !matches!(
                game.parse_moves(&storage, board, tables, &mut moves),
                Some(Ok(()))
            ) {
                continue;
            }

            if moves.len() < params.fmin_ply || moves.len() < params.ffrom_ply {
                continue;
            }

            if let Some(candidate) = pick_candidate(board, tables, params, &moves, &mut rng) {
                samples.push(candidate);
            }
        }

        buf = storage.into_buf().into();
    }

    if samples.is_empty() {
        return Err(anyhow::anyhow!(
            "sharpness calibration found no candidate positions in {:?}",
            db_paths
        ));
    }

    let sampled = samples.len();
    let mut dist = tuner::instability_distribution(samples.into_iter(), TT_SIZE_MB, depth, tables);
    tuner::sort_ascending(&mut dist);
    let cut = tuner::top_percent_cut(&dist, top_percent);

    println!(
        "Sharpness calibration complete: keeping top {:.2}% of {} candidates searched at depth {} -> raw instability cut {:.6}",
        top_percent, sampled, depth, cut
    );

    Ok(cut)
}

struct Worker<'a> {
    rx_chunks: &'a Receiver<WorkerChunkInput>,
    tx_positions: &'a Sender<WorkerChunkResult>,
    pool_tx: &'a Sender<Vec<u8>>,
    params: &'a PositionExtractParams,
    tables: &'a tables::Tables,
    board: &'a ChessGame,
    dedup: &'a Mutex<HashMap<u64, bool>>,
    sharpness_raw_cut: Option<f64>,
}

impl Worker<'_> {
    fn new<'a>(
        rx_chunks: &'a Receiver<WorkerChunkInput>,
        tx_positions: &'a Sender<WorkerChunkResult>,
        pool_tx: &'a Sender<Vec<u8>>,
        params: &'a PositionExtractParams,
        tables: &'a tables::Tables,
        board: &'a ChessGame,
        dedup: &'a Mutex<HashMap<u64, bool>>,
        sharpness_raw_cut: Option<f64>,
    ) -> Worker<'a> {
        Worker {
            rx_chunks,
            tx_positions,
            pool_tx,
            params,
            tables,
            board,
            dedup,
            sharpness_raw_cut,
        }
    }

    fn process_game(
        &mut self,
        game: &PgnGame,
        storage: &PgnReadBuf,
        moves: &mut Vec<u16>,
        rng: &mut rand::rngs::StdRng,
        sharpness_search: &mut Option<Search<'_, { EngineForm::TacticalB }>>,
        result_out: &mut WorkerChunkResult,
    ) {
        let params = self.params;

        moves.clear();

        if self.params.fcompleted_only
            && game.termination(storage) != Some(PgnGameTermination::Normal)
        {
            return;
        }

        match game.parse_moves(storage, self.board, self.tables, moves) {
            Some(Ok(())) => {
                if moves.len() < params.fmin_ply || moves.len() < params.ffrom_ply {
                    return;
                }

                for _ in 0..params.fattempts_per_position {
                    let Some(game_board) =
                        pick_candidate(self.board, self.tables, params, moves, rng)
                    else {
                        continue;
                    };

                    result_out.visited += 1;

                    let canonical_key = game_board.canonical_seed_key(self.tables);

                    if params.fno_duplicates
                        && let Some(pass_filters) = self.dedup.lock().unwrap().get(&canonical_key)
                    {
                        result_out.rejected_duplicates += *pass_filters as usize;
                        result_out.rejected_filters += (!*pass_filters) as usize;
                        continue;
                    }

                    let fen = game_board.gen_fen();

                    if let Some(search) = sharpness_search.as_mut() {
                        search.new_game();
                        search.load_from_fen(&fen, self.tables).unwrap();
                        search.new_search();
                        search.search(Some(params.fsharpness_depth));

                        let score = search.search_score();

                        if let Some(cut) = self.sharpness_raw_cut {
                            let instability = stability::winprob_instability(search.depth_stats());
                            if !instability.is_some_and(|i| i >= cut) {
                                if params.fno_duplicates {
                                    // Add to dedup set to avoid re-searching this position in future games
                                    self.dedup.lock().unwrap().insert(canonical_key, false);
                                }

                                result_out.rejected_filters += 1;
                                continue;
                            }
                        }

                        if let Some(cp_threshold) = params.fcp_threshold {
                            if score.abs() > cp_threshold {
                                if params.fno_duplicates {
                                    // Add to dedup set to avoid re-searching this position in future games
                                    self.dedup.lock().unwrap().insert(canonical_key, false);
                                }
                                result_out.rejected_filters += 1;
                                continue;
                            }
                        }
                    }

                    if params.fno_duplicates {
                        match self.dedup.lock().unwrap().insert(canonical_key, true) {
                            Some(_) => {
                                result_out.rejected_duplicates += 1;
                                continue;
                            }
                            None => {}
                        }
                    }

                    result_out.accepted.push(fen);
                    return;
                }
            }
            Some(Err(e)) => {
                eprintln!(
                    "Failed to parse moves for game {:?}: {}",
                    game.site(storage),
                    e
                );
            }
            None => {}
        }
    }

    fn worker_thread(&mut self) {
        let tt = SyncUnsafeCell::new(transposition::TranspositionTable::new(TT_SIZE_MB));
        let mut tm = SyncUnsafeCell::new(timeman::TimeManager::new());
        tm.get_mut().disable();

        let mut sharpness_search =
            if self.params.fcp_threshold.is_some() || self.sharpness_raw_cut.is_some() {
                Some(Search::<{ EngineForm::TacticalB }>::new(
                    self.tables,
                    &tt,
                    &tm,
                    repetition::RepetitionTable::new(),
                ))
            } else {
                None
            };

        let mut moves: Vec<u16> = Vec::new();
        let mut games: Vec<PgnGame> = Vec::new();

        loop {
            let chunk = match self.rx_chunks.recv() {
                Ok(chunk) => chunk,
                Err(_) => break,
            };

            let mut rng = rand::rngs::StdRng::seed_from_u64(self.params.fseed ^ chunk.chunk_index);

            games.clear();
            parse::parse_games(&chunk.buf[..chunk.len], &mut games);
            let storage = PgnReadBuf::from_buf(chunk.buf);

            let mut result = WorkerChunkResult {
                accepted: Vec::new(),
                visited: 0,
                rejected_duplicates: 0,
                rejected_filters: 0,
                games: games.len(),
            };

            for game in games.drain(..) {
                self.process_game(
                    &game,
                    &storage,
                    &mut moves,
                    &mut rng,
                    &mut sharpness_search,
                    &mut result,
                );
            }

            let _ = self.pool_tx.send(storage.into_buf());

            if self.tx_positions.send(result).is_err() {
                break;
            }
        }
    }
}

struct ReaderSource<'a> {
    reader: Box<dyn Read + Send + 'a>,
    chunker: parse::PgnChunker,
    stride: f64,
    pass: f64,
    active: bool,
}

fn build_sources<'a>(
    db_paths: &[String],
    bytes_read: &'a AtomicU64,
) -> anyhow::Result<Vec<ReaderSource<'a>>> {
    let mut sources: Vec<ReaderSource> = Vec::with_capacity(db_paths.len());
    for db_path in db_paths {
        let path = std::path::Path::new(db_path);
        let size = std::fs::metadata(db_path).map(|m| m.len()).unwrap_or(0);
        let reader = open_pgn_source(path, bytes_read)?;
        sources.push(ReaderSource {
            reader,
            chunker: parse::PgnChunker::new(),
            stride: if size > 0 { 1.0 / size as f64 } else { 0.0 },
            pass: 0.0,
            active: size > 0,
        });
    }
    Ok(sources)
}

fn next_strided_chunk(sources: &mut [ReaderSource<'_>], buf: &mut [u8]) -> Option<usize> {
    loop {
        let i = sources
            .iter()
            .enumerate()
            .filter(|(_, s)| s.active)
            .min_by(|(_, a), (_, b)| a.pass.partial_cmp(&b.pass).unwrap())
            .map(|(i, _)| i)?;

        match sources[i].chunker.next_chunk(&mut *sources[i].reader, buf) {
            Some(len) => {
                sources[i].pass += sources[i].stride;
                return Some(len);
            }
            None => sources[i].active = false,
        }
    }
}

fn reader_thread(
    sources: &mut [ReaderSource<'_>],
    tx_chunks: &Sender<WorkerChunkInput>,
    pool_rx: &Receiver<Vec<u8>>,
) {
    let mut chunk_index = 0u64;

    loop {
        let mut buf = match pool_rx.recv() {
            Ok(b) => b,
            Err(_) => break,
        };

        let Some(len) = next_strided_chunk(sources, &mut buf) else {
            break;
        };

        if tx_chunks
            .send(WorkerChunkInput {
                chunk_index,
                buf,
                len,
            })
            .is_err()
        {
            break;
        }

        chunk_index += 1;
    }
}

#[derive(Default)]
struct ExtractStats {
    games: AtomicUsize,
    visited: AtomicUsize,
    added: AtomicUsize,
    rejected_duplicates: AtomicUsize,
    rejected_filters: AtomicUsize,
}

fn stats_logger(
    done_rx: &Receiver<()>,
    stats: &ExtractStats,
    bytes_read: &AtomicU64,
    source_size: u64,
    target_positions: usize,
    start_time: std::time::Instant,
) {
    let mut last_time = std::time::Instant::now();
    let mut last_added = 0usize;
    let mut last_visited = 0usize;
    let mut last_bytes = 0u64;

    loop {
        match done_rx.recv_timeout(std::time::Duration::from_secs(60)) {
            Err(crossbeam::channel::RecvTimeoutError::Timeout) => {}
            _ => break,
        }

        let now = std::time::Instant::now();
        let interval = now.duration_since(last_time).as_secs_f64();

        let added = stats.added.load(Ordering::Relaxed);
        let visited = stats.visited.load(Ordering::Relaxed);
        let rejected_duplicates = stats.rejected_duplicates.load(Ordering::Relaxed);
        let rejected_filters = stats.rejected_filters.load(Ordering::Relaxed);

        let add_rate = (added - last_added) as f64 / interval;
        let visit_rate = (visited - last_visited) as f64 / interval;

        let [
            rej_duplicate_percent,
            rej_filters_percent,
            acceptance_percent,
        ] = if visited > 0 {
            [
                rejected_duplicates as f64 / visited as f64,
                rejected_filters as f64 / visited as f64,
                added as f64 / visited as f64,
            ]
            .map(|p| p * 100.0)
        } else {
            [0.0, 0.0, 0.0]
        };

        let bytes_now = bytes_read.load(Ordering::Relaxed);
        let read_rate = (bytes_now - last_bytes) as f64 / interval;
        let remaining = target_positions.saturating_sub(added);
        let eta = if add_rate > 0.0 {
            util::time_format((remaining as f64 / add_rate * 1000.0) as u64)
        } else {
            "?".to_string()
        };

        println!(
            "Extracting ({:0>5.2}%) | added {:>4.0}/s | visited {:>4.0}/s | dup% {:<5.2}% | filters<% {:<5.2}% | end acceptance rate {:<5.2}% | read {:>9} / {:<9} ({:>9}/s) | ETA {:<11} | elapsed {}",
            (added as f64 / target_positions as f64) * 100.0,
            add_rate,
            visit_rate,
            rej_duplicate_percent,
            rej_filters_percent,
            acceptance_percent,
            util::byte_size_string(bytes_now as usize),
            util::byte_size_string(source_size as usize),
            util::byte_size_string(read_rate as usize),
            eta,
            util::time_format(now.duration_since(start_time).as_millis() as u64),
        );

        last_time = now;
        last_added = added;
        last_visited = visited;
        last_bytes = bytes_now;
    }
}

pub fn extract_positions(
    db_paths: &[String],
    out_path: &str,
    params: PositionExtractParams,
    threads: usize,
) -> anyhow::Result<()> {
    if db_paths.is_empty() {
        return Err(anyhow::anyhow!("No --db input paths provided"));
    }

    for db_path in db_paths {
        let path = std::path::Path::new(db_path);
        match path.try_exists() {
            Ok(true) => {}
            Ok(false) => {
                return Err(anyhow::anyhow!("Pgn file path {} does not exist", db_path));
            }
            Err(e) => {
                return Err(anyhow::anyhow!(
                    "Failed to access pgn file path {}: {}",
                    db_path,
                    e
                ));
            }
        }
        if !path.is_file() {
            return Err(anyhow::anyhow!("Pgn path {} is not a file", db_path));
        }
    }

    let tables = tables::Tables::new();

    let board = {
        let mut board = chess_v2::ChessGame::new();
        assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());
        board
    };

    let bytes_read = AtomicU64::new(0);
    let source_size: u64 = db_paths
        .iter()
        .map(|p| std::fs::metadata(p).map(|m| m.len()).unwrap_or(0))
        .sum();

    let dedup: Mutex<HashMap<u64, bool>> = Mutex::new(HashMap::new());

    let sharpness_raw_cut = match params.fsharpness_top_percent {
        Some(top_percent) => Some(calibrate_sharpness_cut(
            db_paths,
            &params,
            &board,
            &tables,
            top_percent,
            params.fsharpness_depth,
        )?),
        _ => None,
    };

    let buf_size = parse::CHUNK_SIZE + parse::BACKBUF_SIZE;
    let pool_size = threads + 4 + db_paths.len();

    let (tx_chunks, rx_chunks) = crossbeam::channel::bounded::<WorkerChunkInput>(pool_size);
    let (tx_positions, rx_positions) = crossbeam::channel::bounded::<WorkerChunkResult>(256);
    let (pool_tx, pool_rx) = crossbeam::channel::unbounded::<Vec<u8>>();

    for _ in 0..pool_size {
        pool_tx.send(vec![0u8; buf_size]).unwrap();
    }

    let mut sources = build_sources(db_paths, &bytes_read)?;

    let mut writer = ShardedFenWriter::new(out_path, params.fout_shard_size)?;

    let stats = ExtractStats::default();
    let start_time = std::time::Instant::now();
    let (done_tx, done_rx) = crossbeam::channel::bounded::<()>(1);

    std::thread::scope(|s| -> anyhow::Result<()> {
        {
            let tx_chunks = tx_chunks.clone();
            s.spawn(move || reader_thread(&mut sources, &tx_chunks, &pool_rx));
        }

        for i in 0..threads {
            let rx_chunks = rx_chunks.clone();
            let tx_positions = tx_positions.clone();
            let pool_tx = pool_tx.clone();
            let params = &params;
            let tables = &tables;
            let board = &board;
            let dedup = &dedup;

            s.spawn(move || {
                util::pin_thread_for_worker(i);

                let mut worker = Worker::new(
                    &rx_chunks,
                    &tx_positions,
                    &pool_tx,
                    params,
                    tables,
                    board,
                    dedup,
                    sharpness_raw_cut,
                );

                worker.worker_thread();
            });
        }

        {
            let stats = &stats;
            let bytes_read = &bytes_read;
            let params = &params;
            s.spawn(move || {
                stats_logger(
                    &done_rx,
                    stats,
                    bytes_read,
                    source_size,
                    params.fnum_positions,
                    start_time,
                );
            });
        }

        // Drop originals owned by main thread
        drop(tx_chunks);
        drop(rx_chunks);
        drop(tx_positions);
        drop(pool_tx);

        'recv: loop {
            let result = match rx_positions.recv() {
                Ok(result) => result,
                Err(_) => break,
            };

            stats.games.fetch_add(result.games, Ordering::Relaxed);
            stats.visited.fetch_add(result.visited, Ordering::Relaxed);
            stats
                .rejected_duplicates
                .fetch_add(result.rejected_duplicates, Ordering::Relaxed);
            stats
                .rejected_filters
                .fetch_add(result.rejected_filters, Ordering::Relaxed);

            for accepted in &result.accepted {
                if stats.added.load(Ordering::Relaxed) >= params.fnum_positions {
                    break 'recv;
                }

                writer.write_line(accepted)?;
                stats.added.fetch_add(1, Ordering::Relaxed);
            }
        }

        drop(done_tx);
        writer.finish()?;
        drop(rx_positions);
        Ok(())
    })?;

    println!(
        "Extracted {} positions from {} games",
        stats.added.load(Ordering::Relaxed),
        stats.games.load(Ordering::Relaxed)
    );

    Ok(())
}
