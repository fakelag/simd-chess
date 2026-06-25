use std::{
    cell::SyncUnsafeCell,
    collections::HashSet,
    fs::File,
    io::{BufReader, Read, Write},
    sync::{
        Mutex,
        atomic::{AtomicU64, Ordering},
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
    pgn::parse::{self, PgnGame, PgnGameTermination, PgnReadBuf},
    util,
};

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
    pub fsharpness_threshold: Option<f64>,
    pub fsharpness_depth: u8,
}

struct WorkerChunkInput {
    chunk_index: u64,
    buf: Vec<u8>,
    len: usize,
}

struct WorkerChunkResult {
    fens: Vec<String>,
    visited: usize,
    games: usize,
}

struct Worker<'a> {
    rx_chunks: &'a Receiver<WorkerChunkInput>,
    tx_positions: &'a Sender<WorkerChunkResult>,
    pool_tx: &'a Sender<Vec<u8>>,
    params: &'a PositionExtractParams,
    tables: &'a tables::Tables,
    board: &'a ChessGame,
    dedup: &'a Mutex<HashSet<u64>>,
}

impl Worker<'_> {
    fn new<'a>(
        rx_chunks: &'a Receiver<WorkerChunkInput>,
        tx_positions: &'a Sender<WorkerChunkResult>,
        pool_tx: &'a Sender<Vec<u8>>,
        params: &'a PositionExtractParams,
        tables: &'a tables::Tables,
        board: &'a ChessGame,
        dedup: &'a Mutex<HashSet<u64>>,
    ) -> Worker<'a> {
        Worker {
            rx_chunks,
            tx_positions,
            pool_tx,
            params,
            tables,
            board,
            dedup,
        }
    }

    fn process_game(
        &mut self,
        game: &PgnGame,
        storage: &PgnReadBuf,
        moves: &mut Vec<u16>,
        rng: &mut rand::rngs::StdRng,
        sharpness_search: &mut Option<Search<'_, { EngineForm::TacticalB }>>,
        fens_out: &mut Vec<String>,
    ) -> usize {
        let params = self.params;

        moves.clear();

        if self.params.fcompleted_only
            && game.termination(storage) != Some(PgnGameTermination::Normal)
        {
            return 0;
        }

        match game.parse_moves(storage, self.board, self.tables, moves) {
            Some(Ok(())) => {
                if moves.len() < params.fmin_ply || moves.len() < params.ffrom_ply {
                    return 0;
                }

                let mut visited = 0;

                for _ in 0..params.fattempts_per_position {
                    let to_move = match params.fpick {
                        MovePick::First => params.ffrom_ply,
                        MovePick::Random => {
                            let min = params.ffrom_ply;
                            let max = moves.len().min(params.fto_ply);

                            if min >= max {
                                continue;
                            }

                            rng.random_range(min..max)
                        }
                    };

                    visited += 1;

                    let mut game_board = self.board.clone();

                    for mv in moves.iter().take(to_move) {
                        unsafe { game_board.make_move(*mv, self.tables) };
                    }

                    let zobrist_key = game_board.zobrist_key();

                    if params.fno_duplicates && self.dedup.lock().unwrap().contains(&zobrist_key) {
                        continue;
                    }

                    let fen = game_board.gen_fen();

                    if let Some(search) = sharpness_search.as_mut() {
                        search.new_game();
                        search.load_from_fen(&fen, self.tables).unwrap();
                        search.new_search();
                        search.search(Some(params.fsharpness_depth));

                        let sharpness = stability::calc_sharpness(search.depth_stats());
                        let score = search.search_score();

                        if let Some(threshold) = params.fsharpness_threshold {
                            if !sharpness.is_some_and(|s| s >= threshold) {
                                continue;
                            }
                        }

                        if let Some(cp_threshold) = params.fcp_threshold {
                            if score.abs() > cp_threshold {
                                continue;
                            }
                        }
                    }

                    if params.fno_duplicates && !self.dedup.lock().unwrap().insert(zobrist_key) {
                        continue;
                    }

                    fens_out.push(fen);
                    return visited;
                }

                visited
            }
            Some(Err(e)) => {
                eprintln!(
                    "Failed to parse moves for game {:?}: {}",
                    game.site(storage),
                    e
                );
                0
            }
            None => 0,
        }
    }

    fn worker_thread(&mut self) {
        let tt = SyncUnsafeCell::new(transposition::TranspositionTable::new(16));
        let mut tm = SyncUnsafeCell::new(timeman::TimeManager::new());
        tm.get_mut().disable();

        let mut sharpness_search =
            if self.params.fsharpness_threshold.is_some() || self.params.fcp_threshold.is_some() {
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

            let games_count = games.len();
            let mut fens: Vec<String> = Vec::new();
            let mut visited = 0usize;

            for game in games.drain(..) {
                visited += self.process_game(
                    &game,
                    &storage,
                    &mut moves,
                    &mut rng,
                    &mut sharpness_search,
                    &mut fens,
                );
            }

            let _ = self.pool_tx.send(storage.into_buf());

            if self
                .tx_positions
                .send(WorkerChunkResult {
                    fens,
                    visited,
                    games: games_count,
                })
                .is_err()
            {
                break;
            }
        }
    }
}

fn reader_thread(
    source: &mut dyn Read,
    tx_chunks: &Sender<WorkerChunkInput>,
    pool_rx: &Receiver<Vec<u8>>,
) {
    let mut chunker = parse::PgnChunker::new();
    let mut chunk_index = 0u64;

    loop {
        let mut buf = match pool_rx.recv() {
            Ok(buf) => buf,
            Err(_) => break,
        };

        match chunker.next_chunk(source, &mut buf) {
            Some(len) => {
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
            None => break,
        }
    }
}

pub fn extract_positions(
    db_path: &str,
    out_path: &str,
    params: PositionExtractParams,
    threads: usize,
) -> anyhow::Result<()> {
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

    let db_extension = path.extension().and_then(std::ffi::OsStr::to_str);

    let tables = tables::Tables::new();

    let board = {
        let mut board = chess_v2::ChessGame::new();
        assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());
        board
    };

    let bytes_read = AtomicU64::new(0);
    let source_size = std::fs::metadata(db_path).map(|m| m.len()).unwrap_or(0);

    let dedup: Mutex<HashSet<u64>> = Mutex::new(HashSet::new());

    let mut source: Box<dyn Read + Send> = match db_extension {
        Some("zst") => {
            let counted = CountingReader {
                inner: File::open(path)?,
                bytes: &bytes_read,
            };
            Box::new(zstd::Decoder::new(BufReader::new(counted))?)
        }
        Some("pgn") => {
            let counted = CountingReader {
                inner: File::open(path)?,
                bytes: &bytes_read,
            };
            Box::new(BufReader::new(counted))
        }
        _ => {
            return Err(anyhow::anyhow!(
                "Unsupported pgn file extension: {:?}",
                db_extension
            ));
        }
    };

    let buf_size = parse::CHUNK_SIZE + parse::BACKBUF_SIZE;
    let pool_size = threads + 4;

    let (tx_chunks, rx_chunks) = crossbeam::channel::bounded::<WorkerChunkInput>(pool_size);
    let (tx_positions, rx_positions) = crossbeam::channel::bounded::<WorkerChunkResult>(256);
    let (pool_tx, pool_rx) = crossbeam::channel::unbounded::<Vec<u8>>();

    for _ in 0..pool_size {
        pool_tx.send(vec![0u8; buf_size]).unwrap();
    }

    let mut writer = std::io::BufWriter::new(File::create(out_path)?);

    let mut total_games = 0usize;
    let mut positions_added = 0usize;

    std::thread::scope(|s| -> anyhow::Result<()> {
        {
            let tx_chunks = tx_chunks.clone();
            s.spawn(move || reader_thread(&mut *source, &tx_chunks, &pool_rx));
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
                );

                worker.worker_thread();
            });
        }

        // Drop originals owned by main thread
        drop(tx_chunks);
        drop(rx_chunks);
        drop(tx_positions);
        drop(pool_tx);

        let start_time = std::time::Instant::now();
        let mut positions_visited = 0usize;
        let mut out_buf = String::new();
        let mut last_log_time = std::time::Instant::now();
        let mut last_log_added = 0usize;
        let mut last_log_visited = 0usize;
        let mut last_log_bytes = 0u64;

        loop {
            let result = match rx_positions.recv() {
                Ok(result) => result,
                Err(_) => break,
            };

            total_games += result.games;
            positions_visited += result.visited;

            for fen in result.fens {
                if positions_added >= params.fnum_positions {
                    break;
                }
                out_buf.push_str(&fen);
                out_buf.push('\n');
                positions_added += 1;
            }

            if out_buf.len() >= 1024 * 128 {
                writer.write_all(out_buf.as_bytes())?;
                out_buf.clear();
            }

            if last_log_time.elapsed().as_secs() >= 10 {
                let now = std::time::Instant::now();
                let interval = now.duration_since(last_log_time).as_secs_f64();
                let add_rate = (positions_added - last_log_added) as f64 / interval;
                let visit_rate = (positions_visited - last_log_visited) as f64 / interval;
                let bytes_now = bytes_read.load(Ordering::Relaxed);
                let read_rate = (bytes_now - last_log_bytes) as f64 / interval;
                let remaining = params.fnum_positions.saturating_sub(positions_added);
                let eta = if add_rate > 0.0 {
                    util::time_format((remaining as f64 / add_rate * 1000.0) as u64)
                } else {
                    "?".to_string()
                };

                println!(
                    "Extracting {}/{} ({:.2}%) | added {:.0}/s | visited {:.0}/s | read {}/{} ({}/s) | ETA {} | elapsed {}",
                    positions_added,
                    params.fnum_positions,
                    (positions_added as f64 / params.fnum_positions as f64) * 100.0,
                    add_rate,
                    visit_rate,
                    util::byte_size_string(bytes_now as usize),
                    util::byte_size_string(source_size as usize),
                    util::byte_size_string(read_rate as usize),
                    eta,
                    util::time_format(now.duration_since(start_time).as_millis() as u64),
                );

                last_log_time = now;
                last_log_added = positions_added;
                last_log_visited = positions_visited;
                last_log_bytes = bytes_now;
            }

            if positions_added >= params.fnum_positions {
                break;
            }
        }

        writer.write_all(out_buf.as_bytes())?;

        drop(rx_positions);

        Ok(())
    })?;

    writer.flush()?;

    println!(
        "Extracted {} positions from {} games",
        positions_added, total_games
    );

    Ok(())
}
