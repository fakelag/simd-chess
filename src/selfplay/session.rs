use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

use crossbeam::channel::RecvTimeoutError;

use super::run::unix_now;
use super::run::{
    Outcome, Params, PendingSeed, Progress, RunConfig, RunDir, Session, Shard, Stats,
};
use super::shard::{self, ShardWriter};
use crate::engine::tables;
use crate::selfplay::fen_feeder::StatefulFenQueue;
use crate::selfplay::selfplay::play_games;
use crate::selfplay::{Failure, GameEnding, PlayedGame, WorkerMsg};
use crate::util::{self, interrupt};

const CHECKPOINT_INTERVAL: Duration = Duration::from_secs(60);

/// Test controls; real sessions use the default.
#[derive(Default)]
pub struct Hooks {
    /// Stop as Ctrl+C would once this many games were written.
    pub stop_after_games: Option<u64>,
    /// Stop without the final commit once this many games were written, as if the process died
    /// right after the writer reached the disk.
    pub crash_after_games: Option<u64>,
    /// Checkpoint (commit and progress line) this often instead of once a minute.
    pub checkpoint_interval: Option<Duration>,
}

pub struct Run<'a> {
    pub dir: &'a RunDir,
    pub config: &'a mut RunConfig,
}

enum Halt {
    Stop(Outcome),
    WorkerFailed(String),
    WriteFailed(anyhow::Error),
}

/// For a run, `params` and `input_seeds` are its config's.
pub fn play(
    run: Option<Run>,
    params: &Params,
    input_seeds: u64,
    queue: StatefulFenQueue,
    hooks: &Hooks,
) -> Result<Outcome, Failure> {
    let (cursor, pending, end) = (queue.cursor(), queue.pending().len(), queue.end());
    let queue = Mutex::new(queue);

    let mut session = SessionState {
        params,
        hooks,
        input_seeds,
        positions_before: run.as_ref().map_or(0, |run| run.config.totals.positions),
        ledger: run.map(Ledger::begin).transpose()?,
        stats: Stats::default(),
        halt: None,
        started: Instant::now(),
        last_checkpoint: Instant::now(),
        games_at_checkpoint: 0,
    };

    println!(
        "Starting selfplay{}: {} threads, depth {}, seed cursor {} of {} (+{} pending){}{}",
        session
            .ledger
            .as_ref()
            .map_or(" (dry run)".to_string(), |l| format!(
                " run '{}'",
                l.run.config.name
            )),
        params.threads,
        params.depth,
        cursor,
        input_seeds,
        pending,
        if end == u64::MAX {
            String::new()
        } else {
            format!(", seeds end at {}", end)
        },
        params
            .max_positions
            .map_or(String::new(), |max| format!(", max positions {}", max)),
    );

    let tables = tables::Tables::new();
    let stop = AtomicBool::new(false);
    let (tx, rx) = crossbeam::channel::bounded::<WorkerMsg>(256);

    std::thread::scope(|s| {
        for i in 0..params.threads {
            let (queue, stop, tables, tx) = (&queue, &stop, &tables, tx.clone());
            s.spawn(move || {
                util::pin_thread_for_worker(i);
                play_games(i, queue, stop, tx, tables, params);
            });
        }
        drop(tx);

        loop {
            match rx.recv_timeout(Duration::from_millis(100)) {
                Ok(WorkerMsg::Game(game)) if session.halt.is_none() => {
                    if let Err(e) = session.on_game(game, &queue) {
                        session.halt = Some(Halt::WriteFailed(e));
                    }
                }
                Ok(WorkerMsg::Failed(e)) if session.halt.is_none() => {
                    session.halt = Some(Halt::WorkerFailed(e));
                }
                // Halted: the seed stays pending.
                Ok(_) => {}
                Err(RecvTimeoutError::Timeout) => {}
                Err(RecvTimeoutError::Disconnected) => break,
            }

            if session.halt.is_none() && interrupt::requested() {
                session.halt = Some(Halt::Stop(Outcome::Interrupted));
            }

            if session.halt.is_some() && !stop.load(Ordering::Relaxed) {
                stop.store(true, Ordering::Relaxed);
                queue.lock().unwrap().stop();
            }

            let interval = hooks.checkpoint_interval.unwrap_or(CHECKPOINT_INTERVAL);
            if session.halt.is_none() && session.last_checkpoint.elapsed() >= interval {
                if let Err(e) = session.checkpoint(&queue) {
                    session.halt = Some(Halt::WriteFailed(e));
                }
            }
        }
    });

    let outcome = match session.halt.take() {
        None => Outcome::Finished,
        // The ledger drops with the session: buffered games reach the shard without a commit.
        Some(Halt::Stop(Outcome::Crashed)) => return Ok(Outcome::Crashed),
        Some(Halt::Stop(outcome)) => outcome,
        Some(Halt::WriteFailed(e)) => {
            println!(
                "Selfplay stopped: {:#}. Resume rolls back to the last checkpoint.",
                e
            );
            return Err(Failure::Failed(e));
        }
        Some(Halt::WorkerFailed(e)) => {
            if let Some(ledger) = &mut session.ledger {
                ledger.commit(&queue, Outcome::Failed)?;
            }
            return Err(Failure::Failed(anyhow::anyhow!(
                "a game failed, its seed stays pending: {}",
                e
            )));
        }
    };

    if let Some(ledger) = &mut session.ledger {
        ledger.commit(&queue, outcome)?;
    }
    session.print_summary(outcome, queue.lock().unwrap().skipped());

    Ok(outcome)
}

struct SessionState<'r, 'p> {
    params: &'p Params,
    hooks: &'p Hooks,
    ledger: Option<Ledger<'r>>,
    input_seeds: u64,
    positions_before: u64,
    /// Games of this session only.
    stats: Stats,
    halt: Option<Halt>,
    started: Instant,
    last_checkpoint: Instant,
    games_at_checkpoint: u64,
}

impl SessionState<'_, '_> {
    fn on_game(&mut self, game: PlayedGame, queue: &Mutex<StatefulFenQueue>) -> anyhow::Result<()> {
        if let Some(ledger) = &mut self.ledger {
            ledger.record(&game)?;
        }
        queue.lock().unwrap().complete(game.seed);
        tally(&mut self.stats, &game);

        // Before any commit, so the simulated crash always leaves this game uncommitted.
        if self
            .hooks
            .crash_after_games
            .is_some_and(|k| self.stats.games >= k)
        {
            self.halt = Some(Halt::Stop(Outcome::Crashed));
            return Ok(());
        }

        if let Some(ledger) = &mut self.ledger
            && ledger.shard_full()
        {
            ledger.checkpoint(queue, true)?;
        }

        let positions = self.positions_before + self.stats.positions;
        if self
            .params
            .max_positions
            .is_some_and(|max| positions >= max)
        {
            self.halt = Some(Halt::Stop(Outcome::MaxPositions));
        } else if self
            .hooks
            .stop_after_games
            .is_some_and(|k| self.stats.games >= k)
        {
            self.halt = Some(Halt::Stop(Outcome::Interrupted));
        }

        Ok(())
    }

    fn checkpoint(&mut self, queue: &Mutex<StatefulFenQueue>) -> anyhow::Result<()> {
        if let Some(ledger) = &mut self.ledger {
            ledger.checkpoint(queue, false)?;
        }

        let window_minutes = self.last_checkpoint.elapsed().as_secs_f64() / 60.0;
        let total_minutes = self.started.elapsed().as_secs_f64() / 60.0;
        let games = self.stats.games + self.stats.terminated;
        let games_per_minute = (games - self.games_at_checkpoint) as f64 / window_minutes;
        let average_per_minute = games as f64 / total_minutes;

        let (seeds_left, skipped) = {
            let queue = queue.lock().unwrap();
            (queue.seeds_left(self.input_seeds), queue.skipped())
        };
        let positions_left = self
            .params
            .max_positions
            .map(|max| max.saturating_sub(self.positions_before + self.stats.positions));
        let positions_per_minute = self.stats.positions as f64 / total_minutes;
        let eta_minutes = [
            Some(seeds_left as f64 / average_per_minute),
            positions_left.map(|left| left as f64 / positions_per_minute),
        ]
        .into_iter()
        .flatten()
        .filter(|minutes| minutes.is_finite())
        .reduce(f64::min);

        println!(
            "Checkpoint after {} games ({:.02} mins). Games per minute: ~{:.02} ({:.02} avg). {} positions, ~{:.02} per game. win-adj%: {:.02}, draw-adj%: {:.02}, term: {}{}{}. ETA: {}",
            games,
            total_minutes,
            games_per_minute,
            average_per_minute,
            self.stats.positions,
            self.stats.positions as f64 / self.stats.games.max(1) as f64,
            percent(self.stats.win_adj, self.stats.games),
            percent(self.stats.draw_adj, self.stats.games),
            self.stats.terminated,
            skipped_note(skipped),
            self.ledger
                .as_ref()
                .and_then(|ledger| ledger.run.config.shards.last())
                .map_or(String::new(), |shard| format!(
                    ", {}: {}",
                    shard.file,
                    util::byte_size_string(shard.bytes as usize)
                )),
            eta_minutes.map_or("-".to_string(), |minutes| util::time_format(
                (minutes * 60_000.0) as u64
            )),
        );

        self.last_checkpoint = Instant::now();
        self.games_at_checkpoint = games;
        Ok(())
    }

    fn print_summary(&self, outcome: Outcome, skipped: u64) {
        let minutes = self.started.elapsed().as_secs_f64() / 60.0;
        println!(
            "Selfplay {:?} after {}: {} games, {} positions, ~{:.02} games per minute. win-adj%: {:.02}, draw-adj%: {:.02}, terminated: {}{}",
            outcome,
            util::time_format(self.started.elapsed().as_millis() as u64),
            self.stats.games,
            self.stats.positions,
            self.stats.games as f64 / minutes,
            percent(self.stats.win_adj, self.stats.games),
            percent(self.stats.draw_adj, self.stats.games),
            self.stats.terminated,
            skipped_note(skipped),
        );

        if let Some(ledger) = &self.ledger {
            let config = &ledger.run.config;
            println!(
                "Run '{}': {} games, {} positions in {} shard(s), seed cursor {} with {} pending",
                config.name,
                config.totals.games,
                config.totals.positions,
                config.shards.len(),
                config.progress.cursor,
                config.progress.pending.len(),
            );
        }
    }
}

fn percent(part: u64, whole: u64) -> f64 {
    part as f64 / whole.max(1) as f64 * 100.0
}

fn skipped_note(count: u64) -> String {
    if count == 0 {
        String::new()
    } else {
        format!(", skipped {} unusable positions", count)
    }
}

fn tally(stats: &mut Stats, game: &PlayedGame) {
    if game.ending == GameEnding::Terminated {
        stats.terminated += 1;
        return;
    }

    stats.games += 1;
    stats.positions += game.entries.len() as u64;
    match game.result {
        1 => stats.white_wins += 1,
        -1 => stats.black_wins += 1,
        _ => stats.draws += 1,
    }
    match game.ending {
        GameEnding::WinAdjudication => stats.win_adj += 1,
        GameEnding::DrawAdjudication => stats.draw_adj += 1,
        _ => {}
    }
}

/// Keeps the shards and the config in step: the config only ever describes bytes that were
/// flushed, synced and read back, and every seed it doesn't account for is pending.
struct Ledger<'a> {
    run: Run<'a>,
    writer: Option<ShardWriter>,
    session: usize,
    last_commit: Instant,
}

impl<'a> Ledger<'a> {
    /// Records the session's start in the config.
    fn begin(run: Run<'a>) -> anyhow::Result<Self> {
        run.config.sessions.push(Session {
            started: unix_now(),
            ended: None,
            outcome: Outcome::Running,
            threads: run.config.params.threads,
            games: 0,
            positions: 0,
        });
        run.dir.save(run.config)?;

        Ok(Self {
            session: run.config.sessions.len() - 1,
            run,
            writer: None,
            last_commit: Instant::now(),
        })
    }

    fn record(&mut self, game: &PlayedGame) -> anyhow::Result<()> {
        let config = &mut *self.run.config;

        if config.shards.last().is_none_or(|shard| shard.closed) {
            let index = config.shards.len();
            config.shards.push(Shard {
                file: shard::file_name(index),
                bytes: 0,
                closed: false,
                created: unix_now(),
                closed_at: None,
                cursor_min: None,
                cursor_max: None,
                stats: Stats::default(),
            });
            // The entry is committed before its file exists, so resume knows every shard file.
            self.run.dir.save(config)?;
        }

        let shard = config.shards.last_mut().unwrap();
        if game.ending != GameEnding::Terminated {
            if self.writer.is_none() {
                let path = self.run.dir.path().join(&shard.file);
                self.writer = Some(ShardWriter::open(&path, shard.bytes)?);
            }
            self.writer.as_mut().unwrap().write_game(&game.entries)?;

            shard.cursor_min = Some(shard.cursor_min.map_or(game.seed, |c| c.min(game.seed)));
            shard.cursor_max = Some(shard.cursor_max.map_or(game.seed, |c| c.max(game.seed)));

            let session = &mut config.sessions[self.session];
            session.games += 1;
            session.positions += game.entries.len() as u64;
        }
        tally(&mut shard.stats, game);

        Ok(())
    }

    fn shard_full(&self) -> bool {
        let config = &self.run.config;
        let shard = config.shards.last();
        config
            .params
            .shard_size
            .is_some_and(|size| shard.is_some_and(|s| !s.closed && s.stats.positions >= size))
    }

    fn commit(&mut self, queue: &Mutex<StatefulFenQueue>, outcome: Outcome) -> anyhow::Result<()> {
        self.sync(queue, outcome, false)?;
        self.run.dir.save(self.run.config)
    }

    /// A failed save only warns: the next save writes this state too. Until then the config on
    /// disk lags the shard, which resume handles like a crash.
    fn checkpoint(
        &mut self,
        queue: &Mutex<StatefulFenQueue>,
        close_shard: bool,
    ) -> anyhow::Result<()> {
        self.sync(queue, Outcome::Running, close_shard)?;
        if let Err(e) = self.run.dir.save(self.run.config) {
            println!(
                "Warning: the checkpoint wasn't saved, retrying at the next one: {:#}",
                e
            );
        }
        Ok(())
    }

    /// Brings the config in memory up to the shard and the queue.
    fn sync(
        &mut self,
        queue: &Mutex<StatefulFenQueue>,
        outcome: Outcome,
        close_shard: bool,
    ) -> anyhow::Result<()> {
        let config = &mut *self.run.config;

        if let Some(writer) = self.writer.take() {
            config.shards.last_mut().unwrap().bytes = writer.finish()?;
        }

        let (cursor, exhausted, seed_hash, pending) = {
            let queue = queue.lock().unwrap();
            (
                queue.cursor(),
                queue.exhausted(),
                queue.seed_hash(),
                queue.pending(),
            )
        };

        let now = Instant::now();
        if let Some(shard) = config.shards.last_mut().filter(|shard| !shard.closed) {
            shard.stats.seconds += now.duration_since(self.last_commit).as_secs_f64();
            if close_shard {
                shard.closed = true;
                shard.closed_at = Some(unix_now());
            }
        }
        self.last_commit = now;

        config.progress = Progress {
            cursor,
            exhausted,
            seed_hash,
            pending: pending
                .into_iter()
                .map(|(cursor, fen)| PendingSeed { cursor, fen })
                .collect(),
        };

        let session = &mut config.sessions[self.session];
        session.outcome = outcome;
        if outcome != Outcome::Running {
            session.ended = Some(unix_now());
        }

        config.update_totals();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::super::run::current_engine_config;
    use super::*;
    use crate::util::test_dir::TestDir;

    fn one_seed_run(name: &str) -> (TestDir, RunDir, RunConfig, Mutex<StatefulFenQueue>) {
        let dir = TestDir::new(name);
        let positions = dir.path().join("positions.epd");
        std::fs::write(&positions, "8/8/4k3/8/8/4K3/8/8 w - - 0 1\n").unwrap();
        let positions = vec![positions.to_string_lossy().into_owned()];

        let queue = StatefulFenQueue::new(&positions, 0, Vec::new(), u64::MAX, None, || false)
            .unwrap()
            .unwrap();
        let params = Params {
            from: 0,
            count: None,
            max_positions: None,
            threads: 1,
            depth: 1,
            shard_size: None,
            win_adj: false,
            draw_adj: false,
        };
        let config = RunConfig::new(
            "run",
            params,
            Vec::new(),
            1,
            current_engine_config(),
            queue.seed_hash(),
        );
        let run_dir = RunDir::create(dir.path(), "run", &config).ok().unwrap();
        (dir, run_dir, config, Mutex::new(queue))
    }

    // Dropped games need a search past the node cap, so the ledger is fed one directly.
    fn dropped(seed: u64) -> PlayedGame {
        PlayedGame {
            seed,
            entries: Vec::new(),
            ending: GameEnding::Terminated,
            result: 0,
        }
    }

    #[test]
    fn test_ledger_charges_dropped_games_without_writing() {
        let (_dir, run_dir, mut config, queue) = one_seed_run("ledger_dropped");
        let (seed, _) = queue.lock().unwrap().next().unwrap();

        let mut ledger = Ledger::begin(Run {
            dir: &run_dir,
            config: &mut config,
        })
        .unwrap();
        ledger.record(&dropped(seed)).unwrap();
        queue.lock().unwrap().complete(seed);
        ledger.commit(&queue, Outcome::Finished).unwrap();

        let saved = run_dir.load().unwrap();
        let shard = &saved.shards[0];
        assert_eq!(
            (shard.bytes, shard.stats.games, shard.stats.terminated),
            (0, 0, 1)
        );
        assert_eq!((shard.cursor_min, shard.cursor_max), (None, None));
        assert!(
            !run_dir.path().join(&shard.file).exists(),
            "a dropped game writes no file"
        );
        assert_eq!(saved.totals.terminated, 1);
        assert_eq!(saved.sessions[0].games, 0);
        assert_eq!(saved.sessions[0].outcome, Outcome::Finished);
        assert!(saved.progress.pending.is_empty());
        assert_eq!(saved.progress.cursor, 1);
        assert_eq!(saved.progress.seed_hash, queue.lock().unwrap().seed_hash());
    }

    #[cfg(windows)]
    #[test]
    fn test_only_the_final_commit_fails_on_a_busy_config() {
        use std::os::windows::fs::OpenOptionsExt;
        const FILE_SHARE_READ: u32 = 1;

        let (_dir, run_dir, mut config, queue) = one_seed_run("ledger_busy_config");
        let (seed, _) = queue.lock().unwrap().next().unwrap();

        let mut ledger = Ledger::begin(Run {
            dir: &run_dir,
            config: &mut config,
        })
        .unwrap();
        ledger.record(&dropped(seed)).unwrap();
        queue.lock().unwrap().complete(seed);

        // Without delete sharing the config can't be replaced while this is open.
        let reader = std::fs::OpenOptions::new()
            .read(true)
            .share_mode(FILE_SHARE_READ)
            .open(run_dir.path().join(super::super::run::CONFIG_FILE_NAME))
            .unwrap();
        ledger.checkpoint(&queue, false).unwrap();
        assert_eq!(
            run_dir.load().unwrap().progress.cursor,
            0,
            "the checkpoint wasn't saved"
        );
        assert!(ledger.commit(&queue, Outcome::Finished).is_err());
        drop(reader);

        ledger.commit(&queue, Outcome::Finished).unwrap();
        let saved = run_dir.load().unwrap();
        assert_eq!(saved.progress.cursor, 1);
        assert_eq!(saved.sessions[0].outcome, Outcome::Finished);
    }
}
