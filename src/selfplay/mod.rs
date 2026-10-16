pub mod cli;
pub mod fen_feeder;
mod run;
pub mod selfplay;
mod session;
mod shard;

pub enum Failure {
    Refused(String),
    Failed(anyhow::Error),
}

impl From<anyhow::Error> for Failure {
    fn from(e: anyhow::Error) -> Self {
        Failure::Failed(e)
    }
}

impl From<std::io::Error> for Failure {
    fn from(e: std::io::Error) -> Self {
        Failure::Failed(e.into())
    }
}

pub fn refused<T>(message: impl Into<String>) -> Result<T, Failure> {
    Err(Failure::Refused(message.into()))
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum GameEnding {
    Natural,
    WinAdjudication,
    DrawAdjudication,
    Terminated, // Node budget exceeded or no move found; the game is dropped
}

pub struct PlayedGame {
    pub seed: u64,
    pub ending: GameEnding,
    pub entries: Vec<sfbinpack::TrainingDataEntry>,
    /// Result from white's point of view.
    pub result: i16,
}

pub enum WorkerMsg {
    Game(PlayedGame),
    Failed(String),
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::collections::HashSet;
    use std::path::Path;
    use std::time::Duration;

    use rand::{Rng, SeedableRng};
    use sfbinpack::TrainingDataEntry;

    use super::Failure;
    use super::cli::execute;
    use super::run::{
        CONFIG_FILE_NAME, Outcome, PendingSeed, RunConfig, Stats, current_engine_config,
    };
    use super::session::Hooks;
    use super::shard;
    use crate::engine::{chess_v2::ChessGame, tables::Tables};
    use crate::selfplay::fen_feeder::StatefulFenQueue;
    use crate::util::{self, test_dir::TestDir};

    const SEEDS: usize = 16;
    const DEPTH: &str = "2";
    const SHARD_SIZE: &str = "300";

    fn legal_moves(board: &ChessGame, tables: &Tables) -> Vec<u16> {
        let mut moves = [0u16; 256];
        (0..board.gen_moves_avx512::<false, _>(&mut moves))
            .map(|i| moves[i])
            .filter(|&mv| {
                let mut next = *board;
                let made = unsafe { next.make_move(mv, tables) };
                made && !next.in_check(tables, !next.b_move())
            })
            .collect()
    }

    /// Unique, playable positions a few random plies from the start.
    fn write_seeds(dir: &Path, count: usize) -> String {
        let tables = Tables::new();
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);
        let mut seen = HashSet::new();
        let mut fens = Vec::new();

        while fens.len() < count {
            let mut board = ChessGame::new();
            board.load_fen(util::FEN_STARTPOS, &tables).unwrap();

            for _ in 0..rng.random_range(4..10) {
                let legal = legal_moves(&board, &tables);
                if legal.is_empty() {
                    break;
                }
                let mv = legal[rng.random_range(0..legal.len())];
                unsafe { board.make_move(mv, &tables) };
            }

            if !legal_moves(&board, &tables).is_empty()
                && seen.insert(board.canonical_seed_key(&tables))
            {
                fens.push(board.gen_fen());
            }
        }

        let path = dir.join("positions.epd");
        std::fs::write(&path, fens.join("\n") + "\n").unwrap();
        path.to_string_lossy().into_owned()
    }

    struct Fixture {
        dir: TestDir,
        positions: String,
    }

    impl Fixture {
        fn new(name: &str) -> Self {
            let dir = TestDir::new(name);
            let positions = write_seeds(dir.path(), SEEDS);
            Self { dir, positions }
        }

        fn base(&self) -> String {
            self.dir.path().to_string_lossy().into_owned()
        }

        fn new_run(
            &self,
            name: &str,
            extra: &[&str],
            hooks: &Hooks,
        ) -> Result<Outcome, super::Failure> {
            let mut args = vec![
                "--new",
                name,
                "--rundir",
                &self.base(),
                "--positions",
                &self.positions,
                "--depth",
                DEPTH,
                "--win-adj",
                "--draw-adj",
            ]
            .into_iter()
            .map(str::to_string)
            .collect::<Vec<_>>();
            if !extra.contains(&"--shard-size") {
                args.extend(["--shard-size".to_string(), SHARD_SIZE.to_string()]);
            }
            args.extend(extra.iter().map(|s| s.to_string()));
            execute(args.into_iter(), hooks, &|_: &str| -> bool {
                panic!("unexpected prompt")
            })
        }

        fn resume(
            &self,
            name: &str,
            extra: &[&str],
            hooks: &Hooks,
            confirm: &dyn Fn(&str) -> bool,
        ) -> Result<Outcome, super::Failure> {
            let mut args: Vec<String> = ["--resume", name, "--rundir", &self.base()]
                .into_iter()
                .map(str::to_string)
                .collect();
            args.extend(extra.iter().map(|s| s.to_string()));
            execute(args.into_iter(), hooks, confirm)
        }

        fn config(&self, name: &str) -> RunConfig {
            let text =
                std::fs::read_to_string(self.dir.path().join(name).join(CONFIG_FILE_NAME)).unwrap();
            toml::from_str(&text).unwrap()
        }

        /// Every entry of the run, rendered and sorted, after checking each shard against the config.
        fn entries(&self, name: &str) -> Vec<String> {
            let config = self.config(name);
            let shard_size: u64 = SHARD_SIZE.parse().unwrap();
            let mut entries = Vec::new();

            let mut totals = Stats::default();
            for shard in &config.shards {
                let path = self.dir.path().join(name).join(&shard.file);
                let bytes = std::fs::read(&path).unwrap_or_default();
                assert_eq!(bytes.len() as u64, shard.bytes, "{}", shard.file);

                let decoded = shard::decode(&bytes).unwrap();
                assert_eq!(
                    decoded.len() as u64,
                    shard.stats.positions,
                    "{}",
                    shard.file
                );
                assert_eq!(
                    games(&decoded).len() as u64,
                    shard.stats.games,
                    "{}",
                    shard.file
                );

                if shard.closed {
                    let last_game = games(&decoded).last().map_or(0, |game| game.len()) as u64;
                    assert!(
                        shard.stats.positions >= shard_size
                            && shard.stats.positions - last_game < shard_size,
                        "{} closed at {} positions, its last game has {}",
                        shard.file,
                        shard.stats.positions,
                        last_game
                    );
                }

                totals.add(&shard.stats);
                entries.extend(decoded.iter().map(describe));
            }

            assert!(same_counts(&totals, &config.totals));
            entries.sort();
            entries
        }
    }

    fn describe(entry: &TrainingDataEntry) -> String {
        format!(
            "{} {} {} {} {}",
            entry.pos.fen().unwrap(),
            entry.mv.as_uci(),
            entry.score,
            entry.ply,
            entry.result
        )
    }

    fn games(entries: &[TrainingDataEntry]) -> Vec<&[TrainingDataEntry]> {
        let mut games = Vec::new();
        let mut start = 0;
        for i in 1..=entries.len() {
            if i == entries.len() || !entries[i - 1].is_continuation(&entries[i]) {
                games.push(&entries[start..i]);
                start = i;
            }
        }
        games
    }

    fn same_counts(a: &Stats, b: &Stats) -> bool {
        (
            a.games,
            a.positions,
            a.white_wins,
            a.draws,
            a.black_wins,
            a.win_adj,
            a.draw_adj,
            a.terminated,
        ) == (
            b.games,
            b.positions,
            b.white_wins,
            b.draws,
            b.black_wins,
            b.win_adj,
            b.draw_adj,
            b.terminated,
        )
    }

    fn outcome(result: Result<Outcome, Failure>) -> Outcome {
        match result {
            Ok(outcome) => outcome,
            Err(Failure::Refused(message)) => panic!("refused: {}", message),
            Err(Failure::Failed(e)) => panic!("failed: {:#}", e),
        }
    }

    fn refusal(result: Result<Outcome, Failure>) -> String {
        match result {
            Err(Failure::Refused(message)) => message,
            Err(Failure::Failed(e)) => panic!("failed instead of refused: {:#}", e),
            Ok(outcome) => panic!("accepted: {:?}", outcome),
        }
    }

    fn failure(result: Result<Outcome, Failure>) -> String {
        match result {
            Err(Failure::Failed(e)) => format!("{:#}", e),
            Err(Failure::Refused(message)) => panic!("refused instead of failed: {}", message),
            Ok(outcome) => panic!("accepted: {:?}", outcome),
        }
    }

    fn always(answer: bool) -> impl Fn(&str) -> bool {
        move |_| answer
    }

    /// The finished run accounts for every seed exactly once and matches the reference game for game.
    fn assert_matches_reference(fixture: &Fixture, name: &str, reference: &[String]) {
        let config = fixture.config(name);
        assert!(
            config.progress.pending.is_empty(),
            "{}: seeds left pending",
            name
        );
        assert_eq!(config.progress.cursor, SEEDS as u64, "{}", name);
        assert!(
            config
                .sessions
                .iter()
                .all(|session| session.outcome != Outcome::Running),
            "{}",
            name
        );
        assert_eq!(fixture.entries(name), reference, "{}", name);
    }

    fn reference(fixture: &Fixture) -> Vec<String> {
        assert_eq!(
            outcome(fixture.new_run("reference", &[], &Hooks::default())),
            Outcome::Finished
        );
        let entries = fixture.entries("reference");
        let config = fixture.config("reference");
        assert_eq!(config.input_seeds, SEEDS as u64);
        assert_eq!(config.totals.games + config.totals.terminated, SEEDS as u64);
        assert!(
            config.shards.len() > 1,
            "the fixture should fill several shards"
        );
        entries
    }

    #[test]
    fn test_games_depend_only_on_their_seed() {
        let fixture = Fixture::new("selfplay_threads");
        let reference = reference(&fixture);

        assert_eq!(
            outcome(fixture.new_run("threads", &["--threads", "4"], &Hooks::default())),
            Outcome::Finished
        );
        assert_matches_reference(&fixture, "threads", &reference);

        let config = fixture.config("threads");
        assert!(config.progress.exhausted && config.is_complete());
        assert_eq!(
            outcome(fixture.resume(
                "threads",
                &["--count", "100"],
                &Hooks::default(),
                &always(false)
            )),
            Outcome::Finished,
            "an exhausted run resumes as a no-op"
        );
        assert_eq!(fixture.config("threads"), config);
    }

    #[test]
    fn test_resume_after_every_stop_point_matches_reference() {
        let fixture = Fixture::new("selfplay_stops");
        let reference = reference(&fixture);

        for k in 1..=SEEDS as u64 {
            let name = format!("stop{}", k);
            let stop = Hooks {
                stop_after_games: Some(k),
                ..Hooks::default()
            };
            // The hook fires on the k-th game even when it is the last one.
            let first = outcome(fixture.new_run(&name, &[], &stop));
            assert_eq!(first, Outcome::Interrupted, "{}", name);

            let config = fixture.config(&name);
            assert_eq!(
                config.totals.games + config.totals.terminated,
                k,
                "{}",
                name
            );

            let second = fixture.resume(&name, &[], &Hooks::default(), &always(false));
            assert_eq!(outcome(second), Outcome::Finished, "{}", name);
            assert_matches_reference(&fixture, &name, &reference);
        }
    }

    #[test]
    fn test_crashes_and_stops_across_thread_counts_match_reference() {
        let fixture = Fixture::new("selfplay_crashes");
        let reference = reference(&fixture);

        let questions = RefCell::new(Vec::new());
        let confirm = |question: &str| {
            questions.borrow_mut().push(question.to_string());
            true
        };

        let hooks = |stop: Option<u64>, crash: Option<u64>| Hooks {
            stop_after_games: stop,
            crash_after_games: crash,
            checkpoint_interval: Some(Duration::ZERO),
        };

        let name = "crashes";
        assert_eq!(
            outcome(fixture.new_run(name, &["--threads", "4"], &hooks(None, Some(3)))),
            Outcome::Crashed
        );
        assert_eq!(
            outcome(fixture.resume(name, &[], &hooks(Some(3), None), &confirm)),
            Outcome::Interrupted
        );
        assert_eq!(
            outcome(fixture.resume(name, &["--threads", "2"], &hooks(None, Some(5)), &confirm)),
            Outcome::Crashed
        );
        assert_eq!(
            outcome(fixture.resume(name, &["--threads", "3"], &Hooks::default(), &confirm)),
            Outcome::Finished
        );

        let questions = questions.into_inner();
        assert_eq!(
            questions
                .iter()
                .filter(|q| q.starts_with("Truncate"))
                .count(),
            2,
            "{:?}",
            questions
        );
        assert_matches_reference(&fixture, name, &reference);

        let sessions: Vec<Outcome> = fixture
            .config(name)
            .sessions
            .iter()
            .map(|session| session.outcome)
            .collect();
        assert_eq!(
            sessions,
            [
                Outcome::Crashed,
                Outcome::Interrupted,
                Outcome::Crashed,
                Outcome::Finished
            ]
        );
    }

    #[test]
    fn test_max_positions_stops_after_the_crossing_game() {
        let fixture = Fixture::new("selfplay_max_positions");
        let reference = reference(&fixture);

        let name = "capped";
        assert_eq!(
            outcome(fixture.new_run(
                name,
                &["--max-positions", "500", "--threads", "2"],
                &Hooks::default()
            )),
            Outcome::MaxPositions
        );
        let config = fixture.config(name);
        assert!(config.is_complete());

        // Games land in write order, so the crossing game is the last one of the last shard.
        let last_shard = config.shards.last().unwrap();
        let bytes = std::fs::read(fixture.dir.path().join(name).join(&last_shard.file)).unwrap();
        let decoded = shard::decode(&bytes).unwrap();
        let crossing = games(&decoded).last().unwrap().len() as u64;
        assert!(config.totals.positions >= 500 && config.totals.positions - crossing < 500);

        assert_eq!(
            outcome(fixture.resume(name, &[], &Hooks::default(), &always(false))),
            Outcome::Finished,
            "a complete run resumes as a no-op"
        );
        assert_eq!(
            fixture.config(name),
            config,
            "the no-op resume changed the config"
        );

        // Killed after the commit that completed the run, before the session recorded its end.
        let config_path = fixture.dir.path().join(name).join(CONFIG_FILE_NAME);
        let mut stale = config.clone();
        stale.sessions.last_mut().unwrap().outcome = Outcome::Running;
        std::fs::write(&config_path, toml::to_string_pretty(&stale).unwrap()).unwrap();
        assert_eq!(
            outcome(fixture.resume(name, &[], &Hooks::default(), &always(false))),
            Outcome::Finished
        );
        let mut expected = config.clone();
        expected.sessions.last_mut().unwrap().outcome = Outcome::Crashed;
        assert_eq!(fixture.config(name), expected);

        assert_eq!(
            outcome(fixture.resume(
                name,
                &["--max-positions", "1000000"],
                &Hooks::default(),
                &always(false)
            )),
            Outcome::Finished
        );
        assert_matches_reference(&fixture, name, &reference);
    }

    #[test]
    fn test_count_bounds_the_seeds() {
        let fixture = Fixture::new("selfplay_count");
        let reference = reference(&fixture);

        let name = "counted";
        assert_eq!(
            outcome(fixture.new_run(name, &["--count", "5"], &Hooks::default())),
            Outcome::Finished
        );
        let config = fixture.config(name);
        assert_eq!(config.progress.cursor, 5);
        assert_eq!(config.totals.games + config.totals.terminated, 5);

        assert!(
            refusal(fixture.resume(name, &["--count", "4"], &Hooks::default(), &always(false)))
                .contains("already handed out")
        );

        assert_eq!(
            outcome(fixture.resume(name, &["--count", "16"], &Hooks::default(), &always(false))),
            Outcome::Finished
        );
        assert_matches_reference(&fixture, name, &reference);
    }

    #[test]
    fn test_engine_mismatch_prompt() {
        let fixture = Fixture::new("selfplay_engine");
        let reference = reference(&fixture);

        let name = "engine";
        let stop = Hooks {
            stop_after_games: Some(4),
            ..Hooks::default()
        };
        assert_eq!(
            outcome(fixture.new_run(name, &[], &stop)),
            Outcome::Interrupted
        );

        // Pretend the run was started by a build with another tunable value.
        let config_path = fixture.dir.path().join(name).join(CONFIG_FILE_NAME);
        let mut config = fixture.config(name);
        config
            .engine_config
            .get_mut("tunables")
            .unwrap()
            .insert("RFP_MARGIN".to_string(), toml::Value::Integer(999));
        std::fs::write(&config_path, toml::to_string_pretty(&config).unwrap()).unwrap();
        let before = std::fs::read(&config_path).unwrap();

        assert!(
            refusal(fixture.resume(name, &[], &Hooks::default(), &always(false)))
                .contains("engine mismatch")
        );
        assert_eq!(
            std::fs::read(&config_path).unwrap(),
            before,
            "a declined resume wrote"
        );

        let stop = Hooks {
            stop_after_games: Some(2),
            ..Hooks::default()
        };
        assert_eq!(
            outcome(fixture.resume(name, &[], &stop, &always(true))),
            Outcome::Interrupted
        );

        // The accepted engine replaced the recorded one, so there is no second prompt.
        assert_eq!(fixture.config(name).engine_config, current_engine_config());
        let finish = fixture.resume(name, &[], &Hooks::default(), &|_: &str| -> bool {
            panic!("no prompt expected")
        });
        assert_eq!(outcome(finish), Outcome::Finished);
        assert_matches_reference(&fixture, name, &reference);
    }

    #[test]
    fn test_refusals_and_failures_leave_the_run_untouched() {
        let fixture = Fixture::new("selfplay_refusals");
        let base = fixture.base();

        assert!(
            refusal(fixture.resume("missing", &[], &Hooks::default(), &always(true)))
                .contains("not found")
        );

        let stop = Hooks {
            stop_after_games: Some(2),
            ..Hooks::default()
        };
        assert_eq!(
            outcome(fixture.new_run("run", &[], &stop)),
            Outcome::Interrupted
        );
        assert!(refusal(fixture.new_run("run", &[], &Hooks::default())).contains("already exists"));

        let missing_dir = execute(
            ["--resume", "run", "--rundir", &format!("{}\\nope", base)]
                .into_iter()
                .map(str::to_string),
            &Hooks::default(),
            &always(true),
        );
        assert!(refusal(missing_dir).contains("runs directory"));

        let config_path = fixture.dir.path().join("run").join(CONFIG_FILE_NAME);
        let config_before = std::fs::read(&config_path).unwrap();

        // A stray binpack in the run directory.
        let stray = fixture.dir.path().join("run").join("selfplay.9999.binpack");
        std::fs::write(&stray, b"").unwrap();
        assert!(
            failure(fixture.resume("run", &[], &Hooks::default(), &always(true)))
                .contains("not a shard")
        );
        std::fs::remove_file(&stray).unwrap();

        // Uncommitted bytes in the last shard and the truncation declined.
        let shard_path = fixture
            .dir
            .path()
            .join("run")
            .join(&fixture.config("run").shards.last().unwrap().file);
        let shard_before = std::fs::read(&shard_path).unwrap();
        let mut grown = shard_before.clone();
        grown.extend_from_slice(b"BINP\x05\0\0\0abc");
        std::fs::write(&shard_path, &grown).unwrap();
        assert!(
            refusal(fixture.resume("run", &[], &Hooks::default(), &always(false))).contains("tail")
        );
        assert_eq!(std::fs::read(&shard_path).unwrap(), grown);
        std::fs::write(&shard_path, &shard_before).unwrap();

        // A changed positions file.
        let positions_before = std::fs::read(&fixture.positions).unwrap();
        let mut changed = positions_before.clone();
        changed[0] = if changed[0] == b'r' { b'n' } else { b'r' };
        std::fs::write(&fixture.positions, &changed).unwrap();
        assert!(
            failure(fixture.resume("run", &[], &Hooks::default(), &always(true)))
                .contains("changed")
        );
        std::fs::write(&fixture.positions, &positions_before).unwrap();

        assert_eq!(std::fs::read(&config_path).unwrap(), config_before);
        assert_eq!(
            outcome(fixture.resume("run", &[], &Hooks::default(), &always(true))),
            Outcome::Finished
        );

        // A --from past the input's end is refused before anything is created.
        let listing = || -> Vec<_> {
            std::fs::read_dir(fixture.dir.path())
                .unwrap()
                .map(|entry| entry.unwrap().file_name())
                .collect()
        };
        let before = listing();
        let from = (SEEDS + 1).to_string();
        assert!(
            refusal(fixture.new_run("past", &["--from", &from], &Hooks::default()))
                .contains("--from")
        );
        let past_dry = execute(
            [
                "--dry-run",
                "--positions",
                &fixture.positions,
                "--from",
                &from,
            ]
            .into_iter()
            .map(str::to_string),
            &Hooks::default(),
            &always(true),
        );
        assert!(refusal(past_dry).contains("--from"));
        assert_eq!(listing(), before);

        // Missing positions files are refused before anything is created.
        let nope = format!("{}\\nope.epd", base);
        let missing_new = execute(
            ["--new", "other", "--rundir", &base, "--positions", &nope]
                .into_iter()
                .map(str::to_string),
            &Hooks::default(),
            &always(true),
        );
        assert!(refusal(missing_new).contains("--positions"));
        assert!(!fixture.dir.path().join("other").exists());

        let missing_dry = execute(
            ["--dry-run", "--positions", &nope]
                .into_iter()
                .map(str::to_string),
            &Hooks::default(),
            &always(true),
        );
        assert!(refusal(missing_dry).contains("not found"));
    }

    #[test]
    fn test_failed_game_stops_the_session_and_keeps_its_seed_pending() {
        let fixture = Fixture::new("selfplay_failed_game");

        let name = "failing";
        let stop = Hooks {
            stop_after_games: Some(1),
            ..Hooks::default()
        };
        assert_eq!(
            outcome(fixture.new_run(name, &[], &stop)),
            Outcome::Interrupted
        );

        // A hand-edited config whose next pending seed the binpack writer can't encode.
        let bad_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
        let config_path = fixture.dir.path().join(name).join(CONFIG_FILE_NAME);
        let mut config = fixture.config(name);
        let cursor = config.progress.cursor;
        config.progress.cursor += 1;
        config.progress.pending.insert(
            0,
            PendingSeed {
                cursor,
                fen: bad_fen.to_string(),
            },
        );
        let skipped = StatefulFenQueue::new(
            &[fixture.positions.clone()],
            config.progress.cursor,
            Vec::new(),
            u64::MAX,
            None,
            || false,
        )
        .unwrap()
        .unwrap();
        config.progress.seed_hash = skipped.seed_hash();
        std::fs::write(&config_path, toml::to_string_pretty(&config).unwrap()).unwrap();

        let message = failure(fixture.resume(name, &[], &Hooks::default(), &always(false)));
        assert!(
            message.contains("a game failed") && message.contains("can't encode"),
            "{}",
            message
        );

        let config = fixture.config(name);
        assert_eq!(config.sessions.last().unwrap().outcome, Outcome::Failed);
        assert!(
            config
                .progress
                .pending
                .iter()
                .any(|pending| pending.cursor == cursor && pending.fen == bad_fen)
        );
    }

    #[test]
    fn test_seed_hash_mismatch_fails_before_any_write() {
        let fixture = Fixture::new("selfplay_seed_hash");
        let reference = reference(&fixture);

        let name = "hashed";
        let crash = Hooks {
            crash_after_games: Some(3),
            ..Hooks::default()
        };
        assert_eq!(
            outcome(fixture.new_run(name, &[], &crash)),
            Outcome::Crashed
        );

        // As if a build with other rules for usable positions resumed the run.
        let config_path = fixture.dir.path().join(name).join(CONFIG_FILE_NAME);
        let mut config = fixture.config(name);
        config.progress.seed_hash ^= 1;
        std::fs::write(&config_path, toml::to_string_pretty(&config).unwrap()).unwrap();
        let config_before = std::fs::read(&config_path).unwrap();

        let last = config.shards.last().unwrap();
        let shard_path = fixture.dir.path().join(name).join(&last.file);
        let shard_before = std::fs::read(&shard_path).unwrap();
        assert!(
            shard_before.len() as u64 > last.bytes,
            "the crash should leave a tail"
        );

        let message = failure(fixture.resume(name, &[], &Hooks::default(), &always(true)));
        assert!(message.contains("no longer yields"), "{}", message);
        assert_eq!(std::fs::read(&config_path).unwrap(), config_before);
        assert_eq!(
            std::fs::read(&shard_path).unwrap(),
            shard_before,
            "the confirmed truncation waits for the hash check"
        );

        config.progress.seed_hash ^= 1;
        std::fs::write(&config_path, toml::to_string_pretty(&config).unwrap()).unwrap();
        assert_eq!(
            outcome(fixture.resume(name, &[], &Hooks::default(), &always(true))),
            Outcome::Finished
        );
        assert_matches_reference(&fixture, name, &reference);
        assert_eq!(
            fixture.config(name).progress.seed_hash,
            fixture.config("reference").progress.seed_hash
        );
    }

    #[test]
    fn test_write_failure_rolls_back_to_the_last_checkpoint() {
        let fixture = Fixture::new("selfplay_write_failure");
        let reference = reference(&fixture);

        let name = "read_only";
        let stop = Hooks {
            stop_after_games: Some(2),
            ..Hooks::default()
        };
        assert_eq!(
            outcome(fixture.new_run(name, &["--shard-size", "100000"], &stop)),
            Outcome::Interrupted
        );

        let config = fixture.config(name);
        let shard = config.shards.last().unwrap();
        assert!(!shard.closed);
        let shard_path = fixture.dir.path().join(name).join(&shard.file);
        let set_read_only = |read_only: bool| {
            let mut permissions = std::fs::metadata(&shard_path).unwrap().permissions();
            permissions.set_readonly(read_only);
            std::fs::set_permissions(&shard_path, permissions).unwrap();
        };

        set_read_only(true);
        let message = failure(fixture.resume(name, &[], &Hooks::default(), &always(false)));
        set_read_only(false);
        assert!(message.contains("opening"), "{}", message);

        let after = fixture.config(name);
        assert_eq!(after.progress, config.progress, "nothing was committed");
        assert_eq!(after.sessions.last().unwrap().outcome, Outcome::Running);

        assert_eq!(
            outcome(fixture.resume(name, &[], &Hooks::default(), &always(false))),
            Outcome::Finished
        );
        assert_matches_reference(&fixture, name, &reference);

        let sessions: Vec<Outcome> = fixture
            .config(name)
            .sessions
            .iter()
            .map(|session| session.outcome)
            .collect();
        assert_eq!(
            sessions,
            [Outcome::Interrupted, Outcome::Crashed, Outcome::Finished]
        );
    }

    #[test]
    fn test_shards_must_hold_their_committed_bytes() {
        let fixture = Fixture::new("selfplay_shard_sizes");
        let reference = reference(&fixture);

        let name = "sizes";
        let stop = Hooks {
            stop_after_games: Some(8),
            ..Hooks::default()
        };
        assert_eq!(
            outcome(fixture.new_run(name, &[], &stop)),
            Outcome::Interrupted
        );

        let config = fixture.config(name);
        let dir = fixture.dir.path().join(name);
        let resume_fails = |expected: &str| {
            let message = failure(fixture.resume(name, &[], &Hooks::default(), &always(true)));
            assert!(message.contains(expected), "{}", message);
        };

        let last = dir.join(&config.shards.last().unwrap().file);
        let last_bytes = std::fs::read(&last).unwrap();
        std::fs::write(&last, &last_bytes[..last_bytes.len() - 1]).unwrap();
        resume_fails("should be");
        std::fs::write(&last, &last_bytes).unwrap();

        let closed = config
            .shards
            .iter()
            .find(|shard| shard.closed)
            .expect("8 games fill a shard");
        let closed_path = dir.join(&closed.file);
        let closed_bytes = std::fs::read(&closed_path).unwrap();
        let mut grown = closed_bytes.clone();
        grown.push(0);
        std::fs::write(&closed_path, &grown).unwrap();
        resume_fails("should be");

        std::fs::rename(&closed_path, dir.join("moved_away")).unwrap();
        resume_fails("missing");
        std::fs::rename(dir.join("moved_away"), &closed_path).unwrap();
        std::fs::write(&closed_path, &closed_bytes).unwrap();

        assert_eq!(
            fixture.config(name),
            config,
            "failed resumes change nothing"
        );
        assert_eq!(
            outcome(fixture.resume(name, &[], &Hooks::default(), &always(false))),
            Outcome::Finished
        );
        assert_matches_reference(&fixture, name, &reference);
    }

    #[test]
    fn test_checkpoints_on_every_loop_match_reference() {
        let fixture = Fixture::new("selfplay_ticking");
        let reference = reference(&fixture);

        let ticking = |stop: Option<u64>| Hooks {
            stop_after_games: stop,
            checkpoint_interval: Some(Duration::ZERO),
            ..Hooks::default()
        };

        let name = "ticking";
        let limits = [
            "--count",
            "16",
            "--max-positions",
            "1000000",
            "--threads",
            "3",
        ];
        assert_eq!(
            outcome(fixture.new_run(name, &limits, &ticking(Some(5)))),
            Outcome::Interrupted
        );
        assert_eq!(
            outcome(fixture.resume(name, &[], &ticking(None), &always(false))),
            Outcome::Finished
        );
        assert_matches_reference(&fixture, name, &reference);
    }

    #[test]
    fn test_main_exit_codes() {
        let fixture = Fixture::new("selfplay_main");
        let base = fixture.base();
        let main = |args: &[&str]| super::cli::main(args.iter().map(|s| s.to_string()));

        assert_eq!(main(&["--resume", "missing", "--rundir", &base]), 2);
        assert_eq!(main(&["--new", "run"]), 2);

        let new = [
            "--new",
            "run",
            "--rundir",
            &base,
            "--positions",
            &fixture.positions,
            "--depth",
            DEPTH,
            "--count",
            "2",
            "--win-adj",
            "--draw-adj",
        ];
        assert_eq!(main(&new), 0);

        let before = std::fs::read(&fixture.positions).unwrap();
        std::fs::write(&fixture.positions, b"changed\n").unwrap();
        assert_eq!(main(&["--resume", "run", "--rundir", &base]), 1);
        std::fs::write(&fixture.positions, &before).unwrap();

        assert_eq!(
            main(&["--resume", "run", "--rundir", &base, "--count", "3"]),
            0
        );
        assert_eq!(fixture.config("run").progress.cursor, 3);
    }

    #[test]
    fn test_dry_run_writes_nothing() {
        let fixture = Fixture::new("selfplay_dry");

        let result = execute(
            [
                "--dry-run",
                "--positions",
                &fixture.positions,
                "--depth",
                DEPTH,
                "--count",
                "3",
                "--win-adj",
                "--draw-adj",
            ]
            .into_iter()
            .map(str::to_string),
            &Hooks::default(),
            &always(true),
        );
        assert_eq!(outcome(result), Outcome::Finished);

        let files: Vec<_> = std::fs::read_dir(fixture.dir.path())
            .unwrap()
            .map(|entry| entry.unwrap().file_name())
            .collect();
        assert_eq!(files, ["positions.epd"]);
    }
}
