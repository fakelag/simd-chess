use std::path::{Path, PathBuf};
use std::str::FromStr;

use super::run::{self, InputFile, Outcome, Params, RunConfig, RunDir};
use super::session::{self, Hooks, Run};
use super::shard;
use crate::selfplay::fen_feeder::{self, StatefulFenQueue};
use crate::selfplay::selfplay::ANNOTATION_DEPTH;
use crate::selfplay::{Failure, refused};
use crate::util::{interrupt, prompt};

const DEFAULT_RUNS_DIR: &str = r"..\..\data\selfplay";

const RESUME_OPTIONS: [&str; 5] = [
    "--resume",
    "--rundir",
    "--count",
    "--max-positions",
    "--threads",
];

const NEW_OPTIONS: [&str; 11] = [
    "--new",
    "--rundir",
    "--positions",
    "--from",
    "--count",
    "--max-positions",
    "--threads",
    "--depth",
    "--shard-size",
    "--win-adj",
    "--draw-adj",
];

const DRY_RUN_OPTIONS: [&str; 9] = [
    "--dry-run",
    "--positions",
    "--from",
    "--count",
    "--max-positions",
    "--threads",
    "--depth",
    "--win-adj",
    "--draw-adj",
];

#[derive(Default)]
struct Args {
    new: Option<String>,
    resume: Option<String>,
    dry_run: bool,
    rundir: Option<String>,
    positions: Vec<String>,
    from: Option<u64>,
    count: Option<u64>,
    max_positions: Option<u64>,
    threads: Option<usize>,
    depth: Option<u8>,
    shard_size: Option<u64>,
    win_adj: bool,
    draw_adj: bool,
    given: Vec<String>,
}

impl Args {
    fn params(&self) -> Params {
        Params {
            from: self.from.unwrap_or(0),
            count: self.count,
            max_positions: self.max_positions,
            threads: self.threads.unwrap_or(1),
            depth: self.depth.unwrap_or(ANNOTATION_DEPTH),
            shard_size: self.shard_size,
            win_adj: self.win_adj,
            draw_adj: self.draw_adj,
        }
    }
}

/// Exit code: 0 finished or stopped with progress saved, 1 failed, 2 refused.
pub fn main(args: impl Iterator<Item = String>) -> i32 {
    if let Err(e) = interrupt::install() {
        eprintln!("selfplay: can't install the Ctrl+C handler: {}", e);
        return 1;
    }

    match execute(args, &Hooks::default(), &prompt::confirm) {
        Ok(_) => 0,
        Err(Failure::Refused(message)) => {
            eprintln!("selfplay: {}", message);
            2
        }
        Err(Failure::Failed(e)) => {
            eprintln!("selfplay: {:#}", e);
            1
        }
    }
}

pub(super) fn execute(
    args: impl Iterator<Item = String>,
    hooks: &Hooks,
    confirm: &dyn Fn(&str) -> bool,
) -> Result<Outcome, Failure> {
    let args = parse(args)?;

    if args.dry_run {
        dry_run(&args, hooks)
    } else if let Some(name) = &args.new {
        new_run(name, &args, hooks)
    } else {
        resume_run(args.resume.as_deref().unwrap(), &args, hooks, confirm)
    }
}

fn parse(mut args: impl Iterator<Item = String>) -> Result<Args, Failure> {
    let mut parsed = Args::default();

    while let Some(arg) = args.next() {
        let option = arg.as_str().to_string();
        match option.as_str() {
            "--new" => parsed.new = Some(value(&mut args, "--new")?),
            "--resume" => parsed.resume = Some(value(&mut args, "--resume")?),
            "--dry-run" => parsed.dry_run = true,
            "--rundir" => parsed.rundir = Some(value(&mut args, "--rundir")?),
            "--positions" => parsed.positions.push(value(&mut args, "--positions")?),
            "--from" => parsed.from = Some(number(&mut args, "--from")?),
            "--count" => parsed.count = Some(number(&mut args, "--count")?),
            "--max-positions" => parsed.max_positions = Some(number(&mut args, "--max-positions")?),
            "--threads" => parsed.threads = Some(number(&mut args, "--threads")?),
            "--depth" => parsed.depth = Some(number(&mut args, "--depth")?),
            "--shard-size" => parsed.shard_size = Some(number(&mut args, "--shard-size")?),
            "--win-adj" => parsed.win_adj = true,
            "--draw-adj" => parsed.draw_adj = true,

            other => return refused(format!("unknown argument {}", other)),
        };

        if option != "--positions" && parsed.given.contains(&option) {
            return refused(format!("{} given twice", option));
        }
        parsed.given.push(option);
    }

    let modes = [
        parsed.new.is_some(),
        parsed.resume.is_some(),
        parsed.dry_run,
    ];
    if modes.iter().filter(|&&mode| mode).count() != 1 {
        return refused("pass exactly one of --new <name>, --resume <name> or --dry-run");
    }

    let (mode, allowed): (&str, &[&str]) = if parsed.resume.is_some() {
        ("--resume", &RESUME_OPTIONS)
    } else if parsed.dry_run {
        ("--dry-run", &DRY_RUN_OPTIONS)
    } else {
        ("--new", &NEW_OPTIONS)
    };

    if let Some(option) = parsed
        .given
        .iter()
        .find(|option| !allowed.contains(&option.as_str()))
    {
        return refused(if mode == "--resume" {
            format!(
                "{} can't be changed on --resume; only --count, --max-positions and --threads can",
                option
            )
        } else {
            format!("{} can't be used with {}", option, mode)
        });
    }

    if mode != "--resume" && parsed.positions.is_empty() {
        return refused("expected at least one --positions <file>");
    }

    let zero = [
        ("--count", parsed.count == Some(0)),
        ("--max-positions", parsed.max_positions == Some(0)),
        ("--threads", parsed.threads == Some(0)),
        ("--depth", parsed.depth == Some(0)),
        ("--shard-size", parsed.shard_size == Some(0)),
    ];
    if let Some((option, _)) = zero.iter().find(|(_, is_zero)| *is_zero) {
        return refused(format!("{} must be at least 1", option));
    }

    // The config stores these as TOML integers, which are i64.
    let stored = [
        ("--from", parsed.from),
        ("--count", parsed.count),
        ("--max-positions", parsed.max_positions),
        ("--threads", parsed.threads.map(|threads| threads as u64)),
        ("--shard-size", parsed.shard_size),
    ];
    if let Some((option, _)) = stored
        .iter()
        .find(|(_, value)| value.is_some_and(|value| value > i64::MAX as u64))
    {
        return refused(format!("{} must be at most {}", option, i64::MAX));
    }

    Ok(parsed)
}

fn value(args: &mut impl Iterator<Item = String>, option: &str) -> Result<String, Failure> {
    args.next()
        .ok_or_else(|| Failure::Refused(format!("{} needs a value", option)))
}

fn number<T: FromStr>(args: &mut impl Iterator<Item = String>, option: &str) -> Result<T, Failure> {
    let raw = value(args, option)?;
    raw.parse()
        .map_err(|_| Failure::Refused(format!("{} expects a number, got \"{}\"", option, raw)))
}

fn runs_base(args: &Args) -> Result<PathBuf, Failure> {
    let base = PathBuf::from(args.rundir.as_deref().unwrap_or(DEFAULT_RUNS_DIR));
    if !base.is_dir() {
        return refused(format!(
            "runs directory {} not found; run from a worktree root or pass --rundir",
            base.display()
        ));
    }
    Ok(base)
}

fn count_seeds(positions: &[String]) -> u64 {
    println!(
        "Counting unique positions in {} input file(s)...",
        positions.len()
    );
    let (seeds, lines) = fen_feeder::count_seeds(positions);
    println!("{} unique positions in {} lines", seeds, lines);
    seeds
}

/// `None` when Ctrl+C stopped the skip to the cursor, which reads every seed below it.
fn make_queue(
    inputs: &[String],
    cursor: u64,
    pending: Vec<(u64, String)>,
    end: u64,
    expected_hash: Option<u64>,
) -> anyhow::Result<Option<StatefulFenQueue>> {
    if cursor > 0 {
        println!("Skipping to seed {}...", cursor);
    }
    let queue = StatefulFenQueue::new(
        inputs,
        cursor,
        pending,
        end,
        expected_hash,
        interrupt::requested,
    )?;
    if queue.is_none() {
        println!("Interrupted before the first game, nothing was written.");
    }
    Ok(queue)
}

/// The skip to `--from` fails when the input has fewer seeds.
fn make_queue_from(
    positions: &[String],
    params: &Params,
) -> Result<Option<StatefulFenQueue>, Failure> {
    make_queue(positions, params.from, Vec::new(), params.end(), None)
        .map_err(|e| Failure::Refused(format!("--from {}: {:#}", params.from, e)))
}

fn dry_run(args: &Args, hooks: &Hooks) -> Result<Outcome, Failure> {
    if let Some(missing) = args
        .positions
        .iter()
        .find(|path| !Path::new(path).is_file())
    {
        return refused(format!("--positions {} not found", missing));
    }

    let input_seeds = count_seeds(&args.positions);
    let params = args.params();
    let Some(queue) = make_queue_from(&args.positions, &params)? else {
        return Ok(Outcome::Interrupted);
    };
    session::play(None, &params, input_seeds, queue, hooks)
}

/// Everything is checked before the run directory appears.
fn new_run(name: &str, args: &Args, hooks: &Hooks) -> Result<Outcome, Failure> {
    run::validate_name(name)?;
    let base = runs_base(args)?;
    RunDir::ensure_free(&base, name)?;

    let mut inputs = Vec::new();
    for path in &args.positions {
        let hash = run::digest_input(Path::new(path))
            .map_err(|e| Failure::Refused(format!("--positions {}: {}", path, e)))?;
        let relative = run::relative_path(Path::new(path), &base.join(name))?;
        inputs.push(InputFile {
            path: relative.to_string_lossy().into_owned(),
            hash,
        });
    }

    let input_seeds = count_seeds(&args.positions);
    let params = args.params();
    let Some(queue) = make_queue_from(&args.positions, &params)? else {
        return Ok(Outcome::Interrupted);
    };

    let mut config = RunConfig::new(
        name,
        params.clone(),
        inputs,
        input_seeds,
        run::current_engine_config(),
        queue.seed_hash(),
    );
    let dir = RunDir::create(&base, name, &config)?;
    println!("Created run {}", dir.path().display());

    let run = Run {
        dir: &dir,
        config: &mut config,
    };
    session::play(Some(run), &params, input_seeds, queue, hooks)
}

fn resume_run(
    name: &str,
    args: &Args,
    hooks: &Hooks,
    confirm: &dyn Fn(&str) -> bool,
) -> Result<Outcome, Failure> {
    let base = runs_base(args)?;
    let dir = RunDir::open(&base, name)?;
    let mut config = dir.load()?;

    for input in &config.inputs {
        let path = dir.path().join(&input.path);
        let hash = run::digest_input(&path)
            .map_err(|e| anyhow::anyhow!("positions file {}: {}", path.display(), e))?;
        if hash != input.hash {
            return Err(Failure::Failed(anyhow::anyhow!(
                "positions file {} changed since the run started; resuming would skip the wrong seeds",
                path.display()
            )));
        }
    }

    // A session that never recorded its end died after its last checkpoint.
    let mut crashed = false;
    for session in &mut config.sessions {
        if session.outcome == Outcome::Running {
            session.outcome = Outcome::Crashed;
            crashed = true;
        }
    }

    let mut updated = config.clone();
    if let Some(count) = args.count {
        let handed_out = config.progress.cursor - config.params.from;
        if count < handed_out {
            return refused(format!(
                "--count {} is below the {} seeds this run already handed out",
                count, handed_out
            ));
        }
        updated.params.count = Some(count);
    }
    if let Some(max_positions) = args.max_positions {
        updated.params.max_positions = Some(max_positions);
    }
    if let Some(threads) = args.threads {
        updated.params.threads = threads;
    }

    if updated.is_complete() {
        if crashed {
            dir.save(&config)?;
        }
        println!(
            "Run '{}' is complete: {} games, {} positions in {} shard(s). Raise --count or --max-positions to continue.",
            config.name,
            config.totals.games,
            config.totals.positions,
            config.shards.len()
        );
        return Ok(Outcome::Finished);
    }
    let mut config = updated;

    let current_engine_config = run::current_engine_config();
    let diff = run::engine_diff(&config.engine_config, &current_engine_config);
    if !diff.is_empty() {
        print_engine_diff(&diff);
        if !confirm("Continue?") {
            return refused("engine mismatch not accepted, nothing changed");
        }
        config.engine_config = current_engine_config;
    }

    let truncate_to = check_shards(&dir, &config, confirm)?;

    let inputs: Vec<String> = config
        .inputs
        .iter()
        .map(|input| dir.path().join(&input.path).to_string_lossy().into_owned())
        .collect();
    let pending = config
        .progress
        .pending
        .iter()
        .map(|pending| (pending.cursor, pending.fen.clone()))
        .collect();
    let Some(queue) = make_queue(
        &inputs,
        config.progress.cursor,
        pending,
        config.params.end(),
        Some(config.progress.seed_hash),
    )?
    else {
        return Ok(Outcome::Interrupted);
    };

    // Every check passed and every question was answered: from here on the run changes.
    if let Some((path, len)) = &truncate_to {
        shard::truncate(path, *len)?;
    }
    dir.save(&config)?;

    let (params, input_seeds) = (config.params.clone(), config.input_seeds);
    let run = Run {
        dir: &dir,
        config: &mut config,
    };
    session::play(Some(run), &params, input_seeds, queue, hooks)
}

fn print_engine_diff(diff: &[[String; 3]]) {
    let width = |column: usize, title: &str| {
        diff.iter()
            .map(|row| row[column].len())
            .chain([title.len()])
            .max()
            .unwrap()
    };
    let (key_width, run_width) = (width(0, "setting"), width(1, "run"));

    println!("The engine differs from the one that wrote this run's latest games:");
    println!(
        "  {:<key_width$}  {:<run_width$}  {}",
        "setting", "run", "current"
    );
    for [key, recorded, current] in diff {
        println!(
            "  {:<key_width$}  {:<run_width$}  {}",
            key, recorded, current
        );
    }
}

/// Every shard file must hold exactly its committed bytes. The open shard may be longer when the
/// last session died after its last checkpoint; its tail is cut off once confirmed.
fn check_shards(
    dir: &RunDir,
    config: &RunConfig,
    confirm: &dyn Fn(&str) -> bool,
) -> Result<Option<(PathBuf, u64)>, Failure> {
    let mut truncate_to = None;

    for (index, shard) in config.shards.iter().enumerate() {
        let path = dir.path().join(&shard.file);
        let len = shard::file_len(&path)?;
        let open_last = index + 1 == config.shards.len() && !shard.closed;

        match len {
            Some(len) if len == shard.bytes => {}
            None if shard.bytes == 0 && open_last => {}
            Some(len) if len > shard.bytes && open_last => {
                println!(
                    "{} is {} bytes but {} are committed: the last session ended without a final checkpoint. \
                     The games past the committed size are played again from their pending seeds.",
                    shard.file, len, shard.bytes
                );
                if !confirm(&format!(
                    "Truncate {} to {} bytes?",
                    shard.file, shard.bytes
                )) {
                    return refused(
                        "the shard keeps its uncommitted tail, so the run can't continue",
                    );
                }
                truncate_to = Some((path, shard.bytes));
            }
            _ => {
                return Err(Failure::Failed(anyhow::anyhow!(
                    "{} should be {} bytes but is {}",
                    path.display(),
                    shard.bytes,
                    len.map_or("missing".to_string(), |len| format!("{} bytes", len))
                )));
            }
        }
    }

    let known: Vec<&str> = config
        .shards
        .iter()
        .map(|shard| shard.file.as_str())
        .collect();
    for entry in std::fs::read_dir(dir.path())? {
        let file_name = entry?.file_name().to_string_lossy().into_owned();
        if file_name.ends_with(".binpack") && !known.contains(&file_name.as_str()) {
            return Err(Failure::Failed(anyhow::anyhow!(
                "{} is not a shard of this run",
                dir.path().join(file_name).display()
            )));
        }
    }

    Ok(truncate_to)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(line: &str) -> Vec<String> {
        line.split_whitespace().map(str::to_string).collect()
    }

    fn refusal(line: &str) -> String {
        match parse(args(line).into_iter()) {
            Err(Failure::Refused(message)) => message,
            Err(Failure::Failed(e)) => panic!("{}: failed instead of refused: {}", line, e),
            Ok(_) => panic!("{}: accepted", line),
        }
    }

    #[test]
    fn test_parse_accepts_each_mode() {
        let new = parse(
            args("--new r --positions a.epd --positions b.epd --from 5 --count 9 --max-positions 7 --threads 3 --depth 4 --shard-size 100 --win-adj --draw-adj --rundir x")
                .into_iter(),
        )
        .ok()
        .unwrap();
        assert_eq!(new.new.as_deref(), Some("r"));
        assert_eq!(new.positions, ["a.epd", "b.epd"]);
        assert_eq!(
            (
                new.from,
                new.count,
                new.max_positions,
                new.threads,
                new.depth,
                new.shard_size
            ),
            (Some(5), Some(9), Some(7), Some(3), Some(4), Some(100))
        );
        assert!(new.win_adj && new.draw_adj);
        assert_eq!(new.rundir.as_deref(), Some("x"));

        let resume = parse(
            args("--resume r --count 9 --max-positions 7 --threads 3 --rundir x").into_iter(),
        )
        .ok()
        .unwrap();
        assert_eq!(resume.resume.as_deref(), Some("r"));

        let dry = parse(args("--dry-run --positions a.epd --threads 2").into_iter())
            .ok()
            .unwrap();
        assert!(dry.dry_run);
    }

    #[test]
    fn test_parse_refusals() {
        for line in [
            "--positions a.epd",
            "--new a --resume b --positions a.epd",
            "--new a --dry-run --positions a.epd",
            "--new a",
            "--dry-run",
            "--new a --positions a.epd --count",
            "--new a --positions a.epd --count x",
            "--new a --positions a.epd --count -1",
            "--new a --positions a.epd --count 0",
            "--new a --positions a.epd --threads 0",
            "--new a --positions a.epd --depth 0",
            "--new a --positions a.epd --depth 300",
            "--new a --positions a.epd --shard-size 0",
            "--new a --positions a.epd --max-positions 0",
            "--new a --positions a.epd --count 9223372036854775808",
            "--new a --positions a.epd --from 9223372036854775808",
            "--new a --positions a.epd --count 5 --count 6",
            "--new a --positions a.epd --out x.binpack",
            "--resume a --positions a.epd",
            "--resume a --from 3",
            "--resume a --depth 3",
            "--resume a --shard-size 3",
            "--resume a --win-adj",
            "--resume a --draw-adj",
            "--dry-run --positions a.epd --rundir x",
            "--dry-run --positions a.epd --shard-size 9",
        ] {
            refusal(line);
        }

        assert!(
            refusal("--resume a --from 3").contains("only --count, --max-positions and --threads")
        );
    }
}
