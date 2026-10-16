use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Component, Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crate::selfplay::selfplay::fingerprint_consts;
use crate::selfplay::{Failure, refused};
use crate::util::fnv1a::{FNV_OFFSET, fnv1a};
use serde::{Deserialize, Serialize};

use crate::engine::search::{search, tunables};

pub const CONFIG_FILE_NAME: &str = "config.toml";
const CONFIG_FILE_VERSION: u32 = 1;

static SEARCH_FLAGS: [(&str, bool); 3] = [
    ("EVAL_NNUE", search::EVAL_NNUE),
    ("FLAG_NMP_MATE_CLAMP", search::FLAG_NMP_MATE_CLAMP),
    ("FLAG_TT_50MV_GUARD", search::FLAG_TT_50MV_GUARD),
];

// section -> name -> value
pub type Engine = BTreeMap<String, BTreeMap<String, toml::Value>>;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RunConfig {
    pub version: u32,
    pub name: String,
    pub created: u64,
    /// Unique usable positions across the inputs: the seeds the run can hand out.
    pub input_seeds: u64,
    pub params: Params,
    pub progress: Progress,
    pub totals: Stats,
    pub inputs: Vec<InputFile>,
    pub engine_config: Engine,
    pub shards: Vec<Shard>,
    pub sessions: Vec<Session>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Params {
    pub from: u64,
    pub count: Option<u64>,
    pub max_positions: Option<u64>,
    pub threads: usize,
    pub depth: u8,
    pub shard_size: Option<u64>,
    pub win_adj: bool,
    pub draw_adj: bool,
}

impl Params {
    /// No seed at or past this index is handed out.
    pub fn end(&self) -> u64 {
        self.count.map_or(u64::MAX, |count| self.from + count)
    }
}

/// Every seed below `cursor` has its game in a shard, was dropped at the node cap, or is pending.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Progress {
    pub cursor: u64,
    /// The positions input has no seeds left.
    pub exhausted: bool,
    /// Hash of the seeds below `cursor`; resume checks that it is about to skip the same ones.
    #[serde(with = "hex")]
    pub seed_hash: u64,
    pub pending: Vec<PendingSeed>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct PendingSeed {
    pub cursor: u64,
    pub fen: String,
}

/// Results are from white's point of view. Terminated games hit the node cap and were not written.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
pub struct Stats {
    pub games: u64,
    pub positions: u64,
    pub white_wins: u64,
    pub draws: u64,
    pub black_wins: u64,
    pub win_adj: u64,
    pub draw_adj: u64,
    pub terminated: u64,
    pub seconds: f64,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct InputFile {
    /// Relative to the run directory
    pub path: String,
    #[serde(with = "hex")]
    pub hash: u64,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Shard {
    pub file: String,
    /// Committed length. Bytes past it were written after the last checkpoint -> their seeds are
    /// still pending, so they are cut off on resume.
    pub bytes: u64,
    pub closed: bool,
    pub created: u64,
    pub closed_at: Option<u64>,
    /// Lowest and highest cursor a game written to this shard was handed out at.
    pub cursor_min: Option<u64>,
    pub cursor_max: Option<u64>,
    pub stats: Stats,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Session {
    pub started: u64,
    pub ended: Option<u64>,
    pub outcome: Outcome,
    pub threads: usize,
    pub games: u64,
    pub positions: u64,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Running,
    Finished,
    Interrupted,
    MaxPositions,
    Failed,
    Crashed,
}

impl Stats {
    pub fn add(&mut self, other: &Stats) {
        self.games += other.games;
        self.positions += other.positions;
        self.white_wins += other.white_wins;
        self.draws += other.draws;
        self.black_wins += other.black_wins;
        self.win_adj += other.win_adj;
        self.draw_adj += other.draw_adj;
        self.terminated += other.terminated;
        self.seconds += other.seconds;
    }
}

impl RunConfig {
    /// `seed_hash` covers the seeds below `params.from`, which the run skips.
    pub fn new(
        name: &str,
        params: Params,
        inputs: Vec<InputFile>,
        input_seeds: u64,
        engine_config: Engine,
        seed_hash: u64,
    ) -> Self {
        Self {
            version: CONFIG_FILE_VERSION,
            name: name.to_string(),
            created: unix_now(),
            input_seeds,
            progress: Progress {
                cursor: params.from,
                exhausted: false,
                seed_hash,
                pending: Vec::new(),
            },
            params,
            totals: Stats::default(),
            inputs,
            engine_config,
            shards: Vec::new(),
            sessions: Vec::new(),
        }
    }

    pub fn is_complete(&self) -> bool {
        let positions_reached = self
            .params
            .max_positions
            .is_some_and(|max| self.totals.positions >= max);
        let seeds_done = self.progress.pending.is_empty()
            && (self.progress.cursor >= self.params.end() || self.progress.exhausted);
        positions_reached || seeds_done
    }

    pub fn update_totals(&mut self) {
        self.totals = Stats::default();
        for shard in &self.shards {
            self.totals.add(&shard.stats);
        }
    }
}

pub struct RunDir {
    path: PathBuf,
    _lock: File,
}

impl RunDir {
    pub fn ensure_free(base: &Path, name: &str) -> Result<(), Failure> {
        let path = base.join(name);
        if path.exists() {
            return refused(format!("run {} already exists", path.display()));
        }
        Ok(())
    }

    /// The run appears complete or not at all: it is assembled under a temporary name first.
    pub fn create(base: &Path, name: &str, config: &RunConfig) -> Result<Self, Failure> {
        Self::ensure_free(base, name)?;

        let text = toml::to_string_pretty(&config).map_err(anyhow::Error::from)?;
        let path = base.join(name);
        let staging = base.join(format!(".{}.{}.tmp", name, std::process::id()));

        fs::create_dir(&staging)?;
        let assembled = write_synced(&staging.join(CONFIG_FILE_NAME), &text)
            .and_then(|()| fs::rename(&staging, &path));
        if let Err(e) = assembled {
            let _ = fs::remove_dir_all(&staging);
            Self::ensure_free(base, name)?;
            return Err(e.into());
        }
        let lock = lock(&path)?;
        Ok(Self { path, _lock: lock })
    }

    pub fn open(base: &Path, name: &str) -> Result<Self, Failure> {
        let path = base.join(name);
        if !path.join(CONFIG_FILE_NAME).is_file() {
            return refused(format!(
                "run {} not found: {} has no {}",
                name,
                path.display(),
                CONFIG_FILE_NAME
            ));
        }

        let lock = lock(&path)?;
        Ok(Self { path, _lock: lock })
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn load(&self) -> anyhow::Result<RunConfig> {
        let path = self.path.join(CONFIG_FILE_NAME);
        let text = fs::read_to_string(&path)?;
        let config: RunConfig =
            toml::from_str(&text).map_err(|e| anyhow::anyhow!("{}: {}", path.display(), e))?;
        anyhow::ensure!(
            config.version == CONFIG_FILE_VERSION,
            "{}: unsupported config version {}",
            path.display(),
            config.version
        );
        Ok(config)
    }

    /// Replaces the config atomically, so a crash leaves either the old or the new one.
    pub fn save(&self, config: &RunConfig) -> anyhow::Result<()> {
        let text = toml::to_string_pretty(config)?;
        let tmp = self.path.join(format!("{}.tmp", CONFIG_FILE_NAME));
        let target = self.path.join(CONFIG_FILE_NAME);
        write_synced(&tmp, &text)?;

        // A reader holding config.toml open blocks the replace for a moment.
        let mut attempts = 0;
        loop {
            match fs::rename(&tmp, &target) {
                Ok(()) => return Ok(()),
                Err(_) if attempts < 40 => {
                    attempts += 1;
                    std::thread::sleep(Duration::from_millis(50));
                }
                Err(e) => anyhow::bail!("replacing {}: {}", target.display(), e),
            }
        }
    }
}

fn write_synced(path: &Path, text: &str) -> io::Result<()> {
    let mut file = File::create(path)?;
    file.write_all(text.as_bytes())?;
    file.sync_all()
}

// The OS drops the lock with the process, so a crash never leaves the run locked.
fn lock(dir: &Path) -> Result<File, Failure> {
    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(false)
        .open(dir.join("run.lock"))?;

    match file.try_lock() {
        Ok(()) => Ok(file),
        Err(fs::TryLockError::WouldBlock) => {
            refused("the run is in use by another selfplay process")
        }
        Err(fs::TryLockError::Error(e)) => Err(e.into()),
    }
}

pub fn validate_name(name: &str) -> Result<(), Failure> {
    let alnum_ends = name
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphanumeric())
        && name
            .chars()
            .last()
            .is_some_and(|c| c.is_ascii_alphanumeric());
    let charset = name
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-'));

    let stem = name.split('.').next().unwrap_or("").to_ascii_uppercase();
    let device = matches!(stem.as_str(), "CON" | "PRN" | "AUX" | "NUL")
        || (stem.len() == 4
            && (stem.starts_with("COM") || stem.starts_with("LPT"))
            && stem.as_bytes()[3].is_ascii_digit());

    if !(name.len() <= 64 && alnum_ends && charset && !device) {
        return refused(format!(
            "invalid run name \"{}\": use up to 64 of A-Z a-z 0-9 . _ -, starting and ending with a letter or digit",
            name
        ));
    }
    Ok(())
}

/// 64-bit hashes are stored as hex text: TOML integers are i64.
mod hex {
    use serde::{Deserialize, Deserializer, Serializer, de::Error};

    pub fn to_string(hash: u64) -> String {
        format!("{:016x}", hash)
    }

    pub fn serialize<S: Serializer>(hash: &u64, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&to_string(*hash))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<u64, D::Error> {
        let text = String::deserialize(deserializer)?;
        u64::from_str_radix(&text, 16)
            .map_err(|_| D::Error::custom(format!("\"{}\" is not a hex hash", text)))
    }
}

pub fn unix_now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_secs())
}

pub fn digest_input(fen_input_path: &Path) -> io::Result<u64> {
    let mut file = File::open(fen_input_path)?;

    let mut hash = FNV_OFFSET;
    let mut buf = vec![0u8; 4 << 20];
    loop {
        let n = file.read(&mut buf)?;
        if n == 0 {
            return Ok(hash);
        }
        hash = fnv1a(hash, &buf[..n]);
    }
}

/// `target` relative to `base`, or absolute when they don't share a root (another drive).
pub fn relative_path(target: &Path, base: &Path) -> io::Result<PathBuf> {
    let target = std::path::absolute(target)?;
    let base = std::path::absolute(base)?;

    let target_parts: Vec<Component> = target.components().collect();
    let base_parts: Vec<Component> = base.components().collect();

    if target_parts.first() != base_parts.first() {
        return Ok(target);
    }

    let common = target_parts
        .iter()
        .zip(&base_parts)
        .take_while(|(a, b)| a == b)
        .count();

    let mut relative = PathBuf::new();
    for _ in common..base_parts.len() {
        relative.push("..");
    }
    for part in &target_parts[common..] {
        relative.push(part.as_os_str());
    }
    Ok(relative)
}

pub fn current_engine_config() -> Engine {
    use toml::Value::{Boolean, Integer, String as Text};

    let net = BTreeMap::from([
        ("path".to_string(), Text(search::NET_PATH.to_string())),
        ("hidden".to_string(), Integer(search::NET_HIDDEN as i64)),
        ("buckets".to_string(), Integer(search::NET_OSIZE as i64)),
        ("bytes".to_string(), Integer(search::NET_BYTES.len() as i64)),
        (
            "hash".to_string(),
            Text(hex::to_string(fnv1a(FNV_OFFSET, search::NET_BYTES))),
        ),
    ]);

    let flags = SEARCH_FLAGS
        .iter()
        .map(|(name, value)| (name.to_string(), Boolean(*value)))
        .collect();

    let tunables = tunables::values()
        .into_iter()
        .map(|(name, value)| (name.to_string(), Integer(value as i64)))
        .collect();

    Engine::from([
        ("net".to_string(), net),
        ("search".to_string(), flags),
        ("tunables".to_string(), tunables),
        ("selfplay".to_string(), fingerprint_consts()),
    ])
}

/// `[key, recorded, current]` for every value that differs.
pub fn engine_diff(recorded: &Engine, current: &Engine) -> Vec<[String; 3]> {
    let keys: BTreeSet<(&str, &str)> = recorded
        .iter()
        .chain(current)
        .flat_map(|(section, values)| {
            values
                .keys()
                .map(move |name| (section.as_str(), name.as_str()))
        })
        .collect();

    let show =
        |value: Option<&toml::Value>| value.map_or("(missing)".to_string(), |v| v.to_string());

    keys.into_iter()
        .filter_map(|(section, name)| {
            let old = recorded.get(section).and_then(|values| values.get(name));
            let new = current.get(section).and_then(|values| values.get(name));
            (old != new).then(|| [format!("{}.{}", section, name), show(old), show(new)])
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_config() -> RunConfig {
        let stats = Stats {
            games: 10,
            positions: 1234,
            white_wins: 3,
            draws: 5,
            black_wins: 2,
            win_adj: 4,
            draw_adj: 1,
            terminated: 0,
            seconds: 12.5,
        };

        let mut config = RunConfig {
            version: CONFIG_FILE_VERSION,
            name: "unit".to_string(),
            created: 1_700_000_000,
            input_seeds: 239_000,
            params: Params {
                from: 5,
                count: Some(1000),
                max_positions: None,
                threads: 30,
                depth: 9,
                shard_size: Some(10_000_000),
                win_adj: true,
                draw_adj: false,
            },
            progress: Progress {
                cursor: 412,
                exhausted: false,
                seed_hash: u64::MAX,
                pending: vec![
                    PendingSeed {
                        cursor: 400,
                        fen: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string(),
                    },
                    PendingSeed {
                        cursor: 411,
                        fen: "8/8/4k3/8/8/4K3/8/8 w - - 0 1".to_string(),
                    },
                ],
            },
            totals: Stats::default(),
            inputs: vec![InputFile {
                path: r"..\..\openings\UHO_4060_v4.epd".to_string(),
                hash: 0xfedc_ba98_7654_3210, // above i64::MAX
            }],
            engine_config: current_engine_config(),
            shards: vec![
                Shard {
                    file: "selfplay.0000.binpack".to_string(),
                    bytes: 4096,
                    closed: true,
                    created: 1_700_000_001,
                    closed_at: Some(1_700_000_100),
                    cursor_min: Some(5),
                    cursor_max: Some(301),
                    stats: stats.clone(),
                },
                Shard {
                    file: "selfplay.0001.binpack".to_string(),
                    bytes: 0,
                    closed: false,
                    created: 1_700_000_100,
                    closed_at: None,
                    cursor_min: None,
                    cursor_max: None,
                    stats: Stats::default(),
                },
            ],
            sessions: vec![Session {
                started: 1_700_000_000,
                ended: None,
                outcome: Outcome::MaxPositions,
                threads: 30,
                games: 10,
                positions: 1234,
            }],
        };
        config.update_totals();
        config
    }

    #[test]
    fn test_config_round_trip() {
        let config = sample_config();
        let text = toml::to_string_pretty(&config).unwrap();
        let parsed: RunConfig = toml::from_str(&text).unwrap();
        assert_eq!(parsed, config, "{}", text);
        assert_eq!(parsed.totals.positions, 1234);
        assert!(
            text.contains("seed_hash = \"ffffffffffffffff\""),
            "{}",
            text
        );
        assert!(text.contains("hash = \"fedcba9876543210\""), "{}", text);
    }

    #[test]
    fn test_config_create_save_load_and_lock() {
        let dir = crate::util::test_dir::TestDir::new("run_config");
        let config = sample_config();

        let run = RunDir::create(dir.path(), "unit", &config).ok().unwrap();
        assert_eq!(run.load().unwrap(), config);
        assert!(matches!(
            RunDir::create(dir.path(), "unit", &config),
            Err(Failure::Refused(_))
        ));
        assert!(
            matches!(RunDir::open(dir.path(), "unit"), Err(Failure::Refused(_))),
            "the run is locked by `run`"
        );

        let mut changed = config.clone();
        changed.progress.cursor += 1;
        run.save(&changed).unwrap();
        assert_eq!(run.load().unwrap(), changed);

        drop(run);
        let reopened = RunDir::open(dir.path(), "unit").ok().unwrap();
        assert_eq!(reopened.load().unwrap(), changed);

        let entries: Vec<_> = fs::read_dir(dir.path())
            .unwrap()
            .map(|entry| entry.unwrap().file_name())
            .collect();
        assert_eq!(entries, ["unit"], "no staging directory is left behind");
    }

    #[test]
    fn test_create_leaves_nothing_for_an_unstorable_config() {
        let dir = crate::util::test_dir::TestDir::new("run_config_unstorable");
        let mut config = sample_config();
        config.params.count = Some(u64::MAX); // TOML integers are i64

        assert!(RunDir::create(dir.path(), "big", &config).is_err());
        assert_eq!(fs::read_dir(dir.path()).unwrap().count(), 0);
    }

    #[test]
    fn test_engine_diff() {
        let recorded = current_engine_config();
        assert!(engine_diff(&recorded, &current_engine_config()).is_empty());

        let mut current = recorded.clone();
        current
            .get_mut("tunables")
            .unwrap()
            .insert("RFP_MARGIN".to_string(), toml::Value::Integer(-1));
        current.get_mut("search").unwrap().remove("EVAL_NNUE");

        let diff = engine_diff(&recorded, &current);
        assert_eq!(diff.len(), 2);
        assert_eq!(diff[0][0], "search.EVAL_NNUE");
        assert_eq!(diff[0][2], "(missing)");
        assert_eq!(diff[1][0], "tunables.RFP_MARGIN");
        assert_eq!(diff[1][2], "-1");
    }

    #[test]
    fn test_validate_name() {
        for ok in ["a", "uho-d9.1536", "run_2", "A1.b-c_d"] {
            assert!(validate_name(ok).is_ok(), "{}", ok);
        }
        for bad in [
            "", ".", "..", "-a", "a-", "a.", "a/b", r"a\b", "a b", "con", "NUL.x", "com1", "Lpt9",
        ] {
            assert!(validate_name(bad).is_err(), "{}", bad);
        }
        assert!(validate_name(&"a".repeat(64)).is_ok());
        assert!(validate_name(&"a".repeat(65)).is_err());
    }

    #[cfg(windows)]
    #[test]
    fn test_relative_path() {
        let base = Path::new(r"C:\ws\data\selfplay\run");
        assert_eq!(
            relative_path(Path::new(r"C:\ws\data\openings\x.epd"), base).unwrap(),
            Path::new(r"..\..\openings\x.epd")
        );
        assert_eq!(
            relative_path(Path::new(r"D:\other\x.epd"), base).unwrap(),
            Path::new(r"D:\other\x.epd")
        );
    }

    #[test]
    fn test_load_rejects_bad_configs() {
        let dir = crate::util::test_dir::TestDir::new("run_config_bad");
        let run = RunDir::create(dir.path(), "bad", &sample_config())
            .ok()
            .unwrap();
        let path = run.path().join(CONFIG_FILE_NAME);

        std::fs::write(&path, "version = [not toml").unwrap();
        assert!(run.load().is_err());

        let mut config = sample_config();
        config.version = CONFIG_FILE_VERSION + 1;
        std::fs::write(&path, toml::to_string_pretty(&config).unwrap()).unwrap();
        let error = run.load().unwrap_err().to_string();
        assert!(error.contains("unsupported config version"), "{}", error);

        let text = toml::to_string_pretty(&sample_config()).unwrap();
        std::fs::write(&path, text.replace("ffffffffffffffff", "not hex")).unwrap();
        let error = run.load().unwrap_err().to_string();
        assert!(error.contains("not a hex hash"), "{}", error);
    }

    #[cfg(windows)]
    #[test]
    fn test_save_waits_for_a_reader_holding_the_config() {
        use std::os::windows::fs::OpenOptionsExt;
        const FILE_SHARE_READ: u32 = 1;

        let dir = crate::util::test_dir::TestDir::new("run_config_reader");
        let mut config = sample_config();
        let run = RunDir::create(dir.path(), "held", &config).ok().unwrap();

        // Opened like Python's open(): without delete sharing the config can't be replaced.
        let hold = |duration: Duration| {
            let reader = OpenOptions::new()
                .read(true)
                .share_mode(FILE_SHARE_READ)
                .open(run.path().join(CONFIG_FILE_NAME))
                .unwrap();
            std::thread::spawn(move || {
                std::thread::sleep(duration);
                drop(reader);
            })
        };

        config.progress.cursor = 1000;
        let reader = hold(Duration::from_millis(300));
        run.save(&config).unwrap();
        reader.join().unwrap();
        assert_eq!(run.load().unwrap().progress.cursor, 1000);

        config.progress.cursor = 2000;
        let reader = hold(Duration::from_secs(4));
        let error = run.save(&config).unwrap_err().to_string();
        reader.join().unwrap();
        assert!(error.contains("replacing"), "{}", error);
        assert_eq!(
            run.load().unwrap().progress.cursor,
            1000,
            "a failed save leaves the previous config"
        );
    }
}
