use std::cell::RefCell;
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use sfbinpack::{ChunkReader, CompressedTrainingDataEntryWriter, TrainingDataEntry};

use crate::util::file_sink::{FileSink, SinkStatus};

const CHUNK_HEADER: usize = 8;

pub fn file_name(index: usize) -> String {
    format!("selfplay.{:04}.binpack", index)
}

/// Appends whole games to a shard, starting at its committed length. `finish` ends the binpack
/// stream by dropping the writer, which flushes the last chunk, and then checks that exactly the
/// written entries landed on disk.
pub struct ShardWriter {
    writer: CompressedTrainingDataEntryWriter<FileSink>,
    status: Rc<RefCell<SinkStatus>>,
    path: PathBuf,
    start: u64,
    entries: u64,
}

impl ShardWriter {
    pub fn open(path: &Path, committed: u64) -> anyhow::Result<Self> {
        let file = OpenOptions::new()
            .append(true)
            .create(committed == 0)
            .open(path)
            .map_err(|e| anyhow::anyhow!("opening {}: {}", path.display(), e))?;

        let len = file.metadata()?.len();
        anyhow::ensure!(
            len == committed,
            "{} is {} bytes but {} are committed",
            path.display(),
            len,
            committed
        );

        let (sink, status) = FileSink::new(file);
        Ok(Self {
            writer: CompressedTrainingDataEntryWriter::new(sink)?,
            status,
            path: path.to_path_buf(),
            start: committed,
            entries: 0,
        })
    }

    pub fn write_game(&mut self, entries: &[TrainingDataEntry]) -> anyhow::Result<()> {
        for entry in entries {
            self.writer.write_entry(entry)?;
        }
        self.entries += entries.len() as u64;
        Ok(())
    }

    /// Returns the shard's new length, which is safe to commit.
    pub fn finish(self) -> anyhow::Result<u64> {
        let Self {
            writer,
            status,
            path,
            start,
            entries,
        } = self;

        drop(writer);

        let mut status = status.borrow_mut();
        if let Some(e) = status.error.take() {
            anyhow::bail!("writing {}: {}", path.display(), e);
        }

        let end = start + status.written;
        let decoded = count_entries(&read_range(&path, start, end)?)
            .map_err(|e| anyhow::anyhow!("{} after byte {}: {}", path.display(), start, e))?;

        anyhow::ensure!(
            decoded == entries,
            "{}: wrote {} entries but {} read back",
            path.display(),
            entries,
            decoded
        );

        Ok(end)
    }
}

fn read_range(path: &Path, start: u64, end: u64) -> anyhow::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let len = file.metadata()?.len();
    anyhow::ensure!(
        len == end,
        "{} is {} bytes, expected {}",
        path.display(),
        len,
        end
    );

    file.seek(SeekFrom::Start(start))?;
    let mut bytes = vec![0u8; (end - start) as usize];
    file.read_exact(&mut bytes)?;
    Ok(bytes)
}

/// Calls `f` with each chunk's payload; a bad header or a chunk running past the end is an error.
fn for_each_chunk(mut bytes: &[u8], mut f: impl FnMut(&[u8])) -> anyhow::Result<()> {
    while !bytes.is_empty() {
        anyhow::ensure!(
            bytes.len() >= CHUNK_HEADER && &bytes[..4] == b"BINP",
            "bad chunk header"
        );

        let size = u32::from_le_bytes(bytes[4..CHUNK_HEADER].try_into().unwrap()) as usize;
        anyhow::ensure!(
            bytes.len() - CHUNK_HEADER >= size,
            "chunk runs past the end of the data"
        );

        f(&bytes[CHUNK_HEADER..CHUNK_HEADER + size]);
        bytes = &bytes[CHUNK_HEADER + size..];
    }
    Ok(())
}

pub fn count_entries(bytes: &[u8]) -> anyhow::Result<u64> {
    let mut entries = 0;
    for_each_chunk(bytes, |chunk| {
        let mut reader = ChunkReader::default();
        while reader.has_next(chunk) {
            reader.next(chunk);
            entries += 1;
        }
    })?;
    Ok(entries)
}

#[cfg(test)]
pub fn decode(bytes: &[u8]) -> anyhow::Result<Vec<TrainingDataEntry>> {
    let mut entries = Vec::new();
    for_each_chunk(bytes, |chunk| entries.extend(sfbinpack::parse_chunk(chunk)))?;
    Ok(entries)
}

pub fn file_len(path: &Path) -> io::Result<Option<u64>> {
    match std::fs::metadata(path) {
        Ok(metadata) => Ok(Some(metadata.len())),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e),
    }
}

pub fn truncate(path: &Path, len: u64) -> io::Result<()> {
    let file = OpenOptions::new().write(true).open(path)?;
    file.set_len(len)?;
    file.sync_all()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::{chess_v2::ChessGame, tables::Tables};
    use crate::selfplay::selfplay::convert_move_to_bp;
    use crate::util::test_dir::TestDir;
    use rand::{Rng, SeedableRng};
    use sfbinpack::chess::position::Position;

    /// Random legal games from the start position; `is_continuation` holds inside each game.
    fn synthetic_games(count: usize, seed: u64) -> Vec<Vec<TrainingDataEntry>> {
        let tables = Tables::new();
        let mut rng = rand::rngs::StdRng::seed_from_u64(seed);

        (0..count)
            .map(|_| {
                let mut board = ChessGame::new();
                board.load_fen(crate::util::FEN_STARTPOS, &tables).unwrap();
                let mut pos = Position::from_fen(crate::util::FEN_STARTPOS).unwrap();
                let mut game = Vec::new();

                for ply in 0..rng.random_range(1..120) {
                    let mut moves = [0u16; 256];
                    let legal: Vec<u16> = (0..board.gen_moves_avx512::<false, _>(&mut moves))
                        .map(|i| moves[i])
                        .filter(|&mv| {
                            let mut next = board;
                            let made = unsafe { next.make_move(mv, &tables) };
                            made && !next.in_check(&tables, !next.b_move())
                        })
                        .collect();
                    if legal.is_empty() {
                        break;
                    }

                    let mv = legal[rng.random_range(0..legal.len())];
                    let entry = TrainingDataEntry {
                        pos,
                        mv: convert_move_to_bp(board.b_move(), mv),
                        score: rng.random_range(-10000..=10000),
                        ply: ply as u16,
                        result: if ply % 2 == 0 { 1 } else { -1 },
                    };
                    pos = pos.after_move(entry.mv);
                    unsafe { board.make_move(mv, &tables) };
                    game.push(entry);
                }
                game
            })
            .collect()
    }

    #[test]
    fn test_shard_writer_reopened_after_every_game_reads_back() {
        let dir = TestDir::new("shard_reopen");
        let path = dir.path().join(file_name(0));
        let games = synthetic_games(40, 7);

        let mut committed = 0;
        for game in &games {
            let mut writer = ShardWriter::open(&path, committed).unwrap();
            writer.write_game(game).unwrap();
            committed = writer.finish().unwrap();
        }

        let bytes = std::fs::read(&path).unwrap();
        assert_eq!(bytes.len() as u64, committed);
        assert_eq!(decode(&bytes).unwrap(), games.concat());
    }

    #[test]
    fn test_shard_writer_sessions_of_any_size_concatenate() {
        let dir = TestDir::new("shard_sessions");
        let path = dir.path().join(file_name(3));
        let games = synthetic_games(60, 11);

        // Sessions of 0..=4 games; an empty one is a checkpoint with nothing new to write.
        let mut committed = 0;
        let mut written = 0;
        let mut session = 0;
        while written < games.len() {
            let take = (session % 5).min(games.len() - written);
            let mut writer = ShardWriter::open(&path, committed).unwrap();
            for game in &games[written..written + take] {
                writer.write_game(game).unwrap();
            }
            let previous = committed;
            committed = writer.finish().unwrap();
            assert_eq!(take == 0, committed == previous, "session {}", session);
            assert_eq!(
                file_len(&path).unwrap(),
                Some(committed),
                "session {}",
                session
            );

            written += take;
            session += 1;
        }

        let bytes = std::fs::read(&path).unwrap();
        assert_eq!(decode(&bytes).unwrap(), games.concat());
        assert_eq!(count_entries(&bytes).unwrap(), games.concat().len() as u64);
    }

    #[test]
    fn test_shard_writer_large_segment_spans_chunks() {
        let dir = TestDir::new("shard_large");
        let path = dir.path().join(file_name(1));
        // ~180 bytes per game, so about three 1 MiB chunks.
        let games = synthetic_games(15000, 3);

        let mut writer = ShardWriter::open(&path, 0).unwrap();
        for game in &games {
            writer.write_game(game).unwrap();
        }
        let committed = writer.finish().unwrap();

        let bytes = std::fs::read(&path).unwrap();
        let mut chunks = 0;
        for_each_chunk(&bytes, |_| chunks += 1).unwrap();
        assert!(chunks > 1, "the data should span several 1 MiB chunks");
        assert_eq!(bytes.len() as u64, committed);
        assert_eq!(decode(&bytes).unwrap(), games.concat());
    }

    #[test]
    fn test_shard_open_refuses_length_mismatch_and_truncate_recovers() {
        let dir = TestDir::new("shard_tail");
        let path = dir.path().join(file_name(0));
        let games = synthetic_games(10, 5);

        let mut writer = ShardWriter::open(&path, 0).unwrap();
        writer.write_game(&games[0]).unwrap();
        let committed = writer.finish().unwrap();

        // Uncommitted data after the checkpoint, ending in a torn chunk.
        let mut writer = ShardWriter::open(&path, committed).unwrap();
        writer.write_game(&games[1]).unwrap();
        drop(writer);
        let mut torn = std::fs::read(&path).unwrap();
        torn.extend_from_slice(b"BINP\x10\0\0\0abc");
        std::fs::write(&path, &torn).unwrap();
        assert!(count_entries(&torn[committed as usize..]).is_err());

        assert!(ShardWriter::open(&path, committed).is_err());
        assert!(ShardWriter::open(&path, 0).is_err());

        truncate(&path, committed).unwrap();
        let mut writer = ShardWriter::open(&path, committed).unwrap();
        writer.write_game(&games[2]).unwrap();
        writer.finish().unwrap();

        let bytes = std::fs::read(&path).unwrap();
        assert_eq!(
            decode(&bytes).unwrap(),
            [&games[0][..], &games[2][..]].concat()
        );
    }

    #[test]
    fn test_count_entries_checks_framing() {
        assert_eq!(count_entries(b"").unwrap(), 0);
        assert_eq!(count_entries(b"BINP\0\0\0\0").unwrap(), 0);
        assert!(count_entries(b"BIN").is_err(), "short header");
        assert!(count_entries(b"XINP\0\0\0\0").is_err(), "bad magic");
        assert!(
            count_entries(b"BINP\x04\0\0\0abc").is_err(),
            "chunk past the end"
        );
    }

    #[test]
    fn test_finish_detects_entries_lost_in_the_file() {
        let dir = TestDir::new("shard_verify");
        let path = dir.path().join(file_name(0));
        let games = synthetic_games(3, 9);

        let mut writer = ShardWriter::open(&path, 0).unwrap();
        writer.write_game(&games[0]).unwrap();
        writer.entries += 1;
        assert!(writer.finish().is_err());
    }
}
