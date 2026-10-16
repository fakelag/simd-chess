use std::collections::{BTreeMap, HashSet, VecDeque};
use std::io::Read;

use sfbinpack::chess::{coords::Square, position::Position};

use crate::engine::{
    chess_v2::{ChessGame, PieceIndex},
    tables,
};
use crate::util::fnv1a::{FNV_OFFSET, fnv1a};

const MAX_FULLMOVE: u16 = 4096;

struct CountingFileReader {
    name: String,
    chunk: Vec<String>,
    overflow: String,
    buf: Vec<u8>,
    reader: std::io::BufReader<std::fs::File>,
    stride: f64,
    pass: f64,
    active: bool,
    lines: u64,
}

impl CountingFileReader {
    fn new(path: &str) -> Self {
        let file = std::fs::File::open(path).expect("Failed to open FEN file");
        let mut reader = std::io::BufReader::new(file);

        let mut buf = vec![0; 1024 * 1024 * 4];
        let mut lines = 0;
        let mut last_byte = b'\n';
        loop {
            let n = reader.read(&mut buf).expect("Failed to read FEN file");
            if n == 0 {
                break;
            }
            lines += buf[..n].iter().filter(|&&b| b == b'\n').count() as u64;
            last_byte = buf[n - 1];
        }
        if last_byte != b'\n' {
            lines += 1;
        }

        let file = std::fs::File::open(path).expect("Failed to open FEN file");

        Self {
            name: path.to_string(),
            chunk: Vec::new(),
            overflow: String::new(),
            buf,
            reader: std::io::BufReader::new(file),
            stride: if lines > 0 { 1.0 / lines as f64 } else { 0.0 },
            pass: 0.0,
            active: lines > 0,
            lines,
        }
    }

    fn read_chunk(&mut self) {
        loop {
            // The serving order depends on where chunks end, and resumed selfplay runs replay that order, so
            // every chunk is a full buffer no matter how much a single read() returns.
            let mut n = 0;
            while n < self.buf.len() {
                match self
                    .reader
                    .read(&mut self.buf[n..])
                    .expect("Failed to read FEN file")
                {
                    0 => break,
                    read => n += read,
                }
            }

            if n == 0 {
                // The last line has no newline
                if !self.overflow.is_empty() {
                    self.chunk.push(std::mem::take(&mut self.overflow));
                }
                break;
            }

            let Some(bp) = self.buf[..n].iter().rposition(|&b| b == b'\n') else {
                self.overflow
                    .push_str(&String::from_utf8_lossy(&self.buf[..n]));
                continue;
            };

            let content = format!(
                "{}{}",
                self.overflow,
                String::from_utf8_lossy(&self.buf[..bp])
            );
            self.overflow = String::from_utf8_lossy(&self.buf[bp + 1..n]).to_string();

            for lines in content.lines() {
                self.chunk.push(lines.to_string());
            }
            break;
        }
    }

    fn next_line(&mut self) -> Option<String> {
        if self.chunk.is_empty() {
            self.read_chunk();
        }
        self.chunk.pop()
    }
}

pub fn binpack_position(fen: &str) -> Option<Position> {
    let fields: Vec<&str> = fen.split_whitespace().collect();
    if fields.len() < 6 {
        return None;
    }

    let rank_files = |rank: &str| -> u32 {
        rank.chars()
            .map(|c| match c {
                '1'..='8' => c as u32 - '0' as u32,
                'p' | 'n' | 'b' | 'r' | 'q' | 'k' | 'P' | 'N' | 'B' | 'R' | 'Q' | 'K' => 1,
                _ => 99,
            })
            .sum()
    };

    let placement_ok =
        fields[0].split('/').count() == 8 && fields[0].split('/').all(|rank| rank_files(rank) == 8);

    // from_string checks the byte length, then unwraps the second char
    let parsable = placement_ok
        && (fields[3] == "-" || fields[3].is_ascii() && Square::from_string(fields[3]).is_some())
        && fields[4].parse::<u8>().is_ok()
        && fields[5]
            .parse::<u16>()
            .is_ok_and(|fullmove| (1..=MAX_FULLMOVE).contains(&fullmove));

    parsable.then(|| Position::from_fen(fen).ok()).flatten()
}

pub(crate) fn has_legal_move(board: &ChessGame, tables: &tables::Tables) -> bool {
    let mut moves = [0u16; 256];
    (0..board.gen_moves_avx512::<false, _>(&mut moves)).any(|i| {
        let mut next = *board;
        let ok = unsafe { next.make_move(moves[i], tables) };
        ok && !next.in_check(tables, !next.b_move())
    })
}

/// Why a game can't be played from `board`, or the binpack writer can't store its position: it
/// keeps castling rights only as rooks on their corners, en passant only as the pawn that just
/// moved, the side to move as the black king, and at most 32 pieces.
fn check_board_valid(board: &ChessGame, tables: &tables::Tables) -> Option<&'static str> {
    const BACK_RANKS: u64 = 0xFF00_0000_0000_00FF;

    let piece_board = |index: PieceIndex| board.bitboards()[index as usize];
    let spt = |index: PieceIndex, square: u8| piece_board(index) & (1u64 << square) != 0;

    if piece_board(PieceIndex::WhiteKing).count_ones() != 1
        || piece_board(PieceIndex::BlackKing).count_ones() != 1
    {
        return Some("not one king per side");
    }

    if board.occupancy().count_ones() > 32 {
        return Some("more than 32 pieces");
    }

    if (piece_board(PieceIndex::WhitePawn) | piece_board(PieceIndex::BlackPawn)) & BACK_RANKS != 0 {
        return Some("a pawn on the first or last rank");
    }

    let castling = [
        (0b1000, PieceIndex::WhiteKing, 4, PieceIndex::WhiteRook, 7),
        (0b0100, PieceIndex::WhiteKing, 4, PieceIndex::WhiteRook, 0),
        (0b0010, PieceIndex::BlackKing, 60, PieceIndex::BlackRook, 63),
        (0b0001, PieceIndex::BlackKing, 60, PieceIndex::BlackRook, 56),
    ];

    if castling
        .iter()
        .any(|&(right, king, king_sq, rook, rook_sq)| {
            board.castles() & right != 0 && !(spt(king, king_sq) && spt(rook, rook_sq))
        })
    {
        return Some("castling rights without the king and rook on their squares");
    }

    let ep = board.ep_square();
    if ep != 0 {
        let (ep_rank, pawn, pawn_offset) = if board.b_move() {
            (2, PieceIndex::WhitePawn, 8)
        } else {
            (5, PieceIndex::BlackPawn, -8)
        };
        if ep / 8 != ep_rank
            || !spt(pawn, (ep as i32 + pawn_offset) as u8)
            || board.occupancy() & (1u64 << ep) != 0
        {
            return Some("an en-passant square without the pawn that just moved");
        }
    }

    if board.in_check(tables, !board.b_move()) {
        return Some("the side not to move is in check");
    }

    if !has_legal_move(board, tables) {
        return Some("no legal move");
    }
    None
}

/// Serves the unique positions of one or more files in a fixed interleaved order. Files are
/// drawn in proportion to their line counts, and duplicates (including color flip) are dropped.
struct MixedFenFeeder {
    files: Vec<CountingFileReader>,
    seen: HashSet<u64>,
    board: ChessGame,
    tables: tables::Tables,
    served_total: u64,

    served_hash: u64,
    log_skips: bool,
    skipped: u64,
}

impl MixedFenFeeder {
    fn new(paths: &[String]) -> Self {
        assert!(
            !paths.is_empty(),
            "MixedFenFeeder needs at least one input file"
        );

        Self {
            files: paths.iter().map(|p| CountingFileReader::new(p)).collect(),
            seen: HashSet::new(),
            board: ChessGame::new(),
            tables: tables::Tables::new(),
            served_total: 0,
            served_hash: FNV_OFFSET,
            log_skips: true,
            skipped: 0,
        }
    }

    fn key_of(&mut self, fen: &str) -> Option<u64> {
        self.board.load_fen(fen, &self.tables).ok()?;
        Some(self.board.canonical_seed_key(&self.tables))
    }

    fn next_unique(&mut self) -> Option<String> {
        loop {
            let pick = self
                .files
                .iter()
                .enumerate()
                .filter(|(_, f)| f.active)
                .min_by(|(_, a), (_, b)| a.pass.partial_cmp(&b.pass).unwrap())
                .map(|(i, _)| i);

            let Some(i) = pick else {
                return None;
            };

            let mut served_line = None;
            while let Some(line) = self.files[i].next_line() {
                let Some(key) = self.key_of(&line) else {
                    continue;
                };

                if self.seen.contains(&key) {
                    continue;
                }

                // Not marked seen, so a later usable copy of the position is still served.
                let unusable = match binpack_position(&line) {
                    None => Some("the binpack writer can't encode it"),
                    Some(_) => check_board_valid(&self.board, &self.tables),
                };

                if let Some(reason) = unusable {
                    self.log_position_skipped(i, &line, reason);
                    continue;
                }

                self.seen.insert(key);
                served_line = Some(line);
                break;
            }

            match served_line {
                Some(line) => {
                    self.files[i].pass += self.files[i].stride;
                    self.served_total += 1;
                    self.served_hash = fnv1a(fnv1a(self.served_hash, line.as_bytes()), b"\n");
                    return Some(line);
                }
                None => {
                    self.files[i].active = false;
                }
            }
        }
    }

    /// Reads past the seeds below `cursor`, or to the end of the input, without logging skipped
    /// positions. False when `check_interrupt` fired first.
    fn skip_to(&mut self, cursor: u64, check_interrupt: impl Fn() -> bool) -> bool {
        self.log_skips = false;
        while self.served_total < cursor {
            if self.served_total & 0xFFFF == 0 && check_interrupt() {
                return false;
            }
            if self.next_unique().is_none() {
                break;
            }
        }
        self.log_skips = true;
        true
    }

    fn log_position_skipped(&mut self, file: usize, line: &str, reason: &str) {
        const LOGGED_SKIPS: u64 = 100;

        if !self.log_skips {
            return;
        }

        self.skipped += 1;
        if self.skipped <= LOGGED_SKIPS {
            println!(
                "Skipping a position in {} ({}): {}",
                self.files[file].name,
                reason,
                line.trim()
            );
        }

        if self.skipped == LOGGED_SKIPS {
            println!("Further unusable positions are only counted.");
        }
    }
}

/// The seeds the inputs hold and their lines.
pub fn count_seeds(paths: &[String]) -> (u64, u64) {
    let mut feeder = MixedFenFeeder::new(paths);
    feeder.skip_to(u64::MAX, || false);
    let lines = feeder.files.iter().map(|file| file.lines).sum();
    (feeder.served_total, lines)
}

pub struct StatefulFenQueue {
    feeder: MixedFenFeeder,
    replay: VecDeque<(u64, String)>,
    in_flight: BTreeMap<u64, String>,
    end: u64,
    stopped: bool,
    exhausted: bool,
}

impl StatefulFenQueue {
    /// Skipping rebuilds the duplicate filter, so it reads every seed below `cursor`. With
    /// `expected_hash` it also proves they are the seeds an earlier session served. Returns `None`
    /// when `check_interrupt` fires during the skip. No seed at or past `end` is handed out.
    pub fn new(
        paths: &[String],
        cursor: u64,
        replay: Vec<(u64, String)>,
        end: u64,
        expected_hash: Option<u64>,
        check_interrupt: impl Fn() -> bool,
    ) -> anyhow::Result<Option<Self>> {
        let mut feeder = MixedFenFeeder::new(paths);
        if !feeder.skip_to(cursor, check_interrupt) {
            return Ok(None);
        }
        anyhow::ensure!(
            feeder.served_total == cursor,
            "the positions input ends after {} seeds, before the cursor {}",
            feeder.served_total,
            cursor
        );

        if expected_hash.is_some_and(|hash| hash != feeder.served_hash) {
            anyhow::bail!(
                "the positions input no longer yields the {} seeds this run used: the input or the \
                 rules for which positions are usable changed between builds, so resuming would \
                 skip or repeat seeds",
                cursor
            );
        }

        Ok(Some(Self {
            feeder,
            replay: replay.into(),
            in_flight: BTreeMap::new(),
            end,
            stopped: false,
            exhausted: false,
        }))
    }

    pub fn next(&mut self) -> Option<(u64, String)> {
        if self.stopped {
            return None;
        }

        let (seed, fen) = match self.replay.pop_front() {
            Some(replayed) => replayed,
            None if self.feeder.served_total >= self.end => return None,
            None => {
                let seed = self.feeder.served_total;
                let Some(fen) = self.feeder.next_unique() else {
                    self.exhausted = true;
                    return None;
                };
                (seed, fen)
            }
        };

        self.in_flight.insert(seed, fen.clone());
        Some((seed, fen))
    }

    /// The seed's game was written, or dropped for good.
    pub fn complete(&mut self, seed: u64) {
        let removed = self.in_flight.remove(&seed);
        debug_assert!(removed.is_some(), "seed {} completed twice", seed);
    }

    pub fn stop(&mut self) {
        self.stopped = true;
    }

    pub fn cursor(&self) -> u64 {
        self.feeder.served_total
    }

    pub fn end(&self) -> u64 {
        self.end
    }

    pub fn exhausted(&self) -> bool {
        self.exhausted
    }

    /// Num of unusable positions met since the queue was created, not counting the skip to the cursor.
    pub fn skipped(&self) -> u64 {
        self.feeder.skipped
    }

    /// Identifies the seeds below cursor.
    pub fn seed_hash(&self) -> u64 {
        self.feeder.served_hash
    }

    /// Seeds still to play of the `input_seeds` the input holds: the pending ones and those not
    /// handed out yet.
    pub fn seeds_left(&self, input_seeds: u64) -> u64 {
        let pending = (self.in_flight.len() + self.replay.len()) as u64;
        let end = self.end.min(input_seeds);
        end.saturating_sub(self.feeder.served_total) + pending
    }

    /// Seeds handed out without a written game plus replays not handed out yet, in seed order.
    pub fn pending(&self) -> Vec<(u64, String)> {
        let mut pending: Vec<(u64, String)> = self
            .in_flight
            .iter()
            .map(|(seed, fen)| (*seed, fen.clone()))
            .chain(self.replay.iter().cloned())
            .collect();
        pending.sort_unstable_by_key(|(seed, _)| *seed);
        pending
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::test_dir::TestDir;

    const FENS_A: [&str; 8] = [
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3",
        "rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2",
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
        "this is not a fen",
        "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
        "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq - 0 1",
        "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq - 0 1",
    ];

    const FENS_B: [&str; 6] = [
        "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1",
        "rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 5 9",
        "r1bqkbnr/pppppppp/2n5/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 2",
        "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
        "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
        "rnbqkbnr/pppppp1p/6p1/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
    ];

    fn write_inputs(dir: &TestDir) -> Vec<String> {
        [("a.epd", &FENS_A[..]), ("b.epd", &FENS_B[..])]
            .iter()
            .map(|(name, fens)| {
                let path = dir.path().join(name);
                std::fs::write(&path, fens.join("\n") + "\n").unwrap();
                path.to_string_lossy().into_owned()
            })
            .collect()
    }

    fn drain(queue: &mut StatefulFenQueue) -> Vec<(u64, String)> {
        std::iter::from_fn(|| queue.next()).collect()
    }

    #[test]
    fn test_seed_queue_resume_reproduces_order() {
        let dir = TestDir::new("seed_queue");
        let paths = write_inputs(&dir);

        let mut reference_queue =
            StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || false)
                .unwrap()
                .unwrap();
        let reference = drain(&mut reference_queue);
        assert!(reference_queue.exhausted());

        // 14 lines, one unparseable and one duplicate of a position in the other file.
        assert_eq!(reference.len(), 12);
        assert!(
            reference
                .iter()
                .enumerate()
                .all(|(i, (seed, _))| *seed == i as u64)
        );

        for cursor in 0..=reference.len() {
            let replay: Vec<(u64, String)> = reference[..cursor]
                .iter()
                .filter(|(seed, _)| seed % 3 == 1)
                .cloned()
                .collect();

            let mut queue = StatefulFenQueue::new(
                &paths,
                cursor as u64,
                replay.clone(),
                u64::MAX,
                None,
                || false,
            )
            .unwrap()
            .unwrap();

            let mut expected = replay;
            expected.extend_from_slice(&reference[cursor..]);
            assert_eq!(drain(&mut queue), expected, "cursor {}", cursor);
        }
    }

    #[test]
    fn test_seed_queue_end_and_pending() {
        let dir = TestDir::new("seed_queue_end");
        let paths = write_inputs(&dir);

        let mut reference_queue =
            StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || false)
                .unwrap()
                .unwrap();
        let reference = drain(&mut reference_queue);

        let replay = vec![reference[1].clone()];
        let mut queue = StatefulFenQueue::new(&paths, 4, replay, 7, None, || false)
            .unwrap()
            .unwrap();

        let handed: Vec<_> = (0..3).map(|_| queue.next().unwrap()).collect();
        assert_eq!(
            handed,
            [&reference[1], &reference[4], &reference[5]].map(|s| s.clone())
        );

        queue.complete(4);
        assert_eq!(queue.cursor(), 6);
        assert_eq!(
            queue.pending(),
            vec![reference[1].clone(), reference[5].clone()]
        );

        assert_eq!(queue.next(), Some(reference[6].clone()));
        assert_eq!(queue.next(), None, "seed 7 is past the end");
        assert_eq!(queue.cursor(), 7);
        assert!(!queue.exhausted());

        queue.stop();
        assert_eq!(
            queue.pending(),
            vec![
                reference[1].clone(),
                reference[5].clone(),
                reference[6].clone()
            ]
        );
    }

    #[test]
    fn test_seed_queue_input_shorter_than_cursor() {
        let dir = TestDir::new("seed_queue_short");
        let paths = write_inputs(&dir);

        assert!(StatefulFenQueue::new(&paths, 13, vec![], u64::MAX, None, || false).is_err());
        assert!(
            StatefulFenQueue::new(&paths, 12, vec![], u64::MAX, None, || false)
                .unwrap()
                .is_some()
        );
    }

    #[test]
    fn test_seed_queue_interrupted_while_skipping() {
        let dir = TestDir::new("seed_queue_interrupt");
        let paths = write_inputs(&dir);

        assert!(
            StatefulFenQueue::new(&paths, 5, vec![], u64::MAX, None, || true)
                .unwrap()
                .is_none()
        );
        assert!(
            StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || true)
                .unwrap()
                .is_some(),
            "nothing to skip, nothing to interrupt"
        );
    }

    /// Distinct legal king and knight against king placements, enough to span several chunks.
    fn write_large_input(dir: &TestDir, count: usize) -> String {
        let distance = |a: i32, b: i32| ((a % 8 - b % 8).abs(), (a / 8 - b / 8).abs());
        let mut fens = Vec::with_capacity(count);
        'placements: for white_king in 0..64 {
            for black_king in 0..64 {
                for knight in 0..64 {
                    let kings = distance(white_king, black_king);
                    let check = distance(knight, black_king);
                    if white_king == black_king
                        || knight == white_king
                        || knight == black_king
                        || kings.0.max(kings.1) <= 1
                        || check == (1, 2)
                        || check == (2, 1)
                    {
                        continue;
                    }
                    let mut squares = [' '; 64];
                    squares[white_king as usize] = 'K';
                    squares[black_king as usize] = 'k';
                    squares[knight as usize] = 'N';

                    let ranks: Vec<String> = (0..8)
                        .rev()
                        .map(|rank| {
                            let mut text = String::new();
                            let mut empty = 0;
                            for &piece in &squares[rank * 8..rank * 8 + 8] {
                                if piece == ' ' {
                                    empty += 1;
                                    continue;
                                }
                                if empty > 0 {
                                    text.push_str(&empty.to_string());
                                    empty = 0;
                                }
                                text.push(piece);
                            }
                            if empty > 0 {
                                text.push_str(&empty.to_string());
                            }
                            text
                        })
                        .collect();
                    fens.push(format!("{} w - - 0 1", ranks.join("/")));

                    if fens.len() == count {
                        break 'placements;
                    }
                }
            }
        }

        let path = dir.path().join("large.epd");
        std::fs::write(&path, fens.join("\n") + "\n").unwrap();
        path.to_string_lossy().into_owned()
    }

    #[test]
    fn test_seed_queue_resume_across_chunk_boundaries() {
        const COUNT: usize = 180_000;

        let dir = TestDir::new("seed_queue_large");
        let path = write_large_input(&dir, COUNT);
        let bytes = std::fs::read(&path).unwrap();
        assert!(
            bytes.len() > 4 << 20,
            "the file must span more than one chunk"
        );
        let paths = vec![path];

        let mut reference_queue =
            StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || false)
                .unwrap()
                .unwrap();
        let reference = drain(&mut reference_queue);
        assert_eq!(reference.len(), COUNT);

        // The first chunk holds the whole lines of its first 4 MB; the rest of the line it cuts
        // is carried into the second.
        let first_chunk = bytes[..4 << 20].iter().filter(|&&b| b == b'\n').count();
        for cursor in [
            1,
            first_chunk - 1,
            first_chunk,
            first_chunk + 1,
            COUNT - 1,
            COUNT,
        ] {
            let mut queue =
                StatefulFenQueue::new(&paths, cursor as u64, vec![], u64::MAX, None, || false)
                    .unwrap()
                    .unwrap();
            assert!(
                drain(&mut queue) == reference[cursor..],
                "resume at {} diverges",
                cursor
            );
        }
    }

    #[test]
    fn test_count_seeds_matches_what_a_queue_serves() {
        let dir = TestDir::new("seed_queue_count");
        let paths = write_inputs(&dir);
        let take = |queue: &mut StatefulFenQueue, count: usize| {
            for _ in 0..count {
                let (seed, _) = queue.next().unwrap();
                queue.complete(seed);
            }
        };

        // 14 lines, one unparseable and one duplicate of a position in the other file.
        assert_eq!(count_seeds(&paths), (12, 14));

        let mut queue = StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || false)
            .unwrap()
            .unwrap();
        assert_eq!(queue.seeds_left(12), 12);
        let (in_flight, _) = queue.next().unwrap();
        take(&mut queue, 4);
        assert_eq!(queue.seeds_left(12), 7 + 1, "the one in flight counts");

        queue.complete(in_flight);
        take(&mut queue, 7);
        assert_eq!(queue.next(), None);
        assert!(queue.exhausted() && queue.seeds_left(12) == 0);

        let mut counted = StatefulFenQueue::new(&paths, 0, vec![], 8, None, || false)
            .unwrap()
            .unwrap();
        take(&mut counted, 5);
        assert_eq!(counted.seeds_left(12), 3, "--count ends it first");
    }

    #[test]
    fn test_unencodable_log_is_capped_but_all_are_counted() {
        let dir = TestDir::new("seed_queue_many_unencodable");
        let path = dir.path().join("no_counters.epd");

        // Lines are served bottom-up within a chunk, so the encodable copy at the top comes last:
        // served then, the copies before it would be skipped as duplicates instead.
        let mut lines = vec![FENS_A[0]];
        lines.extend(["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -"; 250]);
        std::fs::write(&path, lines.join("\n") + "\n").unwrap();

        let paths = vec![path.to_string_lossy().into_owned()];
        let mut queue = StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || false)
            .unwrap()
            .unwrap();
        assert_eq!(drain(&mut queue), [(0, FENS_A[0].to_string())]);
        assert_eq!(queue.skipped(), 250);
    }

    #[test]
    fn test_binpack_position_never_panics() {
        for fen in [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
            "8/8/4k3/8/8/4K3/8/8 w - - 255 4096",
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 bm e4; id \"x\";",
        ] {
            assert!(binpack_position(fen).is_some(), "{}", fen);
        }

        for fen in [
            "",
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -",
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0",
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w",
            "8/8/4k3/8/8/4K3/8/8 w - - 256 1",
            "8/8/4k3/8/8/4K3/8/8 w - - 0 0",
            "8/8/4k3/8/8/4K3/8/8 w - - 0 4097",
            "8/8/4k3/8/8/4K3/8/8 w - - 0 65536",
            "8/8/4k3/8/8/4K3/8/8 w - - -1 1",
            "8/8/4k3/8/8/4K3/8/8 w - e9 0 1",
            "8/8/4k3/8/8/4K3/8/8 w - e 0 1",
            "8/8/4k3/8/8/4K3/8/8 w - é3 0 1",
            "8/8/4k3/8/8/4K3/8/8 w - é 0 1",
            "8/8/4k3/8/8/4K3/8/8/8 w - - 0 1",
            "8/8/4k3/8/8/4K3/8 w - - 0 1",
            "9/8/4k3/8/8/4K3/8/8 w - - 0 1",
            "8/8/4k3/8/8/4K3/8/7 w - - 0 1",
            "8/8/4kx2/8/8/4K3/8/8 w - - 0 1",
            "8/8/4k3/8/8/4K3/8/0P6 w - - 0 1",
        ] {
            assert!(binpack_position(fen).is_none(), "{}", fen);
        }
    }

    #[test]
    fn test_unencodable_positions_are_skipped_and_counted() {
        let dir = TestDir::new("seed_queue_unencodable");
        let path = dir.path().join("mixed.epd");
        let lines = [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            // load_fen accepts these, the binpack writer can't encode them.
            "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq -",
            "rnbqkb1r/pppppppp/5n2/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 300 2",
            // The first one again, encodable this time: served, not taken for a duplicate.
            "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3",
        ];
        // Lines are served bottom-up within a chunk, so write them reversed.
        let reversed: Vec<&str> = lines.iter().rev().copied().collect();
        std::fs::write(&path, reversed.join("\n") + "\n").unwrap();
        let paths = vec![path.to_string_lossy().into_owned()];

        let mut queue = StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || false)
            .unwrap()
            .unwrap();
        let served: Vec<String> = drain(&mut queue).into_iter().map(|(_, fen)| fen).collect();
        assert_eq!(served, [lines[0], lines[3]]);
        assert_eq!(queue.skipped(), 2);

        // A resume that skips past them neither serves nor reports them again...
        let mut resumed = StatefulFenQueue::new(&paths, 2, vec![], u64::MAX, None, || false)
            .unwrap()
            .unwrap();
        assert_eq!(resumed.next(), None);
        assert_eq!(resumed.skipped(), 0);

        // ...while one whose cursor is still before them meets them for the first time.
        let mut resumed = StatefulFenQueue::new(&paths, 1, vec![], u64::MAX, None, || false)
            .unwrap()
            .unwrap();
        assert_eq!(resumed.next(), Some((1, lines[3].to_string())));
        assert_eq!(resumed.skipped(), 2);
    }

    #[test]
    fn test_unplayable_positions_are_skipped_and_counted() {
        let dir = TestDir::new("seed_queue_unplayable");
        let path = dir.path().join("unplayable.epd");
        let playable = "4k3/8/8/8/8/8/8/4K3 w - - 0 1";
        let unplayable = [
            "R6k/6pp/8/8/8/8/8/7K b - - 0 1", // checkmated
            "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1", // stalemated
            "K6k/8/8/8/8/8/8/K7 w - - 0 1",   // two white kings
            "rnbqkbnr/pppppppp/pppppppp/8/8/PPPPPPPP/PPPPPPPP/RNBQKBNR w - - 0 1", // 48 pieces
            "P6k/8/8/8/8/8/8/7K w - - 0 1",   // pawn on rank 8
            "4k3/8/8/8/8/8/8/4K3 w K - 0 1",  // castling, no rook
            "4k3/8/8/3P4/8/8/8/4K3 w - e6 0 1", // en passant, no pawn
            "4k3/8/8/8/8/8/8/r3K3 b - - 0 1", // side not to move in check
        ];
        let mut lines = vec![playable];
        lines.extend(unplayable);
        std::fs::write(&path, lines.join("\n") + "\n").unwrap();

        let paths = vec![path.to_string_lossy().into_owned()];
        let mut queue = StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || false)
            .unwrap()
            .unwrap();
        assert_eq!(drain(&mut queue), [(0, playable.to_string())]);
        assert_eq!(queue.skipped(), unplayable.len() as u64);
    }

    #[test]
    fn test_last_line_without_newline_is_served() {
        let dir = TestDir::new("seed_queue_no_newline");

        let one = dir.path().join("one.epd");
        std::fs::write(&one, FENS_A[0]).unwrap();
        let mut queue = StatefulFenQueue::new(
            &[one.to_string_lossy().into_owned()],
            0,
            vec![],
            u64::MAX,
            None,
            || false,
        )
        .unwrap()
        .unwrap();
        assert_eq!(drain(&mut queue), [(0, FENS_A[0].to_string())]);

        let two = dir.path().join("two.epd");
        std::fs::write(&two, format!("{}\n{}", FENS_A[0], FENS_A[1])).unwrap();
        let mut queue = StatefulFenQueue::new(
            &[two.to_string_lossy().into_owned()],
            0,
            vec![],
            u64::MAX,
            None,
            || false,
        )
        .unwrap()
        .unwrap();
        let served: Vec<String> = drain(&mut queue).into_iter().map(|(_, fen)| fen).collect();
        assert_eq!(served, [FENS_A[0], FENS_A[1]]);
    }

    #[test]
    fn test_seed_hash_guards_resume() {
        let dir = TestDir::new("seed_queue_hash");
        let paths = write_inputs(&dir);

        let mut queue = StatefulFenQueue::new(&paths, 0, vec![], u64::MAX, None, || false)
            .unwrap()
            .unwrap();
        (0..5).for_each(|_| {
            queue.next().unwrap();
        });
        let hash = queue.seed_hash();

        assert!(
            StatefulFenQueue::new(&paths, 5, vec![], u64::MAX, Some(hash), || false)
                .unwrap()
                .is_some()
        );
        let error = StatefulFenQueue::new(&paths, 5, vec![], u64::MAX, Some(hash ^ 1), || false)
            .err()
            .unwrap()
            .to_string();
        assert!(error.contains("no longer yields"), "{}", error);
        assert!(
            StatefulFenQueue::new(&paths, 4, vec![], u64::MAX, Some(hash), || false).is_err(),
            "the hash covers exactly the seeds below the cursor"
        );

        // An input that serves the same number of seeds, but different ones.
        std::fs::write(
            &paths[1],
            FENS_B.iter().rev().cloned().collect::<Vec<_>>().join("\n") + "\n",
        )
        .unwrap();
        assert!(StatefulFenQueue::new(&paths, 5, vec![], u64::MAX, Some(hash), || false).is_err());
    }
}
