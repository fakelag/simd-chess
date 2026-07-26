use crate::engine::chess_v2::ChessGame;
use crate::engine::tables::Tables;

const REPTABLE_SIZE: usize = 1 << 10;

#[derive(Debug, Clone)]
pub struct RepetitionTable {
    pub cursor: usize,
    pub irreversible: [u64; REPTABLE_SIZE / 64],
    pub hashes: Box<[u64; REPTABLE_SIZE]>,
}

impl RepetitionTable {
    pub fn new() -> Self {
        Self {
            hashes: vec![0; REPTABLE_SIZE]
                .into_boxed_slice()
                .try_into()
                .unwrap(),
            irreversible: [0; REPTABLE_SIZE / 64],
            cursor: 0,
        }
    }

    #[inline(always)]
    pub fn push_position(&mut self, hash: u64, last_move_irreversible: bool) {
        let cursor = self.cursor & (REPTABLE_SIZE - 1);

        let irreversible_bit = 1 << (cursor & 63);
        let irreversible_next = (last_move_irreversible as u64) << (cursor & 63);
        let last_irreversible = cursor / 64;

        let irreversible = self.irreversible[last_irreversible] & !irreversible_bit;
        self.irreversible[last_irreversible] = irreversible | irreversible_next;

        self.hashes[cursor] = hash;
        self.cursor += 1;

        debug_assert!(
            self.cursor <= REPTABLE_SIZE,
            "Repetition table overflow: cursor = {}, size = {}",
            self.cursor,
            REPTABLE_SIZE
        );
    }

    #[inline(always)]
    pub fn pop_position(&mut self) {
        debug_assert!(self.cursor > 0, "Cannot pop from an empty repetition table");
        self.cursor -= 1;
    }

    #[inline(always)]
    pub fn is_repeated(&self, hash: u64) -> bool {
        let cursor = self.cursor & (REPTABLE_SIZE - 1);

        for i in (0..cursor).rev() {
            if self.hashes[i] == hash {
                return true;
            }
            if (self.irreversible[i / 64] & (1 << (i & 63))) != 0 {
                break;
            }
        }

        false
    }

    #[inline(always)]
    pub fn is_repeated_search(&self, hash: u64, half_moves: usize) -> bool {
        let cursor = self.cursor & (REPTABLE_SIZE - 1);
        let end = half_moves.min(cursor);

        let mut i = 2;
        while i <= end {
            debug_assert!(i <= cursor);

            // Safety: i is in [2, end] with end <= cursor <= REPTABLE_SIZE - 1.
            if unsafe { *self.hashes.get_unchecked(cursor - i) } == hash {
                return true;
            }
            i += 2;
        }

        false
    }

    #[inline(always)]
    pub fn is_repeated_times(&self, hash: u64) -> u32 {
        let cursor = self.cursor & (REPTABLE_SIZE - 1);
        let mut num_repetitions = 0;

        for i in (0..cursor).rev() {
            if self.hashes[i] == hash {
                num_repetitions += 1;
            }
            if (self.irreversible[i / 64] & (1 << (i & 63))) != 0 {
                break;
            }
        }

        num_repetitions
    }

    #[inline(always)]
    pub fn check_upcoming_cycle(&self, board: &ChessGame, tables: &Tables, ply: i32) -> bool {
        let cursor = self.cursor & (REPTABLE_SIZE - 1);
        let end = (board.half_moves() as usize).min(cursor);

        if end < 3 {
            return false;
        }

        let original = board.zobrist_key();
        let side = tables.zobrist_hash_keys.hash_side_to_move;
        let occ = board.occupancy();
        let k = |back: usize| {
            debug_assert!(back <= cursor);
            // Safety: back is in [1, end] with end <= cursor <= REPTABLE_SIZE - 1.
            unsafe { *self.hashes.get_unchecked(cursor - back) }
        };

        let mut other = original ^ k(1) ^ side;
        let mut i = 3;
        while i <= end {
            other ^= k(i - 1) ^ k(i) ^ side;
            if other == 0 {
                if let Some(mv) = tables.cuckoo_lookup(original ^ k(i)) {
                    let s1 = (mv & 0x3F) as usize;
                    let s2 = ((mv >> 6) & 0x3F) as usize;
                    if tables.between(s1, s2) & occ == 0 && ply > i as i32 {
                        return true;
                    }
                }
            }
            i += 2;
        }

        false
    }

    #[inline(always)]
    pub fn clear(&mut self) {
        self.cursor = 0;
        self.irreversible = [0; REPTABLE_SIZE / 64];
        self.hashes.fill(0);
    }
}

impl PartialEq for RepetitionTable {
    fn eq(&self, other: &Self) -> bool {
        if self.cursor != other.cursor {
            return false;
        }

        for i in (0..self.cursor).rev() {
            if self.hashes[i] != other.hashes[i] {
                return false;
            }
            if (self.irreversible[i / 64] & (1 << (i & 63))) != 0 {
                break;
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        engine::{chess_v2, tables},
        util,
    };

    use super::*;

    fn get_legal_moves(board: &mut chess_v2::ChessGame, tables: &tables::Tables) -> Vec<u16> {
        let mut legal_moves = vec![];
        let mut move_list = [0u16; 256];
        let move_count = board.gen_moves_slow(tables, &mut move_list);

        for i in 0..move_count {
            let mv = move_list[i];

            let board_copy = board.clone();

            if !unsafe { board.make_move(mv, tables) } || board.in_check(tables, !board.b_move()) {
                *board = board_copy;
                continue;
            }

            legal_moves.push(mv);
            *board = board_copy;
        }

        legal_moves
    }

    // Creates a list of moves with specified length in num_moves_to_play that ends in a repetition
    fn find_moves_with_repetition(
        board: &mut chess_v2::ChessGame,
        tables: &tables::Tables,
        num_moves_to_play: usize,
        moves: &mut Vec<u16>,
    ) -> bool {
        if num_moves_to_play == 0 {
            return true;
        }

        for mv in get_legal_moves(board, tables) {
            let from_sq = mv & 0x3F;
            let to_sq = (mv >> 6) & 0x3F;

            let board_copy = board.clone();

            assert!(unsafe { board.make_move(mv, tables) });

            let is_irreversible =
                board.half_moves() == 0 || board.castles() != board_copy.castles();

            match num_moves_to_play {
                5 if !is_irreversible => {
                    *board = board_copy;
                    continue;
                }
                // Play first 2 reversible quiet moves
                4 | 3 => {
                    if is_irreversible {
                        *board = board_copy;
                        continue;
                    }
                }
                // Play mirror move
                2 | 1 => {
                    let source_mv = moves[moves.len() - 2];

                    if (from_sq != (source_mv >> 6) & 0x3F) || to_sq != (source_mv & 0x3F) {
                        *board = board_copy;
                        continue;
                    }
                }
                _ => {}
            }

            moves.push(mv);
            if find_moves_with_repetition(board, tables, num_moves_to_play - 1, moves) {
                *board = board_copy;
                return true;
            }

            moves.pop();
            *board = board_copy;
        }

        false
    }

    #[test]
    fn test_repetition_table_simple() {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();

        assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());

        let mut table = RepetitionTable::new();

        let moves = [
            ("e2e4", false),
            ("e7e5", false),
            ("g1f3", false),
            ("g8f6", false),
            ("f3h4", false),
            ("f6g8", false),
            ("h4f3", true),
        ];

        table.push_position(board.zobrist_key(), true);

        for (mv_string, is_repeated) in moves {
            let mv = board.fix_move(util::create_move(mv_string));
            assert!(unsafe { board.make_move(mv, &tables) });

            assert_eq!(
                table.is_repeated(board.zobrist_key()),
                is_repeated,
                "Move {} should {}be repeated",
                mv_string,
                if is_repeated { "" } else { "not " }
            );

            table.push_position(board.zobrist_key(), board.half_moves() == 0);
        }

        assert!(
            table.is_repeated(board.zobrist_key()),
            "Last move should be repeated"
        );
        for i in 0..moves.len() {
            table.pop_position();
            assert_eq!(table.is_repeated(board.zobrist_key()), i <= 3);
        }
    }

    #[test]
    fn test_repetition_table_times() {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();

        assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());

        let mut table = RepetitionTable::new();

        let moves = [
            ("e2e4", 0),
            ("e7e5", 0),
            ("g1f3", 0),
            ("g8f6", 0),
            ("f3h4", 0),
            ("f6g8", 0),
            ("h4f3", 1),
            ("g8f6", 1),
            ("f3h4", 1),
            ("f6g8", 1),
            ("h4f3", 2),
            ("g8f6", 2),
            ("f3h4", 2),
            ("f6g8", 2),
            ("h4f3", 3),
            ("g8f6", 3),
            ("f3h4", 3),
            ("f6g8", 3),
            ("h4f3", 4),
            ("g8f6", 4),
            ("f3h4", 4),
            ("f6g8", 4),
            ("h4g6", 0),
            ("g8f6", 0),
            ("g6h4", 5),
            ("f6e4", 0),
            ("h4f3", 0),
            ("e4f6", 0),
            ("f3h4", 0),
        ];

        table.push_position(board.zobrist_key(), true);

        for (mv_string, rep_times) in moves {
            let mv = board.fix_move(util::create_move(mv_string));
            assert!(unsafe { board.make_move(mv, &tables) });

            assert_eq!(
                table.is_repeated_times(board.zobrist_key()),
                rep_times,
                "Move {} should be repeated {} times",
                mv_string,
                rep_times,
            );

            table.push_position(board.zobrist_key(), board.half_moves() == 0);
        }
    }

    #[test]
    fn test_is_repeated_search() {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();
        assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());

        let mut table = RepetitionTable::new();
        let moves = [
            ("e2e4", false),
            ("e7e5", false),
            ("g1f3", false),
            ("g8f6", false),
            ("f3h4", false),
            ("f6g8", false),
            ("h4f3", true),
        ];

        table.push_position(board.zobrist_key(), true);
        for (mv_string, is_repeated) in moves {
            let mv = board.fix_move(util::create_move(mv_string));
            assert!(unsafe { board.make_move(mv, &tables) });

            let hm = board.half_moves() as usize;
            assert_eq!(
                table.is_repeated_search(board.zobrist_key(), hm),
                is_repeated,
                "Move {} search-repeat mismatch",
                mv_string
            );

            table.push_position(board.zobrist_key(), board.half_moves() == 0);
        }
    }

    #[test]
    fn test_is_repeated_search_fuzz() {
        let tables = tables::Tables::new();
        let mut moves = vec![];
        let moves = &mut moves;

        for i in 4..96 {
            let mut board = chess_v2::ChessGame::new();
            assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());

            moves.clear();
            assert!(find_moves_with_repetition(&mut board, &tables, i, moves));

            // Replay contiguously (every position pushed), checking before each push, the way the
            // search calls it. is_repeated_search must flag the repetition only on the final move.
            let mut board = chess_v2::ChessGame::new();
            assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());
            let mut reptable = RepetitionTable::new();
            reptable.push_position(board.zobrist_key(), true);

            for (index, mv) in moves.iter().enumerate() {
                assert!(unsafe { board.make_move(*mv, &tables) });
                let key = board.zobrist_key();
                let hm = board.half_moves() as usize;

                // On contiguous history the specialized search variant must agree with the
                // general scan, and the constructed final move must register as a repetition.
                assert_eq!(
                    reptable.is_repeated_search(key, hm),
                    reptable.is_repeated(key),
                    "search vs scan mismatch: len {}, move index {}",
                    i,
                    index
                );
                if index + 1 == i {
                    assert!(
                        reptable.is_repeated_search(key, hm),
                        "final move must repeat, len {}",
                        i
                    );
                }

                reptable.push_position(key, board.half_moves() == 0);
            }
        }
    }

    #[test]
    fn test_repetition_table_fuzz() {
        let tables = tables::Tables::new();

        // Overflow moves ringbuffer with a random number of legal moves,
        // create a repetition and check if it is detected correctly
        let mut moves = vec![];
        let moves = &mut moves;

        for offset in 4..128 {
            for i in offset..257 {
                let mut reptable = RepetitionTable::new();
                let mut board = chess_v2::ChessGame::new();
                assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());

                reptable.push_position(board.zobrist_key(), true);

                moves.clear();
                assert!(find_moves_with_repetition(&mut board, &tables, i, moves));

                for mv in moves.iter().take(i - 4) {
                    assert!(unsafe { board.make_move(*mv, &tables) });
                    assert!(!board.in_check(&tables, !board.b_move()));

                    reptable.push_position(board.zobrist_key(), board.half_moves() == 0);
                }

                for (index, mv) in moves.iter().enumerate().skip(i - 4) {
                    assert!(unsafe { board.make_move(*mv, &tables) });
                    assert!(!board.in_check(&tables, !board.b_move()));
                    assert!(reptable.is_repeated(board.zobrist_key()) == (index + 1 == i));
                }
            }
        }
    }

    #[test]
    fn test_cuckoo_table_build() {
        let tables = tables::Tables::new();
        let psq = &tables.zobrist_hash_keys.hash_piece_squares_new;
        let side = tables.zobrist_hash_keys.hash_side_to_move;

        let key = psq[5][21] ^ psq[5][31] ^ side;
        let mv = tables
            .cuckoo_lookup(key)
            .expect("white knight f3<->h4 must be a cuckoo entry");
        assert_eq!(
            ((mv & 0x3F) as usize, ((mv >> 6) & 0x3F) as usize),
            (21, 31)
        );

        let rook_key = psq[3][0] ^ psq[3][56] ^ side;
        assert!(
            tables.cuckoo_lookup(rook_key).is_some(),
            "rook a1<->a8 must be a cuckoo entry"
        );
    }

    fn play(board: &mut chess_v2::ChessGame, tables: &tables::Tables, mv: &str) {
        let m = board.fix_move(util::create_move(mv));
        assert!(unsafe { board.make_move(m, tables) });
    }

    #[test]
    fn test_upcoming_cycle_detects() {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();
        assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());

        let mut rt = RepetitionTable::new();
        rt.push_position(board.zobrist_key(), true);

        for mv in ["e2e4", "e7e5", "g1f3", "g8f6", "f3h4"] {
            play(&mut board, &tables, mv);
            rt.push_position(board.zobrist_key(), board.half_moves() == 0);
        }
        play(&mut board, &tables, "f6g8");

        assert!(
            rt.check_upcoming_cycle(&board, &tables, 100),
            "Nh4-f3 reaches the position 3 plies back"
        );
        assert!(
            !rt.check_upcoming_cycle(&board, &tables, 3),
            "ply <= i must not fire (in-tree guard)"
        );
    }

    #[test]
    fn test_upcoming_cycle_negative() {
        let tables = tables::Tables::new();
        let mut board = chess_v2::ChessGame::new();
        assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());

        let mut rt = RepetitionTable::new();
        rt.push_position(board.zobrist_key(), true);
        assert!(!rt.check_upcoming_cycle(&board, &tables, 100));

        for mv in ["g1f3", "g8f6", "b1c3", "b8c6"] {
            play(&mut board, &tables, mv);
            rt.push_position(board.zobrist_key(), board.half_moves() == 0);
        }
        play(&mut board, &tables, "f1e2");

        assert!(
            !rt.check_upcoming_cycle(&board, &tables, 100),
            "distinct reversible moves are not an upcoming repetition"
        );
    }
}
