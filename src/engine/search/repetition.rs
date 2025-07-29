pub struct RepetitionTable {
    /// Repetition table which records the last 128 moves (Zobrist hash of the table after the move)
    /// in a ring buffer. A chess game ends after 100 reversible moves, 128 is used to allow for quick
    /// wrap-around with bitwise and. last_irreversible tracks the start of reversible moves
    pub moves: [u64; 128],
    pub cursor: usize,
    pub last_irreversible: usize,
}

impl RepetitionTable {
    pub fn new() -> Self {
        Self {
            moves: [0; 128],
            cursor: 0,
            last_irreversible: 0,
        }
    }

    pub fn add_position(&mut self, hash: u64, last_move_irreversible: bool) {
        self.moves[self.cursor] = hash;
        self.cursor = (self.cursor + 1) & 127;
        if last_move_irreversible {
            self.last_irreversible = self.cursor.wrapping_sub(1) & 127;
        }
    }

    pub fn is_repeated(&self, hash: u64) -> bool {
        let mut c = self.cursor;

        loop {
            c = c.wrapping_sub(1) & 127;

            // Avoid a single repetition.
            // @todo - Check if counting 2 repetitions would be preferable
            // due to 3-fold repetition rule
            if unsafe { *self.moves.get_unchecked(c) } == hash {
                return true;
            }

            if c == self.last_irreversible {
                break;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        engine::{chess, tables},
        util,
    };

    use super::*;

    fn get_legal_moves(board: &mut chess::ChessGame, tables: &tables::Tables) -> Vec<u16> {
        let mut legal_moves = vec![];
        let mut move_list = [0u16; 256];
        let move_count = board.gen_moves_slow(tables, &mut move_list);

        for i in 0..move_count {
            let mv = move_list[i];

            let board_copy = board.clone();

            if !board.make_move_slow(mv, tables) || board.in_check_slow(tables, !board.b_move) {
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
        board: &mut chess::ChessGame,
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

            assert!(board.make_move_slow(mv, tables));

            let is_irreversible = board.half_moves == 0 || board.castles != board_copy.castles;

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
        let mut board = chess::ChessGame::new();

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

        table.add_position(board.zobrist_key, true);

        for (mv_string, is_repeated) in moves {
            let mv = util::fix_move(&board, util::create_move(mv_string));
            assert!(board.make_move_slow(mv, &tables));

            assert_eq!(
                table.is_repeated(board.zobrist_key),
                is_repeated,
                "Move {} should {}be repeated",
                mv_string,
                if is_repeated { "" } else { "not " }
            );

            table.add_position(board.zobrist_key, board.half_moves == 0);
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
                let mut board = chess::ChessGame::new();
                assert!(board.load_fen(util::FEN_STARTPOS, &tables).is_ok());

                reptable.add_position(board.zobrist_key, true);

                moves.clear();
                assert!(find_moves_with_repetition(&mut board, &tables, i, moves));

                for mv in moves.iter().take(i - 4) {
                    assert!(board.make_move_slow(*mv, &tables));
                    assert!(!board.in_check_slow(&tables, !board.b_move));
                    reptable.add_position(board.zobrist_key, board.half_moves == 0);
                }

                for (index, mv) in moves.iter().enumerate().skip(i - 4) {
                    assert!(board.make_move_slow(*mv, &tables));
                    assert!(!board.in_check_slow(&tables, !board.b_move));
                    assert!(reptable.is_repeated(board.zobrist_key) == (index + 1 == i));
                }
            }
        }
    }
}
