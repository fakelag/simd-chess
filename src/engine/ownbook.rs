use std::{collections::BTreeMap, fs::File, io::BufReader};

use crate::{
    engine::{chess_v2, tables},
    pgn, util,
};

pub struct OwnBookEntry {
    pub bestmove: u16,
}

pub struct OwnBook {
    pub book_size: usize,
    pub book_positions: usize,
    book: BTreeMap<u64, OwnBookEntry>,
}

impl OwnBook {
    pub fn from_pgn<F>(tables: &tables::Tables, pgn_path: &str, filter: F) -> anyhow::Result<Self>
    where
        F: Fn(&pgn::parse::PgnGame, &chess_v2::ChessGame, u16) -> bool,
    {
        let mut book: BTreeMap<u64, OwnBookEntry> = BTreeMap::new();

        let mut board = chess_v2::ChessGame::new();

        board.load_fen(util::FEN_STARTPOS, tables).map_err(|e| {
            anyhow::anyhow!("Failed to load startpos FEN into board for own book: {}", e)
        })?;
        let board = &board;

        let mut games = Vec::new();
        let mut moves = Vec::new();

        let mut st = pgn::parse::PgnReadBuf::new();
        let mut it = pgn::parse::PgnGameParser::new();

        let file = File::open(pgn_path)?;
        let mut reader = BufReader::new(file);

        let mut book_size = 0;
        let mut book_positions = 0;

        loop {
            if !it.next_batch(&mut reader, &mut st, &mut games) {
                break;
            }

            'next_game: for game in games.drain(..) {
                moves.clear();

                let mut game_board = board.clone();

                match game.parse_moves(&st, board, tables, &mut moves) {
                    Some(Ok(())) => {}
                    Some(Err(e)) => {
                        return Err(anyhow::anyhow!(
                            "Book generation failed to parse moves for game {}: {}",
                            game.site(&st).unwrap_or("<unknown>"),
                            e
                        ));
                    }
                    None => continue, // No moves
                }

                for mv in moves.iter() {
                    let mv = *mv;

                    if !filter(&game, &game_board, mv) {
                        continue 'next_game;
                    }

                    let key = game_board.zobrist_key();

                    if !book.contains_key(&key) {
                        book_positions += 1;

                        // @todo - For now just use the first position and move
                        book.insert(key, OwnBookEntry { bestmove: mv });
                    }

                    if !unsafe { game_board.make_move(mv, tables) } {
                        return Err(anyhow::anyhow!(
                            "Book generation encountered illegal move {}",
                            util::move_string(mv),
                        ));
                    }

                    if game_board.in_check(tables, !game_board.b_move()) {
                        return Err(anyhow::anyhow!(
                            "Book generation encountered illegal move {} that leaves king in check",
                            util::move_string(mv),
                        ));
                    }
                }

                book_size += 1;
            }
        }

        Ok(Self {
            book_size,
            book_positions,
            book,
        })
    }

    #[inline(always)]
    pub fn probe(&self, zobrist_key: u64) -> Option<&OwnBookEntry> {
        self.book.get(&zobrist_key)
    }
}
