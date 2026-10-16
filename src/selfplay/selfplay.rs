use std::cell::SyncUnsafeCell;
use std::collections::BTreeMap;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};

use sfbinpack::TrainingDataEntry;
use sfbinpack::chess::{coords, r#move, piece};

use super::fen_feeder::{StatefulFenQueue, binpack_position, has_legal_move};
use super::run::Params;

use crate::selfplay::{GameEnding, PlayedGame, WorkerMsg};
use crate::{
    engine::{
        self,
        chess_v2::{self, GameState, PieceIndex},
        search::{
            EngineForm, SearchStrategy, search::Search, timeman::TimeManager,
            transposition::TranspositionTable,
        },
        tables,
    },
    util,
};

pub const ANNOTATION_DEPTH: u8 = 9;
const FORM: EngineForm = EngineForm::TacticalB;

const LABELING_HARD_NODES: u64 = 100_000_000;
const TT_SIZE_MB: usize = 8;
const SCORE_CLAMP: i16 = 10000;

const INSUFFICIENT_MATERIAL_DRAW: bool = true;
const THREE_FOLD_REPETITION_DRAW: bool = true;

const WIN_ADJ_SCORE: i32 = 2000;
const WIN_ADJ_PLIES: i32 = 7;
const DRAW_ADJ_SCORE: i32 = 7;
const DRAW_ADJ_PLIES: u32 = 17;
const DRAW_ADJ_MIN_PLY: u32 = 60;

pub fn fingerprint_consts() -> BTreeMap<String, toml::Value> {
    use toml::Value::{Boolean, Integer, String as Text};

    let form = match FORM {
        EngineForm::Strategy => "Strategy",
        EngineForm::TacticalA => "TacticalA",
        EngineForm::TacticalB => "TacticalB",
    };

    [
        ("FORM", Text(form.to_string())),
        ("LABELING_HARD_NODES", Integer(LABELING_HARD_NODES as i64)),
        ("TT_SIZE_MB", Integer(TT_SIZE_MB as i64)),
        ("SCORE_CLAMP", Integer(SCORE_CLAMP as i64)),
        (
            "INSUFFICIENT_MATERIAL_DRAW",
            Boolean(INSUFFICIENT_MATERIAL_DRAW),
        ),
        (
            "THREE_FOLD_REPETITION_DRAW",
            Boolean(THREE_FOLD_REPETITION_DRAW),
        ),
        ("WIN_ADJ_SCORE", Integer(WIN_ADJ_SCORE as i64)),
        ("WIN_ADJ_PLIES", Integer(WIN_ADJ_PLIES as i64)),
        ("DRAW_ADJ_SCORE", Integer(DRAW_ADJ_SCORE as i64)),
        ("DRAW_ADJ_PLIES", Integer(DRAW_ADJ_PLIES as i64)),
        ("DRAW_ADJ_MIN_PLY", Integer(DRAW_ADJ_MIN_PLY as i64)),
    ]
    .into_iter()
    .map(|(name, value)| (name.to_string(), value))
    .collect()
}

#[derive(Default)]
struct AdjudicationState {
    win_streak: i32,
    draw_streak: u32,
}

struct SelfplayEngine<'a> {
    thread_id: usize,
    tables: &'a tables::Tables,
    search: Search<'a, { FORM }>,
    zobrist_key: u64,
}

/// Plays games until the queue runs dry or `stop` is set. A game cut short by `stop`
/// is dropped and its seed stays pending in the queue.
pub fn play_games(
    thread_id: usize,
    queue: &Mutex<StatefulFenQueue>,
    stop: &AtomicBool,
    tx: crossbeam::channel::Sender<WorkerMsg>,
    tables: &tables::Tables,
    params: &Params,
) {
    let tt = SyncUnsafeCell::new(TranspositionTable::new(TT_SIZE_MB));

    let mut tm = SyncUnsafeCell::new(TimeManager::new());
    tm.get_mut().disable();
    tm.get_mut().set_nodes(LABELING_HARD_NODES);

    let mut engine = SelfplayEngine::new(thread_id, &tt, &tm, tables);

    while !stop.load(Ordering::Relaxed) {
        let Some((seed, fen)) = queue.lock().unwrap().next() else {
            break;
        };

        let msg = match play_game(&mut engine, &fen, params, stop) {
            Ok(Some((entries, ending, result))) => WorkerMsg::Game(PlayedGame {
                seed,
                entries,
                ending,
                result,
            }),
            Ok(None) => break,
            Err(e) => WorkerMsg::Failed(format!("seed {} \"{}\": {:#}", seed, fen.trim(), e)),
        };

        let failed = matches!(msg, WorkerMsg::Failed(_));
        if tx.send(msg).is_err() || failed {
            break;
        }
    }
}

/// `Ok(None)` when `stop` cut the game short.
fn play_game(
    engine: &mut SelfplayEngine,
    fen: &str,
    params: &Params,
    stop: &AtomicBool,
) -> anyhow::Result<Option<(Vec<TrainingDataEntry>, GameEnding, i16)>> {
    engine.new_game(fen)?;

    let mut entries = Vec::new();
    let mut last_entry = TrainingDataEntry {
        pos: binpack_position(fen)
            .ok_or_else(|| anyhow::anyhow!("the binpack writer can't encode the FEN"))?,
        mv: r#move::Move::null(),
        ply: engine.ply(),
        result: 0,
        score: 0,
    };

    let initial_b_move = engine.b_move();
    let mut adj = AdjudicationState::default();

    let (result, ending) = loop {
        if stop.load(Ordering::Relaxed) {
            return Ok(None);
        }

        let (bestmove, score, node_aborted) = engine.new_move(params.depth);

        if node_aborted || bestmove == 0 {
            break (0, GameEnding::Terminated);
        }

        let mover_bmove = engine.b_move();

        last_entry.mv = convert_move_to_bp(mover_bmove, bestmove);
        last_entry.score = (score as i16).clamp(-SCORE_CLAMP, SCORE_CLAMP);
        entries.push(last_entry);
        last_entry.pos = last_entry.pos.after_move(last_entry.mv);
        last_entry.ply += 1;

        let (mut game_state, rep_count) = engine.make_move(bestmove)?;

        if THREE_FOLD_REPETITION_DRAW && game_state == chess_v2::GameState::Ongoing {
            if rep_count >= 3 {
                game_state = chess_v2::GameState::Draw;
            }
        }

        if INSUFFICIENT_MATERIAL_DRAW && game_state == chess_v2::GameState::Ongoing {
            let occupancy = engine.occupancy();
            let bitboards = engine.bitboards();

            let king_vs_king = occupancy.count_ones() == 2;
            let king_and_bishop_vs_king = occupancy.count_ones() == 3
                && (bitboards[PieceIndex::WhiteBishop as usize].count_ones() == 1
                    || bitboards[PieceIndex::BlackBishop as usize].count_ones() == 1);
            let king_and_knight_vs_king = occupancy.count_ones() == 3
                && (bitboards[PieceIndex::WhiteKnight as usize].count_ones() == 1
                    || bitboards[PieceIndex::BlackKnight as usize].count_ones() == 1);

            if king_vs_king || king_and_bishop_vs_king || king_and_knight_vs_king {
                game_state = chess_v2::GameState::Draw;
            }
        }

        if game_state == chess_v2::GameState::Ongoing {
            let white_pov_score = if engine.b_move() { score } else { -score };

            if let Some(adj_result) = adjudicate(
                &mut adj,
                white_pov_score,
                engine.ply() as u32,
                params.win_adj,
                params.draw_adj,
            ) {
                break adj_result;
            }
        }

        match game_state {
            chess_v2::GameState::Ongoing => {}
            chess_v2::GameState::Checkmate(side) => {
                break (
                    if side == util::Side::White { 1 } else { -1 },
                    GameEnding::Natural,
                );
            }
            chess_v2::GameState::Draw => {
                break (0, GameEnding::Natural);
            }
        }
    };

    // alternate with initia_b_move
    let mut b_move = initial_b_move;
    for e in entries.iter_mut() {
        e.result = if b_move { -result } else { result };
        b_move = !b_move;
    }

    Ok(Some((entries, ending, result)))
}

fn check_game_state(board: &chess_v2::ChessGame, tables: &tables::Tables) -> chess_v2::GameState {
    board.check_game_state(tables, !has_legal_move(board, tables), board.b_move())
}

pub fn convert_move_to_bp(b_move: bool, mv: u16) -> r#move::Move {
    let from_sq = (mv & 0x3F) as u8;
    let mut to_sq = ((mv >> 6) & 0x3F) as u8;

    let mut move_type = r#move::MoveType::Normal;
    let mut promoted_piece = piece::Piece::none();

    match mv & chess_v2::MV_FLAGS_PR_MASK {
        chess_v2::MV_FLAGS_PR_QUEEN => {
            move_type = r#move::MoveType::Promotion;
            promoted_piece = if b_move {
                piece::Piece::BLACK_QUEEN
            } else {
                piece::Piece::WHITE_QUEEN
            };
        }
        chess_v2::MV_FLAGS_PR_ROOK => {
            move_type = r#move::MoveType::Promotion;
            promoted_piece = if b_move {
                piece::Piece::BLACK_ROOK
            } else {
                piece::Piece::WHITE_ROOK
            };
        }
        chess_v2::MV_FLAGS_PR_BISHOP => {
            move_type = r#move::MoveType::Promotion;
            promoted_piece = if b_move {
                piece::Piece::BLACK_BISHOP
            } else {
                piece::Piece::WHITE_BISHOP
            };
        }
        chess_v2::MV_FLAGS_PR_KNIGHT => {
            move_type = r#move::MoveType::Promotion;
            promoted_piece = if b_move {
                piece::Piece::BLACK_KNIGHT
            } else {
                piece::Piece::WHITE_KNIGHT
            };
        }
        _ => {
            if (mv & chess_v2::MV_FLAGS) == chess_v2::MV_FLAGS_CASTLE_KING {
                to_sq += 1;
                move_type = r#move::MoveType::Castle;
            } else if (mv & chess_v2::MV_FLAGS) == chess_v2::MV_FLAGS_CASTLE_QUEEN {
                move_type = r#move::MoveType::Castle;
                to_sq -= 2;
            } else if (mv & chess_v2::MV_FLAGS) == chess_v2::MV_FLAG_EPCAP {
                move_type = r#move::MoveType::EnPassant;
            }
        }
    }

    r#move::Move::new(
        coords::Square::new(from_sq as u32),
        coords::Square::new(to_sq as u32),
        move_type,
        promoted_piece,
    )
}

fn adjudicate(
    adj: &mut AdjudicationState,
    white_score: i32,
    ply: u32,
    win_adj: bool,
    draw_adj: bool,
) -> Option<(i16, GameEnding)> {
    if win_adj {
        if white_score >= WIN_ADJ_SCORE {
            adj.win_streak = adj.win_streak.max(0) + 1;
        } else if white_score <= -WIN_ADJ_SCORE {
            adj.win_streak = adj.win_streak.min(0) - 1;
        } else {
            adj.win_streak = 0;
        }

        if adj.win_streak >= WIN_ADJ_PLIES {
            return Some((1, GameEnding::WinAdjudication));
        }

        if adj.win_streak <= -WIN_ADJ_PLIES {
            return Some((-1, GameEnding::WinAdjudication));
        }
    }

    if draw_adj {
        if ply >= DRAW_ADJ_MIN_PLY && white_score.abs() <= DRAW_ADJ_SCORE {
            adj.draw_streak += 1;
        } else {
            adj.draw_streak = 0;
        }

        if adj.draw_streak >= DRAW_ADJ_PLIES {
            return Some((0, GameEnding::DrawAdjudication));
        }
    }

    None
}

impl<'a> SelfplayEngine<'a> {
    fn new(
        thread_id: usize,
        tt: &'a SyncUnsafeCell<TranspositionTable>,
        tm: &'a SyncUnsafeCell<TimeManager>,
        tables: &'a tables::Tables,
    ) -> Self {
        let search = Search::<{ FORM }>::new(
            tables,
            &tt,
            &tm,
            engine::search::repetition::RepetitionTable::new(),
        );

        Self {
            thread_id,
            tables,
            search,
            zobrist_key: 0,
        }
    }

    fn new_game(&mut self, fen: &str) -> anyhow::Result<()> {
        self.search.new_game();

        self.search.load_from_fen(fen, self.tables).map_err(|err| {
            anyhow::anyhow!(
                "Failed to load FEN \"{}\" during annotated selfplay - {:?}",
                fen,
                err
            )
        })?;

        let board_zobrist = self.search.get_board_mut().zobrist_key();

        self.search.get_rt_mut().push_hash(board_zobrist);

        self.zobrist_key = board_zobrist;

        Ok(())
    }

    fn new_move(&mut self, search_depth: u8) -> (u16, i32, bool) {
        self.search.new_search();

        self.search.get_rt_mut().pop_position();

        let bestmove = self.search.search(search_depth.into());

        self.search.get_rt_mut().push_hash(self.zobrist_key);

        let node_aborted = self.search.was_node_aborted();

        let score = self.search.search_score();

        debug_assert!(
            self.search.get_board_mut().zobrist_key() == self.zobrist_key,
            "Qlk board changed during search!"
        );

        (bestmove, score, node_aborted)
    }

    fn make_move(&mut self, mv: u16) -> anyhow::Result<(GameState, u32)> {
        let mut board = self.search.get_board_mut().clone();

        if unsafe { board.make_move_nnue(mv, self.tables) }.is_none() {
            return Err(anyhow::anyhow!(
                "[Thread {}]: Failed to make move during annotated selfplay, fen=\"{}\": {}",
                self.thread_id,
                board.gen_fen(),
                util::move_string_dbg(mv),
            ));
        }

        if board.in_check(self.tables, !board.b_move()) {
            return Err(anyhow::anyhow!(
                "[Thread {}]: Illegal move (leaves player in check) during annotated selfplay, fen=\"{}\": {:?}",
                self.thread_id,
                board.gen_fen(),
                util::move_string_dbg(mv),
            ));
        }

        self.search.get_rt_mut().push_hash(board.zobrist_key());
        // Pushing root moves as lazy NNUE updates would grow the update stack with the game and
        // overflow LAZY_NNUE_MAX_PLY in long games; a reload keeps it empty at every root.
        self.search.load_from_board(&board);

        self.zobrist_key = board.zobrist_key();

        Ok((
            check_game_state(&board, self.tables),
            self.search
                .get_rt()
                .is_repeated_times(self.zobrist_key, board.half_moves() as usize),
        ))
    }

    fn ply(&mut self) -> u16 {
        self.search.get_board_mut().ply()
    }

    fn b_move(&mut self) -> bool {
        self.search.get_board_mut().b_move()
    }

    fn occupancy(&mut self) -> u64 {
        self.search.get_board_mut().occupancy()
    }

    fn bitboards(&mut self) -> &[u64; 16] {
        self.search.get_board_mut().bitboards()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const REP_FEN_LOSING: &str = "4k1n1/8/8/8/8/8/8/3QK1N1 b - - 10 1";
    const REP_FEN_WINNING: &str = "4k1n1/8/8/8/8/8/8/3QK1N1 w - - 10 1";
    const TEST_DEPTH: u8 = 8;

    const REPRO_GAME: &str = "d2d4 e7e6 g1f3 c7c5 g2g3 c5d4 f3d4 d7d5 f1g2 b8c6 e1g1 f8c5 d4b3 c5b6 c2c4 g8e7 c4d5 e6d5 b1c3 d5d4 c3a4 e8g8 a4b6 d8b6 e2e3 c8e6 b3d4 f8d8 b2b3 e7f5 c1b2 f5d4 e3d4 c6d4 d1e1 a8c8 e1e4 d4c6 a1d1 h7h6 f1e1 d8d1 e1d1 b6c5 e4e1 c8c7 h2h4 c5e7 d1d2 c7d7 b2c3 d7d2 e1d2 e7c7 b3b4 b7b6 a2a4 c6e7 a4a5 c7c8 c3e5 f7f6 e5f4 g8f7 f4e3 c8c7 g1h2 e7f5 e3f4 c7c8 d2b2 g7g5 h4g5 h6g5 f4d2 f5h6 d2c3 e6c4 f2f3 c8e6 b2c2 e6f5 c2d1 g5g4 h2g1 f5e6 d1d2 h6f5 a5b6 e6b6 g1h2 b6e6 d2f4 f5e3 f3g4 e3g2 h2g2 a7a6 f4f5 c4b5 f5e6 f7e6 c3d2 e6d5 g4g5 f6g5 d2g5 d5c4 g5e7 b5c6 g2h3 c6d7 h3h4 d7e6 e7d6 c4b5 h4h5 e6d7 h5g5 d7h3 d6f8 b5c4 g5h5 h3e6 h5h6 e6c8 h6g7 c4b5 g7g6 c8g4 g6g5 g4h3 g5h5 h3d7 h5h4 b5c4 f8d6 c4b5 d6e7 b5a4 h4g5 d7h3 g5f4 a4b5 e7f8 b5c4 f8d6 h3e6 d6e7 e6c8 f4e5 c8h3 e5e4 h3c8 e4f4 c8h3 f4g5 c4b5 g5f4 b5c4 e7f8 c4b5 f8d6 b5c4 f4e5 h3g4 d6f8 g4h3 e5f6 c4b5 f8d6 b5c4 d6e7 c4b5 e7f8 b5c4 f6e5 h3g4";

    struct Harness {
        tt: SyncUnsafeCell<TranspositionTable>,
        tm: SyncUnsafeCell<TimeManager>,
        tables: tables::Tables,
    }

    impl Harness {
        fn new() -> Self {
            let mut tm = SyncUnsafeCell::new(TimeManager::new());
            tm.get_mut().disable();
            tm.get_mut().set_nodes(LABELING_HARD_NODES);

            Self {
                tt: SyncUnsafeCell::new(TranspositionTable::new(8)),
                tm,
                tables: tables::Tables::new(),
            }
        }

        fn engine(&self) -> SelfplayEngine<'_> {
            SelfplayEngine::new(0, &self.tt, &self.tm, &self.tables)
        }
    }

    fn mv_of(engine: &mut SelfplayEngine, mv: &str) -> u16 {
        let board = engine.search.get_board_mut();
        board.fix_move(util::create_move(mv))
    }

    fn play(engine: &mut SelfplayEngine, mv: &str) -> (GameState, u32) {
        let mv = mv_of(engine, mv);
        engine.make_move(mv).expect("legal move")
    }

    fn extended_pv(engine: &mut SelfplayEngine) -> Vec<u16> {
        engine.search.get_rt_mut().pop_position();

        let pv = engine.search.get_pv();

        let key = engine.zobrist_key;
        engine.search.get_rt_mut().push_hash(key);

        pv
    }

    fn drive_shuffle(engine: &mut SelfplayEngine, fen: &str, shuffle: [&str; 4]) {
        engine.new_game(fen).unwrap();
        let start_key = engine.zobrist_key;

        for mv in shuffle {
            play(engine, mv);
        }

        assert_eq!(
            engine.zobrist_key, start_key,
            "the shuffle must return to the loaded position"
        );
    }

    #[test]
    fn test_selfplay_rep_count_sequence() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        let moves = [
            ("e2e4", 1),
            ("e7e5", 1),
            ("g1f3", 1),
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
            ("h4f3", 5),
            ("g8f6", 5),
            ("f3h4", 5),
            ("f6g8", 5),
            ("h4g6", 1),
            ("g8f6", 1),
            ("g6h4", 6),
            ("f6e4", 1),
            ("h4f3", 1),
            ("e4f6", 1),
            ("f3h4", 1),
        ];

        let mut first_threefold = None;

        for (index, (mv, expected)) in moves.iter().enumerate() {
            let (state, rep_count) = play(&mut engine, mv);

            assert_eq!(state, GameState::Ongoing, "move {} ended the game", mv);
            assert_eq!(rep_count, *expected, "move {} rep count mismatch", mv);

            if rep_count >= 3 && first_threefold.is_none() {
                first_threefold = Some(index);
            }
        }

        assert_eq!(
            first_threefold,
            Some(10),
            "three-fold must first trigger on the 11th move"
        );
    }

    #[test]
    fn test_selfplay_root_moves_match_a_fresh_load() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        // Ends on a pawn move, so no earlier position can repeat and the history doesn't matter.
        for mv in REPRO_GAME.split_whitespace().take(40) {
            play(&mut engine, mv);
        }
        let fen = engine.search.get_board_mut().gen_fen();
        let (bestmove, score, aborted) = engine.new_move(TEST_DEPTH);
        assert!(!aborted);

        let fresh_harness = Harness::new();
        let mut fresh = fresh_harness.engine();
        fresh.new_game(&fen).unwrap();
        let (fresh_bestmove, fresh_score, _) = fresh.new_move(TEST_DEPTH);
        assert_eq!((bestmove, score), (fresh_bestmove, fresh_score));

        // The next game on the same engine starts from an empty NNUE update stack.
        engine.new_game(&fen).unwrap();
        assert_eq!(engine.new_move(TEST_DEPTH), (bestmove, score, false));
    }

    #[test]
    fn test_selfplay_rt_balanced_across_search() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        for mv in ["e2e4", "e7e5", "g1f3", "b8c6"] {
            play(&mut engine, mv);
        }

        for _ in 0..3 {
            let cursor_before = engine.search.get_rt().cursor;
            let key_before = engine.zobrist_key;

            let (bestmove, _, aborted) = engine.new_move(TEST_DEPTH);

            assert!(!aborted);
            assert_eq!(engine.search.get_rt().cursor, cursor_before);
            assert_eq!(engine.zobrist_key, key_before);
            assert_eq!(
                engine.search.get_rt().hashes[cursor_before - 1],
                key_before,
                "the current position must be the table top between searches"
            );

            engine.make_move(bestmove).unwrap();
        }
    }

    #[test]
    fn test_selfplay_finds_pre_root_repetition() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        drive_shuffle(
            &mut engine,
            REP_FEN_LOSING,
            ["g8f6", "g1f3", "f6g8", "f3g1"],
        );

        let repeating = mv_of(&mut engine, "g8f6");
        let (bestmove, score, aborted) = engine.new_move(TEST_DEPTH);

        assert!(!aborted);
        assert_eq!(
            bestmove, repeating,
            "the losing side must repeat a pre-root position"
        );
        assert_eq!(score, 0, "repeating must score as a draw");
    }

    #[test]
    fn test_selfplay_avoids_pre_root_repetition() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        drive_shuffle(
            &mut engine,
            REP_FEN_WINNING,
            ["g1f3", "g8f6", "f3g1", "f6g8"],
        );

        let repeating = mv_of(&mut engine, "g1f3");
        let (bestmove, score, aborted) = engine.new_move(TEST_DEPTH);

        assert!(!aborted);
        assert_ne!(
            bestmove, repeating,
            "the winning side must not repeat a pre-root position"
        );
        assert!(score > 1000, "winning side scored {}", score);
    }

    #[test]
    fn test_pv_extension_stops_at_root_revisit() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine
            .new_game("4k1n1/8/8/8/8/8/8/4K1N1 w - - 10 1")
            .unwrap();

        engine.new_move(10);

        let root_key = engine.zobrist_key;
        let mut board = *engine.search.get_board_mut();
        let mut cycle = Vec::new();

        for mv in ["g1f3", "g8f6", "f3g1", "f6g8"] {
            let mv = board.fix_move(util::create_move(mv));
            assert!(
                unsafe { board.make_move(mv, &harness.tables) },
                "{} must be legal",
                util::move_string_dbg(mv)
            );
            cycle.push(mv);
        }

        assert_eq!(
            board.zobrist_key(),
            root_key,
            "the shuffle must return to the root position"
        );

        engine.search.get_rt_mut().pop_position();

        let mut extended = cycle.clone();
        engine.search.extend_pv_from_tt(&mut extended);

        engine.search.get_rt_mut().push_hash(root_key);

        assert_eq!(
            extended.len(),
            cycle.len(),
            "PV ran {} ply past a return to the root: {:?}",
            extended.len().saturating_sub(cycle.len()),
            extended
                .iter()
                .map(|mv| util::move_string(*mv))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_pv_never_extends_past_repetition_in_repro_game() {
        const REPLAY_DEPTH: u8 = 14;

        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        let game: Vec<&str> = REPRO_GAME.split_whitespace().collect();
        let warm_from = game.len().saturating_sub(40);

        for (ply, mv) in game.iter().enumerate() {
            if ply >= warm_from {
                engine.new_move(REPLAY_DEPTH);

                let pv = extended_pv(&mut engine);
                let root_key = engine.zobrist_key;
                let rt = engine.search.get_rt();

                let mut history: Vec<u64> = rt.hashes[..rt.cursor].to_vec();
                let mut board = *engine.search.get_board_mut();

                for (index, pv_move) in pv.iter().enumerate() {
                    assert!(
                        unsafe { board.make_move(*pv_move, &harness.tables) },
                        "ply {}: PV move {} is not makeable",
                        ply,
                        index
                    );

                    let key = board.zobrist_key();
                    let window = (board.half_moves() as usize).min(history.len());
                    let repeats =
                        history[history.len() - window..].contains(&key) || key == root_key;

                    assert!(
                        !repeats || index + 1 == pv.len(),
                        "ply {}: PV extends past a repetition at move {}: {:?}",
                        ply,
                        index,
                        pv.iter().map(|m| util::move_string(*m)).collect::<Vec<_>>()
                    );

                    history.push(key);
                }
            }

            play(&mut engine, mv);
        }
    }

    #[test]
    fn test_selfplay_pv_terminates_at_repetition() {
        let harness = Harness::new();
        let mut engine = harness.engine();
        engine.new_game(util::FEN_STARTPOS).unwrap();

        for mv in ["e2e4", "e7e5", "g1f3", "b8c6", "f1c4", "f8c5"] {
            play(&mut engine, mv);
        }

        for _ in 0..4 {
            let (bestmove, _, aborted) = engine.new_move(TEST_DEPTH);
            assert!(!aborted);

            let pv = extended_pv(&mut engine);
            assert!(!pv.is_empty());
            assert_eq!(pv[0], bestmove);

            let rt = engine.search.get_rt();
            let mut history: Vec<u64> = rt.hashes[..rt.cursor].to_vec();
            let mut board = *engine.search.get_board_mut();

            for (index, mv) in pv.iter().enumerate() {
                assert!(
                    unsafe { board.make_move(*mv, &harness.tables) },
                    "PV move {} is not makeable",
                    index
                );
                assert!(
                    !board.in_check(&harness.tables, !board.b_move()),
                    "PV move {} leaves the mover in check",
                    index
                );

                let key = board.zobrist_key();
                let window = (board.half_moves() as usize).min(history.len());
                let repeats = history[history.len() - window..].contains(&key);

                assert!(
                    !repeats || index + 1 == pv.len(),
                    "PV extends past a repetition at move {}",
                    index
                );

                history.push(key);
            }

            engine.make_move(bestmove).unwrap();
        }
    }
}
