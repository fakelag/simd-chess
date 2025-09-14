use std::sync::{Arc, Mutex};

use core_affinity::CoreId;
use crossbeam::channel;
use sfbinpack::{chess::*, *};

use crate::{
    engine::{
        chess::MV_FLAGS_CASTLE_KING,
        chess_v2::{
            self, MV_FLAG_EPCAP, MV_FLAGS, MV_FLAGS_CASTLE_QUEEN, MV_FLAGS_PR_BISHOP,
            MV_FLAGS_PR_KNIGHT, MV_FLAGS_PR_MASK, MV_FLAGS_PR_QUEEN, MV_FLAGS_PR_ROOK,
        },
        tables,
    },
    util,
};

#[derive(Debug)]
pub struct NewMove {
    pub board: chess_v2::ChessGame,
    pub mv: u16,
}

struct TrainingMatch {
    w: Arc<Mutex<CompressedTrainingDataEntryWriter>>,
    initial_board: chess_v2::ChessGame,
    result: chess_v2::GameState,
    moves: Vec<u16>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TrainingStatus {
    Ready,
    InGame,
    Analysing,
}

pub struct TrainingDataCollector {
    fen: String,
    board: chess_v2::ChessGame,
    moves: Vec<u16>,

    tx_match: channel::Sender<TrainingMatch>,
    rx_status: channel::Receiver<()>,

    status: TrainingStatus,

    training_entries: Vec<TrainingDataEntry>,
    writer: Arc<Mutex<CompressedTrainingDataEntryWriter>>,
}

impl TrainingDataCollector {
    pub fn new(
        core_affinity: usize,
        writer: Arc<Mutex<CompressedTrainingDataEntryWriter>>,
    ) -> Self {
        let (tx_match, rx_match) = channel::bounded(1);
        let (tx_status, rx_status) = channel::bounded(1);

        std::thread::spawn(move || {
            core_affinity::set_for_current(CoreId { id: core_affinity });
            Self::match_update_thread(rx_match, tx_status);
        });

        Self {
            tx_match,
            rx_status,
            status: TrainingStatus::Ready,
            moves: Vec::new(),
            board: chess_v2::ChessGame::new(),
            fen: String::new(),
            training_entries: Vec::new(),
            writer,
        }
    }

    pub fn load_fen(&mut self, fen: &str, tables: &tables::Tables) -> anyhow::Result<()> {
        self.fen = fen.to_string();
        self.training_entries.clear();
        self.moves.clear();
        self.board
            .load_fen(fen, tables)
            .map_err(|e| anyhow::anyhow!("Failed to load FEN '{}': {}", fen, e))?;
        self.status = TrainingStatus::InGame;
        Ok(())
    }

    pub fn add_move(&mut self, mv: u16) {
        assert!(self.fen.len() > 0);
        self.moves.push(mv);
    }

    fn foo(&mut self) {
        // let mv = Self::convert_move(new_move.board.b_move(), new_move.mv);

        // println!(
        //     "Adding move to training data: {}",
        //     util::move_string_dbg(new_move.mv)
        // );
        // println!("converted {:?}", mv);

        // let prev = self.training_entries.last();

        // let entry = if let Some(prev) = prev {
        //     let xf = prev.pos.after_move(prev.mv).fen();
        //     let yf = new_move.board.gen_fen();
        //     let mut x = xf.split(' ');
        //     let mut y = yf.split(' ');
        //     debug_assert_eq!(x.next().unwrap(), y.next().unwrap());
        //     debug_assert_eq!(x.next().unwrap(), y.next().unwrap());
        //     debug_assert_eq!(x.next().unwrap(), y.next().unwrap());
        //     y.next();
        //     x.next();
        //     debug_assert_eq!(x.next().unwrap(), y.next().unwrap());
        //     debug_assert_eq!(x.next().unwrap(), y.next().unwrap());
        //     TrainingDataEntry {
        //         ply: prev.ply + 1,
        //         score: 0, // TODO
        //         result: 0,
        //         pos: prev.pos.after_move(prev.mv),
        //         mv,
        //     }
        // } else {
        //     TrainingDataEntry {
        //         ply: 0,
        //         score: 0, // TODO
        //         result: 0,
        //         pos: position::Position::from_fen(&self.fen),
        //         mv,
        //     }
        // };

        // self.training_entries.push(entry);
    }

    pub fn collect(&mut self, state: chess_v2::GameState) {
        assert!(self.fen.len() > 0);

        if self.status != TrainingStatus::InGame {
            panic!("Not in game");
        }
        self.status = TrainingStatus::Analysing;

        self.tx_match
            .send(TrainingMatch {
                w: self.writer.clone(),
                initial_board: self.board.clone(),
                moves: self.moves.clone(),
                result: state,
            })
            .unwrap();

        // for entry in &mut self.training_entries {
        //     match state {
        //         chess_v2::GameState::Ongoing => unreachable!(),
        //         chess_v2::GameState::Checkmate(side) => {
        //             entry.result = if side == util::Side::White { 1 } else { -1 };
        //         }
        //         chess_v2::GameState::Stalemate | chess_v2::GameState::DrawByFiftyMoveRule => {
        //             entry.result = 0;
        //         }
        //     }

        //     // @todo - Analyse position and set stm relative entry.score
        // }

        // {
        //     let writer = &mut self.writer.lock().unwrap();

        //     for entry in &self.training_entries {
        //         writer.write_entry(entry).unwrap();
        //     }
        // }

        // let mut reader = CompressedTrainingDataEntryReader::new(
        //     "nnue/test80-2024-06-jun-2tb7p.min-v2.v6.binpack",
        // )
        // .unwrap();

        // for i in 0..100 {
        //     let entry = reader.next();

        //     if entry.ply < 3 {
        //         // println!("{:?}", entry);
        //         println!("{:?}", entry.score);
        //         println!("{}", entry.pos.fen());
        //         println!("{}", entry.mv.as_uci());
        //     }
        // }
        // let mut writer = CompressedTrainingDataEntryWriter::new("test.binpack", false).unwrap();

        // let entry = TrainingDataEntry {
        //     pos: chess::position::Position::from_fen(util::FEN_STARTPOS),
        //     mv: chess::r#move::Move::new(
        //         chess::coords::Square::from_string("e2").unwrap(),
        //         chess::coords::Square::from_string("e4").unwrap(),
        //         chess::r#move::MoveType::Normal,
        //         chess::piece::Piece::none(),
        //     ),
        //     ply: 1,
        //     result: 1,
        //     score: 0,
        // };

        // writer.write_entry(&entry).unwrap();

        return;
    }

    pub fn poll(&mut self) {
        match self.status {
            TrainingStatus::Analysing => {
                if let Ok(()) = self.rx_status.try_recv() {
                    self.status = TrainingStatus::Ready;
                }
            }
            _ => {}
        }
    }

    fn match_update_thread(rx: channel::Receiver<TrainingMatch>, tx: channel::Sender<()>) {
        println!("thread started, waiting for updates..");

        while let Ok(update) = rx.recv() {
            update.initial_board;
            std::thread::sleep(std::time::Duration::from_millis(1000));
            tx.send(()).unwrap();
        }

        println!("exiting..");
    }

    // fn writer_thread(coll: Arc<Mutex<Self>>) {
    //     // f
    // }

    fn convert_move(b_move: bool, mv: u16) -> r#move::Move {
        let from_sq = (mv & 0x3F) as u8;
        let mut to_sq = ((mv >> 6) & 0x3F) as u8;

        let mut move_type = r#move::MoveType::Normal;
        let mut promoted_piece = piece::Piece::none();

        match mv & MV_FLAGS_PR_MASK {
            MV_FLAGS_PR_QUEEN => {
                move_type = r#move::MoveType::Promotion;
                promoted_piece = if b_move {
                    piece::Piece::BLACK_QUEEN
                } else {
                    piece::Piece::WHITE_QUEEN
                };
            }
            MV_FLAGS_PR_ROOK => {
                move_type = r#move::MoveType::Promotion;
                promoted_piece = if b_move {
                    piece::Piece::BLACK_ROOK
                } else {
                    piece::Piece::WHITE_ROOK
                };
            }
            MV_FLAGS_PR_BISHOP => {
                move_type = r#move::MoveType::Promotion;
                promoted_piece = if b_move {
                    piece::Piece::BLACK_BISHOP
                } else {
                    piece::Piece::WHITE_BISHOP
                };
            }
            MV_FLAGS_PR_KNIGHT => {
                move_type = r#move::MoveType::Promotion;
                promoted_piece = if b_move {
                    piece::Piece::BLACK_KNIGHT
                } else {
                    piece::Piece::WHITE_KNIGHT
                };
            }
            _ => {
                if (mv & MV_FLAGS) == MV_FLAGS_CASTLE_KING {
                    to_sq += 1;
                    move_type = r#move::MoveType::Castle;
                } else if (mv & MV_FLAGS) == MV_FLAGS_CASTLE_QUEEN {
                    move_type = r#move::MoveType::Castle;
                    to_sq -= 2;
                } else if (mv & MV_FLAGS) == MV_FLAG_EPCAP {
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
}
