#![feature(sync_unsafe_cell)]
#![feature(iter_array_chunks)]
#![feature(likely_unlikely)]
#![feature(cold_path)]

use std::arch::x86_64::*;
use std::hint::black_box;
use std::{cell::SyncUnsafeCell, thread::JoinHandle};

use crossbeam::channel;
use winit::event_loop::{ControlFlow, EventLoop};

use crate::engine::chess_v2::PieceIndex;
use crate::{
    engine::{
        chess, chess_v2,
        search::{self, AbortSignal, SigAbort, search_params, transposition},
        tables,
    },
    ui::chess_ui::ChessUi,
};

mod clipb;
mod engine;
mod matchmaking;
mod ui;
mod uicomponents;
mod util;
mod window;

struct GoCommand {
    start_time: std::time::Instant,
    params: search_params::SearchParams,
    chess: chess::ChessGame,
    sig: AbortSignal,
    repetition_table: search::repetition::RepetitionTable,
}

fn chess_ui() -> anyhow::Result<()> {
    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    let chess_ui = ChessUi::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    // "8/PPPPPPPP/7k/8/8/7K/pppppppp/8 w KQkq - 0 1");

    let mut app = window::App::new(chess_ui);
    event_loop.run_app(&mut app)?;

    Ok(())
}

fn search_thread(
    rx_search: channel::Receiver<GoCommand>,
    tables: &tables::Tables,
    tt: &SyncUnsafeCell<transposition::TranspositionTable>,
) {
    let tt = unsafe { &mut *tt.get() };
    loop {
        match rx_search.recv() {
            Ok(go) => {
                let debug = go.params.debug;
                // let mut search_engine = search::v10_mvcache::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );
                // let mut search_engine = search::v9_prune::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );
                // let mut search_engine = search::v8_quiesc::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );
                // let mut search_engine = search::v7_mvvlva::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );

                // let mut search_engine = search::v6_psquare::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );

                // let mut search_engine = search::v5_tt::Search::new(
                //     go.params,
                //     go.chess,
                //     tables,
                //     tt,
                //     go.repetition_table,
                //     &go.sig,
                // );

                // let mut search_engine =
                //     search::v4_pv::Search::new(go.params, go.chess, tables, &go.sig);

                // let mut search_engine =
                //     search::v3_itdep::IterativeDeepening::new(go.params, go.chess, tables, &go.sig);

                // let mut search_engine =
                //     search::v2_alphabeta::Alphabeta::new(go.params, go.chess, tables, &go.sig);

                // let best_move = search_engine.search();

                // if debug {
                //     let elapsed = go.start_time.elapsed();
                //     let search_nodes = search_engine.num_nodes_searched();
                //     let search_depth = search_engine.get_depth();
                //     let search_score = search_engine.search_score();

                //     let tt_stats = tt.calc_stats();

                //     println!(
                //         "info searched {} nodes in {} with depth {} bestmove {} ({:016b}) score {} ({:.02}% tt occupancy)",
                //         search_nodes,
                //         util::time_format(elapsed.as_millis() as u64),
                //         search_depth,
                //         util::move_string(best_move),
                //         best_move,
                //         search_score,
                //         (tt_stats.1 + tt_stats.2 + tt_stats.3) as f64 / (tt_stats.0 as f64) * 100.0
                //     );
                // }

                // println!(
                //     "bestmove {}",
                //     if best_move != 0 {
                //         util::move_string(best_move)
                //     } else {
                //         "0000".to_string()
                //     }
                // );
            }
            Err(_) => {
                println!("info search thread terminated");
                break;
            }
        }
    }
}

fn chess_uci(
    tx_search: channel::Sender<GoCommand>,
    tables: &tables::Tables,
    tt: &SyncUnsafeCell<transposition::TranspositionTable>,
) -> anyhow::Result<()> {
    let mut debug = false;
    let mut game_board: Option<chess::ChessGame> = None;
    let mut repetition_table: Option<search::repetition::RepetitionTable> = None;

    struct GoContext {
        tx_abort: channel::Sender<SigAbort>,
        tx_stop: channel::Sender<SigAbort>,
        timeout_handle: Option<JoinHandle<()>>,
    }

    let mut context: Option<GoContext> = None;

    fn abort_search_uci(_debug: bool, context: &mut Option<GoContext>) {
        if let Some(context) = context.take() {
            let _ = context.tx_abort.try_send(SigAbort {});

            if let Some(handle) = context.timeout_handle {
                // Exit timeout thread
                if !handle.is_finished() {
                    context.tx_stop.try_send(SigAbort {}).unwrap();
                    handle.join().unwrap();
                }
            }
        }
    }

    'next_cmd: loop {
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer)?;

        let mut input = buffer.split_whitespace();

        match input.next() {
            Some("uci") => {
                println!("id name chess\nuciok")
            }
            Some("debug") => match input.next() {
                Some("on") => debug = true,
                Some("off") => debug = false,
                _ => panic!("Expected 'on' or 'off' after debug command"),
            },
            Some("stop") => abort_search_uci(debug, &mut context),
            Some("isready") => println!("readyok"),
            Some("position") => {
                let mut board = chess::ChessGame::new();
                let mut rep_table = search::repetition::RepetitionTable::new();

                // Safety: Engine should not be calculating when receiving a position command
                let tt = unsafe { &mut *tt.get() };

                tt.clear();

                let position_start_index = input
                    .next()
                    .expect("Expected \"startpos\" or \"fen\" after position")
                    .as_ptr() as usize
                    - buffer.as_ptr() as usize;

                util::parse_position(
                    &buffer[position_start_index..],
                    &mut board,
                    tables,
                    Some(&mut rep_table),
                    None,
                    None,
                )?;

                game_board = Some(board);
                repetition_table = Some(rep_table);
            }
            Some("go") => {
                let start_time = std::time::Instant::now();

                abort_search_uci(debug, &mut context);

                let (tx_abort, rx_abort) = channel::bounded(1);
                let (tx_stop, rx_stop) = channel::bounded(1);

                let mut new_context = GoContext {
                    tx_abort,
                    tx_stop,
                    timeout_handle: None,
                };

                let mut search_params = search_params::SearchParams::from_iter(input);
                search_params.debug = debug;

                tx_search.send(GoCommand {
                    start_time,
                    params: search_params,
                    chess: game_board
                        .take()
                        .expect("Expected position command to be sent before go"),
                    repetition_table: repetition_table
                        .take()
                        .expect("Expected position command to be sent before go"),
                    sig: rx_abort,
                })?;

                // @todo - Calc thinking time
                let think_time = std::time::Duration::from_millis(100);

                // Timeout thread
                let tx_abort = new_context.tx_abort.clone();

                new_context.timeout_handle = Some(std::thread::spawn(move || {
                    match rx_stop.recv_timeout(think_time) {
                        Ok(_) => {}
                        Err(channel::RecvTimeoutError::Timeout) => {
                            let _ = tx_abort.try_send(SigAbort {});
                        }
                        Err(channel::RecvTimeoutError::Disconnected) => {
                            panic!("Timeout thread disconnected")
                        }
                    }
                }));

                context = Some(new_context);
            }
            Some("quit") => break,
            Some(arg) => return Err(anyhow::anyhow!("Unknown command: \"{}\"", arg)),
            None => return Err(anyhow::anyhow!("No command provided")),
        }
    }

    if debug {
        println!("info exiting UCI mode");
    }

    abort_search_uci(debug, &mut context);

    Ok(())
}

// fn test() {
//     use std::arch::x86_64::*;

//     let mut bb = [0u64; 12];
//     let b_move = false;

//     bb[0] = 0b100;
//     bb[1] = 0b010;
//     bb[2] = 0b001;

//     struct T {
//         tables: [[u64; 64]; 6],
//     }

//     let mut t = T {
//         tables: [
//             [0b10; 64],
//             [0b111; 64],
//             [0b1111; 64],
//             [0; 64],
//             [0; 64],
//             [0; 64],
//         ],
//     };

//     t.tables[1][1] = 0b101;

//     unsafe {
//         let most_sig_bit_1 = _mm512_set1_epi64(1 << 63);
//         let sixtythree = _mm512_set1_epi64(63);

//         // Getting piece squares and popping ms1b for next it
//         let pieces_vec = _mm512_loadu_si512(bb.as_ptr().add(b_move as usize * 6) as *const _);
//         let ms1b_vec = _mm512_lzcnt_epi64(pieces_vec);
//         let shft = _mm512_srlv_epi64(most_sig_bit_1, ms1b_vec);
//         let popped = _mm512_xor_si512(pieces_vec, shft);
//         let realindexes = _mm512_sub_epi64(sixtythree, ms1b_vec);

//         println!("Pieces: {:?}", pieces_vec);
//         println!("MS1B: {:?}", ms1b_vec);
//         println!("shft: {:?}", shft);
//         println!("Popped: {:?}", popped);
//         println!("Real indexes: {:?}", realindexes);

//         // Gathering tables from mem
//         let vindex = _mm256_set_epi32(0, 0, 0, 0, 0, 64 * 2, 64 + 1, 0);
//         let lol = _mm512_i32gather_epi64(vindex, t.tables.as_ptr() as *const i64, 8);

//         println!("Gathered tables: {:?}", lol);
//     }
// }

#[inline(always)]
fn rdtscp() -> u64 {
    unsafe { std::arch::x86_64::__rdtscp(&mut 0u32) }
}

#[inline(never)]
fn print_vec512_epi64(name: &str, vec: __m512i) {
    let mut arr: [i64; 8] = [0; 8];
    unsafe {
        _mm512_storeu_si512(arr.as_mut_ptr() as *mut __m512i, vec);
    }
    println!("{}:", name);
    println!("{:?}", arr);
    // println!("Binary representation:");
    // for i in 0..8 {
    //     println!("{i}");
    //     println!("{:064b}", arr[i]);
    // }
}

#[inline(never)]
fn print_vec512_epi8(name: &str, vec: __m512i) {
    let mut arr: [i8; 64] = [0; 64];
    unsafe {
        _mm512_storeu_si512(arr.as_mut_ptr() as *mut __m512i, vec);
    }
    println!("{}:", name);
    println!("{:?}", arr);
}

#[inline(never)]
fn print_vec128_epi16(name: &str, vec: __m128i) {
    let mut arr: [i16; 8] = [0; 8];
    unsafe {
        _mm_storeu_si128(arr.as_mut_ptr() as *mut __m128i, vec);
    }
    println!("{}:", name);
    println!("{:?}", arr);
}

fn get_occupancy_indices_debug(
    tables: &tables::Tables,
    sq: usize,
    full_board: u64,
) -> ((usize, u64), (usize, u64)) {
    let rook_blockers = full_board & tables::Tables::LT_ROOK_OCCUPANCY_MASKS[sq];
    let rook_occupancy_index = tables::Tables::calc_occupancy_index::<true>(sq, rook_blockers);

    let bishop_blockers = full_board & tables::Tables::LT_BISHOP_OCCUPANCY_MASKS[sq];
    let bishop_occupancy_index = tables::Tables::calc_occupancy_index::<false>(sq, bishop_blockers);

    (
        (
            rook_occupancy_index,
            tables.get_slider_move_mask::<true>(sq, rook_blockers),
        ),
        (
            bishop_occupancy_index,
            tables.get_slider_move_mask::<false>(sq, bishop_blockers),
        ),
    )
}

#[inline(always)]
fn load_bitboards_with_split_pawns(bitboards: &[u64; 16], b_move_offset: usize) -> __m512i {
    unsafe {
        let mut bitboard_x8 =
            _mm512_load_si512(bitboards.as_ptr().add(b_move_offset) as *const __m512i);

        let const_pawn_split_1_cross_lane_selector = _mm512_set_epi64(1, 2, 3, 4, 5, 6, 6, 6);

        bitboard_x8 = _mm512_permutexvar_epi64(const_pawn_split_1_cross_lane_selector, bitboard_x8);

        let const_pawn_split_mask_abc: u64 = 0xE0E0E0E0E0E0E0E0u64;
        let const_pawn_split_mask_def: u64 = 0x1C1C1C1C1C1C1C1Cu64;
        let const_pawn_split_mask_gh: u64 = 0x0303030303030303u64;

        let const_pawn_split_x8 = _mm512_set_epi64(
            !0,
            !0,
            !0,
            !0,
            !0,
            const_pawn_split_mask_gh as i64,
            const_pawn_split_mask_def as i64,
            const_pawn_split_mask_abc as i64,
        );

        bitboard_x8 = _mm512_and_si512(bitboard_x8, const_pawn_split_x8);

        bitboard_x8
    }
}

#[inline(always)]
fn load_bitboards_with_sliders(bitboards: &[u64; 16], b_move_offset: usize) -> (__m512i, __m512i) {
    unsafe {
        let bitboard_x8 =
            _mm512_load_si512(bitboards.as_ptr().add(b_move_offset) as *const __m512i);

        // [bishop, rook, queen_b, queen_r, 0, 0, 0, 0]
        let const_slider_selector = _mm512_set_epi64(0, 0, 0, 0, 2, 2, 3, 4);

        // [pawn, knight, king, 0, 0, 0, 0, 0]
        let const_nonslider_selector = _mm512_set_epi64(0, 0, 0, 0, 0, 1, 5, 6);

        let sliders_x8 = _mm512_permutex2var_epi64(bitboard_x8, const_slider_selector, bitboard_x8);

        let non_sliders_x8 =
            _mm512_permutex2var_epi64(bitboard_x8, const_nonslider_selector, bitboard_x8);

        (sliders_x8, non_sliders_x8)
    }
}

#[inline(never)]
fn get_slider_moves_x8(
    tables: &tables::Tables,
    full_board_x8: __m512i,
    piece_indices: __m512i,
    mask: u8,
) -> __m512i {
    unsafe {
        // 0x0 = rook, 0x40 = bishop
        let const_magic_masks_gather_offsets_x8 = _mm512_set_epi64(0, 0, 0, 0, 0, 0x40, 0, 0x40);

        let movement_gather_square_offsets_x8 = _mm512_set_epi64(
            0,
            0,
            0,
            0,
            tables::Tables::ROOK_OCCUPANCY_BITS as i64,
            tables::Tables::BISHOP_OCCUPANCY_BITS as i64,
            tables::Tables::ROOK_OCCUPANCY_BITS as i64,
            tables::Tables::BISHOP_OCCUPANCY_BITS as i64,
        );

        const BISHOP_MV_GATHER_OFFSET: i64 = (64 * tables::Tables::ROOK_OCCUPANCY_MAX) as i64;
        let const_moves_gather_offsets_x8 = _mm512_set_epi64(
            0,
            0,
            0,
            0,
            0,
            BISHOP_MV_GATHER_OFFSET,
            0,
            BISHOP_MV_GATHER_OFFSET,
        );

        let masks_x8 = _mm512_mask_i64gather_epi64(
            _mm512_setzero_si512(),
            mask,
            _mm512_add_epi64(piece_indices, const_magic_masks_gather_offsets_x8),
            tables::Tables::LT_SLIDER_MASKS_GATHER.0.as_ptr() as *const i64,
            8,
        );

        let magics_x8 = _mm512_mask_i64gather_epi64(
            _mm512_setzero_si512(),
            mask,
            _mm512_add_epi64(piece_indices, const_magic_masks_gather_offsets_x8),
            tables::Tables::LT_SLIDER_MAGICS_GATHER.0.as_ptr() as *const i64,
            8,
        );

        let const_shamt_selector = _mm512_set_epi8(
            0, 0, 0, 0, 0, 0, 0, 63, //
            0, 0, 0, 0, 0, 0, 0, 55, //
            0, 0, 0, 0, 0, 0, 0, 47, //
            0, 0, 0, 0, 0, 0, 0, 39, //
            0, 0, 0, 0, 0, 0, 0, 31, //
            0, 0, 0, 0, 0, 0, 0, 23, //
            0, 0, 0, 0, 0, 0, 0, 15, //
            0, 0, 0, 0, 0, 0, 0, 7, //
        );
        let shamt_x8 =
            _mm512_maskz_shuffle_epi8(0x0101010101010101, magics_x8, const_shamt_selector);

        let occupancy_indices_x8 = _mm512_srlv_epi64(
            _mm512_mullo_epi64(_mm512_and_epi64(full_board_x8, masks_x8), magics_x8),
            shamt_x8,
        );

        let movement_gather_indices_x8 = _mm512_add_epi64(
            _mm512_sllv_epi64(piece_indices, movement_gather_square_offsets_x8),
            occupancy_indices_x8,
        );

        let slider_movement_mask_x8 = _mm512_mask_i64gather_epi64(
            _mm512_setzero_si512(),
            mask,
            _mm512_add_epi64(movement_gather_indices_x8, const_moves_gather_offsets_x8),
            tables.slider_combined_move_masks.as_ptr() as *const i64,
            8,
        );

        slider_movement_mask_x8

        // let slider_index_x64 = _mm512_permutexvar_epi8(
        //     _mm512_set_epi8(
        //         56, 56, 56, 56, 56, 56, 56, 56, 48, 48, 48, 48, 48, 48, 48, 48, //
        //         40, 40, 40, 40, 40, 40, 40, 40, 32, 32, 32, 32, 32, 32, 32, 32, //
        //         24, 24, 24, 24, 24, 24, 24, 24, 16, 16, 16, 16, 16, 16, 16, 16, //
        //         8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0, 0, 0, 0, 0, 0, //
        //     ),
        //     piece_indices,
        // );

        // let mut permuted = _mm512_permutexvar_epi8(
        //     slider_index_x64,
        //     _mm512_load_si512(
        //         Tables::LT_ROOK_OCCUPANCY_MAGICS_PARTITIONED.0[0].as_ptr() as *const __m512i
        //     ),
        // );
        // permuted = _mm512_mask_permutexvar_epi8(
        //     permuted,
        //     BISHOP_MASK,
        //     slider_index_x64,
        //     _mm512_load_si512(
        //         Tables::LT_BISHOP_OCCUPANCY_MAGICS_PARTITIONED.0[0].as_ptr() as *const __m512i
        //     ),
        // );

        // macro_rules! load_magics_part {
        //     ($base:ident, $mask:expr, $index:expr) => {
        //         _mm512_mask_permutexvar_epi8(
        //             _mm512_mask_permutexvar_epi8(
        //                 $base,
        //                 ($mask) & ROOK_MASK,
        //                 slider_index_x64,
        //                 _mm512_load_si512(
        //                     Tables::LT_ROOK_OCCUPANCY_MAGICS_PARTITIONED.0[$index].as_ptr()
        //                         as *const __m512i,
        //                 ),
        //             ),
        //             ($mask) & BISHOP_MASK,
        //             slider_index_x64,
        //             _mm512_load_si512(
        //                 Tables::LT_BISHOP_OCCUPANCY_MAGICS_PARTITIONED.0[$index].as_ptr()
        //                     as *const __m512i,
        //             ),
        //         )
        //     };
        // }

        // permuted = load_magics_part!(permuted, 0x0202020202020202u64, 1);
        // permuted = load_magics_part!(permuted, 0x0404040404040404u64, 2);
        // permuted = load_magics_part!(permuted, 0x0808080808080808u64, 3);
        // permuted = load_magics_part!(permuted, 0x1010101010101010u64, 4);
        // permuted = load_magics_part!(permuted, 0x2020202020202020u64, 5);
        // permuted = load_magics_part!(permuted, 0x4040404040404040u64, 6);
        // permuted = load_magics_part!(permuted, 0x8080808080808080u64, 7);

        // return permuted;
    }
}

#[inline(never)]
fn test1(
    board: &chess_v2::ChessGame,
    tables: &tables::Tables,
    quiet_list: &mut [u16; 218],
    capture_list: &mut [u16; 74],
) -> (usize, usize) {
    unsafe {
        let bitboards = board.bitboards_new();
        let b_move = board.b_move();

        let friendly_move_offset = (b_move as usize) << 3;
        let opponent_move_offset = (!b_move as usize) << 3;

        let full_board = bitboards.iter().fold(0, |acc, &bb| acc | bb);
        let friendly_board = bitboards[friendly_move_offset..friendly_move_offset + 8]
            .iter()
            .fold(0, |acc, &bb| acc | bb);
        let opponent_board = bitboards[opponent_move_offset..opponent_move_offset + 8]
            .iter()
            .fold(0, |acc, &bb| acc | bb);

        let opponent_queen_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhiteQueen as usize + opponent_move_offset] as i64,
        );
        let opponent_rook_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhiteRook as usize + opponent_move_offset] as i64,
        );
        let opponent_bishop_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhiteBishop as usize + opponent_move_offset] as i64,
        );
        let opponent_knight_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhiteKnight as usize + opponent_move_offset] as i64,
        );
        let opponent_pawn_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhitePawn as usize + opponent_move_offset] as i64,
        );
        let opponent_ep_x8 =
            _mm512_set1_epi64((1u64 << (board.ep_square() as u64) >> 1 << 1) as i64);

        let const_63_x8 = _mm512_set1_epi64(63);
        let const_1_x8 = _mm512_set1_epi64(1);
        let const_n1_x8 = _mm512_set1_epi64(-1);
        let const_zero_x8 = _mm512_setzero_si512();

        // Flags
        let const_promotion_flag_x8 = _mm512_set1_epi64(chess_v2::MV_FLAG_PROMOTION as u64 as i64);
        let const_epcap_flag_x8 = _mm512_set1_epi64(chess_v2::MV_FLAG_EPCAP as u64 as i64);
        let const_cap_flag_x8 = _mm512_set1_epi64(chess_v2::MV_FLAG_CAP as u64 as i64);
        let const_dpp_flag_x8 = _mm512_set1_epi64(chess_v2::MV_FLAG_DPP as u64 as i64);

        let b_move_rank_offset = (56 * (b_move as u64)) as u8;

        let friendly_move_offset_x8 = _mm512_set1_epi64(friendly_move_offset as i64);
        let full_board_x8 = _mm512_set1_epi64(full_board as i64);
        let full_board_inv_x8 = _mm512_set1_epi64(!full_board as i64);
        let friendly_board_inv_x8 = _mm512_set1_epi64(!friendly_board as i64);
        let opponent_board_x8 = _mm512_set1_epi64(opponent_board as i64);
        let pawn_promotion_rank_x8 =
            _mm512_set1_epi64((0xFF00000000000000u64 >> b_move_rank_offset) as i64);
        let pawn_double_push_rank_x8 =
            _mm512_set1_epi64((0xFF000000u64 << ((b_move as usize) << 3)) as i64);

        let pawn_push_offset_ranks =
            (opponent_move_offset as u64 + b_move_rank_offset as u64) as i64;
        let pawn_push_rank_rolv_offset_x8 = _mm512_set1_epi64(pawn_push_offset_ranks); // white=8, black=56

        let (mut sliders_x8, mut nonsliders_x8) =
            load_bitboards_with_sliders(&bitboards, friendly_move_offset);

        let mut quiet_cursor = 0u8;

        // @todo 2*8*30=480 bytes - alloc in heap to keep stack small
        let mut capture_moves: [[u16; 8]; 30] = [[0u16; 8]; 30];
        let mut capture_cursors: [u8; 30] = [0; 30];

        #[derive(Debug)]
        enum CaptureMoves {
            PxQ,
            NxQ,
            BxQ,
            RxQ,
            QxQ,
            KxQ,

            PxR,
            NxR,
            BxR,
            RxR,
            QxR,
            KxR,

            PxB,
            NxB,
            BxB,
            RxB,
            QxB,
            KxB,

            PxN,
            NxN,
            BxN,
            RxN,
            QxN,
            KxN,

            PxP,
            NxP,
            BxP,
            RxP,
            QxP,
            KxP,
        }

        macro_rules! capture_compress {
            ($cap:expr, $src_piece_mask:ident, $cap_piece_mask:ident, $full_move_epi16_x8:ident) => {{
                let cap_mask = $src_piece_mask & $cap_piece_mask;
                let cap_cursor = &mut capture_cursors[$cap as usize];

                _mm_mask_compressstoreu_epi16(
                    capture_moves[$cap as usize]
                        .as_mut_ptr()
                        .add((*cap_cursor & 7) as usize) as *mut i16,
                    cap_mask,
                    $full_move_epi16_x8,
                );
                *cap_cursor += cap_mask.count_ones() as u8;
            }};
        }

        macro_rules! quiet_compress {
            ($mask:expr, $full_move_epi16_x8:ident) => {{
                let mask = $mask;
                _mm_mask_compressstoreu_epi16(
                    quiet_list.as_mut_ptr().add(quiet_cursor as usize) as *mut i16,
                    mask,
                    $full_move_epi16_x8,
                );
                quiet_cursor += mask.count_ones() as u8;
            }};
        }

        loop {
            // @todo - Try sub+and to pop ls1b instead of ms1b
            let one_of_each_slider_index_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(sliders_x8));
            let one_of_each_non_slider_index_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(nonsliders_x8));

            let active_pieces_non_slider_mask =
                _mm512_cmpneq_epi64_mask(one_of_each_non_slider_index_x8, const_n1_x8);
            let active_pieces_slider_mask =
                _mm512_cmpneq_epi64_mask(one_of_each_slider_index_x8, const_n1_x8);

            if (active_pieces_non_slider_mask | active_pieces_slider_mask) == 0 {
                break;
            }

            // Source square bit for masking, shifting by -1 zeroes bits for inactive lanes
            let src_sq_bit_x8 = _mm512_sllv_epi64(const_1_x8, one_of_each_non_slider_index_x8);

            let mut slider_moves_x8 = get_slider_moves_x8(
                tables,
                full_board_x8,
                one_of_each_slider_index_x8,
                active_pieces_slider_mask,
            );

            let pawn_mask = 0b00000001 & active_pieces_non_slider_mask;
            let knight_mask = 0b00000010 & active_pieces_non_slider_mask;
            let king_mask = 0b00000100 & active_pieces_non_slider_mask;

            let bishop_mask = 0b00000001 & active_pieces_slider_mask;
            let rook_mask = 0b00000010 & active_pieces_slider_mask;
            let queen_mask = 0b00001100 & active_pieces_slider_mask;

            let const_piece_indices_x8 = _mm512_set_epi64(
                64 * PieceIndex::WhiteNullPiece as i64,
                64 * PieceIndex::WhiteNullPiece as i64,
                64 * PieceIndex::WhiteNullPiece as i64,
                64 * PieceIndex::WhiteNullPiece as i64,
                64 * PieceIndex::WhiteNullPiece as i64,
                64 * PieceIndex::WhiteKing as i64,
                64 * PieceIndex::WhiteKnight as i64,
                64 * PieceIndex::WhitePawn as i64,
            );

            let mut non_slider_moves_x8 = _mm512_mask_i64gather_epi64(
                _mm512_setzero_si512(),
                active_pieces_non_slider_mask,
                _mm512_add_epi64(
                    _mm512_add_epi64(const_piece_indices_x8, friendly_move_offset_x8),
                    one_of_each_non_slider_index_x8,
                ),
                tables::Tables::LT_NON_SLIDER_MASKS_GATHER.0.as_ptr() as *const i64,
                8,
            );
            non_slider_moves_x8 = _mm512_and_si512(non_slider_moves_x8, friendly_board_inv_x8);

            // Add pawn push moves
            {
                let pawn_push_single_bit_x8 = _mm512_and_epi64(
                    _mm512_rolv_epi64(src_sq_bit_x8, pawn_push_rank_rolv_offset_x8),
                    full_board_inv_x8,
                );
                let pawn_push_double_bit_x8 = _mm512_and_epi64(
                    _mm512_rolv_epi64(pawn_push_single_bit_x8, pawn_push_rank_rolv_offset_x8),
                    _mm512_and_epi64(full_board_inv_x8, pawn_double_push_rank_x8),
                );
                let pawn_push_single_dst_sq_x8 = _mm512_slli_epi64(
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(pawn_push_single_bit_x8)),
                    6,
                );
                let pawn_push_double_dst_sq_x8 = _mm512_slli_epi64(
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(pawn_push_double_bit_x8)),
                    6,
                );
                let pawn_push_single_mask =
                    pawn_mask & _mm512_cmpneq_epi64_mask(pawn_push_single_bit_x8, const_zero_x8);
                let pawn_push_double_mask =
                    pawn_mask & _mm512_cmpneq_epi64_mask(pawn_push_double_bit_x8, const_zero_x8);

                let pawn_push_single_move_epi16_x8 = _mm512_cvtepi64_epi16(_mm512_or_epi64(
                    pawn_push_single_dst_sq_x8,
                    one_of_each_non_slider_index_x8,
                ));
                let pawn_push_double_move_epi16_x8 = _mm512_cvtepi64_epi16(_mm512_or_epi64(
                    pawn_push_double_dst_sq_x8,
                    _mm512_or_epi64(one_of_each_non_slider_index_x8, const_dpp_flag_x8),
                ));

                quiet_compress!(pawn_push_single_mask, pawn_push_single_move_epi16_x8);
                quiet_compress!(pawn_push_double_mask, pawn_push_double_move_epi16_x8);
            }

            loop {
                let non_slider_dst_sq_x8 =
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_slider_moves_x8));
                let non_slider_dst_sq_mask =
                    _mm512_cmpneq_epi64_mask(non_slider_dst_sq_x8, const_n1_x8);

                let slider_dst_sq_x8 =
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(slider_moves_x8));
                let slider_dst_sq_mask = _mm512_cmpneq_epi64_mask(slider_dst_sq_x8, const_n1_x8);

                if (non_slider_dst_sq_mask | slider_dst_sq_mask) == 0 {
                    // No more moves left
                    break;
                }

                // Dst square bits for masking
                let non_slider_dst_sq_bit_x8 = _mm512_sllv_epi64(const_1_x8, non_slider_dst_sq_x8);
                let slider_dst_sq_bit_x8 = _mm512_sllv_epi64(const_1_x8, slider_dst_sq_x8);

                // Non-slider moves
                {
                    let mut non_slider_full_move_x8 = _mm512_or_epi64(
                        _mm512_slli_epi64(non_slider_dst_sq_x8, 6),
                        one_of_each_non_slider_index_x8,
                    );

                    // Compute flags while move is still in 64-bits
                    {
                        // Promotion flag for pawn moves on the last rank
                        // NOTE: Requires special handling on move maker side
                        let promotion_mask = pawn_mask
                            & _mm512_test_epi64_mask(
                                non_slider_dst_sq_bit_x8,
                                pawn_promotion_rank_x8,
                            );

                        // EP flag for en passant captures
                        let ep_mask = pawn_mask
                            & _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_ep_x8);

                        // Capture flag
                        let cap_mask =
                            _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_board_x8);

                        non_slider_full_move_x8 = _mm512_mask_or_epi64(
                            non_slider_full_move_x8,
                            promotion_mask,
                            non_slider_full_move_x8,
                            const_promotion_flag_x8,
                        );
                        non_slider_full_move_x8 = _mm512_mask_or_epi64(
                            non_slider_full_move_x8,
                            ep_mask,
                            non_slider_full_move_x8,
                            const_epcap_flag_x8,
                        );
                        non_slider_full_move_x8 = _mm512_mask_or_epi64(
                            non_slider_full_move_x8,
                            cap_mask,
                            non_slider_full_move_x8,
                            const_cap_flag_x8,
                        );
                    }

                    // Convert full move to 16-bit format
                    let move_epi16_x8 = _mm512_cvtepi64_epi16(non_slider_full_move_x8);

                    // Create masks for captures
                    let xq_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_queen_x8);
                    let xr_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_rook_x8);
                    let xb_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_bishop_x8);
                    let xn_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_knight_x8);
                    let xp_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_pawn_x8);
                    let quiet_mask =
                        _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, full_board_inv_x8);

                    // Pawn captures
                    {
                        let xp_ep_mask = xp_mask
                            | _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_ep_x8);

                        capture_compress!(CaptureMoves::PxQ, pawn_mask, xq_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::PxR, pawn_mask, xr_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::PxB, pawn_mask, xb_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::PxN, pawn_mask, xn_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::PxP, pawn_mask, xp_ep_mask, move_epi16_x8);
                    }

                    // Knight captures
                    {
                        capture_compress!(CaptureMoves::NxQ, knight_mask, xq_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::NxR, knight_mask, xr_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::NxB, knight_mask, xb_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::NxN, knight_mask, xn_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::NxP, knight_mask, xp_mask, move_epi16_x8);
                        quiet_compress!(knight_mask & quiet_mask, move_epi16_x8);
                    }

                    // King captures
                    {
                        capture_compress!(CaptureMoves::KxQ, king_mask, xq_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::KxR, king_mask, xr_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::KxB, king_mask, xb_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::KxN, king_mask, xn_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::KxP, king_mask, xp_mask, move_epi16_x8);
                        quiet_compress!(king_mask & quiet_mask, move_epi16_x8);
                    }
                }

                // Slider moves
                {
                    let cap_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_board_x8);

                    let slider_full_move_x8 = _mm512_or_epi64(
                        _mm512_slli_epi64(slider_dst_sq_x8, 6),
                        _mm512_mask_or_epi64(
                            one_of_each_slider_index_x8,
                            cap_mask,
                            one_of_each_slider_index_x8,
                            const_cap_flag_x8,
                        ),
                    );

                    // Convert full move to 16-bit format
                    let move_epi16_x8 = _mm512_cvtepi64_epi16(slider_full_move_x8);

                    // Create masks for captures
                    let xq_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_queen_x8);
                    let xr_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_rook_x8);
                    let xb_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_bishop_x8);
                    let xn_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_knight_x8);
                    let xp_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_pawn_x8);
                    let quiet_mask =
                        _mm512_test_epi64_mask(slider_dst_sq_bit_x8, full_board_inv_x8);

                    // Rook moves
                    {
                        capture_compress!(CaptureMoves::RxQ, rook_mask, xq_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::RxR, rook_mask, xr_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::RxB, rook_mask, xb_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::RxN, rook_mask, xn_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::RxP, rook_mask, xp_mask, move_epi16_x8);
                        quiet_compress!(rook_mask & quiet_mask, move_epi16_x8);
                    }

                    // Bishop moves
                    {
                        capture_compress!(CaptureMoves::BxQ, bishop_mask, xq_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::BxR, bishop_mask, xr_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::BxB, bishop_mask, xb_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::BxN, bishop_mask, xn_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::BxP, bishop_mask, xp_mask, move_epi16_x8);
                        quiet_compress!(bishop_mask & quiet_mask, move_epi16_x8);
                    }

                    // Queen moves
                    {
                        capture_compress!(CaptureMoves::QxQ, queen_mask, xq_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::QxR, queen_mask, xr_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::QxB, queen_mask, xb_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::QxN, queen_mask, xn_mask, move_epi16_x8);
                        capture_compress!(CaptureMoves::QxP, queen_mask, xp_mask, move_epi16_x8);
                        quiet_compress!(queen_mask & quiet_mask, move_epi16_x8);
                    }
                }

                non_slider_moves_x8 =
                    _mm512_xor_epi64(non_slider_moves_x8, non_slider_dst_sq_bit_x8);
                slider_moves_x8 = _mm512_xor_epi64(slider_moves_x8, slider_dst_sq_bit_x8);
            }

            // Pop pieces
            nonsliders_x8 = _mm512_xor_epi64(
                nonsliders_x8,
                _mm512_sllv_epi64(const_1_x8, one_of_each_non_slider_index_x8),
            );
            sliders_x8 = _mm512_xor_epi64(
                sliders_x8,
                _mm512_sllv_epi64(const_1_x8, one_of_each_slider_index_x8),
            );
        }

        // Castling moves
        let king_bitboard = bitboards[PieceIndex::WhiteKing as usize + ((b_move as usize) << 3)];
        let src_sq = king_bitboard.trailing_zeros() as u16;

        quiet_list[quiet_cursor as usize] =
            ((src_sq + 2) << 6) | src_sq | chess_v2::MV_FLAGS_CASTLE_KING;
        quiet_cursor += board.is_kingside_castle_allowed(b_move) as u8;
        quiet_list[quiet_cursor as usize] =
            ((src_sq - 2) << 6) | src_sq | chess_v2::MV_FLAGS_CASTLE_QUEEN;
        quiet_cursor += board.is_queenside_castle_allowed(b_move) as u8;

        macro_rules! copy_capture_moves {
            ($cap:expr) => {
                let offset = capture_cursors[0..$cap as usize].iter().sum::<u8>() as usize;
                capture_list[offset..offset + capture_cursors[$cap as usize] as usize]
                    .copy_from_slice(
                        &capture_moves[$cap as usize][0..capture_cursors[$cap as usize] as usize],
                    );
            };
        }

        copy_capture_moves!(CaptureMoves::PxQ);
        copy_capture_moves!(CaptureMoves::NxQ);
        copy_capture_moves!(CaptureMoves::BxQ);
        copy_capture_moves!(CaptureMoves::RxQ);
        copy_capture_moves!(CaptureMoves::QxQ);
        copy_capture_moves!(CaptureMoves::KxQ);
        copy_capture_moves!(CaptureMoves::PxR);
        copy_capture_moves!(CaptureMoves::NxR);
        copy_capture_moves!(CaptureMoves::BxR);
        copy_capture_moves!(CaptureMoves::RxR);
        copy_capture_moves!(CaptureMoves::QxR);
        copy_capture_moves!(CaptureMoves::KxR);
        copy_capture_moves!(CaptureMoves::PxB);
        copy_capture_moves!(CaptureMoves::NxB);
        copy_capture_moves!(CaptureMoves::BxB);
        copy_capture_moves!(CaptureMoves::RxB);
        copy_capture_moves!(CaptureMoves::QxB);
        copy_capture_moves!(CaptureMoves::KxB);
        copy_capture_moves!(CaptureMoves::PxN);
        copy_capture_moves!(CaptureMoves::NxN);
        copy_capture_moves!(CaptureMoves::BxN);
        copy_capture_moves!(CaptureMoves::RxN);
        copy_capture_moves!(CaptureMoves::QxN);
        copy_capture_moves!(CaptureMoves::KxN);
        copy_capture_moves!(CaptureMoves::PxP);
        copy_capture_moves!(CaptureMoves::NxP);
        copy_capture_moves!(CaptureMoves::BxP);
        copy_capture_moves!(CaptureMoves::RxP);
        copy_capture_moves!(CaptureMoves::QxP);
        copy_capture_moves!(CaptureMoves::KxP);

        // capture_list[0..capture_cursors[CaptureMoves::PxQ as usize] as usize].copy_from_slice(
        //     &capture_moves[CaptureMoves::PxQ as usize]
        //         [0..capture_cursors[CaptureMoves::PxQ as usize] as usize],
        // );

        for i in 0..30 {
            assert!(capture_cursors[i] <= 8);
        }

        // for i in 0..30 {
        //     println!(
        //         "[{:?}]: {:?}",
        //         std::mem::transmute::<u8, CaptureMoves>(i),
        //         capture_moves[i as usize]
        //             .iter()
        //             .map(|mv| {
        //                 if *mv == 0 {
        //                     return "".to_string();
        //                 }
        //                 util::move_string_dbg(*mv as u16)
        //             })
        //             .collect::<Vec<_>>()
        //     );
        // }

        // for mv in quiet_moves {
        //     if mv == 0 {
        //         break;
        //     }
        //     println!("Quiet: {:?} ({})", mv, util::move_string_dbg(mv as u16));
        // }

        (
            quiet_cursor as usize,
            capture_cursors[0..30].iter().sum::<u8>() as usize,
        )
    }
}

#[inline(never)]
fn testing() {
    // let lol = tables::Tables::gen_slider_hash_functions::<false>();
    // println!("lol: {:#x?}", lol);
    // return;

    let tables = tables::Tables::new();

    let b_move = black_box(false);

    let mut board = black_box(chess_v2::ChessGame::new());
    // "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    assert!(
        board
            .load_fen(
                // "rnbqkbnr/pppppppp/8/8/5q1q/6P1/PPPPPPP1/RNBQKBNR w KQkq - 0 1",
                "rnbqkbnr/pPpppp1p/4P3/6pP/5q1q/1P4P1/P1PPp3/RNBQKBNR w KQkq g6 0 2",
                &tables
            )
            .is_ok()
    );

    // let mut move_list = [0u16; 256];
    // for i in 0..64 {
    //     black_box(board.gen_moves_slow(&tables, &mut move_list));
    // }

    // let s = rdtscp();

    // let mut move_count = board.gen_moves_slow(&tables, &mut move_list);

    // let e = rdtscp() - s;
    // println!(
    //     "Move generation took {} cycles, found {} moves",
    //     e, move_count
    // );

    // black_box(move_count);
    // black_box(move_list);

    // [0, K, Q, R, B, N, P, 0]
    // [0, k, q, r, b, n, p, 0]

    // 1. PV Move (if any)
    // 2. TT Move (if any)
    // 3. Captures
    //  3.1 Captures of higher captured piece
    //   3.1.1 Captures with lower own piece
    // 4. Quiet moves
    //  4.1 Beta moves
    //  4.2 Alpha moves

    // Idea 1:
    // Process all pawns at once first, then all other pieces, parallelise on piece type.
    // 1. Put all pawn indices into a vector with _mm512_maskz_compress_epi8 and a identity vector, widen with _mm512_mask_cvtepi8_epi64

    // Idea 2:
    // Process one of each piece type at a time. Create masks, Use a gather to get possible movement masks
    // Process king separately because it is only one? or reserve the lane for pawns on next iteration?
    // For pawns, captures ONLY and do push separately

    // Maximum pieces:
    // 8 pawns, 10 knights, 10 rooks, 10 bishops, 9 queens, 1 king

    // [QUEEN MASK, ...]
    // [ROOK MASK, ...]

    let mut quiet_list = [0u16; 218];
    let mut capture_list = [0u16; 74];
    let (q_size, c_size) = test1(&board, &tables, &mut quiet_list, &mut capture_list);

    println!(
        "Capture list: {:?}",
        capture_list
            .iter()
            .take(c_size)
            .map(|&mv| util::move_string_dbg(mv as u16))
            .collect::<Vec<_>>()
    );
    println!(
        "Quiet list: {:?}",
        quiet_list
            .iter()
            .take(q_size)
            .map(|&mv| util::move_string_dbg(mv as u16))
            .collect::<Vec<_>>()
    );
}

fn main() {
    testing();

    // return;
    // use std::arch::x86_64::*;
    // use std::hint::black_box;

    // // use std::simd::*;
    // // use std::simd::{cmp::*, num::*};

    // fn print_vec(vec: __m512i) {
    //     let mut arr: [i64; 8] = [0; 8];
    //     unsafe {
    //         _mm512_storeu_si512(arr.as_mut_ptr() as *mut __m512i, vec);
    //     }
    //     println!("{:?}", arr);
    //     // println!("Binary representation:");
    //     // for i in 0..8 {
    //     //     println!("{i}");
    //     //     println!("{:064b}", arr[i]);
    //     // }
    // }

    // fn print_vec256(vec: __m256i) {
    //     let mut arr: [i32; 8] = [0; 8];
    //     unsafe {
    //         _mm256_storeu_si256(arr.as_mut_ptr() as *mut __m256i, vec);
    //     }
    //     println!("{:?}", arr);
    // }

    // const WEIGHT_KING: i32 = 10000;
    // const WEIGHT_QUEEN: i32 = 1000;
    // const WEIGHT_ROOK: i32 = 525;
    // const WEIGHT_BISHOP: i32 = 350;
    // const WEIGHT_KNIGHT: i32 = 350;
    // const WEIGHT_PAWN: i32 = 100;
    // const PV_DEPTH: usize = 64;

    // const WEIGHT_TABLE: [i32; 12] = [
    //     WEIGHT_KING,
    //     WEIGHT_QUEEN,
    //     WEIGHT_ROOK,
    //     WEIGHT_BISHOP,
    //     WEIGHT_KNIGHT,
    //     WEIGHT_PAWN,
    //     -WEIGHT_KING,
    //     -WEIGHT_QUEEN,
    //     -WEIGHT_ROOK,
    //     -WEIGHT_BISHOP,
    //     -WEIGHT_KNIGHT,
    //     -WEIGHT_PAWN,
    // ];

    // let mut final_score = 0;

    // let mut boards_ref = black_box([0b0u64; 12]);

    // boards_ref[0] = 0b1;
    // boards_ref[1] = 0b100;
    // boards_ref[2] = 0b1;
    // boards_ref[3] = 0b10000000000000;

    // // boards_ref[4] = 0b11000000000000;

    // boards_ref[5] = 0b1111111100000000;

    // boards_ref[6] = 0b1;
    // boards_ref[11] = 0b1111111100000000;

    // // const PAWN_SPLIT_MASK: u64 = 0xF0F0F0F0F0F0F0F0u64;

    // let const_pawn_split_mask_abc: u64 = 0xE0E0E0E0E0E0E0E0u64;
    // let const_pawn_split_mask_def: u64 = 0x1C1C1C1C1C1C1C1Cu64;
    // let const_pawn_split_mask_gh: u64 = 0x0303030303030303u64;

    // let boards_ref = &boards_ref;

    // let mut EVAL_TABLES_INV = [[0i32; 64]; 12];

    // EVAL_TABLES_INV[0][0] = 5;
    // EVAL_TABLES_INV[0][1] = 10;
    // EVAL_TABLES_INV[0][2] = 15;

    // EVAL_TABLES_INV[1][2] = 50;

    // EVAL_TABLES_INV[2][0] = 75;

    // EVAL_TABLES_INV[3][13] = 90;

    // EVAL_TABLES_INV[5][8] = 8;
    // EVAL_TABLES_INV[5][13] = 9;

    // EVAL_TABLES_INV[11][0] = -2;

    // EVAL_TABLES_INV[11][8] = -8; // unused for now
    // EVAL_TABLES_INV[11][13] = -9;

    // unsafe {
    //     let mut boards_0_vec = _mm512_loadu_epi64(boards_ref.as_ptr() as *const i64); // 8 cycles
    //     // @todo - remove masked load
    //     let mut boards_1_vec =
    //         _mm512_maskz_loadu_epi64(0b11111100, boards_ref.as_ptr().add(4) as *const i64); // 8 cycles

    //     // let mut boards_vec = _mm512_loadu_epi64(
    //     //     self.chess
    //     //         .board
    //     //         .bitboards
    //     //         .as_ptr()
    //     //         .add(self.chess.b_move as usize * 6) as *const i64,
    //     // ); // 8 cycles

    //     // let const_boards_0_pawn_split_mask =
    //     //     _mm512_set_epi64(!0, !0, PAWN_SPLIT_MASK as i64, !0, !0, !0, !0, !0);
    //     let const_pawn_split_mask_0_vec = _mm512_set_epi64(
    //         const_pawn_split_mask_gh as i64,
    //         const_pawn_split_mask_def as i64,
    //         const_pawn_split_mask_abc as i64,
    //         !0,
    //         !0,
    //         !0,
    //         !0,
    //         !0,
    //     );
    //     let const_pawn_split_mask_1_vec = _mm512_set_epi64(
    //         const_pawn_split_mask_abc as i64,
    //         !0,
    //         !0,
    //         !0,
    //         !0,
    //         !0,
    //         const_pawn_split_mask_def as i64,
    //         const_pawn_split_mask_gh as i64,
    //     );
    //     let const_pawn_split_1_cross_lane_selector = _mm512_set_epi64(0, 0, 0, 0, 0, 0, 0x7, 0x7);

    //     boards_0_vec =
    //         _mm512_mask_permutex_epi64(boards_0_vec, (1 << 6) | (1 << 7), boards_0_vec, 0b1010000); // 3 cycles
    //     boards_0_vec = _mm512_and_si512(boards_0_vec, const_pawn_split_mask_0_vec); // 1 cycle

    //     boards_1_vec = _mm512_mask_permutex2var_epi64(
    //         boards_1_vec,
    //         (1 << 1) | 1,
    //         const_pawn_split_1_cross_lane_selector,
    //         boards_1_vec,
    //     ); // 3 cycles
    //     boards_1_vec = _mm512_and_si512(boards_1_vec, const_pawn_split_mask_1_vec); // 1 cycle

    //     let const_boards_0_mask = 0b11111111;
    //     let const_boards_1_mask = 0b11111111;
    //     let const_1vec = _mm512_set1_epi64(1);
    //     let const_63vec = _mm512_set1_epi64(63);
    //     let const_64vec = _mm512_set1_epi64(64);
    //     let const_0vec = _mm256_setzero_si256();
    //     let const_piece_offsets_0_vec =
    //         _mm512_set_epi64(64 * 5, 64 * 5, 64 * 5, 64 * 4, 64 * 3, 64 * 2, 64, 0);
    //     let const_piece_offsets_1_vec = _mm512_set_epi64(
    //         64 * 5 + 384,
    //         64 * 4 + 384,
    //         64 * 3 + 384,
    //         64 * 2 + 384,
    //         64 + 384,
    //         384,
    //         64 * 5 + 384,
    //         64 * 5 + 384,
    //     );
    //     let const_weights_0_vec = _mm256_set_epi32(
    //         WEIGHT_PAWN,
    //         WEIGHT_PAWN,
    //         WEIGHT_PAWN,
    //         WEIGHT_KNIGHT,
    //         WEIGHT_BISHOP,
    //         WEIGHT_ROOK,
    //         WEIGHT_QUEEN,
    //         WEIGHT_KING,
    //     );
    //     let const_weights_1_vec = _mm256_set_epi32(
    //         -WEIGHT_PAWN,
    //         -WEIGHT_KNIGHT,
    //         -WEIGHT_BISHOP,
    //         -WEIGHT_ROOK,
    //         -WEIGHT_QUEEN,
    //         -WEIGHT_KING,
    //         -WEIGHT_PAWN,
    //         -WEIGHT_PAWN,
    //     );

    //     let mut score_vec = _mm256_setzero_si256();

    //     loop {
    //         let lzcnt_0_vec =
    //             _mm512_mask_lzcnt_epi64(const_64vec, const_boards_0_mask, boards_0_vec); // 4 cycles

    //         let lzcnt_1_vec =
    //             _mm512_mask_lzcnt_epi64(const_64vec, const_boards_1_mask, boards_1_vec); // 4 cycles

    //         let active_pieces_0_mask = _mm512_cmpneq_epi64_mask(lzcnt_0_vec, const_64vec); // 3 cycles
    //         let active_pieces_1_mask = _mm512_cmpneq_epi64_mask(lzcnt_1_vec, const_64vec); // 3 cycles

    //         if (active_pieces_0_mask | active_pieces_1_mask) == 0 {
    //             break;
    //         }

    //         let piece_index_0_vec = _mm512_sub_epi64(const_63vec, lzcnt_0_vec); // 1 cycle
    //         let piece_index_1_vec = _mm512_sub_epi64(const_63vec, lzcnt_1_vec); // 1 cycle

    //         let gather_offsets_0_vec =
    //             _mm512_add_epi64(const_piece_offsets_0_vec, piece_index_0_vec); // 1 cycle
    //         let bonuses_0 = _mm512_mask_i64gather_epi32(
    //             const_0vec,
    //             active_pieces_0_mask,
    //             gather_offsets_0_vec,
    //             EVAL_TABLES_INV.as_ptr() as *const i32,
    //             4,
    //         ); // 25 cycles

    //         let gather_offsets_1_vec =
    //             _mm512_add_epi64(const_piece_offsets_1_vec, piece_index_1_vec); // 1 cycle
    //         let bonuses_1 = _mm512_mask_i64gather_epi32(
    //             const_0vec,
    //             active_pieces_1_mask,
    //             gather_offsets_1_vec,
    //             EVAL_TABLES_INV.as_ptr() as *const i32,
    //             4,
    //         ); // 25 cycles

    //         let scores_0 =
    //             _mm256_maskz_add_epi32(active_pieces_0_mask, bonuses_0, const_weights_0_vec); // 1 cycle

    //         let scores_1 =
    //             _mm256_maskz_add_epi32(active_pieces_1_mask, bonuses_1, const_weights_1_vec); // 1 cycle

    //         let scores = _mm256_add_epi32(scores_0, scores_1); // 1 cycle
    //         score_vec = _mm256_add_epi32(score_vec, scores); // 1 cycle

    //         let pop_vec = _mm512_sllv_epi64(const_1vec, piece_index_0_vec); // 1 cycle
    //         boards_0_vec = _mm512_xor_epi64(boards_0_vec, pop_vec); // 1 cycle

    //         let pop_vec = _mm512_sllv_epi64(const_1vec, piece_index_1_vec); // 1 cycle
    //         boards_1_vec = _mm512_xor_epi64(boards_1_vec, pop_vec); // 1 cycle

    //         println!("it");
    //     }

    //     final_score =
    //         _mm512_mask_reduce_add_epi32(0b0000000011111111, _mm512_castsi256_si512(score_vec)); // ?? cycles

    //     // final_score += _mm512_reduce_add_epi32(score_vec) as i32; // ?? cycles
    // }

    // println!("Final score: {}", final_score);

    // return;

    // old makemove: 1432
    // new makemove: 1079

    let mode = std::env::args().nth(1).unwrap_or("uci".to_string());

    std::hint::black_box(unsafe {
        chess_v2::ChessGame::new().make_move(std::hint::black_box(0), &tables::Tables::new())
    });
    std::hint::black_box(
        chess_v2::ChessGame::new()
            .in_check_slow(&tables::Tables::new(), std::hint::black_box(false)),
    );
    let mut scratch = std::hint::black_box([[0u16; 32]; 32]);
    let mut scratch2 = std::hint::black_box([0 as *mut i16; 32]);
    std::hint::black_box(chess_v2::ChessGame::new().gen_moves_avx512(
        &tables::Tables::new(),
        std::hint::black_box(&mut [0u16; 218]),
        std::hint::black_box(&mut [0u16; 74]),
        &mut scratch,
        &mut scratch2,
    ));

    let result = match mode.as_str() {
        // "gui" => chess_ui(),
        "uci" => {
            let (tx_search, rx_search) = channel::bounded(1);
            let tables = tables::Tables::new();

            // Safety: TranspositionTable is Send+Sync, it is up to the table implementation itself to
            // implement thread safety correctly. This has a few benefits:
            // 1. TranspositionTable can be accessed without runtime costs such as locks or refcounters
            // 2. The table can potentially be shared between threads efficiently, even unsoundly if races are deemed to be acceptable
            let tt = SyncUnsafeCell::new(transposition::TranspositionTable::new(4));

            let result = std::thread::scope(|s| {
                let st = s.spawn(|| {
                    search_thread(rx_search, &tables, &tt);
                });

                let result = chess_uci(tx_search, &tables, &tt);

                st.join().unwrap();

                result
            });

            result
        }
        _ => panic!("Unknown mode: {}", mode),
    };

    match result {
        Ok(_) => println!("Exited successfully"),
        Err(e) => println!("Error: {}", e),
    };
}
