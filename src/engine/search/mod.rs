pub struct SigAbort {}

pub type AbortSignal = crossbeam::channel::Receiver<SigAbort>;

#[derive(std::marker::ConstParamTy, PartialEq, Eq)]
pub enum EngineForm {
    Strategy,
    TacticalA,
    TacticalB,
}

pub trait SearchStrategy<'a> {
    fn search(&mut self) -> u16;
    fn num_nodes_searched(&self) -> u64;
    fn search_score(&self) -> i32;
}

pub mod eval;
mod moves;
pub mod repetition;
pub mod search;
pub mod search_params;
pub mod see;
pub mod transposition;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        engine::{search::search_params::SearchParams, tables},
        util,
    };

    fn rdtsc() -> u64 {
        unsafe { std::arch::x86_64::_rdtsc() }
    }

    #[test]
    fn search_bench() {
        let tables = tables::Tables::new();

        core_affinity::set_for_current(core_affinity::CoreId { id: 2 });

        // let (tx, rx) = crossbeam::channel::unbounded();
        // let test_fen = "8/3PPP2/4K3/8/P2qN3/3k4/3N4/1q6 w - - 0 1"; // EG
        let test_fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"; // MG
        // let test_fen = util::FEN_STARTPOS;
        // let test_fen = "8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w - - 0 1";

        let bench = || {
            let rt = repetition::RepetitionTable::new();
            // let mut chess = chess_v2::ChessGame::new();
            // assert!(
            //     chess
            //         .load_fen(std::hint::black_box(test_fen), &tables)
            //         .is_ok()
            // );

            let mut params = SearchParams::new();
            params.depth = Some(std::hint::black_box(14));

            // let mut search_engine =
            //     v11_opt::Search::new(params, chess, &tables, unsafe { &mut *tt.get() }, rt, &rx);

            let mut search_engine =
                search::Search::<{ EngineForm::Strategy }>::new(params, &tables, 16, rt);

            search_engine.new_game();
            search_engine.load_from_fen(test_fen, &tables).unwrap();
            search_engine.new_search();

            let (bestmove, delta) = {
                let start = rdtsc();

                let mv = std::hint::black_box(search_engine.search());

                let end = rdtsc();
                (mv, end - start)
            };

            // let corr_stats = &mut search_engine.corr_stats;
            // corr_stats.sort_by(|a, b| b.error.abs().cmp(&a.error.abs()));
            // let len = corr_stats.len();

            // println!("PC: Top 10 pawn correction errors:");
            // for stat in corr_stats.iter().take(10) {
            //     println!(
            //         "Original eval: {}, Correction error: {}",
            //         stat.original_eval, stat.error
            //     );
            // }
            // println!(
            //     "PC: Median pawn correction error: {}",
            //     corr_stats[len / 2].error
            // );
            // println!(
            //     "PC: Average pawn correction error: {}",
            //     corr_stats.iter().map(|s| s.error as i64).sum::<i64>() / len as i64
            // );
            // println!("num corrections applied: {}", corr_stats.len());

            // let categories = [
            //     -501, -500, -400, -300, -200, -100, -50, -20, -10, -5, 0, 5, 10, 20, 50, 100, 200,
            //     300, 400, 500, 501,
            // ];
            // let mut distribution = vec![0; categories.len() + 1];
            // for stat in corr_stats.iter() {
            //     let mut placed = false;
            //     for (i, &cat) in categories.iter().enumerate() {
            //         if stat.error <= cat {
            //             distribution[i] += 1;
            //             placed = true;
            //             break;
            //         }
            //     }
            //     if !placed {
            //         distribution[categories.len()] += 1;
            //     }
            // }

            // println!("PC: Pawn correction error distribution:");
            // for (i, &cat) in categories.iter().enumerate() {
            //     if i == 0 {
            //         println!("  <= {}: {}", cat, distribution[i]);
            //     } else if i == categories.len() - 1 {
            //         println!("  > {}: {}", categories[i - 1], distribution[i]);
            //     } else {
            //         println!("  ({} , {}]: {}", categories[i - 1], cat, distribution[i]);
            //     }
            // }

            println!(
                "Nodes searched: {} ({} quiescence / {:.02}%)",
                search_engine.num_nodes_searched(),
                search_engine.get_quiet_nodes(),
                (search_engine.get_quiet_nodes() as f64
                    / search_engine.num_nodes_searched() as f64)
                    * 100.0
            );
            println!("depth: {}", search_engine.get_depth());
            println!("q-depth: {}", search_engine.get_quiet_depth());
            // println!("TT usage: {:.02}%", tt_stats.fill_percentage * 100.0);
            // println!(
            //     "TT probe hit rate: {:.02}%",
            //     tt_stats.probe_hit as f64
            //         / (tt_stats.probe_hit as f64 + tt_stats.probe_miss as f64)
            //         * 100.0
            // );
            // println!(
            //     "TT store hit rate: {:.02}%",
            //     tt_stats.store_hit as f64
            //         / (tt_stats.store_hit as f64 + tt_stats.store_miss as f64)
            //         * 100.0
            // );

            // let et_stats = search_engine.get_et().calc_stats();
            // println!(
            //     "ET usage: {:.02}% ({} collisions)",
            //     et_stats.fill_percentage * 100.0,
            //     et_stats.collisions
            // );
            // println!(
            //     "Eval probe hit rate: {:.02}%",
            //     et_stats.probe_hit as f64
            //         / (et_stats.probe_hit as f64 + et_stats.probe_miss as f64)
            //         * 100.0
            // );
            println!(
                "PV: {:?}",
                search_engine
                    .get_pv()
                    .iter()
                    .map(|mv| util::move_string(*mv))
                    .collect::<Vec<_>>()
            );
            println!(
                "Search time: {} cycles ({} Mcycles)",
                delta,
                delta / 1_000_000
            );

            delta
        };

        const ITERATIONS: usize = 5;
        let mut total_cycles = 0;
        for _ in 0..ITERATIONS {
            total_cycles += bench();
        }

        println!(
            "Average search time over {} iterations: {} Mcycles",
            ITERATIONS,
            total_cycles / ITERATIONS as u64 / 1_000_000
        );
    }

    #[test]
    fn search_accuracy() {
        use crate::engine::search::eval::WEIGHT_TABLE_ABS;
        use crate::matchmaking::fen_feeder::SharedFenFeeder;
        use crate::matchmaking::matchmaking::PositionFeeder;

        const NUM_POSITIONS: usize = 1000;
        const NUM_THREADS: usize = 16;
        const BENCH_DEPTH: u8 = 10;
        const TT_SIZE_MB: usize = 16;
        const POSITIONS_PATH: &str = r".\data\positions\10m.txt";

        fn total_material(bbs: &[u64; 16]) -> i32 {
            let mut total = 0i32;
            for i in 0..16 {
                total += bbs[i].count_ones() as i32 * WEIGHT_TABLE_ABS[i] as i32;
            }
            total
        }

        let tables = tables::Tables::new();
        let feeder = SharedFenFeeder::new(POSITIONS_PATH);
        feeder.set_max_positions(NUM_POSITIONS);

        let (tx, rx) =
            crossbeam::channel::bounded::<(String, i32, i32, u64, u16, u64, i32, u64, u16, u64)>(
                256,
            );

        println!(
            "position,material,a_score,a_nodes,a_bestmove,a_cycles,b_score,b_nodes,b_bestmove,b_cycles"
        );

        std::thread::scope(|s| {
            for i in 0..NUM_THREADS {
                let tables = &tables;
                let mut feeder = feeder.clone();
                let tx = tx.clone();

                s.spawn(move || {
                    let core_id = if i % 2 == 0 { i & 31 } else { (i + 16) & 31 };
                    core_affinity::set_for_current(core_affinity::CoreId { id: core_id });

                    let mut params_s = SearchParams::new();
                    params_s.depth = Some(BENCH_DEPTH);
                    let mut params_t = SearchParams::new();
                    params_t.depth = Some(BENCH_DEPTH);

                    let mut engine_a = search::Search::<{ EngineForm::TacticalA }>::new(
                        params_s,
                        tables,
                        TT_SIZE_MB,
                        repetition::RepetitionTable::new(),
                    );
                    let mut engine_b = search::Search::<{ EngineForm::TacticalB }>::new(
                        params_t,
                        tables,
                        TT_SIZE_MB,
                        repetition::RepetitionTable::new(),
                    );

                    while let Some(fen) = feeder.next_position() {
                        engine_a.new_game();
                        engine_a.load_from_fen(&fen, tables).unwrap();
                        engine_a.new_search();
                        let material = total_material(engine_a.get_board_mut().bitboards());
                        let start_a = rdtsc();
                        let bestmove_a = engine_a.search();
                        let cycles_a = rdtsc() - start_a;

                        engine_b.new_game();
                        engine_b.load_from_fen(&fen, tables).unwrap();
                        engine_b.new_search();
                        let start_b = rdtsc();
                        let bestmove_b = engine_b.search();
                        let cycles_b = rdtsc() - start_b;

                        let _ = tx.send((
                            fen.to_string(),
                            material,
                            engine_a.search_score(),
                            engine_a.num_nodes_searched(),
                            bestmove_a,
                            cycles_a,
                            engine_b.search_score(),
                            engine_b.num_nodes_searched(),
                            bestmove_b,
                            cycles_b,
                        ));
                    }
                });
            }

            drop(tx);

            let mut count = 0usize;
            while let Ok((
                fen,
                material,
                score_a,
                nodes_a,
                bestmove_a,
                cycles_a,
                score_b,
                nodes_b,
                bestmove_b,
                cycles_b,
            )) = rx.recv()
            {
                println!(
                    "{},{},{},{},{},{},{},{},{},{}",
                    fen,
                    material,
                    score_a,
                    nodes_a,
                    util::move_string(bestmove_a),
                    cycles_a,
                    score_b,
                    nodes_b,
                    util::move_string(bestmove_b),
                    cycles_b,
                );
                count += 1;
                if count % 100 == 0 {
                    eprintln!(
                        "Progress: {:.02}%",
                        count as f64 / NUM_POSITIONS as f64 * 100.0
                    );
                }
            }
        });
    }
}
