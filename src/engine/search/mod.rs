#[derive(std::marker::ConstParamTy, PartialEq, Eq)]
pub enum EngineForm {
    Strategy,
    TacticalA,
    TacticalB,
}

pub trait SearchStrategy<'a> {
    fn search(&mut self, depth: Option<u8>) -> u16;
    fn num_nodes_searched(&self) -> u64;
    fn search_score(&self) -> i32;
}

pub mod eval;
mod moves;
pub mod repetition;
pub mod search;
pub mod search_params;
pub mod see;
pub mod stability;
pub mod timeman;
pub mod transposition;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{engine::tables, util};

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

            let depth = Some(std::hint::black_box(18));

            let tt = std::cell::SyncUnsafeCell::new(transposition::TranspositionTable::new(16));
            let tm = std::cell::SyncUnsafeCell::new(timeman::TimeManager::new());
            let mut search_engine =
                search::Search::<{ EngineForm::Strategy }>::new(&tables, &tt, &tm, rt);

            search_engine.new_game();
            search_engine.load_from_fen(test_fen, &tables).unwrap();
            search_engine.new_search();

            let (bestmove, delta) = {
                let start = rdtsc();

                let mv = std::hint::black_box(search_engine.search(depth));

                let end = rdtsc();
                (mv, end - start)
            };

            println!("Nodes searched: {}", search_engine.num_nodes_searched());
            println!("depth: {}", search_engine.get_depth());

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
        use crate::engine::search::search::DepthStat;
        use crate::matchmaking::fen_feeder::SharedFenFeeder;
        use crate::matchmaking::matchmaking::PositionFeeder;

        const NUM_POSITIONS: usize = 5_000;
        const NUM_THREADS: usize = 16;
        const BENCH_DEPTH: u8 = 9;
        const TT_SIZE_MB: usize = 16;
        const POSITIONS_PATH: &str = r".\data\positions\10m.txt";

        fn total_material(bbs: &[u64; 16]) -> i32 {
            let mut total = 0i32;
            for i in 0..16 {
                total += bbs[i].count_ones() as i32 * WEIGHT_TABLE_ABS[i] as i32;
            }
            total / 2
        }

        let tables = tables::Tables::new();
        let feeder = SharedFenFeeder::new(POSITIONS_PATH);
        feeder.set_max_positions(NUM_POSITIONS);
        let start = std::time::Instant::now();

        let (tx, rx) =
            crossbeam::channel::bounded::<(String, i32, Vec<DepthStat>, Vec<DepthStat>)>(256);

        println!(
            "position,material,depth,a_score,a_nodes,a_bestmove,a_cycles,b_score,b_nodes,b_bestmove,b_cycles"
        );

        std::thread::scope(|s| {
            for i in 0..NUM_THREADS {
                let tables = &tables;
                let mut feeder = feeder.clone();
                let tx = tx.clone();

                s.spawn(move || {
                    let core_id = if i % 2 == 0 { i & 31 } else { (i + 16) & 31 };
                    core_affinity::set_for_current(core_affinity::CoreId { id: core_id });

                    let tm = std::cell::SyncUnsafeCell::new(timeman::TimeManager::new());

                    let tt_a = std::cell::SyncUnsafeCell::new(
                        transposition::TranspositionTable::new(TT_SIZE_MB),
                    );
                    let mut engine_a = search::Search::<{ EngineForm::TacticalA }>::new(
                        tables,
                        &tt_a,
                        &tm,
                        repetition::RepetitionTable::new(),
                    );
                    let tt_b = std::cell::SyncUnsafeCell::new(
                        transposition::TranspositionTable::new(TT_SIZE_MB),
                    );
                    let mut engine_b = search::Search::<{ EngineForm::TacticalB }>::new(
                        tables,
                        &tt_b,
                        &tm,
                        repetition::RepetitionTable::new(),
                    );

                    while let Some(fen) = feeder.next_position() {
                        engine_a.new_game();
                        engine_a.load_from_fen(&fen, tables).unwrap();
                        engine_a.new_search();
                        let material = total_material(engine_a.get_board_mut().bitboards());
                        engine_a.search(Some(BENCH_DEPTH));

                        engine_b.new_game();
                        engine_b.load_from_fen(&fen, tables).unwrap();
                        engine_b.new_search();
                        engine_b.search(Some(BENCH_DEPTH));

                        let _ = tx.send((
                            fen.to_string(),
                            material,
                            engine_a.depth_stats().to_vec(),
                            engine_b.depth_stats().to_vec(),
                        ));
                    }
                });
            }

            drop(tx);

            let mut count = 0usize;
            while let Ok((fen, material, stats_a, stats_b)) = rx.recv() {
                let n = stats_a.len().min(stats_b.len());
                for d in 0..n {
                    let a = stats_a[d];
                    let b = stats_b[d];
                    println!(
                        "{},{},{},{},{},{},{},{},{},{},{}",
                        fen,
                        material,
                        a.depth,
                        a.score,
                        a.nodes,
                        util::move_string(a.bestmove),
                        a.cycles,
                        b.score,
                        b.nodes,
                        util::move_string(b.bestmove),
                        b.cycles,
                    );
                }
                count += 1;
                if count % 100 == 0 {
                    let elapsed = start.elapsed().as_secs_f64();
                    let rate = count as f64 / elapsed;
                    let eta_ms = (((NUM_POSITIONS - count) as f64 / rate) * 1000.0) as u64;
                    eprintln!(
                        "Progress: {:.02}% | Elapsed: {:.0}s | ETA: {}",
                        count as f64 / NUM_POSITIONS as f64 * 100.0,
                        elapsed,
                        util::time_format(eta_ms),
                    );
                }
            }

            eprintln!("Done in {:.1}s", start.elapsed().as_secs_f64());
        });
    }
}
