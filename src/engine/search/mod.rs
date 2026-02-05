pub struct SigAbort {}

pub type AbortSignal = crossbeam::channel::Receiver<SigAbort>;

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

            let mut search_engine = search::Search::new(params, &tables, 4, rt);

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
            println!("β-cutoff count: {}", search_engine.b_cut_count());
            println!("α-raise count: {}", search_engine.a_raise_count());
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
}
