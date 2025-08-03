use crate::engine::{
    chess,
    search::{search_params::SearchParams, transposition::TranspositionTable},
    tables,
};

pub struct SigAbort {}

pub type AbortSignal = crossbeam::channel::Receiver<SigAbort>;

pub trait SearchStrategy<'a> {
    fn search(&mut self) -> u16;
    fn num_nodes_searched(&self) -> u64;
    fn search_score(&self) -> i32;
}

pub mod repetition;
pub mod search_params;
pub mod transposition;
pub mod v1_negamax;
pub mod v2_alphabeta;
pub mod v3_itdep;
pub mod v4_pv;
pub mod v5_tt;
pub mod v6_psquare;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{engine::chess::ChessGame, util};

    fn rdtsc() -> u64 {
        unsafe { std::arch::x86_64::_rdtsc() }
    }

    #[test]
    fn search_bench() {
        let tables = tables::Tables::new();

        let (tx, rx) = crossbeam::channel::unbounded();
        // let test_fen = "8/3PPP2/4K3/8/P2qN3/3k4/3N4/1q6 w - - 0 1"; // EG
        let test_fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"; // MG
        // let test_fen = util::FEN_STARTPOS;

        let bench = || {
            let mut tt = transposition::TranspositionTable::new(4);
            let mut rt = repetition::RepetitionTable::new();

            let mut chess = ChessGame::new();
            assert!(
                chess
                    .load_fen(std::hint::black_box(test_fen), &tables)
                    .is_ok()
            );

            let mut params = SearchParams::new();
            params.depth = Some(std::hint::black_box(7));

            let mut search_engine =
                v6_psquare::Search::new(params, chess, &tables, &mut tt, rt, &rx);

            // let mut search_engine = v5_tt::Search::new(params, chess, &tables, &mut tt, rt, &rx);
            // let mut search_engine = v4_pv::Search::new(params, chess, &tables, &rx);
            // let mut search_engine = v3_itdep::IterativeDeepening::new(params, chess, &tables, &rx);

            let (bestmove, delta) = {
                let start = rdtsc();

                let mv = std::hint::black_box(search_engine.search());
                // for i in 0..100_000_000 {
                //     let eval = search_engine.evaluate();
                //     std::hint::black_box(eval);
                // }
                // let mv = 0;

                let end = rdtsc();
                (mv, end - start)
            };
            println!("Best move: {}", util::move_string(bestmove));

            // search_engine.__evalstats.sort_by(|a, b| a.1.cmp(&b.1));
            // println!(
            //     "eval iterations median: {}",
            //     search_engine.__evalstats[search_engine.__evalstats.len() / 2].1
            // );
            // println!(
            //     "eval iterations average: {}",
            //     search_engine
            //         .__evalstats
            //         .iter()
            //         .map(|&x| x.1 as i64)
            //         .sum::<i64>() as f64
            //         / search_engine.__evalstats.len() as f64
            // );

            // for i in 0..6 {
            //     println!(
            //         "eval average position has {} pieces of type {:?} (unsided)",
            //         search_engine
            //             .__evalstats
            //             .iter()
            //             .map(|x| x.0[i] as u64)
            //             .sum::<u64>()
            //             / search_engine.__evalstats.len() as u64,
            //         util::PieceId::from(i)
            //     )
            // }

            // println!(
            //     "eval stats max: {:?}",
            //     search_engine
            //         .__evalstats
            //         .iter()
            //         .skip(search_engine.__evalstats.len() - 1 - 20)
            //         .collect::<Vec<_>>()
            // );

            println!("Nodes searched: {}", search_engine.num_nodes_searched());
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

        const ITERATIONS: usize = 10;
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
