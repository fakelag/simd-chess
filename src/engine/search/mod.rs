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
        let test_fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";

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
            params.depth = Some(std::hint::black_box(8));

            let mut search_engine = v5_tt::Search::new(params, chess, &tables, &mut tt, rt, &rx);
            // let mut search_engine = v4_pv::Search::new(params, chess, &tables, &rx);
            // let mut search_engine = v3_itdep::IterativeDeepening::new(params, chess, &tables, &rx);

            let (bestmove, delta) = {
                let start = rdtsc();
                let mv = std::hint::black_box(search_engine.search());
                let end = rdtsc();

                (mv, end - start)
            };
            println!("Best move: {}", util::move_string(bestmove));
            println!("Nodes searched: {}", search_engine.num_nodes_searched());
            println!(
                "PV: {:?}",
                search_engine
                    .get_pv()
                    .iter()
                    .map(|mv| util::move_string(*mv))
                    .collect::<Vec<_>>()
            );
            println!("Search time: {} cycles", delta);

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
