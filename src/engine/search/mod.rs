pub struct SigAbort {}

pub type AbortSignal = crossbeam::channel::Receiver<SigAbort>;

pub trait SearchStrategy<'a> {
    fn search(&mut self) -> u16;
    fn num_nodes_searched(&self) -> u64;
    fn search_score(&self) -> i32;
}

pub mod repetition;
pub mod repetition_v2;
pub mod search_params;
pub mod sorting;
pub mod transposition;
pub mod transposition_v2;

pub mod v10_mvcache;
pub mod v11_opt;
pub mod v12_eval;
pub mod v1_negamax;
pub mod v2_alphabeta;
pub mod v3_itdep;
pub mod v4_pv;
pub mod v5_tt;
pub mod v6_psquare;
pub mod v7_mvvlva;
pub mod v8_quiesc;
pub mod v9_prune;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        engine::{chess, chess_v2, search::search_params::SearchParams, tables},
        util,
    };

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
            let tt = std::cell::SyncUnsafeCell::new(transposition_v2::TranspositionTable::new(4));

            let mut rt = repetition_v2::RepetitionTable::new();

            let mut chess = chess_v2::ChessGame::new();
            assert!(
                chess
                    .load_fen(std::hint::black_box(test_fen), &tables)
                    .is_ok()
            );

            let mut params = SearchParams::new();
            params.depth = Some(std::hint::black_box(10));

            let mut search_engine =
                v12_eval::Search::new(params, chess, &tables, unsafe { &mut *tt.get() }, rt, &rx);

            // let tx = tx.clone();
            // std::thread::spawn(move || {
            //     std::thread::sleep(std::time::Duration::from_millis(100));
            //     tx.send(SigAbort {}).unwrap();
            // });

            let (bestmove, delta) = {
                let start = rdtsc();

                let mv = std::hint::black_box(search_engine.search());
                // for i in 0..1_000_000_000 {
                //     let eval = search_engine.evaluate();
                //     std::hint::black_box(eval);
                // }
                // let mv = 0;

                let end = rdtsc();
                (mv, end - start)
            };

            let tt_stats = unsafe { &mut *tt.get() }.calc_stats();

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
            println!(
                "TT usage: {:.02}%",
                ((tt_stats.1 + tt_stats.2 + tt_stats.3) as f64) / (tt_stats.0 as f64) * 100.0
            );
            println!("TT entries: {}", tt_stats.0);
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
