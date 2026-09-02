use crate::{
    engine::{
        search::{EngineForm, SearchStrategy, repetition, search, timeman, transposition},
        tables,
    },
    util,
};

fn rdtsc() -> u64 {
    unsafe { std::arch::x86_64::_rdtsc() }
}

pub fn benchmark(tt_size_mb: usize, warmup: bool) {
    let tables = tables::Tables::new();

    core_affinity::set_for_current(core_affinity::CoreId { id: 2 });

    const ITERATIONS: usize = 5;

    let results = [
        ("startpos", util::FEN_STARTPOS, 19),
        (
            "kiwipete",
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            17,
        ),
        (
            "pawn_endgame",
            "8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w - - 0 1",
            19,
        ),
        (
            "queen_endgame",
            "8/3PPP2/4K3/8/P2qN3/3k4/3N4/1q6 w - - 0 1",
            14,
        ),
    ]
    .into_iter()
    .map(|(name, test_fen, depth)| {
        let bench = || {
            let rt = repetition::RepetitionTable::new();

            let depth = Some(std::hint::black_box(depth));

            let tt =
                std::cell::SyncUnsafeCell::new(transposition::TranspositionTable::new(tt_size_mb));
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

            (
                delta,
                search_engine.num_nodes_searched(),
                search_engine
                    .get_pv()
                    .iter()
                    .map(|mv| util::move_string(*mv))
                    .collect::<Vec<_>>(),
            )
        };

        let mut total_cycles = 0;
        let mut min_cycles = u64::MAX;
        let mut nodes = 0;
        let mut pv = Vec::new();

        println!("Benchmarking {} at depth {}", name, depth);

        if warmup {
            let _ = bench();
        }

        for _ in 0..ITERATIONS {
            let result = bench();

            min_cycles = min_cycles.min(result.0);
            total_cycles += result.0;
            nodes = result.1;
            pv = result.2;
        }

        (
            name,
            depth,
            min_cycles,
            total_cycles / ITERATIONS as u64,
            nodes,
            pv,
        )
    })
    .collect::<Vec<_>>();

    results
            .iter()
            .for_each(|(name, depth, min_cycles, avg_cycles, nodes, pv)| {
                std::hint::black_box(pv);
                println!(
                    "[{:<13}] {:>2} iterations {:>6} avg Mcycles, {:>6} min Mcycles, {:>10} nodes, {:>2} depth",
                    name,
                    ITERATIONS,
                    avg_cycles / 1_000_000,
                    min_cycles / 1_000_000,
                    nodes,
                    depth
                );
            });
}
