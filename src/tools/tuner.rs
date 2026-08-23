use std::cell::SyncUnsafeCell;

use crate::engine::chess_v2::ChessGame;
use crate::engine::search::search::Search;
use crate::engine::search::{
    EngineForm, SearchStrategy, repetition, stability, timeman, transposition,
};
use crate::engine::tables;

pub fn instability_distribution(
    source: impl Iterator<Item = ChessGame>,
    tt_size_mb: usize,
    depth: u8,
    tables: &tables::Tables,
) -> Vec<f64> {
    let tt = SyncUnsafeCell::new(transposition::TranspositionTable::new(tt_size_mb));
    let mut tm = SyncUnsafeCell::new(timeman::TimeManager::new());
    tm.get_mut().disable();
    let mut search = Search::<{ EngineForm::TacticalB }>::new(
        tables,
        &tt,
        &tm,
        repetition::RepetitionTable::new(),
    );

    let mut out = Vec::new();
    for board in source {
        search.new_game();
        search.load_from_board(&board);
        search.new_search();
        search.search(Some(depth));

        if let Some(inst) = stability::winprob_instability(search.depth_stats()) {
            out.push(inst);
        }
    }
    out
}

pub fn quantile_sorted(sorted: &[f64], q: f64) -> f64 {
    assert!(!sorted.is_empty(), "quantile of an empty distribution");

    let pos = q.clamp(0.0, 1.0) * (sorted.len() - 1) as f64;
    let lo = pos.floor() as usize;
    let hi = pos.ceil() as usize;
    if lo == hi {
        return sorted[lo];
    }
    let frac = pos - lo as f64;
    sorted[lo] * (1.0 - frac) + sorted[hi] * frac
}

pub fn tune_thresholds(
    source: impl Iterator<Item = ChessGame>,
    tt_size_mb: usize,
    depth: u8,
    tables: &tables::Tables,
    quantiles: &[f64],
) -> Vec<f64> {
    let mut dist = instability_distribution(source, tt_size_mb, depth, tables);
    sort_ascending(&mut dist);
    quantiles
        .iter()
        .map(|&q| quantile_sorted(&dist, q))
        .collect()
}

pub fn top_percent_cut(sorted: &[f64], top_percent: f64) -> f64 {
    assert!(
        (0.0..=100.0).contains(&top_percent),
        "top_percent is a percentage in [0, 100], got {top_percent}"
    );
    quantile_sorted(sorted, 1.0 - top_percent / 100.0)
}

pub fn sort_ascending(dist: &mut [f64]) {
    dist.sort_unstable_by(|a, b| {
        a.partial_cmp(b)
            .expect("winprob_instability is finite; NaN means an upstream bug")
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quantile_sorted_matches_numpy_linear() {
        let d = [1.0, 2.0, 3.0, 4.0];
        assert_eq!(quantile_sorted(&d, 0.0), 1.0);
        assert_eq!(quantile_sorted(&d, 1.0), 4.0);
        assert_eq!(quantile_sorted(&d, 0.5), 2.5);
        assert!((quantile_sorted(&d, 0.25) - 1.75).abs() < 1e-12);
        assert!((quantile_sorted(&d, 0.75) - 3.25).abs() < 1e-12);
    }

    #[test]
    fn test_quantile_sorted_degenerate() {
        assert_eq!(quantile_sorted(&[7.0], 0.0), 7.0);
        assert_eq!(quantile_sorted(&[7.0], 0.5), 7.0);
        assert_eq!(quantile_sorted(&[7.0], 1.0), 7.0);
    }

    #[test]
    fn test_top_percent_cut_selects_the_sharp_end() {
        let d: Vec<f64> = (0..=100).map(|i| i as f64).collect();

        assert!((top_percent_cut(&d, 10.0) - 90.0).abs() < 1e-9);
        assert!((top_percent_cut(&d, 100.0) - 0.0).abs() < 1e-9);
        assert!((top_percent_cut(&d, 0.0) - 100.0).abs() < 1e-9);

        let cut = top_percent_cut(&d, 25.0);
        let kept = d.iter().filter(|&&x| x >= cut).count();
        assert!((kept as i32 - 26).abs() <= 1, "kept {kept} of 101");
    }
}
