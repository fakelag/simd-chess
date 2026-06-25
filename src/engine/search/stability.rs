use crate::engine::search::search::DepthStat;
use crate::nnue::nnue::QS;

pub(crate) const SHARPNESS_SCALE: [f64; 11] = [
    0.013522, 0.013522, 0.013522, 0.016333, 0.016970, 0.017695, 0.017948, 0.017466, 0.016996,
    0.017052, 0.016508,
];

pub(crate) fn win_prob(score_cp: i32) -> f64 {
    1.0 / (1.0 + (-(score_cp as f64) / QS as f64).exp())
}

pub(crate) fn winprob_instability(stats: &[DepthStat]) -> Option<f64> {
    if stats.len() < 2 {
        return None;
    }
    let (mut num, mut den) = (0.0f64, 0.0f64);
    for w in stats.windows(2) {
        let weight = w[1].depth as f64;
        num += weight * (win_prob(w[1].score) - win_prob(w[0].score)).abs();
        den += weight;
    }
    Some(num / den)
}

pub(crate) fn calc_sharpness(stats: &[DepthStat]) -> Option<f64> {
    let inst = winprob_instability(stats)?;
    let scale = SHARPNESS_SCALE[stats.len().min(SHARPNESS_SCALE.len() - 1)];
    Some(1.0 - (-inst / scale).exp())
}
