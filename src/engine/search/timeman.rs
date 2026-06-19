use std::time::Instant;

use crate::engine::search::eval::{Eval, SCORE_INF, is_mate};
use crate::engine::search::search_params::SearchParams;

const MOVE_OVERHEAD_MS: u64 = 10;

const MATE_TIME_MANAGEMENT: bool = true;
const MATE_VERIFY_MARGIN: u32 = 2;
const MATE_VERIFY_STABLE_ITERS: u32 = 1;
const MATE_SOFT_SCALE: f64 = 0.60;

const SUDDEN_DEATH_DIV: u64 = 20;
const INC_NUM: u64 = 1;
const INC_DEN: u64 = 2;

const HARD_MULT: f64 = 4.0;
const HARD_CAP_NUM: u64 = 3;
const HARD_CAP_DEN: u64 = 4;

const MIN_DYNAMIC_DEPTH: u8 = 4;

const STAB_BASE: f64 = 1.30;
const STAB_STEP: f64 = 0.06;
const STAB_CAP: u32 = 9;
const STAB_MIN: f64 = 0.70;

const EFFORT_BASE: f64 = 1.30;
const EFFORT_SLOPE: f64 = 0.95;
const EFFORT_MIN: f64 = 0.55;
const EFFORT_MAX: f64 = 1.30;

const EVAL_SLOPE: f64 = 0.0040;
const EVAL_MIN: f64 = 0.85;
const EVAL_MAX: f64 = 1.25;

#[derive(Debug, Clone, Copy)]
pub struct TimeLimits {
    pub soft_ms: u64,
    pub hard_ms: u64,
    pub dynamic: bool,
}

pub struct IterationReport {
    pub depth: u8,
    pub score: Eval,
    pub best_move: u16,
    pub best_move_nodes: u64,
    pub iter_nodes: u64,
}

pub struct TimeManager {
    start: Instant,
    limits: TimeLimits,
    time_managed: bool,
    prev_best_move: u16,
    stability: u32,
    stopped: bool,
    nodes: u64,
    prev_score: Eval,
}

pub fn compute_limits(p: &SearchParams, b_move: bool) -> Option<TimeLimits> {
    if p.infinite {
        return None;
    }

    if let Some(mt) = p.movetime {
        let ms = (mt as u64).saturating_sub(MOVE_OVERHEAD_MS).max(1);
        return Some(TimeLimits {
            soft_ms: ms,
            hard_ms: ms,
            dynamic: false,
        });
    }

    let timeleft_ms = match if b_move { p.btime } else { p.wtime } {
        Some(t) => (t as u64).saturating_sub(MOVE_OVERHEAD_MS).max(1),
        None => return None,
    };

    let inc_ms = (if b_move { p.binc } else { p.winc }).unwrap_or(0) as u64;

    let mtg: u64 = match p.movestogo {
        Some(m) => (m as u64).max(1),
        None => SUDDEN_DEATH_DIV,
    };

    let cap_ms = (timeleft_ms * HARD_CAP_NUM / HARD_CAP_DEN).max(1);

    let soft_ms = timeleft_ms / mtg + inc_ms * INC_NUM / INC_DEN;
    let hard_ms = ((soft_ms as f64 * HARD_MULT) as u64).min(cap_ms).max(1);

    Some(TimeLimits {
        soft_ms: soft_ms.min(hard_ms).max(1),
        hard_ms,
        dynamic: true,
    })
}

impl TimeManager {
    pub fn new() -> Self {
        TimeManager {
            start: Instant::now(),
            limits: TimeLimits {
                soft_ms: 0,
                hard_ms: 0,
                dynamic: false,
            },
            stopped: false,
            time_managed: false,
            prev_best_move: 0,
            stability: 0,
            prev_score: 0,
            nodes: 0,
        }
    }

    pub fn disable(&mut self) {
        self.time_managed = false;
        self.stopped = false;
        self.prev_best_move = 0;
        self.stability = 0;
        self.prev_score = 0;
        self.start = Instant::now();
        self.nodes = 0;
    }

    pub fn enable(&mut self, limits: TimeLimits, start: Instant) {
        self.limits = limits;
        self.time_managed = true;
        self.stopped = false;
        self.prev_best_move = 0;
        self.stability = 0;
        self.prev_score = 0;
        self.nodes = 0;
        self.start = start;
    }

    pub fn set_nodes(&mut self, nodes: u64) {
        self.nodes = nodes;
    }

    pub fn stop(&mut self) {
        self.stopped = true;
    }

    #[inline(always)]
    pub fn check_abort(&self, nodes: u64) -> bool {
        if self.stopped {
            return true;
        }

        if self.nodes > 0 && nodes >= self.nodes {
            return true;
        }

        if !self.time_managed {
            return false;
        }

        self.start.elapsed().as_millis() as u64 >= self.limits.hard_ms
    }

    #[inline(always)]
    pub fn should_stop(&mut self, r: &IterationReport) -> bool {
        if self.stopped {
            return true;
        }

        if !self.time_managed {
            return false;
        }

        if r.best_move == self.prev_best_move {
            self.stability += 1;
        } else {
            self.stability = 0;
            self.prev_best_move = r.best_move;
        }

        if MATE_TIME_MANAGEMENT
            && self.limits.dynamic
            && is_mate(r.score)
            && (SCORE_INF - r.score.abs()) as u32 + MATE_VERIFY_MARGIN <= r.depth as u32
            && self.stability >= MATE_VERIFY_STABLE_ITERS
        {
            return true;
        }

        let scale = if self.limits.dynamic && r.depth >= MIN_DYNAMIC_DEPTH {
            let s = self.f_stability() * Self::f_effort(r) * self.f_eval(r.score);
            if MATE_TIME_MANAGEMENT {
                s * Self::f_mate(r)
            } else {
                s
            }
        } else {
            1.0
        };

        self.prev_score = r.score;

        let scaled = (((self.limits.soft_ms as f64) * scale) as u64)
            .min(self.limits.hard_ms)
            .max(1);

        self.start.elapsed().as_millis() as u64 >= scaled
    }

    fn f_stability(&self) -> f64 {
        (STAB_BASE - STAB_STEP * self.stability.min(STAB_CAP) as f64).clamp(STAB_MIN, STAB_BASE)
    }

    fn f_effort(r: &IterationReport) -> f64 {
        if r.iter_nodes == 0 {
            return 1.0;
        }
        let effort = (r.best_move_nodes as f64 / r.iter_nodes as f64).clamp(0.0, 1.0);
        (EFFORT_BASE - EFFORT_SLOPE * effort).clamp(EFFORT_MIN, EFFORT_MAX)
    }

    // Only mates not yet horizon-resolved reach here; resolved ones early-return in should_stop.
    fn f_mate(r: &IterationReport) -> f64 {
        if is_mate(r.score) {
            MATE_SOFT_SCALE
        } else {
            1.0
        }
    }

    fn f_eval(&self, score: Eval) -> f64 {
        if is_mate(score) || is_mate(self.prev_score) {
            return 1.0;
        }
        let drop = (self.prev_score - score) as f64;
        (1.0 + drop * EVAL_SLOPE).clamp(EVAL_MIN, EVAL_MAX)
    }
}
