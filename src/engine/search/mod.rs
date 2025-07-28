use crate::engine::{chess, search::search_params::SearchParams, tables};

pub struct SigAbort {}

pub type AbortSignal = crossbeam::channel::Receiver<SigAbort>;

pub trait SearchStrategy<'a> {
    fn new(
        params: SearchParams,
        chess: chess::ChessGame,
        tables: &'a tables::Tables,
        sig: &'a AbortSignal,
    ) -> Self;
    fn search(&mut self) -> u16;
    fn num_nodes_searched(&self) -> u64;
    fn search_score(&self) -> i32;
}

pub mod search_params;
pub mod transposition;
pub mod v1_negamax;
pub mod v2_alphabeta;
pub mod v3_itdep;
pub mod v4_pv;
pub mod v5_tt;
