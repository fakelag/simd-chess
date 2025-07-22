use crate::engine::{chess, search::search_params::SearchParams, tables};

pub trait Search<'a> {
    fn new(
        params: SearchParams,
        chess: &'a mut chess::ChessGame,
        tables: &'a tables::Tables,
    ) -> Self;
    fn search(&mut self) -> u16;
    fn num_nodes_searched(&self) -> u64;
}

pub mod search_params;
pub mod v1_negamax;
