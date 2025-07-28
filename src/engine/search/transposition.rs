#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum BoundType {
    /// Exact score, entry was stored with a full evaluation of the
    /// position which raised over α and did not get β-cut. This score can
    /// be used as an exact score for the position.
    Exact = 0,
    /// Lower bound, when this entry was stored it wasn't fully evaluated
    /// due to a β-cutoff axing the search before the position had been fully
    /// evaluated. The score contained is guaranteed to be less than or equal
    /// to the actual score of the position, e.g the position is at least this good.
    LowerBound,
    /// Upper bound, when the entry was stored it did not raise over α meaning that
    /// a better move was already found. This is a "fail-low" node
    UpperBound,
}

#[derive(Debug, Clone, Copy)]
pub struct TranspositionEntry {
    // @todo - Store best_move for move ordering
    hash: u64,
    score: i32,
    depth: u8,
    flags: BoundType,
}

impl TranspositionEntry {
    pub fn new() -> Self {
        TranspositionEntry {
            hash: 0,
            score: 0,
            depth: 0,
            flags: BoundType::Exact,
        }
    }
}

pub struct TranspositionTable {
    entries: Box<[TranspositionEntry]>,
}

impl TranspositionTable {
    pub fn new(size_hint_mb: usize) -> Self {
        const ENTRY_SIZE: usize = std::mem::size_of::<TranspositionEntry>();

        let max_entries = size_hint_mb * 1024 * 1024 / ENTRY_SIZE;
        let table_size = max_entries.next_power_of_two();

        TranspositionTable {
            entries: vec![TranspositionEntry::new(); table_size].into_boxed_slice(),
        }
    }

    pub fn probe(&self, hash: u64, depth: u8, alpha: i32, beta: i32) -> Option<i32> {
        let entry_index = (hash as usize) & (self.entries.len() - 1);
        let entry = &self.entries[entry_index];

        if entry.hash != hash || entry.depth < depth {
            return None;
        }

        let score = match entry.flags {
            BoundType::Exact => Some(entry.score),
            BoundType::LowerBound if entry.score >= beta => Some(entry.score),
            BoundType::UpperBound if entry.score <= alpha => Some(entry.score),
            _ => None,
        };

        score
    }

    pub fn store(&mut self, hash: u64, score: i32, depth: u8, flags: BoundType) {
        let entry_index = (hash as usize) & (self.entries.len() - 1);
        let entry = &mut self.entries[entry_index];

        // Replace if the new entry is from a deeper search
        if entry.depth <= depth {
            entry.hash = hash;
            entry.score = score;
            entry.depth = depth;
            entry.flags = flags;
        }
    }

    pub fn calc_stats(&self) -> (usize, usize, usize, usize) {
        let mut exact = 0;
        let mut lower_bound = 0;
        let mut upper_bound = 0;

        for entry in &self.entries {
            if entry.hash != 0 {
                match entry.flags {
                    BoundType::Exact => exact += 1,
                    BoundType::LowerBound => lower_bound += 1,
                    BoundType::UpperBound => upper_bound += 1,
                }
            }
        }

        (self.entries.len(), exact, lower_bound, upper_bound)
    }
}
