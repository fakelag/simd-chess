#[derive(Debug, Clone, Copy)]
pub enum BoundType {
    /// Exact score, entry was stored with a full evaluation of the
    /// position which raised over α and did not get β-cut. This score can
    /// be used as an exact score for the position.
    Exact = 0,
    /// Lower bound, when this entry was stored it wasn't fully evaluated
    /// due to a β-cutoff axing the search before the position had been fully
    /// evaluated. The score contained is guaranteed to be less than or equal
    /// to the actual score of the position, e.g the position is at least this good.
    LowerBound = 1,
    /// Upper bound, when the entry was stored it did not raise over α meaning that
    /// a better move was already found. This is a "fail-low" node
    UpperBound = 2,
}

type TtEval = i16;

#[derive(Debug, Clone, Copy)]
pub struct TranspositionEntry {
    hash: u64,
    score: i16,
    depth: u8,
    mv: u16,
    flags: BoundType,
}

impl TranspositionEntry {
    pub fn new() -> Self {
        TranspositionEntry {
            hash: 0,
            score: 0,
            depth: 0,
            mv: 0,
            flags: BoundType::Exact,
        }
    }
}

pub struct TranspositionTable {
    entries: Box<[TranspositionEntry]>,
    table_size_mask: usize,
}

// Safety: TranspositionTable must implement thread safety by itself
unsafe impl Send for TranspositionTable {}
unsafe impl Sync for TranspositionTable {}

impl TranspositionTable {
    pub fn new(size_hint_mb: usize) -> Self {
        const ENTRY_SIZE: usize = std::mem::size_of::<TranspositionEntry>();

        let max_entries = size_hint_mb * 1024 * 1024 / ENTRY_SIZE;
        let table_size = max_entries.next_power_of_two();

        TranspositionTable {
            table_size_mask: table_size - 1,
            entries: vec![TranspositionEntry::new(); table_size].into_boxed_slice(),
        }
    }

    #[inline(always)]
    pub fn probe(
        &self,
        hash: u64,
        depth: u8,
        alpha: TtEval,
        beta: TtEval,
    ) -> (Option<TtEval>, u16) {
        let entry_index = (hash as usize) & self.table_size_mask;

        // Safety: entry_index is guaranteed to be within bounds due to table_size_mask
        unsafe { std::hint::assert_unchecked(entry_index < self.entries.len()) };

        let entry = &self.entries[entry_index];

        let mut can_use_entry = 1u8;

        can_use_entry &= (entry.hash == hash) as u8;
        can_use_entry &= (entry.depth >= depth) as u8;

        let condition = [
            1u8,
            (entry.score >= beta) as u8,
            (entry.score <= alpha) as u8,
        ];

        can_use_entry &= condition[entry.flags as usize];

        if can_use_entry == 0 {
            return (None, entry.mv);
        }

        (Some(entry.score), entry.mv)
    }

    #[inline(always)]
    pub fn store(&mut self, hash: u64, score: TtEval, depth: u8, mv: u16, flags: BoundType) {
        let entry_index = (hash as usize) & self.table_size_mask;

        // Safety: entry_index is guaranteed to be within bounds due to table_size_mask
        unsafe { std::hint::assert_unchecked(entry_index < self.entries.len()) };

        let entry = &mut self.entries[entry_index];

        // Replace if the new entry is from a deeper search
        if entry.depth <= depth {
            entry.hash = hash;
            entry.score = score;
            entry.depth = depth;
            entry.flags = flags;
            entry.mv = mv;
        }
    }

    pub fn clear(&mut self) {
        for entry in &mut self.entries {
            *entry = TranspositionEntry::new();
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
