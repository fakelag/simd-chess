#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundType {
    Empty = 0,
    /// Exact score, entry was stored with a full evaluation of the
    /// position which raised over α and did not get β-cut. This score can
    /// be used as an exact score for the position.
    Exact = 1,
    /// Lower bound, when this entry was stored it wasn't fully evaluated
    /// due to a β-cutoff axing the search before the position had been fully
    /// evaluated. The score contained is guaranteed to be less than or equal
    /// to the actual score of the position, e.g the position is at least this good.
    LowerBound = 2,
    /// Upper bound, when the entry was stored it did not raise over α meaning that
    /// a better move was already found. This is a "fail-low" node
    UpperBound = 3,
}

type TtEval = i16;

const COLLECT_STATS: bool = false;
const TT_EVICT_GENERATIONS: u8 = 1;

pub struct TtStats {
    pub probe_hit: usize,
    pub probe_miss: usize,
    pub store_hit: usize,
    pub store_miss: usize,
    pub fill_percentage: f64,
    pub collisions: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct TranspositionEntry {
    hash_part_0: u16,
    hash_part_1: u8,
    hash_part_2: u8,
    score: i16,
    depth_and_type: u8,
    mv: u16,
    generation: u8,
}

impl TranspositionEntry {
    pub fn new() -> Self {
        TranspositionEntry {
            hash_part_0: 0,
            hash_part_1: 0,
            hash_part_2: 0,
            score: 0,
            depth_and_type: 0,
            mv: 0,
            generation: 0,
        }
    }

    #[inline(always)]
    pub fn depth(&self) -> u8 {
        self.depth_and_type >> 2
    }

    #[inline(always)]
    pub fn bound_type(&self) -> BoundType {
        unsafe { std::mem::transmute(self.depth_and_type & 0b11) }
    }

    #[inline(always)]
    pub fn set_depth_and_type(&mut self, depth: u8, bt: BoundType) {
        self.depth_and_type = (depth.min(63) << 2) | ((bt as u8) & 0b11);
    }
}

pub struct TranspositionTable {
    entries: Box<[TranspositionEntry]>,
    generation: u8,
    min_generation: u8,
    table_size_mask: usize,
    index_bits: u32,
    hash64: Box<[u64]>,
    stats: Box<TtStats>,
}

// Safety: TranspositionTable must implement thread safety by itself
unsafe impl Send for TranspositionTable {}
unsafe impl Sync for TranspositionTable {}

impl TranspositionTable {
    pub fn new(size_hint_mb: usize) -> Self {
        const ENTRY_SIZE: usize = std::mem::size_of::<TranspositionEntry>();

        let max_entries = size_hint_mb * 1024 * 1024 / ENTRY_SIZE;
        let table_size = max_entries.next_power_of_two();

        let table_mask = table_size - 1;
        let index_bits = table_mask.count_ones();

        TranspositionTable {
            table_size_mask: table_mask,
            entries: vec![TranspositionEntry::new(); table_size].into_boxed_slice(),
            hash64: vec![0u64; table_size].into_boxed_slice(),
            index_bits,
            generation: TT_EVICT_GENERATIONS.wrapping_sub(1),
            min_generation: 0,
            stats: Box::new(TtStats {
                probe_hit: 0,
                probe_miss: 0,
                store_hit: 0,
                store_miss: 0,
                fill_percentage: 0.0,
                collisions: 0,
            }),
        }
    }

    #[inline(always)]
    pub fn new_search(&mut self) {
        self.generation = self.generation.wrapping_add(1);
        self.min_generation = self.generation.wrapping_sub(TT_EVICT_GENERATIONS);

        if self.generation == 0 {
            self.generation = TT_EVICT_GENERATIONS;
            self.min_generation = 0;

            for entry in &mut self.entries {
                entry.generation = entry
                    .generation
                    .saturating_sub(u8::MAX - TT_EVICT_GENERATIONS);
            }
        }
    }

    #[inline(always)]
    pub fn probe(
        &mut self,
        hash: u64,
        depth: u8,
        alpha: TtEval,
        beta: TtEval,
    ) -> (Option<TtEval>, u16) {
        let entry_index = (hash as usize) & self.table_size_mask;

        // Safety: entry_index is guaranteed to be within bounds due to table_size_mask
        unsafe { std::hint::assert_unchecked(entry_index < self.entries.len()) };

        let entry = &mut self.entries[entry_index];

        let mut can_use_entry = 1u8;

        let part = Self::get_hash_part(self.index_bits, hash);
        let p0 = part as u16;
        let p1 = (part >> 16) as u8;
        let p2 = (part >> 24) as u8;

        can_use_entry &= ((entry.hash_part_0 == p0) as u8)
            & ((entry.hash_part_1 == p1) as u8)
            & ((entry.hash_part_2 == p2) as u8);

        can_use_entry &= (entry.depth() >= depth) as u8;

        let condition = [
            0u8,
            1u8,
            (entry.score >= beta) as u8,
            (entry.score <= alpha) as u8,
        ];

        can_use_entry &= condition[entry.bound_type() as usize];

        if COLLECT_STATS {
            if can_use_entry != 0 {
                self.stats.probe_hit += 1;

                let hash64 = self.hash64[entry_index];
                if hash64 != 0 && hash64 != hash {
                    println!(
                        "TT collision detected: existing: {:016x}, new: {:016x}",
                        hash64, hash
                    );
                    self.stats.collisions += 1;
                }
            } else {
                self.stats.probe_miss += 1;
            }
        }

        if can_use_entry == 0 {
            return (None, entry.mv);
        }

        entry.generation = self.generation;

        (Some(entry.score), entry.mv)
    }

    #[inline(always)]
    pub fn store(&mut self, hash: u64, score: TtEval, depth: u8, mv: u16, bound_type: BoundType) {
        let entry_index = (hash as usize) & self.table_size_mask;

        // Safety: entry_index is guaranteed to be within bounds due to table_size_mask
        unsafe { std::hint::assert_unchecked(entry_index < self.entries.len()) };

        let entry = &mut self.entries[entry_index];

        let replace = if entry.bound_type() == BoundType::Empty {
            true
        } else {
            entry.depth() <= depth || entry.generation < self.min_generation
        };

        if replace {
            let tag_parts = Self::get_hash_part(self.index_bits, hash);
            let t0 = tag_parts as u16;
            let t1 = (tag_parts >> 16) as u8;
            let t2 = (tag_parts >> 24) as u8;

            entry.hash_part_0 = t0;
            entry.hash_part_1 = t1;
            entry.hash_part_2 = t2;
            entry.score = score;
            entry.mv = mv;
            entry.generation = self.generation;
            entry.set_depth_and_type(depth, bound_type);

            if COLLECT_STATS {
                self.hash64[entry_index] = hash;
            }
        }

        if COLLECT_STATS {
            if replace {
                self.stats.store_hit += 1;
            } else {
                self.stats.store_miss += 1;
            }
        }
    }

    #[inline(always)]
    pub fn get_hash_part(index_bits: u32, hash: u64) -> u32 {
        let hash_bits = hash >> index_bits;
        hash_bits as u32
    }

    pub fn clear(&mut self) {
        self.generation = TT_EVICT_GENERATIONS.wrapping_sub(1);
        self.min_generation = 0;
        for entry in &mut self.entries {
            entry.depth_and_type = 0;
            entry.generation = 0;
            entry.mv = 0;
        }

        if COLLECT_STATS {
            self.stats.collisions = 0;
            self.stats.fill_percentage = 0.0;
            self.stats.probe_hit = 0;
            self.stats.probe_miss = 0;
            self.stats.store_hit = 0;
            self.stats.store_miss = 0;
            for h in &mut self.hash64 {
                *h = 0;
            }
        }
    }

    pub fn calc_stats(&self) -> TtStats {
        if !COLLECT_STATS {
            return TtStats {
                fill_percentage: 0.0,
                probe_hit: 0,
                probe_miss: 0,
                store_hit: 0,
                store_miss: 0,
                collisions: 0,
            };
        }

        let mut num_filled = 0;

        for entry in &self.entries {
            num_filled += (entry.bound_type() != BoundType::Empty) as usize;
        }

        TtStats {
            fill_percentage: num_filled as f64 / self.entries.len() as f64,
            probe_hit: self.stats.probe_hit,
            probe_miss: self.stats.probe_miss,
            store_hit: self.stats.store_hit,
            store_miss: self.stats.store_miss,
            collisions: self.stats.collisions,
        }
    }
}
