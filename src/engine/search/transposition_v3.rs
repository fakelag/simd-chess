use std::pin::Pin;

use crate::engine::search::eval::Eval;

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

const DEBUG: bool = false;
// const TT_EVICT_GENERATIONS: u8 = 1;

pub struct TtStats {
    pub probe_hit: usize,
    pub probe_miss: usize,
    pub store_hit: usize,
    pub store_miss: usize,
    pub fill_percentage: f64,
    pub collisions: usize,
}

#[derive(Debug)]
pub struct TtProbe {
    pub score: Option<Eval>,
    pub eval: Eval,
    pub mv_index: u8,
}

#[derive(Debug, Copy, Clone)]
pub struct TtKey {
    ptr: *mut TtEntry,
}

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
struct TtEntry {
    keygen: u32,
    eval: Eval,
    score: Eval,
    depthtype: u8,
    mv_index: u8,
}

impl TtEntry {
    pub fn new() -> Self {
        TtEntry {
            keygen: 0,
            eval: 0,
            score: 0,
            depthtype: 0,
            mv_index: 0xFF,
        }
    }

    #[inline(always)]
    pub fn generation(&self) -> u8 {
        (self.keygen >> 30) as u8
    }

    #[inline(always)]
    pub fn set_generation(&mut self, generation: u8) {
        self.keygen = (self.keygen & 0x3FFF_FFFF) | ((generation as u32) << 30);
    }

    #[inline(always)]
    pub fn set_key(&mut self, hash: u64) {
        self.keygen = (self.keygen & 0xC000_0000) | ((hash as u32) & 0x3FFF_FFFF);
    }

    #[inline(always)]
    pub fn key(&self) -> u32 {
        (self.keygen & 0x3FFF_FFFF) as u32
    }

    #[inline(always)]
    pub fn depth(&self) -> u8 {
        self.depthtype >> 2
    }

    #[inline(always)]
    pub fn bound_type(&self) -> BoundType {
        unsafe { std::mem::transmute(self.depthtype & 0b11) }
    }

    #[inline(always)]
    pub fn set_depth_and_type(&mut self, depth: u8, bt: BoundType) {
        self.depthtype = (depth.min(63) << 2) | ((bt as u8) & 0b11);
    }

    #[inline(always)]
    pub fn should_replace(&self, depth: u8, evict_generation: u8) -> bool {
        self.depth() < depth || self.generation() == evict_generation
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(align(32))]
pub struct TtBucket([TtEntry; 3]);

pub struct TranspositionTable {
    entries: Pin<Box<[TtBucket]>>,
    generation: u8,
    evict_generation: u8,
    table_size_mask: usize,
    index_bits: u32,
    num_buckets: usize,

    hash64: std::collections::BTreeMap<*const TtEntry, u64>,
    stats: Box<TtStats>,
}

// Safety: TranspositionTable must implement thread safety by itself
unsafe impl Send for TranspositionTable {}
unsafe impl Sync for TranspositionTable {}

impl TranspositionTable {
    pub fn new(size_hint_mb: usize) -> Self {
        const BUCKET_SIZE: usize = std::mem::size_of::<TtBucket>();

        let num_buckets = size_hint_mb * 1024 * 1024 / BUCKET_SIZE;
        let table_size = num_buckets.next_power_of_two();

        let table_mask = table_size - 1;
        let index_bits = table_mask.count_ones();

        let entries = vec![TtBucket([TtEntry::new(); 3]); table_size].into_boxed_slice();

        TranspositionTable {
            table_size_mask: table_mask,
            entries: Pin::from(entries),
            hash64: std::collections::BTreeMap::new(),
            index_bits,
            num_buckets: table_size,
            generation: 0,
            evict_generation: 0,
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
        self.generation = (self.generation + 1) & 0b11;
        self.evict_generation = (self.generation + 1) & 0b11;

        // 0 -> E: 1
        // 1 -> E: 2
        // 2 -> E: 3
        // 3 -> E: 0
        // 0 ->
    }

    #[inline(always)]
    pub fn probe(
        &mut self,
        chess: &crate::engine::chess_v2::ChessGame,
        hash: u64,
        depth: u8,
        alpha: Eval,
        beta: Eval,
    ) -> (Option<TtKey>, Option<TtProbe>) {
        let generation = self.generation;
        let evict_generation = self.evict_generation;

        // let logdbg = hash == 0x00A760E2A3316CAD4D;

        let bucket_index = ((hash as u128) * (self.num_buckets as u128)) >> 64;

        let key = (hash as u32) & 0x3FFF_FFFF;

        for entry in &mut self.entries[bucket_index as usize].0 {
            if entry.key() != key || entry.bound_type() == BoundType::Empty {
                // if logdbg {
                //     println!(
                //         "TT Probe MISS for hash {:018X} ({}). EKey={:#x},EBType={:?}, EDepth={},EGen={}.Depth={}",
                //         hash,
                //         bucket_index,
                //         entry.key(),
                //         entry.bound_type(),
                //         entry.depth(),
                //         entry.generation(),
                //         depth
                //     );
                // }
                continue;
            }

            let bound_condition = [false, true, entry.score >= beta, entry.score <= alpha];

            let usable = entry.depth() >= depth && bound_condition[entry.bound_type() as usize];

            if usable {
                entry.set_generation(generation);
            }

            if DEBUG {
                let hash_debug = *self.hash64.get(&(entry as *const TtEntry)).unwrap();

                if hash != hash_debug {
                    panic!(
                        "TT Probe Hit Collision: Hash {:016X} ({:016X} in the slot), Depth {}, Type {:?}, Gen {}, HM: {}",
                        hash,
                        hash_debug,
                        entry.depth(),
                        entry.bound_type(),
                        entry.generation(),
                        chess.half_moves(),
                    );
                }
            }

            // if logdbg {
            //     println!(
            //         "V3 TT Probe Hit for hash {:018X}. Depth={},EDepth={},EGen={}",
            //         hash,
            //         depth,
            //         entry.depth(),
            //         entry.generation()
            //     );
            // }

            return (
                if entry.should_replace(depth, evict_generation) {
                    Some(TtKey {
                        ptr: entry as *mut TtEntry,
                    })
                } else {
                    None
                },
                Some(TtProbe {
                    score: if usable { Some(entry.score) } else { None },
                    eval: entry.eval,
                    mv_index: entry.mv_index,
                }),
            );
        }

        self.entries[bucket_index as usize]
            .0
            .iter_mut()
            .find_map(|entry| {
                if entry.bound_type() == BoundType::Empty
                    || (entry.should_replace(depth, evict_generation))
                {
                    // if logdbg {
                    //     println!(
                    //         "Found entry for hash {:018X} ({}) to evict. Depth={},EDepth={},EBt={:?}",
                    //         hash, bucket_index,
                    //         depth,
                    //         entry.depth(),
                    //         entry.bound_type()
                    //     );
                    // }
                    Some(TtKey {
                        ptr: entry as *mut TtEntry,
                    })
                } else {
                    None
                }
            })
            .map(|k| (Some(k), None))
            .unwrap_or((None, None))
    }

    #[inline(always)]
    pub fn store(
        &mut self,
        key: Option<TtKey>,
        hash: u64,
        score: Eval,
        eval: Eval,
        depth: u8,
        mv_index: u8,
        bound_type: BoundType,
    ) {
        let key = match key {
            Some(k) => k,
            None => return,
        };

        let generation = self.generation;

        let entry = unsafe { &mut *key.ptr };

        if DEBUG {
            // println!("Stored in slot {:p}", entry);
            self.hash64
                .entry(entry as *const TtEntry)
                .and_modify(|h| *h = hash)
                .or_insert(hash);
        }

        // let logdbg = hash == 0x00A760E2A3316CAD4D;

        // if logdbg {
        //     println!(
        //         "V3 Storing mv index {} for hash {:018X}. Depth={},EDepth={},EBt={:?}",
        //         mv_index,
        //         hash,
        //         depth,
        //         entry.depth(),
        //         entry.bound_type(),
        //     );
        // }

        entry.eval = eval;
        entry.score = score;
        entry.mv_index = mv_index;
        entry.set_generation(generation);
        entry.set_depth_and_type(depth, bound_type);
        entry.set_key(hash);

        // if logdbg {
        //     println!(
        //         "V3 Stored entry for hash {:018X} ({}). Depth={},EDepth={},KEY={:#x}",
        //         hash,
        //         ((hash as u128) * (self.num_buckets as u128)) >> 64,
        //         depth,
        //         entry.depth(),
        //         entry.key()
        //     );
        // }
    }

    #[inline(always)]
    pub fn get_hash_part(index_bits: u32, hash: u64) -> u32 {
        let hash_bits = hash >> index_bits;
        hash_bits as u32
    }

    pub fn clear(&mut self) {
        self.generation = 0;
        self.evict_generation = 0;
        for bucket in self.entries.iter_mut() {
            for entry in &mut bucket.0 {
                entry.depthtype = 0;
                entry.mv_index = 0xFF;
            }
        }
    }

    pub fn calc_stats(&self) -> TtStats {
        return TtStats {
            fill_percentage: 0.0,
            probe_hit: 0,
            probe_miss: 0,
            store_hit: 0,
            store_miss: 0,
            collisions: 0,
        };
    }
}
