use std::pin::Pin;

use crate::{engine::search::eval::Eval, util};

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
        self.keygen = TranspositionTable::key_from_entry(self.keygen) | ((generation as u32) << 30);
    }

    #[inline(always)]
    pub fn set_key(&mut self, key: u32) {
        self.keygen = (self.keygen & 0xC000_0000) | key;
    }

    #[inline(always)]
    pub fn key(&self) -> u32 {
        TranspositionTable::key_from_entry(self.keygen)
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
    }

    #[inline(always)]
    fn bucket_index(&self, hash: u64) -> usize {
        ((hash as u128) * (self.num_buckets as u128) >> 64) as usize
        // ((hash as usize) & self.table_size_mask) as usize
    }

    #[inline(always)]
    fn bucket_key(&self, hash: u64) -> u32 {
        (hash as u32) & 0x3FFF_FFFF
        // let hash_bits = hash >> self.index_bits;
        // hash_bits as u32 & 0x3FFF_FFFF
    }

    #[inline(always)]
    fn key_from_entry(keygen: u32) -> u32 {
        (keygen as u32) & 0x3FFF_FFFF
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

        let bucket_index = self.bucket_index(hash);

        let key = self.bucket_key(hash);

        for entry in &mut self.entries[bucket_index as usize].0 {
            if entry.key() != key || entry.bound_type() == BoundType::Empty {
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
                    println!(
                        "V3 TT Probe Hit Collision: Hash {:016X} ({:016X} in the slot), Depth {}, Type {:?}, Gen {}, HM: {}",
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
                // if entry.should_replace(depth, evict_generation) {
                //     Some(TtKey {
                //         ptr: entry as *mut TtEntry,
                //     })
                // } else {
                //     None
                // },
                None,
                Some(TtProbe {
                    score: if usable { Some(entry.score) } else { None },
                    eval: entry.eval,
                    mv_index: entry.mv_index,
                }),
            );
        }

        (None, None)

        // if let Some(free) = self.entries[bucket_index as usize]
        //     .0
        //     .iter_mut()
        //     .find_map(|entry| {
        //         if entry.bound_type() == BoundType::Empty {
        //             if util::should_log(hash) {
        //                 println!(
        //                     "V3 Found EMPTY entry for hash {:018X} ({}) to evict. Depth={},EDepth={},EBt={:?}",
        //                     hash, bucket_index,
        //                     depth,
        //                     entry.depth(),
        //                     entry.bound_type()
        //                 );
        //             }
        //             Some(TtKey {
        //                 ptr: entry as *mut TtEntry,
        //             })
        //         } else {
        //             None
        //         }
        //     })
        // {
        //     return (Some(free), None);
        // }

        // self.entries[bucket_index as usize]
        //     .0
        //     .iter_mut()
        //     .find_map(|entry| {
        //         if entry.should_replace(depth, evict_generation) {
        //             if util::should_log(hash) {
        //                 println!(
        //                     "V3 Found REPLACABLE entry for hash {:018X} ({}) to evict. Depth={},EDepth={},EBt={:?}",
        //                     hash, bucket_index,
        //                     depth,
        //                     entry.depth(),
        //                     entry.bound_type()
        //                 );
        //             }
        //             Some(TtKey {
        //                 ptr: entry as *mut TtEntry,
        //             })
        //         } else {
        //             None
        //         }
        //     })
        //     .map(|k| (Some(k), None))
        //     .unwrap_or((None, None))
    }

    #[inline(always)]
    pub fn store(
        &mut self,
        _key: Option<TtKey>,
        hash: u64,
        score: Eval,
        eval: Eval,
        depth: u8,
        mv_index: u8,
        bound_type: BoundType,
    ) {
        // let key = match key {
        //     Some(k) => k,
        //     None => {
        //         if util::should_log(hash) {
        //             println!(
        //                 "V3 TT Store SKIPPED for hash {:018X}. No replacable entry found. Depth={}",
        //                 hash, depth
        //             );
        //         }
        //         return;
        //     }
        // };
        // let entry: &mut TtEntry = unsafe { &mut *key.ptr };

        let bucket_key = self.bucket_key(hash);
        let bucket_index = self.bucket_index(hash);

        let mut entry = None;

        for e in &mut self.entries[bucket_index].0 {
            if e.bound_type() == BoundType::Empty {
                entry = Some(e);
                break;
            }
            if e.key() == bucket_key {
                if e.should_replace(depth, self.evict_generation) {
                    entry = Some(e);
                }
                break;
            }
            if e.should_replace(depth, self.evict_generation) {
                entry = Some(e);
            }
        }

        let entry = match entry {
            Some(e) => e,
            None => return,
        };

        let generation = self.generation;

        if DEBUG {
            // println!("Stored in slot {:p}", entry);
            self.hash64
                .entry(entry as *const TtEntry)
                .and_modify(|h| *h = hash)
                .or_insert(hash);
        }

        entry.eval = eval;
        entry.score = score;
        entry.mv_index = mv_index;
        entry.set_generation(generation);
        entry.set_depth_and_type(depth, bound_type);
        entry.set_key(bucket_key);

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
