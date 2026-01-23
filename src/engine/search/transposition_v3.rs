use std::{arch::x86_64::*, pin::Pin};

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

#[derive(Debug)]
pub struct TtProbe {
    pub score: Option<Eval>,
    pub eval: Eval,
    pub mv_index: u8,
}

#[derive(Debug, Clone, Copy)]
#[repr(packed)]
struct TtEntry {
    keygen: u32,
    depthtype: u8,
    eval: Eval,
    score: Eval,
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
    pub fn should_replace(&self, depth: u8, evict: bool) -> bool {
        self.depth() < depth || evict
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(align(32))]
pub struct TtBucket([TtEntry; 3]);

pub struct TranspositionTable {
    entries: Pin<Box<[TtBucket]>>,
    generation: u8,
    table_size_mask: usize,
    // num_buckets: usize,
    evict_generation: [bool; 4],

    evict_0: __m256i,
    evict_1: __m256i,

    hash64: std::collections::BTreeMap<*const TtEntry, u64>,
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

        let entries = vec![TtBucket([TtEntry::new(); 3]); table_size].into_boxed_slice();

        TranspositionTable {
            table_size_mask: table_mask,
            entries: Pin::from(entries),
            hash64: std::collections::BTreeMap::new(),
            // num_buckets: table_size,
            evict_generation: [false; 4],
            generation: 0,
            evict_0: unsafe { _mm256_set1_epi32(0) },
            evict_1: unsafe { _mm256_set1_epi32(0) },
        }
    }

    #[inline(always)]
    pub fn new_search(&mut self) {
        self.generation = (self.generation + 1) & 0b11;
        for i in 0..4 {
            self.evict_generation[i as usize] =
                (i == (self.generation + 1) & 0b11) || (i == (self.generation + 2) & 0b11);
        }

        let evict_0 = (((self.generation + 1) & 0b11) << 6) as u64;
        let evict_1 = (((self.generation + 2) & 0b11) << 6) as u64;

        (self.evict_0, self.evict_1) = unsafe {
            (
                _mm256_set_epi64x(
                    0,
                    (evict_0 << 56) as i64,
                    (evict_0 << 40) as i64,
                    (evict_0 << 24) as i64,
                ),
                _mm256_set_epi64x(
                    0,
                    (evict_1 << 56) as i64,
                    (evict_1 << 40) as i64,
                    (evict_1 << 24) as i64,
                ),
            )
        };
    }

    #[inline(always)]
    fn bucket_index(&self, hash: u64) -> usize {
        // let b = ((hash as u128) * (self.num_buckets as u128) >> 64) as usize;

        let b = (((hash >> 30) as usize) & self.table_size_mask) as usize;

        unsafe {
            std::hint::assert_unchecked(b < self.entries.len());
        }

        b
    }

    #[inline(always)]
    fn bucket_key(&self, hash: u64) -> u32 {
        (hash as u32) & 0x3FFF_FFFF
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
    ) -> Option<TtProbe> {
        let generation = self.generation;

        let bucket_index = self.bucket_index(hash);

        let key = self.bucket_key(hash);

        for entry in &mut self.entries[bucket_index as usize].0 {
            let can_use_entry =
                ((entry.key() == key) as u8) & ((entry.bound_type() != BoundType::Empty) as u8);

            if can_use_entry == 0 {
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

            return Some(TtProbe {
                score: if usable { Some(entry.score) } else { None },
                eval: entry.eval,
                mv_index: entry.mv_index,
            });
        }

        None
    }

    // #[inline(always)]
    // fn match_entries_late_generation_avx2(&self, bucket_vec: &__m256i) -> __mmask32 {
    //     unsafe {
    //         const SLOT_MASK: i8 = 0xC0 as u8 as i8;

    //         let gen_mask_vec = _mm256_set1_epi8(SLOT_MASK);
    //         let gen_vec = _mm256_and_si256(*bucket_vec, gen_mask_vec);

    //         let evict_a = _mm256_cmpeq_epi8_mask(gen_vec, self.evict_0);
    //         let evict_b = _mm256_cmpeq_epi8_mask(gen_vec, self.evict_1);

    //         evict_a | evict_b
    //     }
    // }

    // #[inline(always)]
    // fn match_entries_depth_avx2(&self, bucket_vec: &__m256i, depth_lt: u8) -> __mmask32 {
    //     unsafe {
    //         let result = _mm256_cmplt_epi8_mask(
    //             _mm256_srai_epi16(*bucket_vec, 2),
    //             _mm256_set1_epi8(depth_lt as i8),
    //         );

    //         result >> 1
    //     }
    // }

    // #[inline(always)]
    // fn match_entries_replace_avx2(&self, bucket_vec: &__m256i, depth: u8) -> __mmask32 {
    //     let depth_mask = self.match_entries_depth_avx2(bucket_vec, depth);
    //     let generation_mask = self.match_entries_late_generation_avx2(bucket_vec);

    //     // unsafe { _pext_u32(generation_mask | depth_mask, 0b100000000010000000001000) as __mmask8 }
    //     ((generation_mask | depth_mask) & 0b100000000010000000001000) << 1
    // }

    // #[inline(always)]
    // fn match_entries_empty_avx2(&self, bucket_vec: &__m256i) -> __mmask32 {
    //     unsafe {
    //         const SLOT_MASK: i8 = 0b11 as u8 as i8;

    //         let test_vec = _mm256_set1_epi8(BoundType::Empty as u8 as i8);

    //         let bt_mask_vec = _mm256_set1_epi8(SLOT_MASK);

    //         let result =
    //             _mm256_cmpeq_epi8_mask(_mm256_and_si256(*bucket_vec, bt_mask_vec), test_vec);

    //         // _pext_u32(result, 0b1000000000100000000010000) as __mmask8
    //         result & 0b1000000000100000000010000
    //     }
    // }

    // #[inline(always)]
    // fn match_entries_key_avx2(&self, bucket_vec: &__m256i, key: u32) -> __mmask32 {
    //     unsafe {
    //         let key_vec = _mm256_set1_epi32(key as i32);

    //         let bucket_aligned_vec = _mm256_permutexvar_epi8(
    //             _mm256_set_epi8(
    //                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //
    //                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //
    //                 23, 22, 21, 20, //
    //                 13, 12, 11, 10, //
    //                 3, 2, 1, 0, //
    //             ),
    //             *bucket_vec,
    //         );

    //         let key_mask_vec = _mm256_set1_epi32(0x3FFF_FFFF as i32);

    //         let result = _mm256_cmpeq_epi32_mask(
    //             _mm256_and_si256(bucket_aligned_vec, key_mask_vec),
    //             key_vec,
    //         ) as u32;

    //         ((result & 1) << 4) | ((result & 2) << 13) | ((result & 4) << 22)
    //     }
    // }

    #[inline(always)]
    pub fn store(
        &mut self,
        hash: u64,
        score: Eval,
        eval: Eval,
        depth: u8,
        mv_index: u8,
        bound_type: BoundType,
    ) {
        let bucket_key = self.bucket_key(hash);
        let bucket_index = self.bucket_index(hash);

        // let entry_mask = unsafe {
        //     let bucket_vec =
        //         _mm256_load_si256(self.entries.as_ptr().add(bucket_index) as *const __m256i);

        //     let replace_mask = self.match_entries_replace_avx2(&bucket_vec, depth);
        //     let empty_mask = self.match_entries_empty_avx2(&bucket_vec);
        //     let key_mask = self.match_entries_key_avx2(&bucket_vec, bucket_key) & !empty_mask;

        //     let replace_key_mask = key_mask & replace_mask;

        //     let no_key_mask = ((key_mask != 0) as u32).wrapping_sub(1);
        //     let no_empty_mask = ((empty_mask != 0) as u32).wrapping_sub(1);

        //     let entry_mask = replace_key_mask
        //         | (empty_mask & no_key_mask)
        //         | (replace_mask & no_key_mask & no_empty_mask);

        //     ((entry_mask >> 4) & 1) | ((entry_mask >> 13) & 2) | ((entry_mask >> 22) & 4)
        // };

        // let entry_index = entry_mask.trailing_zeros();

        // let entry = match entry_index {
        //     0..3 => &mut self.entries[bucket_index].0[entry_index as usize],
        //     _ => return,
        // };

        // 14912215
        // 16323453

        let mut entry = None;

        for e in &mut self.entries[bucket_index].0 {
            if e.bound_type() == BoundType::Empty {
                entry = Some(e);
                break;
            } else if e.key() == bucket_key {
                if e.should_replace(depth, self.evict_generation[e.generation() as usize]) {
                    entry = Some(e);
                } else {
                    entry = None;
                }
                break;
            }
            if e.should_replace(depth, self.evict_generation[e.generation() as usize])
                && entry.is_none()
            {
                entry = Some(e);
            }
        }

        let entry = match entry {
            Some(e) => e,
            None => return,
        };

        let generation = self.generation;

        if DEBUG {
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
        for i in 0..4 {
            self.evict_generation[i] = false;
        }
        for bucket in self.entries.iter_mut() {
            for entry in &mut bucket.0 {
                entry.depthtype = 0;
                entry.mv_index = 0xFF;
            }
        }
    }
}
