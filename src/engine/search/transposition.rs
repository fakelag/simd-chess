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

// fn print_m512(label: &str, a: &__m512i) {
//     let mut arr = [0u8; 64];
//     unsafe {
//         _mm512_storeu_si512(arr.as_mut_ptr() as *mut __m512i, *a);
//     }
//     println!("{:03?} - {}", arr, label);
// }

#[derive(Debug)]
pub struct TtProbe {
    pub score: Option<Eval>,
    pub mv_index: u8,
}

#[derive(Debug, Clone, Copy)]
#[repr(packed, C)]
struct TtEntry {
    keygen: u32,
    depthtype: u8,
    // eval: Eval,
    score: Eval,
    mv_index: u8,
}

impl TtEntry {
    pub fn new() -> Self {
        TtEntry {
            keygen: 0,
            // eval: 0,
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
    pub fn should_replace(&self, depth: u8, bt: BoundType, evict: bool) -> bool {
        self.depth() <= depth
            || evict
            || (bt == BoundType::Exact && self.bound_type() != BoundType::Exact)
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(align(64))]
pub struct TtBucket([TtEntry; 8]);

pub struct TranspositionTable {
    entries: Pin<Box<[TtBucket]>>,
    generation: u8,
    table_size_mask: usize,
    index_bits: u32,
    evict_generation: [bool; 4],

    evict_0: __m512i,
    evict_1: __m512i,
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
        let index_bits = table_mask.count_ones();

        let entries = vec![TtBucket([TtEntry::new(); 8]); table_size].into_boxed_slice();

        TranspositionTable {
            table_size_mask: table_mask,
            entries: Pin::from(entries),
            hash64: std::collections::BTreeMap::new(),
            index_bits,
            evict_generation: [false; 4],
            generation: 0,
            evict_0: unsafe { _mm512_set1_epi32(0) },
            evict_1: unsafe { _mm512_set1_epi32(0) },
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
                _mm512_set_epi64(
                    (evict_0 << 24) as i64,
                    (evict_0 << 24) as i64,
                    (evict_0 << 24) as i64,
                    (evict_0 << 24) as i64,
                    (evict_0 << 24) as i64,
                    (evict_0 << 24) as i64,
                    (evict_0 << 24) as i64,
                    (evict_0 << 24) as i64,
                ),
                _mm512_set_epi64(
                    (evict_1 << 24) as i64,
                    (evict_1 << 24) as i64,
                    (evict_1 << 24) as i64,
                    (evict_1 << 24) as i64,
                    (evict_1 << 24) as i64,
                    (evict_1 << 24) as i64,
                    (evict_1 << 24) as i64,
                    (evict_1 << 24) as i64,
                ),
            )
        };
    }

    #[inline(always)]
    fn bucket_index(&self, hash: u64) -> usize {
        let b = (hash as usize) & self.table_size_mask;

        unsafe {
            std::hint::assert_unchecked(b < self.entries.len());
        }

        b
    }

    #[inline(always)]
    fn bucket_key(&self, hash: u64) -> u32 {
        ((hash >> self.index_bits) & 0x3FFF_FFFF) as u32
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

        let entry = unsafe {
            let bucket_vec =
                _mm512_load_si512(self.entries.as_ptr().add(bucket_index) as *const __m512i);
            let key_mask = self.match_entries_key_avx512(&bucket_vec, key);

            let key_index = key_mask.trailing_zeros();
            match key_index {
                0..8 => &mut self.entries[bucket_index as usize].0[key_index as usize],
                _ => return None,
            }
        };

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

        return Some(TtProbe {
            score: if usable { Some(entry.score) } else { None },
            mv_index: entry.mv_index,
        });
    }

    #[inline(always)]
    fn match_entries_late_generation_avx512(&self, bucket_vec: &__m512i) -> __mmask64 {
        unsafe {
            const SLOT_MASK: i8 = 0xC0 as u8 as i8;

            let gen_mask_vec = _mm512_set1_epi8(SLOT_MASK);
            let gen_vec = _mm512_and_si512(*bucket_vec, gen_mask_vec);

            let evict_a = _mm512_cmpeq_epi8_mask(gen_vec, self.evict_0);
            let evict_b = _mm512_cmpeq_epi8_mask(gen_vec, self.evict_1);

            evict_a | evict_b
        }
    }

    #[inline(always)]
    fn match_entries_depth_avx512(&self, bucket_vec: &__m512i, depth: u8) -> __mmask64 {
        unsafe {
            let result =
                _mm512_cmple_epi8_mask(*bucket_vec, _mm512_set1_epi8((depth << 2) as i8 | 0b11));

            result >> 1
        }
    }

    #[inline(always)]
    fn match_entries_replace_avx512(
        &self,
        bucket_vec: &__m512i,
        bt_vec: &__m512i,
        depth: u8,
        bound_type: BoundType,
    ) -> __mmask8 {
        let depth_mask = self.match_entries_depth_avx512(bucket_vec, depth);
        let generation_mask = self.match_entries_late_generation_avx512(bucket_vec);

        // Always replace non-exact entries with exact
        let bt_mask = unsafe {
            let test_vec = _mm512_set1_epi8(BoundType::Exact as u8 as i8);
            let bt_nonexact_mask = _mm512_cmpgt_epi8_mask(*bt_vec, test_vec);

            (bt_nonexact_mask >> 1) & ((bound_type != BoundType::Exact) as u64).wrapping_sub(1)
        };

        // 0b0000100000001000000010000000100000001000000010000000100000001000;

        let result = (generation_mask | depth_mask | bt_mask) & 0x808080808080808;
        ((result >> 3)
            | (result >> 10)
            | (result >> 17)
            | (result >> 24)
            | (result >> 31)
            | (result >> 38)
            | (result >> 45)
            | (result >> 52)) as __mmask8
        // unsafe { _pext_u64(generation_mask | depth_mask | bt_mask, 0x808080808080808) as __mmask8 }
    }

    #[inline(always)]
    fn match_entries_empty_avx512(&self, bt_vec: &__m512i) -> __mmask8 {
        unsafe {
            let test_vec = _mm512_set1_epi8(BoundType::Empty as u8 as i8);

            // 0x40100401004010;
            // 0b1000000000100000000010000000001000000000100000000010000;
            // 0b00001000000010000000100000001000000010000000100000001000000010000;

            let result = _mm512_cmpeq_epi8_mask(*bt_vec, test_vec) & 0x1010101010101010;

            ((result >> 4)
                | (result >> 11)
                | (result >> 18)
                | (result >> 25)
                | (result >> 32)
                | (result >> 39)
                | (result >> 46)
                | (result >> 53)) as __mmask8

            // _pext_u64(
            //     _mm512_cmpeq_epi8_mask(*bt_vec, test_vec),
            //     0x1010101010101010,
            // ) as __mmask8
        }
    }

    #[inline(always)]
    fn match_entries_key_avx512(&self, bucket_vec: &__m512i, key: u32) -> __mmask8 {
        unsafe {
            let key_vec = _mm512_set1_epi32(key as i32);

            let bucket_aligned_vec = _mm512_permutexvar_epi8(
                _mm512_set_epi64(
                    0,
                    0,
                    0,
                    0,
                    0x3B3A3938_33323130,
                    0x2B2A2928_23222120,
                    0x1B1A1918_13121110,
                    0x0B0A0908_03020100,
                ),
                *bucket_vec,
            );

            let key_mask_vec = _mm512_set1_epi32(0x3FFF_FFFF as i32);

            let result = _mm512_cmpeq_epi32_mask(
                _mm512_and_si512(bucket_aligned_vec, key_mask_vec),
                key_vec,
            );

            (result & 0xFF) as __mmask8
        }
    }

    #[inline(always)]
    pub fn store<F>(
        &mut self,
        hash: u64,
        score: Eval,
        depth: u8,
        mv_index: F,
        bound_type: BoundType,
    ) where
        F: Fn() -> u8,
    {
        let bucket_key = self.bucket_key(hash);
        let bucket_index = self.bucket_index(hash);

        let generation = self.generation;

        // println!("Probe TT");

        // let mut dbg = String::new();

        let entry_mask = unsafe {
            let bucket_vec =
                _mm512_load_si512(self.entries.as_ptr().add(bucket_index) as *const __m512i);

            let bt_mask_vec = _mm512_set1_epi8(0b11 as u8 as i8);
            let bt_vec = _mm512_and_si512(bucket_vec, bt_mask_vec);

            let key_mask = self.match_entries_key_avx512(&bucket_vec, self.bucket_key(hash));

            let replace_mask =
                self.match_entries_replace_avx512(&bucket_vec, &bt_vec, depth, bound_type);
            let empty_mask = self.match_entries_empty_avx512(&bt_vec);
            let key_mask = key_mask & !empty_mask;

            let replace_key_mask = key_mask & replace_mask;

            let no_key_mask = ((key_mask != 0) as u8).wrapping_sub(1);
            let no_empty_mask = ((empty_mask != 0) as u8).wrapping_sub(1);

            let entry_mask = replace_key_mask
                | (empty_mask & no_key_mask)
                | (replace_mask & no_key_mask & no_empty_mask);

            // dbg.push_str(&format!(
            //     "{:08b} - replace_mask\n{:08b} - empty_mask\n{:08b} - key_mask\n{:08b} - entry_mask",
            //     replace_mask, empty_mask, key_mask, entry_mask
            // ));

            entry_mask
        };

        let entry_index = entry_mask.trailing_zeros();

        // let debug_res = self.find_store_index_debug(hash, bound_type, depth, key_mask, &mut dbg);
        // assert!(
        //     (entry_index < 8 && debug_res.is_some_and(|i| i as u32 == entry_index))
        //         || entry_index == 8 && debug_res.is_none(),
        //     "TT Store failed to find a valid index to store the entry. Hash {:016X}, Depth {}, Type {:?}, HM: {}\nfind_result: {:?}, entry_index: {}\ndbg:\n{}",
        //     hash,
        //     depth,
        //     bound_type,
        //     mv_index,
        //     debug_res,
        //     entry_index,
        //     dbg
        // );

        let entry = match entry_index {
            0..8 => &mut self.entries[bucket_index].0[entry_index as usize],
            _ => return,
        };

        if DEBUG {
            self.hash64
                .entry(entry as *const TtEntry)
                .and_modify(|h| *h = hash)
                .or_insert(hash);
        }

        // entry.eval = eval;
        entry.score = score;
        entry.mv_index = mv_index();
        entry.set_generation(generation);
        entry.set_depth_and_type(depth, bound_type);
        entry.set_key(bucket_key);
    }

    fn find_store_index_debug(
        &self,
        hash: u64,
        bt: BoundType,
        depth: u8,
        dbg: &mut String,
    ) -> Option<usize> {
        let bucket_index = self.bucket_index(hash);
        let key = self.bucket_key(hash);

        let evict_0 = (self.generation + 1) & 0b11;
        let evict_1 = (self.generation + 2) & 0b11;

        // println!("Debug probing TT");
        dbg.push_str("Debug probing TT:\n");

        let mut key_found = false;

        for (i, entry) in self.entries[bucket_index].0.iter().enumerate() {
            let key_match = entry.key() == key;

            if key_match && entry.bound_type() != BoundType::Empty {
                key_found = true;

                let should_evict =
                    entry.generation() == evict_0 as u8 || entry.generation() == evict_1 as u8;

                if !entry.should_replace(depth, bt, should_evict) {
                    // Search for other key matches
                    continue;
                }
                dbg.push_str("Entry key found and will be replaced\n");
                //println!("Entry key found and will be replaced");
                return Some(i);
            }
        }

        if key_found {
            dbg.push_str("No matching key found in TT bucket\n");
            // println!("No matching key found in TT bucket");
            return None;
        }

        for (i, entry) in self.entries[bucket_index].0.iter().enumerate() {
            if entry.bound_type() == BoundType::Empty {
                // println!("Found empty entry to store");
                dbg.push_str("Found empty entry to store\n");
                return Some(i);
            }
        }

        for (i, entry) in self.entries[bucket_index].0.iter().enumerate() {
            let should_evict =
                entry.generation() == evict_0 as u8 || entry.generation() == evict_1 as u8;

            if entry.should_replace(depth, bt, should_evict) {
                // println!("Found entry to evict and replace");
                dbg.push_str("Found entry to evict and replace\n");
                return Some(i);
            }
        }

        dbg.push_str("No entry found to store\n");
        // println!("No entry found to store");

        None
    }

    pub fn clear(&mut self) {
        self.generation = 0;
        for i in 0..4 {
            self.evict_generation[i] = false;
        }
        for elem in self.entries.iter_mut() {
            for entry in &mut elem.0 {
                *entry = TtEntry::new();
            }
        }
    }
}
