use std::arch::x86_64::*;

macro_rules! cmp_swap_arr {
    ($arr:ident, $i:expr, $j:expr) => {{
        let a = $arr[$i];
        let b = $arr[$j];
        let diff = (b as i32).wrapping_sub(a as i32);
        let swap_mask = (diff >> 31) as u32;
        let min_val = ((b as u32) & swap_mask) | ((a as u32) & !swap_mask);
        let max_val = ((a as u32) & swap_mask) | ((b as u32) & !swap_mask);
        $arr[$i] = min_val as u16;
        $arr[$j] = max_val as u16;
    }};
}

macro_rules! m128_reverse_epi16 {
    ($a:expr) => {{
        let index = _mm_set_epi8(1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14);
        _mm_shuffle_epi8($a, index)
    }};
}

macro_rules! m256_reverse_epi16 {
    ($a:expr) => {{
        let index = _mm256_set_epi16(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
        _mm256_permutexvar_epi16(index, $a)
    }};
}

macro_rules! m512_reverse_epi16 {
    ($a:expr) => {{
        let index = _mm512_set_epi16(
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31,
        );
        _mm512_permutexvar_epi16(index, $a)
    }};
}

fn sort4(arr: &mut [u16]) {
    debug_assert!(arr.len() == 4);

    cmp_swap_arr!(arr, 0, 1);
    cmp_swap_arr!(arr, 2, 3);
    cmp_swap_arr!(arr, 0, 2);
    cmp_swap_arr!(arr, 1, 3);
    cmp_swap_arr!(arr, 1, 2);
}

fn sort8(arr: &mut [u16]) {
    debug_assert!(arr.len() == 8);

    sort4(&mut arr[0..4]);
    sort4(&mut arr[4..8]);

    for i in 0..4 {
        cmp_swap_arr!(arr, i, 7 - i);
    }

    sort4(&mut arr[0..4]);
    sort4(&mut arr[4..8]);
}

fn sort16(inout_x16: &mut __m256i) {
    unsafe {
        let mut lo_arr: [u16; 8] = [0u16; 8];
        let mut hi_arr: [u16; 8] = [0u16; 8];

        let lo_x8: __m128i = _mm256_castsi256_si128(*inout_x16);
        let hi_x8: __m128i = _mm256_extracti128_si256(*inout_x16, 1);

        _mm_storeu_si128(lo_arr.as_mut_ptr() as *mut __m128i, lo_x8);
        _mm_storeu_si128(hi_arr.as_mut_ptr() as *mut __m128i, hi_x8);

        sort8(&mut lo_arr);
        sort8(&mut hi_arr);

        let lo_sorted_x8 = _mm_loadu_si128(lo_arr.as_ptr() as *const __m128i);
        let hi_sorted_x8 = _mm_loadu_si128(hi_arr.as_ptr() as *const __m128i);
        let hi_reversed_x8 = m128_reverse_epi16!(hi_sorted_x8);

        let min_x8 = _mm_min_epu16(lo_sorted_x8, hi_reversed_x8);
        let max_x8 = _mm_max_epu16(lo_sorted_x8, hi_reversed_x8);

        _mm_storeu_si128(lo_arr.as_mut_ptr() as *mut __m128i, min_x8);
        _mm_storeu_si128(hi_arr.as_mut_ptr() as *mut __m128i, max_x8);

        sort8(&mut lo_arr);
        sort8(&mut hi_arr);

        let lo_x8 = _mm_loadu_si128(lo_arr.as_ptr() as *const __m128i);
        let hi_x8 = _mm_loadu_si128(hi_arr.as_ptr() as *const __m128i);

        *inout_x16 = _mm256_castsi128_si256(lo_x8);
        *inout_x16 = _mm256_inserti128_si256(*inout_x16, hi_x8, 1);
    }
}

fn sort32(inout_x32: &mut __m512i) {
    unsafe {
        let mut lo_x16 = _mm512_castsi512_si256(*inout_x32);
        let mut hi_x16 = _mm512_extracti32x8_epi32(*inout_x32, 1);

        sort16(&mut lo_x16);
        sort16(&mut hi_x16);

        let hi_reversed_x16 = m256_reverse_epi16!(hi_x16);

        let mut min_x16 = _mm256_min_epu16(lo_x16, hi_reversed_x16);
        let mut max_x16 = _mm256_max_epu16(lo_x16, hi_reversed_x16);

        sort16(&mut min_x16);
        sort16(&mut max_x16);

        *inout_x32 = _mm512_castsi256_si512(min_x16);
        *inout_x32 = _mm512_inserti32x8(*inout_x32, max_x16, 1);
    }
}

fn sort64(inout_x32_0: &mut __m512i, inout_x32_1: &mut __m512i) {
    unsafe {
        sort32(inout_x32_0);
        sort32(inout_x32_1);

        let reversed_x32_1 = m512_reverse_epi16!(*inout_x32_1);

        let min_x32 = _mm512_min_epu16(*inout_x32_0, reversed_x32_1);
        let max_x32 = _mm512_max_epu16(*inout_x32_0, reversed_x32_1);

        *inout_x32_0 = min_x32;
        *inout_x32_1 = max_x32;

        sort32(inout_x32_0);
        sort32(inout_x32_1);
    }
}

fn sort128(
    inout_x32_0: &mut __m512i,
    inout_x32_1: &mut __m512i,
    inout_x32_2: &mut __m512i,
    inout_x32_3: &mut __m512i,
) {
    unsafe {
        sort64(inout_x32_0, inout_x32_1);
        sort64(inout_x32_2, inout_x32_3);

        let reversed_x32_2 = m512_reverse_epi16!(*inout_x32_2);
        let reversed_x32_3 = m512_reverse_epi16!(*inout_x32_3);

        let min_x32_03 = _mm512_min_epu16(*inout_x32_0, reversed_x32_3);
        let max_x32_03 = _mm512_max_epu16(*inout_x32_0, reversed_x32_3);
        let min_x32_12 = _mm512_min_epu16(*inout_x32_1, reversed_x32_2);
        let max_x32_12 = _mm512_max_epu16(*inout_x32_1, reversed_x32_2);

        *inout_x32_0 = min_x32_03;
        *inout_x32_1 = min_x32_12;
        *inout_x32_2 = max_x32_03;
        *inout_x32_3 = max_x32_12;

        sort64(inout_x32_0, inout_x32_1);
        sort64(inout_x32_2, inout_x32_3);
    }
}

fn sort256(
    inout_x32_0: &mut __m512i,
    inout_x32_1: &mut __m512i,
    inout_x32_2: &mut __m512i,
    inout_x32_3: &mut __m512i,
    inout_x32_4: &mut __m512i,
    inout_x32_5: &mut __m512i,
    inout_x32_6: &mut __m512i,
    inout_x32_7: &mut __m512i,
) {
    unsafe {
        sort128(inout_x32_0, inout_x32_1, inout_x32_2, inout_x32_3);
        sort128(inout_x32_4, inout_x32_5, inout_x32_6, inout_x32_7);

        let reversed_x32_4 = m512_reverse_epi16!(*inout_x32_4);
        let reversed_x32_5 = m512_reverse_epi16!(*inout_x32_5);
        let reversed_x32_6 = m512_reverse_epi16!(*inout_x32_6);
        let reversed_x32_7 = m512_reverse_epi16!(*inout_x32_7);

        let min_x32_07 = _mm512_min_epu16(*inout_x32_0, reversed_x32_7);
        let max_x32_07 = _mm512_max_epu16(*inout_x32_0, reversed_x32_7);
        let min_x32_16 = _mm512_min_epu16(*inout_x32_1, reversed_x32_6);
        let max_x32_16 = _mm512_max_epu16(*inout_x32_1, reversed_x32_6);
        let min_x32_25 = _mm512_min_epu16(*inout_x32_2, reversed_x32_5);
        let max_x32_25 = _mm512_max_epu16(*inout_x32_2, reversed_x32_5);
        let min_x32_34 = _mm512_min_epu16(*inout_x32_3, reversed_x32_4);
        let max_x32_34 = _mm512_max_epu16(*inout_x32_3, reversed_x32_4);

        *inout_x32_0 = min_x32_07;
        *inout_x32_1 = min_x32_16;
        *inout_x32_2 = min_x32_25;
        *inout_x32_3 = min_x32_34;
        *inout_x32_4 = max_x32_07;
        *inout_x32_5 = max_x32_16;
        *inout_x32_6 = max_x32_25;
        *inout_x32_7 = max_x32_34;

        sort128(inout_x32_0, inout_x32_1, inout_x32_2, inout_x32_3);
        sort128(inout_x32_4, inout_x32_5, inout_x32_6, inout_x32_7);
    }
}

macro_rules! calc_seg_size {
    ($n:expr) => {
        $n.next_power_of_two().min(256).max(16)
    };
}

#[inline(never)]
pub fn sort_218x16_avx512(buf_218: &mut [u16; 256], n: usize) {
    if n <= 8 {
        sort8(&mut buf_218[0..8]);
        return;
    }

    let seg_size = calc_seg_size!(n);

    unsafe {
        match seg_size {
            16 => {
                let mut input_x16 = _mm256_loadu_si256(buf_218.as_ptr() as *const __m256i);
                sort16(&mut input_x16);
                _mm256_storeu_si256(buf_218.as_mut_ptr() as *mut __m256i, input_x16);
            }
            32 => {
                let mut input_x32 = _mm512_loadu_si512(buf_218.as_ptr() as *const __m512i);
                sort32(&mut input_x32);
                _mm512_storeu_si512(buf_218.as_mut_ptr() as *mut __m512i, input_x32);
            }
            64 => {
                let ptr = buf_218.as_ptr() as *const __m512i;

                let mut input_x32_0 = _mm512_loadu_si512(ptr);
                let mut input_x32_1 = _mm512_loadu_si512(ptr.add(1));

                sort64(&mut input_x32_0, &mut input_x32_1);

                _mm512_storeu_si512(buf_218.as_mut_ptr() as *mut __m512i, input_x32_0);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(32) as *mut __m512i, input_x32_1);
            }
            128 => {
                std::hint::cold_path();

                let ptr = buf_218.as_ptr() as *const __m512i;

                let mut input_x32_0 = _mm512_loadu_si512(ptr);
                let mut input_x32_1 = _mm512_loadu_si512(ptr.add(1));
                let mut input_x32_2 = _mm512_loadu_si512(ptr.add(2));
                let mut input_x32_3 = _mm512_loadu_si512(ptr.add(3));

                sort128(
                    &mut input_x32_0,
                    &mut input_x32_1,
                    &mut input_x32_2,
                    &mut input_x32_3,
                );

                _mm512_storeu_si512(buf_218.as_mut_ptr() as *mut __m512i, input_x32_0);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(32) as *mut __m512i, input_x32_1);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(64) as *mut __m512i, input_x32_2);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(96) as *mut __m512i, input_x32_3);
            }
            _ => {
                std::hint::cold_path();

                let ptr = buf_218.as_ptr() as *const __m512i;

                let mut input_x32_0 = _mm512_loadu_si512(ptr);
                let mut input_x32_1 = _mm512_loadu_si512(ptr.add(1));
                let mut input_x32_2 = _mm512_loadu_si512(ptr.add(2));
                let mut input_x32_3 = _mm512_loadu_si512(ptr.add(3));
                let mut input_x32_4 = _mm512_loadu_si512(ptr.add(4));
                let mut input_x32_5 = _mm512_loadu_si512(ptr.add(5));
                let mut input_x32_6 = _mm512_loadu_si512(ptr.add(6));
                // let mut input_x32_7 = _mm512_loadu_si512(ptr.add(7));

                sort256(
                    &mut input_x32_0,
                    &mut input_x32_1,
                    &mut input_x32_2,
                    &mut input_x32_3,
                    &mut input_x32_4,
                    &mut input_x32_5,
                    &mut input_x32_6,
                    &mut _mm512_set1_epi16(0xFFFFu16 as i16),
                );

                _mm512_storeu_si512(buf_218.as_mut_ptr() as *mut __m512i, input_x32_0);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(32) as *mut __m512i, input_x32_1);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(64) as *mut __m512i, input_x32_2);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(96) as *mut __m512i, input_x32_3);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(128) as *mut __m512i, input_x32_4);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(160) as *mut __m512i, input_x32_5);
                _mm512_storeu_si512(buf_218.as_mut_ptr().add(192) as *mut __m512i, input_x32_6);
                // _mm512_storeu_si512(buf_218.as_mut_ptr().add(224) as *mut __m512i, input_x32_7);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sorting_fuzz() {
        use rand::{Rng, SeedableRng};
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);

        for _ in 0..64 {
            for arr_size in 0..218 {
                let mut random_arr = [0xFFFFu16; 256];

                for i in 0..arr_size {
                    random_arr[i] = rng.random_range(0..u16::MAX);
                }

                let mut arr_copy = random_arr;
                sort_218x16_avx512(&mut arr_copy, arr_size);

                assert!(
                    arr_copy[0..arr_size].is_sorted(),
                    "Array not sorted at array size {} / seg size {}",
                    arr_size,
                    if arr_size <= 8 {
                        8
                    } else {
                        calc_seg_size!(arr_size)
                    }
                );

                for i in 0..arr_size {
                    assert!(
                        arr_copy.contains(&random_arr[i]),
                        "Element {} not found in sorted array",
                        random_arr[i]
                    );
                }
            }
        }
    }
}
