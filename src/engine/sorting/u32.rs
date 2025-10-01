use std::arch::x86_64::*;

macro_rules! calc_seg_size {
    ($n:expr) => {
        $n.next_power_of_two().min(256).max(16)
    };
}

macro_rules! cmp_swap_arr {
    ($arr:ident, $i:expr, $j:expr) => {{
        let a = $arr[$i];
        let b = $arr[$j];
        let diff = (b as i64).wrapping_sub(a as i64);
        let swap_mask = (diff >> 63) as u64;
        let min_val = ((b as u64) & swap_mask) | ((a as u64) & !swap_mask);
        let max_val = ((a as u64) & swap_mask) | ((b as u64) & !swap_mask);
        $arr[$i] = min_val as u32;
        $arr[$j] = max_val as u32;
    }};
}

macro_rules! cmp_swap_x16_epu32 {
    ($input:expr, $perm:expr, $mask:expr) => {{
        let input = $input;
        let permuted_x16 = _mm512_permutexvar_epi32($perm, input);
        let min_x16 = _mm512_min_epu32(input, permuted_x16);
        let max_x16 = _mm512_max_epu32(input, permuted_x16);
        _mm512_mask_blend_epi32($mask, max_x16, min_x16)
    }};
}

macro_rules! m512_reverse_epi32 {
    ($a:expr) => {{
        let index = _mm512_set_epi32(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
        _mm512_permutexvar_epi32(index, $a)
    }};
}

#[inline(always)]
fn sort4(arr: &mut [u32]) {
    debug_assert!(arr.len() == 4);

    cmp_swap_arr!(arr, 0, 1);
    cmp_swap_arr!(arr, 2, 3);
    cmp_swap_arr!(arr, 0, 2);
    cmp_swap_arr!(arr, 1, 3);
    cmp_swap_arr!(arr, 1, 2);
}

#[inline(always)]
fn sort8(arr: &mut [u32]) {
    debug_assert!(arr.len() == 8);

    sort4(&mut arr[0..4]);
    sort4(&mut arr[4..8]);

    // 0 vs 7
    // 1 vs 6
    // 2 vs 5
    // 3 vs 4
    for i in 0..4 {
        cmp_swap_arr!(arr, i, 7 - i);
    }

    sort4(&mut arr[0..4]);
    sort4(&mut arr[4..8]);
}

#[inline(always)]
pub fn sort16(inout_x16: &mut __m512i) {
    unsafe {
        const PERM_SELECT_B: i32 = 0x10;

        let identity_x16 = _mm512_set_epi32(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0);

        let const_1_x16 = _mm512_set1_epi32(1);
        let const_2_x16 = _mm512_set1_epi32(2);
        let const_7_x16 = _mm512_set1_epi32(7);

        let const_perm_rev1_x16 = _mm512_set_epi32(
            0, 0, 0, 0, 0, 0, 0, 0, // unused
            8, 9, 10, 11, 12, 13, 14, 15, // reverse order
        );

        let sort4_m512 = |inout_x16: &mut __m512i| {
            let low_adj_mask = 0x5555u16;
            let low_gap2_mask = 0x3333u16;
            let low_mid12_mask = 0x2222u16;

            let perm_x16 = _mm512_xor_si512(identity_x16, const_1_x16);
            *inout_x16 = cmp_swap_x16_epu32!(*inout_x16, perm_x16, low_adj_mask);

            let perm_x16 = _mm512_xor_si512(identity_x16, const_2_x16);
            *inout_x16 = cmp_swap_x16_epu32!(*inout_x16, perm_x16, low_gap2_mask);

            let perm_x16 = _mm512_set_epi32(15, 13, 14, 12, 11, 9, 10, 8, 7, 5, 6, 4, 3, 1, 2, 0);
            *inout_x16 = cmp_swap_x16_epu32!(*inout_x16, perm_x16, low_mid12_mask);
        };

        let crosscompare_m512 = |inout_x16: &mut __m512i| {
            let low_rev8_mask = 0x0F0Fu16;

            let perm_x16 = _mm512_xor_si512(identity_x16, const_7_x16);
            *inout_x16 = cmp_swap_x16_epu32!(*inout_x16, perm_x16, low_rev8_mask);
        };

        let combine8_m512 = |inout_x16: &mut __m512i| {
            let permuted_x16 = _mm512_permutexvar_epi32(const_perm_rev1_x16, *inout_x16);

            let min_x16 = _mm512_min_epu32(*inout_x16, permuted_x16);
            let max_x16 = _mm512_max_epu32(*inout_x16, permuted_x16);

            let combine_perm_x16 = _mm512_set_epi32(
                7 | PERM_SELECT_B,
                6 | PERM_SELECT_B,
                5 | PERM_SELECT_B,
                4 | PERM_SELECT_B,
                3 | PERM_SELECT_B,
                2 | PERM_SELECT_B,
                1 | PERM_SELECT_B,
                0 | PERM_SELECT_B,
                7,
                6,
                5,
                4,
                3,
                2,
                1,
                0,
            );
            *inout_x16 = _mm512_permutex2var_epi32(min_x16, combine_perm_x16, max_x16);
        };

        sort4_m512(inout_x16);
        crosscompare_m512(inout_x16);
        sort4_m512(inout_x16);

        combine8_m512(inout_x16);

        sort4_m512(inout_x16);
        crosscompare_m512(inout_x16);
        sort4_m512(inout_x16);
    }
}

fn sort32(inout_x16_0: &mut __m512i, inout_x16_1: &mut __m512i) {
    unsafe {
        sort16(inout_x16_0);
        sort16(inout_x16_1);

        let reversed_x16_1 = m512_reverse_epi32!(*inout_x16_1);

        let min_x16 = _mm512_min_epu32(*inout_x16_0, reversed_x16_1);
        let max_x16 = _mm512_max_epu32(*inout_x16_0, reversed_x16_1);

        *inout_x16_0 = min_x16;
        *inout_x16_1 = max_x16;

        sort16(inout_x16_0);
        sort16(inout_x16_1);
    }
}

fn sort64(
    inout_x16_0: &mut __m512i,
    inout_x16_1: &mut __m512i,
    inout_x16_2: &mut __m512i,
    inout_x16_3: &mut __m512i,
) {
    unsafe {
        sort32(inout_x16_0, inout_x16_1);
        sort32(inout_x16_2, inout_x16_3);

        let reversed_x16_2 = m512_reverse_epi32!(*inout_x16_2);
        let reversed_x16_3 = m512_reverse_epi32!(*inout_x16_3);

        let min_x16_03 = _mm512_min_epu32(*inout_x16_0, reversed_x16_3);
        let max_x16_03 = _mm512_max_epu32(*inout_x16_0, reversed_x16_3);
        let min_x16_12 = _mm512_min_epu32(*inout_x16_1, reversed_x16_2);
        let max_x16_12 = _mm512_max_epu32(*inout_x16_1, reversed_x16_2);

        *inout_x16_0 = min_x16_03;
        *inout_x16_1 = min_x16_12;
        *inout_x16_2 = max_x16_03;
        *inout_x16_3 = max_x16_12;

        sort32(inout_x16_0, inout_x16_1);
        sort32(inout_x16_2, inout_x16_3);
    }
}

fn sort128(
    inout_x16_0: &mut __m512i,
    inout_x16_1: &mut __m512i,
    inout_x16_2: &mut __m512i,
    inout_x16_3: &mut __m512i,
    inout_x16_4: &mut __m512i,
    inout_x16_5: &mut __m512i,
    inout_x16_6: &mut __m512i,
    inout_x16_7: &mut __m512i,
) {
    unsafe {
        sort64(inout_x16_0, inout_x16_1, inout_x16_2, inout_x16_3);
        sort64(inout_x16_4, inout_x16_5, inout_x16_6, inout_x16_7);

        let reversed_x16_4 = m512_reverse_epi32!(*inout_x16_4);
        let reversed_x16_5 = m512_reverse_epi32!(*inout_x16_5);
        let reversed_x16_6 = m512_reverse_epi32!(*inout_x16_6);
        let reversed_x16_7 = m512_reverse_epi32!(*inout_x16_7);

        let min_x16_07 = _mm512_min_epu32(*inout_x16_0, reversed_x16_7);
        let max_x16_07 = _mm512_max_epu32(*inout_x16_0, reversed_x16_7);
        let min_x16_16 = _mm512_min_epu32(*inout_x16_1, reversed_x16_6);
        let max_x16_16 = _mm512_max_epu32(*inout_x16_1, reversed_x16_6);
        let min_x16_25 = _mm512_min_epu32(*inout_x16_2, reversed_x16_5);
        let max_x16_25 = _mm512_max_epu32(*inout_x16_2, reversed_x16_5);
        let min_x16_34 = _mm512_min_epu32(*inout_x16_3, reversed_x16_4);
        let max_x16_34 = _mm512_max_epu32(*inout_x16_3, reversed_x16_4);

        *inout_x16_0 = min_x16_07;
        *inout_x16_1 = min_x16_16;
        *inout_x16_2 = min_x16_25;
        *inout_x16_3 = min_x16_34;
        *inout_x16_4 = max_x16_07;
        *inout_x16_5 = max_x16_16;
        *inout_x16_6 = max_x16_25;
        *inout_x16_7 = max_x16_34;

        sort64(inout_x16_0, inout_x16_1, inout_x16_2, inout_x16_3);
        sort64(inout_x16_4, inout_x16_5, inout_x16_6, inout_x16_7);
    }
}

fn sort256(
    inout_x16_0: &mut __m512i,
    inout_x16_1: &mut __m512i,
    inout_x16_2: &mut __m512i,
    inout_x16_3: &mut __m512i,
    inout_x16_4: &mut __m512i,
    inout_x16_5: &mut __m512i,
    inout_x16_6: &mut __m512i,
    inout_x16_7: &mut __m512i,
    inout_x16_8: &mut __m512i,
    inout_x16_9: &mut __m512i,
    inout_x16_10: &mut __m512i,
    inout_x16_11: &mut __m512i,
    inout_x16_12: &mut __m512i,
    inout_x16_13: &mut __m512i,
    inout_x16_14: &mut __m512i,
    inout_x16_15: &mut __m512i,
) {
    unsafe {
        sort128(
            inout_x16_0,
            inout_x16_1,
            inout_x16_2,
            inout_x16_3,
            inout_x16_4,
            inout_x16_5,
            inout_x16_6,
            inout_x16_7,
        );
        sort128(
            inout_x16_8,
            inout_x16_9,
            inout_x16_10,
            inout_x16_11,
            inout_x16_12,
            inout_x16_13,
            inout_x16_14,
            inout_x16_15,
        );

        let reversed_x16_8 = m512_reverse_epi32!(*inout_x16_8);
        let reversed_x16_9 = m512_reverse_epi32!(*inout_x16_9);
        let reversed_x16_10 = m512_reverse_epi32!(*inout_x16_10);
        let reversed_x16_11 = m512_reverse_epi32!(*inout_x16_11);
        let reversed_x16_12 = m512_reverse_epi32!(*inout_x16_12);
        let reversed_x16_13 = m512_reverse_epi32!(*inout_x16_13);
        let reversed_x16_14 = m512_reverse_epi32!(*inout_x16_14);
        let reversed_x16_15 = m512_reverse_epi32!(*inout_x16_15);

        let min_x16_015 = _mm512_min_epu32(*inout_x16_0, reversed_x16_15);
        let max_x16_015 = _mm512_max_epu32(*inout_x16_0, reversed_x16_15);
        let min_x16_114 = _mm512_min_epu32(*inout_x16_1, reversed_x16_14);
        let max_x16_114 = _mm512_max_epu32(*inout_x16_1, reversed_x16_14);
        let min_x16_213 = _mm512_min_epu32(*inout_x16_2, reversed_x16_13);
        let max_x16_213 = _mm512_max_epu32(*inout_x16_2, reversed_x16_13);
        let min_x16_312 = _mm512_min_epu32(*inout_x16_3, reversed_x16_12);
        let max_x16_312 = _mm512_max_epu32(*inout_x16_3, reversed_x16_12);
        let min_x16_411 = _mm512_min_epu32(*inout_x16_4, reversed_x16_11);
        let max_x16_411 = _mm512_max_epu32(*inout_x16_4, reversed_x16_11);
        let min_x16_510 = _mm512_min_epu32(*inout_x16_5, reversed_x16_10);
        let max_x16_510 = _mm512_max_epu32(*inout_x16_5, reversed_x16_10);
        let min_x16_69 = _mm512_min_epu32(*inout_x16_6, reversed_x16_9);
        let max_x16_69 = _mm512_max_epu32(*inout_x16_6, reversed_x16_9);
        let min_x16_78 = _mm512_min_epu32(*inout_x16_7, reversed_x16_8);
        let max_x16_78 = _mm512_max_epu32(*inout_x16_7, reversed_x16_8);

        *inout_x16_0 = min_x16_015;
        *inout_x16_1 = min_x16_114;
        *inout_x16_2 = min_x16_213;
        *inout_x16_3 = min_x16_312;
        *inout_x16_4 = min_x16_411;
        *inout_x16_5 = min_x16_510;
        *inout_x16_6 = min_x16_69;
        *inout_x16_7 = min_x16_78;

        *inout_x16_8 = max_x16_015;
        *inout_x16_9 = max_x16_114;
        *inout_x16_10 = max_x16_213;
        *inout_x16_11 = max_x16_312;
        *inout_x16_12 = max_x16_411;
        *inout_x16_13 = max_x16_510;
        *inout_x16_14 = max_x16_69;
        *inout_x16_15 = max_x16_78;

        sort128(
            inout_x16_0,
            inout_x16_1,
            inout_x16_2,
            inout_x16_3,
            inout_x16_4,
            inout_x16_5,
            inout_x16_6,
            inout_x16_7,
        );
        sort128(
            inout_x16_8,
            inout_x16_9,
            inout_x16_10,
            inout_x16_11,
            inout_x16_12,
            inout_x16_13,
            inout_x16_14,
            inout_x16_15,
        );
    }
}

/// Sorts a fixed-allocation array of n elements (up to 256) in const time
/// using avx512 accelerated bitonic sorting networks. Sorting is applied
/// in ascending order. The array is sorted in-place, padding the array with 0xFFFF
/// or another sentinel value should be done by the caller.
#[inline(always)]
pub fn sort_256x32_asc_avx512(buf: &mut [u32; 256], n: usize) {
    if n <= 4 {
        sort4(&mut buf[0..4]);
        return;
    }

    if n <= 8 {
        sort8(&mut buf[0..8]);
        return;
    }

    let seg_size = calc_seg_size!(n);

    unsafe {
        match seg_size {
            16 => {
                let mut input_x16 = _mm512_loadu_si512(buf.as_ptr() as *const __m512i);
                sort16(&mut input_x16);
                _mm512_storeu_si512(buf.as_mut_ptr() as *mut __m512i, input_x16);
            }
            32 => {
                let ptr = buf.as_ptr() as *const __m512i;
                let mut input_x16_0 = _mm512_loadu_si512(ptr);
                let mut input_x16_1 = _mm512_loadu_si512(ptr.add(1));

                sort32(&mut input_x16_0, &mut input_x16_1);

                let ptr = buf.as_mut_ptr() as *mut __m512i;
                _mm512_storeu_si512(ptr, input_x16_0);
                _mm512_storeu_si512(ptr.add(1), input_x16_1);
            }
            64 => {
                let ptr = buf.as_ptr() as *const __m512i;

                let mut input_x16_0 = _mm512_loadu_si512(ptr);
                let mut input_x16_1 = _mm512_loadu_si512(ptr.add(1));
                let mut input_x16_2 = _mm512_loadu_si512(ptr.add(2));
                let mut input_x16_3 = _mm512_loadu_si512(ptr.add(3));

                sort64(
                    &mut input_x16_0,
                    &mut input_x16_1,
                    &mut input_x16_2,
                    &mut input_x16_3,
                );

                let ptr = buf.as_mut_ptr() as *mut __m512i;
                _mm512_storeu_si512(ptr, input_x16_0);
                _mm512_storeu_si512(ptr.add(1), input_x16_1);
                _mm512_storeu_si512(ptr.add(2), input_x16_2);
                _mm512_storeu_si512(ptr.add(3), input_x16_3);
            }
            128 => {
                std::hint::cold_path();

                let ptr = buf.as_ptr() as *const __m512i;

                let mut input_x16_0 = _mm512_loadu_si512(ptr);
                let mut input_x16_1 = _mm512_loadu_si512(ptr.add(1));
                let mut input_x16_2 = _mm512_loadu_si512(ptr.add(2));
                let mut input_x16_3 = _mm512_loadu_si512(ptr.add(3));
                let mut input_x16_4 = _mm512_loadu_si512(ptr.add(4));
                let mut input_x16_5 = _mm512_loadu_si512(ptr.add(5));
                let mut input_x16_6 = _mm512_loadu_si512(ptr.add(6));
                let mut input_x16_7 = _mm512_loadu_si512(ptr.add(7));

                sort128(
                    &mut input_x16_0,
                    &mut input_x16_1,
                    &mut input_x16_2,
                    &mut input_x16_3,
                    &mut input_x16_4,
                    &mut input_x16_5,
                    &mut input_x16_6,
                    &mut input_x16_7,
                );

                let ptr = buf.as_mut_ptr() as *mut __m512i;
                _mm512_storeu_si512(ptr, input_x16_0);
                _mm512_storeu_si512(ptr.add(1), input_x16_1);
                _mm512_storeu_si512(ptr.add(2), input_x16_2);
                _mm512_storeu_si512(ptr.add(3), input_x16_3);
                _mm512_storeu_si512(ptr.add(4), input_x16_4);
                _mm512_storeu_si512(ptr.add(5), input_x16_5);
                _mm512_storeu_si512(ptr.add(6), input_x16_6);
                _mm512_storeu_si512(ptr.add(7), input_x16_7);
            }
            _ => {
                std::hint::cold_path();

                let ptr = buf.as_ptr() as *const __m512i;

                let mut input_x16_0 = _mm512_loadu_si512(ptr);
                let mut input_x16_1 = _mm512_loadu_si512(ptr.add(1));
                let mut input_x16_2 = _mm512_loadu_si512(ptr.add(2));
                let mut input_x16_3 = _mm512_loadu_si512(ptr.add(3));
                let mut input_x16_4 = _mm512_loadu_si512(ptr.add(4));
                let mut input_x16_5 = _mm512_loadu_si512(ptr.add(5));
                let mut input_x16_6 = _mm512_loadu_si512(ptr.add(6));
                let mut input_x16_7 = _mm512_loadu_si512(ptr.add(7));
                let mut input_x16_8 = _mm512_loadu_si512(ptr.add(8));
                let mut input_x16_9 = _mm512_loadu_si512(ptr.add(9));
                let mut input_x16_10 = _mm512_loadu_si512(ptr.add(10));
                let mut input_x16_11 = _mm512_loadu_si512(ptr.add(11));
                let mut input_x16_12 = _mm512_loadu_si512(ptr.add(12));
                let mut input_x16_13 = _mm512_loadu_si512(ptr.add(13));
                let mut input_x16_14 = _mm512_loadu_si512(ptr.add(14));
                let mut input_x16_15 = _mm512_loadu_si512(ptr.add(15));

                sort256(
                    &mut input_x16_0,
                    &mut input_x16_1,
                    &mut input_x16_2,
                    &mut input_x16_3,
                    &mut input_x16_4,
                    &mut input_x16_5,
                    &mut input_x16_6,
                    &mut input_x16_7,
                    &mut input_x16_8,
                    &mut input_x16_9,
                    &mut input_x16_10,
                    &mut input_x16_11,
                    &mut input_x16_12,
                    &mut input_x16_13,
                    &mut input_x16_14,
                    &mut input_x16_15,
                );

                let ptr = buf.as_mut_ptr() as *mut __m512i;
                _mm512_storeu_si512(ptr, input_x16_0);
                _mm512_storeu_si512(ptr.add(1), input_x16_1);
                _mm512_storeu_si512(ptr.add(2), input_x16_2);
                _mm512_storeu_si512(ptr.add(3), input_x16_3);
                _mm512_storeu_si512(ptr.add(4), input_x16_4);
                _mm512_storeu_si512(ptr.add(5), input_x16_5);
                _mm512_storeu_si512(ptr.add(6), input_x16_6);
                _mm512_storeu_si512(ptr.add(7), input_x16_7);
                _mm512_storeu_si512(ptr.add(8), input_x16_8);
                _mm512_storeu_si512(ptr.add(9), input_x16_9);
                _mm512_storeu_si512(ptr.add(10), input_x16_10);
                _mm512_storeu_si512(ptr.add(11), input_x16_11);
                _mm512_storeu_si512(ptr.add(12), input_x16_12);
                _mm512_storeu_si512(ptr.add(13), input_x16_13);
                _mm512_storeu_si512(ptr.add(14), input_x16_14);
                _mm512_storeu_si512(ptr.add(15), input_x16_15);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::hint::black_box;

    pub fn rdtscp() -> u64 {
        let mut aux: u32 = 0;
        unsafe { std::arch::x86_64::__rdtscp(&mut aux as *mut u32) as u64 }
    }

    #[test]
    fn sorting_bench() {
        use rand::{Rng, SeedableRng};
        let mut rng = rand::rngs::StdRng::seed_from_u64(black_box(42));

        core_affinity::set_for_current(core_affinity::CoreId { id: 15 });

        for _ in 0..5 {
            const ARR_SIZE_MAX: usize = 70;
            const ARR_SIZE_MIN: usize = 3;
            const ITERATIONS: usize = 10_000_000;

            let start = rdtscp();

            for _ in 0..ITERATIONS {
                let mut arr = [0xFFFFFFFFu32; 256];
                let len = rng.random_range(ARR_SIZE_MIN..=ARR_SIZE_MAX);
                for i in ARR_SIZE_MIN..len {
                    arr[i] = rng.random_range(0..u32::MAX);
                }

                sort_256x32_asc_avx512(&mut arr, len);

                black_box(&arr);
                arr[0] = arr[1];
            }

            let end = rdtscp();

            let cycles_per_iter = (end - start) as f64 / ITERATIONS as f64;

            println!(
                "Sorting {}-{} u32 elements took {:.2} cycles per iteration",
                ARR_SIZE_MIN, ARR_SIZE_MAX, cycles_per_iter
            );
        }
    }

    #[test]
    fn sorting_fuzz() {
        use rand::{Rng, SeedableRng};
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);

        const ARR_SIZE: usize = 256;

        for _ in 0..128 {
            for arr_size in 0..=ARR_SIZE {
                let mut random_arr = [0xFFFFFFFFu32; 256];

                for i in 0..arr_size {
                    random_arr[i] = rng.random_range(0..u32::MAX);
                }

                let mut arr_copy = random_arr;
                sort_256x32_asc_avx512(&mut arr_copy, arr_size);

                assert!(
                    arr_copy[0..arr_size].is_sorted(),
                    "Array not sorted at array size {} / seg size {}\nOriginal: {:?}\nSorted: {:?}",
                    arr_size,
                    if arr_size <= 8 {
                        8
                    } else {
                        calc_seg_size!(arr_size)
                    },
                    &random_arr[0..arr_size],
                    &arr_copy[0..arr_size]
                );

                let mut array_vec = arr_copy.to_vec();
                for i in 0..arr_size {
                    let index = array_vec.iter().position(|&value| value == random_arr[i]);
                    assert!(
                        index.is_some(),
                        "Element {} not found in sorted array.\nOriginal: {:?}\nSorted: {:?}",
                        random_arr[i],
                        &random_arr[0..arr_size],
                        &arr_copy[0..arr_size]
                    );

                    array_vec.remove(index.unwrap());
                }
            }
        }
    }
}
