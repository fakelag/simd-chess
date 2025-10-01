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
        let diff = (b as i32).wrapping_sub(a as i32);
        let swap_mask = (diff >> 31) as u32;
        let min_val = ((b as u32) & swap_mask) | ((a as u32) & !swap_mask);
        let max_val = ((a as u32) & swap_mask) | ((b as u32) & !swap_mask);
        $arr[$i] = min_val as u16;
        $arr[$j] = max_val as u16;
    }};
}

macro_rules! cmp_swap_x32_epu16 {
    ($input:expr, $perm:expr, $mask:expr) => {{
        let input = $input;
        let permuted_x32 = _mm512_permutexvar_epi16($perm, input);
        let min_x32 = _mm512_min_epu16(input, permuted_x32);
        let max_x32 = _mm512_max_epu16(input, permuted_x32);
        _mm512_mask_blend_epi16($mask, max_x32, min_x32)
    }};
}

macro_rules! cmp_swap_x16_epu16 {
    ($input:expr, $perm:expr, $mask:expr) => {{
        let input = $input;
        let permuted_x16 = _mm256_permutexvar_epi16($perm, input);
        let min_x16 = _mm256_min_epu16(input, permuted_x16);
        let max_x16 = _mm256_max_epu16(input, permuted_x16);
        _mm256_mask_blend_epi16($mask, max_x16, min_x16)
    }};
}

macro_rules! cmp_swap_x8_epu16 {
    ($input:expr, $perm:expr, $mask:expr) => {{
        let input = $input;
        let permuted_x8 = _mm_permutexvar_epi16($perm, input);
        let min_x8 = _mm_min_epu16(input, permuted_x8);
        let max_x8 = _mm_max_epu16(input, permuted_x8);
        _mm_mask_blend_epi16($mask, max_x8, min_x8)
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

#[inline(always)]
fn sort4(arr: &mut [u16]) {
    debug_assert!(arr.len() == 4);

    cmp_swap_arr!(arr, 0, 1);
    cmp_swap_arr!(arr, 2, 3);
    cmp_swap_arr!(arr, 0, 2);
    cmp_swap_arr!(arr, 1, 3);
    cmp_swap_arr!(arr, 1, 2);
}

#[inline(always)]
fn sort8(arr: &mut [u16]) {
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
pub fn sort16(inout_x16: &mut __m256i) {
    unsafe {
        // fn print_m256_epu16(label: &str, a: &__m256i) {
        //     let mut arr: [u16; 16] = [0u16; 16];
        //     unsafe {
        //         _mm256_storeu_si256(arr.as_mut_ptr() as *mut __m256i, *a);
        //     }
        //     println!("{}: {:?}", label, arr);
        // }

        // fn print_m512_epu16(label: &str, a: &__m512i) {
        //     let mut arr: [u16; 32] = [0u16; 32];
        //     unsafe {
        //         _mm512_storeu_si512(arr.as_mut_ptr() as *mut __m512i, *a);
        //     }
        //     println!("{}: {:?}", label, arr);
        // }

        const PERM_SELECT_B: u16 = 0x10;

        let identity_x16 = _mm256_set_epi16(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0);
        // let identity_x8 = _mm_set_epi16(7, 6, 5, 4, 3, 2, 1, 0);

        let const_1_x16 = _mm256_set1_epi16(1);
        let const_2_x16 = _mm256_set1_epi16(2);
        let const_7_x16 = _mm256_set1_epi16(7);

        let const_perm_rev1_x16 = _mm256_set_epi16(
            0, 0, 0, 0, 0, 0, 0, 0, // unused
            8, 9, 10, 11, 12, 13, 14, 15, // reverse order
        );

        let sort4_m256 = |inout_x16: &mut __m256i| {
            let low_adj_mask = 0x5555u16;
            let low_gap2_mask = 0x3333u16;
            let low_mid12_mask = 0x2222u16;

            let perm_x32 = _mm256_xor_si256(identity_x16, const_1_x16);
            *inout_x16 = cmp_swap_x16_epu16!(*inout_x16, perm_x32, low_adj_mask);

            let perm_x32 = _mm256_xor_si256(identity_x16, const_2_x16);
            *inout_x16 = cmp_swap_x16_epu16!(*inout_x16, perm_x32, low_gap2_mask);

            let perm_x16 = _mm256_set_epi16(15, 13, 14, 12, 11, 9, 10, 8, 7, 5, 6, 4, 3, 1, 2, 0);
            *inout_x16 = cmp_swap_x16_epu16!(*inout_x16, perm_x16, low_mid12_mask);
        };

        let crosscompare_m256 = |inout_x16: &mut __m256i| {
            let low_rev8_mask = 0x0F0Fu16;

            let perm_x16 = _mm256_xor_si256(identity_x16, const_7_x16);
            *inout_x16 = cmp_swap_x16_epu16!(*inout_x16, perm_x16, low_rev8_mask);
        };

        let combine_x8_m256 = |inout_x16: &mut __m256i| {
            let permuted_x16 = _mm256_permutexvar_epi16(const_perm_rev1_x16, *inout_x16);

            let min_x32 = _mm256_min_epu16(*inout_x16, permuted_x16);
            let max_x32 = _mm256_max_epu16(*inout_x16, permuted_x16);

            let combine_perm_x16 = _mm256_set_epi16(
                (7 | PERM_SELECT_B) as i16,
                (6 | PERM_SELECT_B) as i16,
                (5 | PERM_SELECT_B) as i16,
                (4 | PERM_SELECT_B) as i16,
                (3 | PERM_SELECT_B) as i16,
                (2 | PERM_SELECT_B) as i16,
                (1 | PERM_SELECT_B) as i16,
                (0 | PERM_SELECT_B) as i16,
                7,
                6,
                5,
                4,
                3,
                2,
                1,
                0,
            );
            *inout_x16 = _mm256_permutex2var_epi16(min_x32, combine_perm_x16, max_x32);
        };

        sort4_m256(inout_x16);
        crosscompare_m256(inout_x16);
        sort4_m256(inout_x16);

        combine_x8_m256(inout_x16);

        sort4_m256(inout_x16);
        crosscompare_m256(inout_x16);
        sort4_m256(inout_x16);
    }
}

#[inline(always)]
pub fn sort32(inout_x32: &mut __m512i) {
    unsafe {
        const PERM_SELECT_B: u16 = 0x20;

        let identity_x32 = _mm512_set_epi16(
            31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,
            9, 8, 7, 6, 5, 4, 3, 2, 1, 0,
        );

        let const_1_x32 = _mm512_set1_epi16(1);
        let const_2_x32 = _mm512_set1_epi16(2);
        let const_7_x32 = _mm512_set1_epi16(7);

        let const_perm_rev2_x32 = _mm512_set_epi16(
            0, 0, 0, 0, 0, 0, 0, 0, // unused
            24, 25, 26, 27, 28, 29, 30, 31, // reverse order
            0, 0, 0, 0, 0, 0, 0, 0, // unused
            8, 9, 10, 11, 12, 13, 14, 15, // reverse order
        );
        let const_perm_rev1_x32 = _mm512_set_epi16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // dont care
            16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, // reverse order
        );

        let sort4_m512 = |inout_x32: &mut __m512i| {
            let low_adj_mask = 0x55555555u32;
            let low_gap2_mask = 0x33333333u32;
            let low_mid12_mask = 0x22222222u32;

            let perm_x32 = _mm512_xor_si512(identity_x32, const_1_x32);
            *inout_x32 = cmp_swap_x32_epu16!(*inout_x32, perm_x32, low_adj_mask);

            let perm_x32 = _mm512_xor_si512(identity_x32, const_2_x32);
            *inout_x32 = cmp_swap_x32_epu16!(*inout_x32, perm_x32, low_gap2_mask);

            let perm_x32 = _mm512_set_epi16(
                31, 29, 30, 28, 27, 25, 26, 24, 23, 21, 22, 20, 19, 17, 18, 16, 15, 13, 14, 12, 11,
                9, 10, 8, 7, 5, 6, 4, 3, 1, 2, 0,
            );
            *inout_x32 = cmp_swap_x32_epu16!(*inout_x32, perm_x32, low_mid12_mask);
        };

        let crosscompare_m512 = |inout_x32: &mut __m512i| {
            let low_rev8_mask = 0x0F0F0F0Fu32;

            let perm_x32 = _mm512_xor_si512(identity_x32, const_7_x32);
            *inout_x32 = cmp_swap_x32_epu16!(*inout_x32, perm_x32, low_rev8_mask);
        };

        let combine_x8_m512 = |inout_x32: &mut __m512i| {
            let permuted_x32 = _mm512_permutexvar_epi16(const_perm_rev2_x32, *inout_x32);

            let min_x32 = _mm512_min_epu16(*inout_x32, permuted_x32);
            let max_x32 = _mm512_max_epu16(*inout_x32, permuted_x32);

            let combine_perm_x32 = _mm512_set_epi16(
                (23 | PERM_SELECT_B) as i16,
                (22 | PERM_SELECT_B) as i16,
                (21 | PERM_SELECT_B) as i16,
                (20 | PERM_SELECT_B) as i16,
                (19 | PERM_SELECT_B) as i16,
                (18 | PERM_SELECT_B) as i16,
                (17 | PERM_SELECT_B) as i16,
                (16 | PERM_SELECT_B) as i16,
                23,
                22,
                21,
                20,
                19,
                18,
                17,
                16,
                (7 | PERM_SELECT_B) as i16,
                (6 | PERM_SELECT_B) as i16,
                (5 | PERM_SELECT_B) as i16,
                (4 | PERM_SELECT_B) as i16,
                (3 | PERM_SELECT_B) as i16,
                (2 | PERM_SELECT_B) as i16,
                (1 | PERM_SELECT_B) as i16,
                (0 | PERM_SELECT_B) as i16,
                7,
                6,
                5,
                4,
                3,
                2,
                1,
                0,
            );
            *inout_x32 = _mm512_permutex2var_epi16(min_x32, combine_perm_x32, max_x32);
        };

        let combine_x16_m512 = |inout_x32: &mut __m512i| {
            let permuted_x32 = _mm512_permutexvar_epi16(const_perm_rev1_x32, *inout_x32);

            let min_x32 = _mm512_min_epu16(*inout_x32, permuted_x32);
            let max_x32 = _mm512_max_epu16(*inout_x32, permuted_x32);

            let combine_perm_x32 = _mm512_set_epi16(
                (15 | PERM_SELECT_B) as i16,
                (14 | PERM_SELECT_B) as i16,
                (13 | PERM_SELECT_B) as i16,
                (12 | PERM_SELECT_B) as i16,
                (11 | PERM_SELECT_B) as i16,
                (10 | PERM_SELECT_B) as i16,
                (9 | PERM_SELECT_B) as i16,
                (8 | PERM_SELECT_B) as i16,
                (7 | PERM_SELECT_B) as i16,
                (6 | PERM_SELECT_B) as i16,
                (5 | PERM_SELECT_B) as i16,
                (4 | PERM_SELECT_B) as i16,
                (3 | PERM_SELECT_B) as i16,
                (2 | PERM_SELECT_B) as i16,
                (1 | PERM_SELECT_B) as i16,
                (0 | PERM_SELECT_B) as i16,
                15,
                14,
                13,
                12,
                11,
                10,
                9,
                8,
                7,
                6,
                5,
                4,
                3,
                2,
                1,
                0,
            );
            *inout_x32 = _mm512_permutex2var_epi16(min_x32, combine_perm_x32, max_x32);
        };

        sort4_m512(inout_x32);
        crosscompare_m512(inout_x32);
        sort4_m512(inout_x32);

        combine_x8_m512(inout_x32);

        sort4_m512(inout_x32);
        crosscompare_m512(inout_x32);
        sort4_m512(inout_x32);

        combine_x16_m512(inout_x32);

        // 16-bit halves have smaller values in the lower half, larger values in the upper half.

        sort4_m512(inout_x32);
        crosscompare_m512(inout_x32);
        sort4_m512(inout_x32);

        combine_x8_m512(inout_x32);

        sort4_m512(inout_x32);
        crosscompare_m512(inout_x32);
        sort4_m512(inout_x32);
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

/// Sorts a fixed-allocation array of n elements (up to 256) in const time
/// using avx512 accelerated bitonic sorting networks. Sorting is applied
/// in ascending order. The array is sorted in-place, padding the array with 0xFFFF
/// or another sentinel value should be done by the caller.
#[inline(always)]
pub fn sort_256x16_asc_avx512(buf: &mut [u16; 256], n: usize) {
    if n <= 8 {
        sort8(&mut buf[0..8]);
        return;
    }

    let seg_size = calc_seg_size!(n);

    unsafe {
        match seg_size {
            16 => {
                let mut input_x16 = _mm256_loadu_si256(buf.as_ptr() as *const __m256i);
                sort16(&mut input_x16);
                _mm256_storeu_si256(buf.as_mut_ptr() as *mut __m256i, input_x16);
            }
            32 => {
                let mut input_x32 = _mm512_loadu_si512(buf.as_ptr() as *const __m512i);
                sort32(&mut input_x32);
                _mm512_storeu_si512(buf.as_mut_ptr() as *mut __m512i, input_x32);
            }
            64 => {
                let ptr = buf.as_ptr() as *const __m512i;

                let mut input_x32_0 = _mm512_loadu_si512(ptr);
                let mut input_x32_1 = _mm512_loadu_si512(ptr.add(1));

                sort64(&mut input_x32_0, &mut input_x32_1);

                _mm512_storeu_si512(buf.as_mut_ptr() as *mut __m512i, input_x32_0);
                _mm512_storeu_si512(buf.as_mut_ptr().add(32) as *mut __m512i, input_x32_1);
            }
            128 => {
                std::hint::cold_path();

                let ptr = buf.as_ptr() as *const __m512i;

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

                _mm512_storeu_si512(buf.as_mut_ptr() as *mut __m512i, input_x32_0);
                _mm512_storeu_si512(buf.as_mut_ptr().add(32) as *mut __m512i, input_x32_1);
                _mm512_storeu_si512(buf.as_mut_ptr().add(64) as *mut __m512i, input_x32_2);
                _mm512_storeu_si512(buf.as_mut_ptr().add(96) as *mut __m512i, input_x32_3);
            }
            _ => {
                std::hint::cold_path();

                let ptr = buf.as_ptr() as *const __m512i;

                let mut input_x32_0 = _mm512_loadu_si512(ptr);
                let mut input_x32_1 = _mm512_loadu_si512(ptr.add(1));
                let mut input_x32_2 = _mm512_loadu_si512(ptr.add(2));
                let mut input_x32_3 = _mm512_loadu_si512(ptr.add(3));
                let mut input_x32_4 = _mm512_loadu_si512(ptr.add(4));
                let mut input_x32_5 = _mm512_loadu_si512(ptr.add(5));
                let mut input_x32_6 = _mm512_loadu_si512(ptr.add(6));
                let mut input_x32_7 = _mm512_loadu_si512(ptr.add(7));

                sort256(
                    &mut input_x32_0,
                    &mut input_x32_1,
                    &mut input_x32_2,
                    &mut input_x32_3,
                    &mut input_x32_4,
                    &mut input_x32_5,
                    &mut input_x32_6,
                    &mut input_x32_7,
                );

                _mm512_storeu_si512(buf.as_mut_ptr() as *mut __m512i, input_x32_0);
                _mm512_storeu_si512(buf.as_mut_ptr().add(32) as *mut __m512i, input_x32_1);
                _mm512_storeu_si512(buf.as_mut_ptr().add(64) as *mut __m512i, input_x32_2);
                _mm512_storeu_si512(buf.as_mut_ptr().add(96) as *mut __m512i, input_x32_3);
                _mm512_storeu_si512(buf.as_mut_ptr().add(128) as *mut __m512i, input_x32_4);
                _mm512_storeu_si512(buf.as_mut_ptr().add(160) as *mut __m512i, input_x32_5);
                _mm512_storeu_si512(buf.as_mut_ptr().add(192) as *mut __m512i, input_x32_6);
                _mm512_storeu_si512(buf.as_mut_ptr().add(224) as *mut __m512i, input_x32_7);
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
                let mut arr = [0xFFFFu16; 256];
                let len = rng.random_range(ARR_SIZE_MIN..=ARR_SIZE_MAX);
                for i in ARR_SIZE_MIN..len {
                    arr[i] = rng.random_range(0..u16::MAX);
                }

                sort_256x16_asc_avx512(&mut arr, len);

                black_box(&arr);
                arr[0] = arr[1];
            }

            let end = rdtscp();

            let cycles_per_iter = (end - start) as f64 / ITERATIONS as f64;

            println!(
                "Sorting {}-{} u16 elements took {:.2} cycles per iteration",
                ARR_SIZE_MIN, ARR_SIZE_MAX, cycles_per_iter
            );
        }
    }

    #[test]
    fn sorting_fuzz() {
        use rand::{Rng, SeedableRng};
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);

        for _ in 0..128 {
            for arr_size in 0..256 {
                let mut random_arr = [0xFFFFu16; 256];

                for i in 0..arr_size {
                    random_arr[i] = rng.random_range(0..u16::MAX);
                }

                let mut arr_copy = random_arr;
                sort_256x16_asc_avx512(&mut arr_copy, arr_size);

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
                        "Element {} not found in sorted array",
                        random_arr[i]
                    );

                    array_vec.remove(index.unwrap());
                }
            }
        }
    }
}
