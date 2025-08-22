#[inline(never)]
pub fn gen_moves_ordered_avx512<const CAPTURE_ONLY: bool>(
    &self,
    tables: &Tables,
    move_list: &mut [u16],
    scratch: &mut MovegenScratch,
    tt_move: u16,
    beta_1: u16,
    beta_2: u16,
    mut stats: Option<&mut Stats>,
) -> (usize, bool, bool, bool) {
    use CaptureMoves::*;

    let mut tt_found = 0u8;
    let mut beta1_found = 0u8;
    let mut beta2_found = 0u8;

    for i in 0..32 {
        scratch.capture_ptr[i] = scratch.capture_list[i].as_mut_ptr() as *mut u16;
    }

    let mut quiet_cursor = 0usize;

    unsafe {
        let bitboards = &self.board.bitboards;
        let b_move = self.b_move;

        let tt_move_epi16_x8 = _mm_set1_epi16(tt_move as i16);
        let beta1_epi16_x8 = _mm_set1_epi16(beta_1 as i16);
        let beta2_epi16_x8 = _mm_set1_epi16(beta_2 as i16);

        let friendly_move_offset = (b_move as usize) << 3;
        let opponent_move_offset = (!b_move as usize) << 3;

        let const_nonslider_selector = _mm512_set_epi64(
            PieceIndex::WhitePawn as i64,
            PieceIndex::WhitePawn as i64,
            PieceIndex::WhitePawn as i64,
            PieceIndex::WhitePawn as i64,
            PieceIndex::WhitePawn as i64,
            PieceIndex::WhiteKnight as i64,
            PieceIndex::WhiteKnight as i64,
            PieceIndex::WhiteKing as i64,
        );
        let const_nonslider_split = _mm512_set_epi64(
            0x4040404040404040u64 as i64,
            0x2020202020202020u64 as i64,
            0x404040404040404u64 as i64,
            0x1212121212121212u64 as i64,
            0x8989898989898989u64 as i64,
            0xF0F0F0F0F0F0F0F0u64 as i64,
            0x0F0F0F0F0F0F0F0Fu64 as i64,
            0xFFFFFFFF_FFFFFFFFu64 as i64,
        );
        const PAWN_LANES: u8 = 0b11111000;
        const KNIGHT_LANES: u8 = 0b00000110;
        const KING_LANES: u8 = 0b00000001;

        let const_slider_selector = _mm512_set_epi64(
            PieceIndex::WhiteQueen as i64,
            PieceIndex::WhiteQueen as i64,
            PieceIndex::WhiteRook as i64,
            PieceIndex::WhiteRook as i64,
            PieceIndex::WhiteRook as i64,
            PieceIndex::WhiteRook as i64,
            PieceIndex::WhiteBishop as i64,
            PieceIndex::WhiteBishop as i64,
        );

        let const_slider_split = _mm512_set_epi64(
            0xFFFFFFFF_FFFFFFFFu64 as i64,
            0xFFFFFFFF_FFFFFFFFu64 as i64,
            0x050A050A050A050Au64 as i64, // left light squares
            0x0A050A050A050A05u64 as i64, // left black squares
            0x50A050A050A050A0u64 as i64, // right light squares
            0xA050A050A050A050u64 as i64, // right black squares
            0xAA55AA55AA55AA55u64 as i64, // black squares
            0x55AA55AA55AA55AAu64 as i64, // light squares
        );
        // 0x0 = rook, 0x40 = bishop
        let const_slider_gather_magic_masks_offsets_x8 =
            _mm512_set_epi64(0x40, 0, 0, 0, 0, 0, 0x40, 0x40);
        let const_slider_gather_moves_shifts_x8 = _mm512_set_epi64(
            Tables::BISHOP_OCCUPANCY_BITS as i64,
            Tables::ROOK_OCCUPANCY_BITS as i64,
            Tables::ROOK_OCCUPANCY_BITS as i64,
            Tables::ROOK_OCCUPANCY_BITS as i64,
            Tables::ROOK_OCCUPANCY_BITS as i64,
            Tables::ROOK_OCCUPANCY_BITS as i64,
            Tables::BISHOP_OCCUPANCY_BITS as i64,
            Tables::BISHOP_OCCUPANCY_BITS as i64,
        );
        const BISHOP_MV_GATHER_OFFSET: i64 = (64 * Tables::ROOK_OCCUPANCY_MAX) as i64;
        let const_moves_gather_base_offsets_x8 = _mm512_set_epi64(
            BISHOP_MV_GATHER_OFFSET,
            0,
            0,
            0,
            0,
            0,
            BISHOP_MV_GATHER_OFFSET,
            BISHOP_MV_GATHER_OFFSET,
        );
        const BISHOP_LANES: u8 = 0b00000011;
        const ROOK_LANES: u8 = 0b00111100;
        const QUEEN_LANES: u8 = 0b11000000;

        let bitboard_x8 =
            _mm512_load_si512(bitboards.as_ptr().add(friendly_move_offset) as *const __m512i);

        let mut sliders_x8 = _mm512_and_epi64(
            _mm512_permutex2var_epi64(bitboard_x8, const_slider_selector, bitboard_x8),
            const_slider_split,
        );

        let mut non_sliders_x8 = _mm512_and_epi64(
            _mm512_permutex2var_epi64(bitboard_x8, const_nonslider_selector, bitboard_x8),
            const_nonslider_split,
        );

        let full_board = bitboards.iter().fold(0, |acc, &bb| acc | bb);
        let friendly_board = _mm512_reduce_or_epi64(bitboard_x8) as u64;
        let opponent_board = bitboards[opponent_move_offset..opponent_move_offset + 8]
            .iter()
            .fold(0, |acc, &bb| acc | bb);

        let opponent_queen_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhiteQueen as usize + opponent_move_offset] as i64,
        );
        let opponent_rook_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhiteRook as usize + opponent_move_offset] as i64,
        );
        let opponent_bishop_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhiteBishop as usize + opponent_move_offset] as i64,
        );
        let opponent_knight_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhiteKnight as usize + opponent_move_offset] as i64,
        );
        let opponent_pawn_x8 = _mm512_set1_epi64(
            bitboards[PieceIndex::WhitePawn as usize + opponent_move_offset] as i64,
        );
        let opponent_ep_x8 = _mm512_set1_epi64((1u64 << self.en_passant >> 1 << 1) as i64);

        let const_63_x8 = _mm512_set1_epi64(63);
        let const_64_x8 = _mm512_set1_epi64(64);
        let const_1_x8 = _mm512_set1_epi64(1);
        let const_n1_x8 = _mm512_set1_epi64(-1);
        let const_zero_x8 = _mm512_setzero_si512();

        // Flags
        let const_promotion_flag_x8 = _mm512_set1_epi64(MV_FLAGS_PR_KNIGHT as u64 as i64);
        let const_epcap_flag_x8 = _mm512_set1_epi64(MV_FLAG_EPCAP as u64 as i64);
        let const_cap_flag_x8 = _mm512_set1_epi64(MV_FLAG_CAP as u64 as i64);
        let const_dpp_flag_x8 = _mm512_set1_epi64(MV_FLAG_DPP as u64 as i64);

        let b_move_rank_offset = (56 * (b_move as u64)) as u8;

        let friendly_move_offset_x8 = _mm512_set1_epi64(friendly_move_offset as i64);
        let full_board_x8 = _mm512_set1_epi64(full_board as i64);
        let full_board_inv_x8 = _mm512_set1_epi64(!full_board as i64);
        let friendly_board_inv_x8 = _mm512_set1_epi64(!friendly_board as i64);
        let opponent_board_x8 = _mm512_set1_epi64(opponent_board as i64);
        let pawn_promotion_rank_x8 =
            _mm512_set1_epi64((0xFF00000000000000u64 >> b_move_rank_offset) as i64);
        let pawn_double_push_rank_x8 =
            _mm512_set1_epi64((0xFF000000u64 << ((b_move as usize) << 3)) as i64);

        let pawn_push_offset_ranks =
            (opponent_move_offset as u64 + b_move_rank_offset as u64) as i64;
        let pawn_push_rank_rolv_offset_x8 = _mm512_set1_epi64(pawn_push_offset_ranks); // white=8, black=56

        // @todo - Try sub+and to pop ls1b instead of ms1b
        let mut one_of_each_slider_index_x8 =
            _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(sliders_x8));
        let mut one_of_each_non_slider_index_x8 =
            _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_sliders_x8));
        let mut active_pieces_non_slider_mask =
            _mm512_cmpneq_epi64_mask(one_of_each_non_slider_index_x8, const_n1_x8);
        let mut active_pieces_slider_mask =
            _mm512_cmpneq_epi64_mask(one_of_each_slider_index_x8, const_n1_x8);

        macro_rules! collect_stats {
            ($stats:ident, $field:ident, $vec:ident) => {
                let mut counts_arr = [0u64; 8];
                _mm512_storeu_si512(counts_arr.as_mut_ptr() as *mut __m512i, $vec);
                $stats.$field = $stats
                    .$field
                    .iter()
                    .enumerate()
                    .map(|(index, &prev)| prev + counts_arr[index] as u8)
                    .collect::<Vec<_>>()
                    .as_slice()
                    .try_into()
                    .unwrap();
            };
        }

        // if let Some(stats) = &mut stats {
        //     let nonslider_piece_counts_x8 = _mm512_popcnt_epi64(non_sliders_x8);
        //     let slider_piece_counts_x8 = _mm512_popcnt_epi64(sliders_x8);
        //     collect_stats!(stats, nonslider_piece_counts, nonslider_piece_counts_x8);
        //     collect_stats!(stats, slider_piece_counts, slider_piece_counts_x8);
        // }

        loop {
            let mut slider_moves_x8 = ChessGame::get_slider_moves_x8(
                tables,
                full_board_x8,
                one_of_each_slider_index_x8,
                const_slider_gather_magic_masks_offsets_x8,
                const_moves_gather_base_offsets_x8,
                const_slider_gather_moves_shifts_x8,
                active_pieces_slider_mask,
            );

            let pawn_mask = PAWN_LANES & active_pieces_non_slider_mask;
            let knight_mask = KNIGHT_LANES & active_pieces_non_slider_mask;
            let king_mask = KING_LANES & active_pieces_non_slider_mask;

            let bishop_mask = BISHOP_LANES & active_pieces_slider_mask;
            let rook_mask = ROOK_LANES & active_pieces_slider_mask;
            let queen_mask = QUEEN_LANES & active_pieces_slider_mask;

            let mut non_slider_moves_x8 = _mm512_mask_i64gather_epi64(
                _mm512_setzero_si512(),
                active_pieces_non_slider_mask,
                _mm512_add_epi64(
                    _mm512_mullo_epi64(
                        _mm512_add_epi64(const_nonslider_selector, friendly_move_offset_x8),
                        const_64_x8,
                    ),
                    one_of_each_non_slider_index_x8,
                ),
                Tables::LT_NON_SLIDER_MASKS_GATHER.0.as_ptr() as *const i64,
                8,
            );

            if CAPTURE_ONLY {
                // Mask out moves that don't capture opponent pieces
                slider_moves_x8 = _mm512_and_si512(slider_moves_x8, opponent_board_x8);
                non_slider_moves_x8 = _mm512_and_si512(non_slider_moves_x8, opponent_board_x8);
            } else {
                slider_moves_x8 = _mm512_and_si512(slider_moves_x8, friendly_board_inv_x8);
                non_slider_moves_x8 = _mm512_and_si512(non_slider_moves_x8, friendly_board_inv_x8);
            }

            macro_rules! priority_move_check {
                ($mask:expr, $mv_x8:ident) => {{
                    let mask = $mask;

                    tt_found |= _mm_mask_cmpeq_epi16_mask(mask, $mv_x8, tt_move_epi16_x8);
                    beta1_found |= _mm_mask_cmpeq_epi16_mask(mask, $mv_x8, beta1_epi16_x8);
                    beta2_found |= _mm_mask_cmpeq_epi16_mask(mask, $mv_x8, beta2_epi16_x8);
                }};
            }

            // Pawn push moves
            if !CAPTURE_ONLY {
                let src_sq_bit_x8 = _mm512_sllv_epi64(const_1_x8, one_of_each_non_slider_index_x8);
                let pawn_push_single_bit_x8 = _mm512_maskz_and_epi64(
                    pawn_mask,
                    _mm512_rolv_epi64(src_sq_bit_x8, pawn_push_rank_rolv_offset_x8),
                    full_board_inv_x8,
                );
                let promotion_mask =
                    _mm512_test_epi64_mask(pawn_push_single_bit_x8, pawn_promotion_rank_x8);
                let pawn_push_double_bit_x8 = _mm512_and_epi64(
                    _mm512_rolv_epi64(pawn_push_single_bit_x8, pawn_push_rank_rolv_offset_x8),
                    _mm512_and_epi64(full_board_inv_x8, pawn_double_push_rank_x8),
                );
                let pawn_push_single_dst_sq_x8 = _mm512_slli_epi64(
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(pawn_push_single_bit_x8)),
                    6,
                );
                let pawn_push_double_dst_sq_x8 = _mm512_slli_epi64(
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(pawn_push_double_bit_x8)),
                    6,
                );
                let pawn_push_single_mask =
                    pawn_mask & _mm512_cmpneq_epi64_mask(pawn_push_single_bit_x8, const_zero_x8);
                let pawn_push_double_mask =
                    pawn_mask & _mm512_cmpneq_epi64_mask(pawn_push_double_bit_x8, const_zero_x8);

                let mut pawn_push_single_move_x8 =
                    _mm512_or_epi64(pawn_push_single_dst_sq_x8, one_of_each_non_slider_index_x8);

                pawn_push_single_move_x8 = _mm512_mask_or_epi64(
                    pawn_push_single_move_x8,
                    promotion_mask,
                    pawn_push_single_move_x8,
                    const_promotion_flag_x8,
                );

                let pawn_push_single_move_epi16_x8 =
                    _mm512_cvtepi64_epi16(pawn_push_single_move_x8);
                let pawn_push_double_move_epi16_x8 = _mm512_cvtepi64_epi16(_mm512_or_epi64(
                    pawn_push_double_dst_sq_x8,
                    _mm512_or_epi64(one_of_each_non_slider_index_x8, const_dpp_flag_x8),
                ));

                priority_move_check!(pawn_push_single_mask, pawn_push_single_move_epi16_x8);
                priority_move_check!(pawn_push_double_mask, pawn_push_double_move_epi16_x8);

                _mm_mask_compressstoreu_epi16(
                    scratch.quiet_list.as_mut_ptr().add(quiet_cursor) as *mut i16,
                    pawn_push_single_mask,
                    pawn_push_single_move_epi16_x8,
                );
                quiet_cursor += pawn_push_single_mask.count_ones() as usize;
                _mm_mask_compressstoreu_epi16(
                    scratch.quiet_list.as_mut_ptr().add(quiet_cursor) as *mut i16,
                    pawn_push_double_mask,
                    pawn_push_double_move_epi16_x8,
                );
                quiet_cursor += pawn_push_double_mask.count_ones() as usize;
            }

            // if let Some(stats) = &mut stats {
            //     let nonslider_move_counts = _mm512_popcnt_epi64(non_slider_moves_x8);
            //     let slider_move_counts = _mm512_popcnt_epi64(slider_moves_x8);
            //     collect_stats!(stats, nonslider_move_counts, nonslider_move_counts);
            //     collect_stats!(stats, slider_move_counts, slider_move_counts);
            // }

            let mut non_slider_dst_sq_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_slider_moves_x8));
            let mut slider_dst_sq_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(slider_moves_x8));
            loop {
                // Dst square bits for masking
                let non_slider_dst_sq_bit_x8 = _mm512_sllv_epi64(const_1_x8, non_slider_dst_sq_x8);

                let mut non_slider_full_move_x8 = _mm512_or_epi64(
                    _mm512_slli_epi64(non_slider_dst_sq_x8, 6),
                    one_of_each_non_slider_index_x8,
                );

                // Promotion flag for pawn moves on the last rank
                // NOTE: This requires special handling on move maker side to try out other promotions
                let promotion_mask = pawn_mask
                    & _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, pawn_promotion_rank_x8);

                // EP flag for en passant captures
                let ep_mask =
                    pawn_mask & _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_ep_x8);

                // Capture flag
                let cap_mask = _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_board_x8);

                non_slider_full_move_x8 = _mm512_mask_or_epi64(
                    non_slider_full_move_x8,
                    promotion_mask,
                    non_slider_full_move_x8,
                    const_promotion_flag_x8,
                );
                non_slider_full_move_x8 = _mm512_mask_or_epi64(
                    non_slider_full_move_x8,
                    ep_mask,
                    non_slider_full_move_x8,
                    const_epcap_flag_x8,
                );
                non_slider_full_move_x8 = _mm512_mask_or_epi64(
                    non_slider_full_move_x8,
                    cap_mask,
                    non_slider_full_move_x8,
                    const_cap_flag_x8,
                );

                // Convert full move to 16-bit format
                let mv_epi16_x8 = _mm512_cvtepi64_epi16(non_slider_full_move_x8);

                // Create masks for captures
                let xq_mask = _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_queen_x8);
                let xr_mask = _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_rook_x8);
                let xb_mask = _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_bishop_x8);
                let xn_mask = _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_knight_x8);
                let xp_mask = _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, opponent_pawn_x8);
                let quiet_mask =
                    _mm512_test_epi64_mask(non_slider_dst_sq_bit_x8, full_board_inv_x8);

                let xp_ep_mask = xp_mask | ep_mask;

                macro_rules! capture_compress {
                    ($acc:ident, $cap:expr, $cap_mask:expr, $full_move_epi16_x8:ident) => {{
                        let cap_mask = $cap_mask;
                        let cap_mask_popcnt = (cap_mask as u64).count_ones() as usize;

                        $acc |= cap_mask;
                        let compressed = _mm_maskz_compress_epi16(cap_mask, $full_move_epi16_x8);
                        _mm_storeu_epi16(
                            scratch.capture_ptr[$cap as usize] as *mut i16,
                            compressed,
                        );

                        scratch.capture_ptr[$cap as usize] =
                            scratch.capture_ptr[$cap as usize].add(cap_mask_popcnt);
                    }};
                }

                macro_rules! quiet_compress {
                    ($acc:ident, $mask:expr, $full_move_epi16_x8:ident) => {{
                        if !CAPTURE_ONLY {
                            let mask = $mask;
                            $acc |= mask;
                            _mm_mask_compressstoreu_epi16(
                                scratch.quiet_list.as_mut_ptr().add(quiet_cursor) as *mut i16,
                                mask,
                                $full_move_epi16_x8,
                            );
                            quiet_cursor += mask.count_ones() as usize;
                        }
                    }};
                }

                let mut acc = 0u8;

                capture_compress!(acc, PxQ, pawn_mask & xq_mask, mv_epi16_x8);
                capture_compress!(acc, NxQ, knight_mask & xq_mask, mv_epi16_x8);
                capture_compress!(acc, KxQ, king_mask & xq_mask, mv_epi16_x8);
                capture_compress!(acc, PxR, pawn_mask & xr_mask, mv_epi16_x8);
                capture_compress!(acc, NxR, knight_mask & xr_mask, mv_epi16_x8);
                capture_compress!(acc, KxR, king_mask & xr_mask, mv_epi16_x8);
                capture_compress!(acc, PxB, pawn_mask & xb_mask, mv_epi16_x8);
                capture_compress!(acc, NxB, knight_mask & xb_mask, mv_epi16_x8);
                capture_compress!(acc, KxB, king_mask & xb_mask, mv_epi16_x8);
                capture_compress!(acc, PxN, pawn_mask & xn_mask, mv_epi16_x8);
                capture_compress!(acc, NxN, knight_mask & xn_mask, mv_epi16_x8);
                capture_compress!(acc, KxN, king_mask & xn_mask, mv_epi16_x8);
                capture_compress!(acc, PxP, pawn_mask & xp_ep_mask, mv_epi16_x8);
                capture_compress!(acc, NxP, knight_mask & xp_mask, mv_epi16_x8);
                capture_compress!(acc, KxP, king_mask & xp_mask, mv_epi16_x8);
                quiet_compress!(acc, knight_mask & quiet_mask, mv_epi16_x8);
                quiet_compress!(acc, king_mask & quiet_mask, mv_epi16_x8);

                priority_move_check!(acc, mv_epi16_x8);

                let slider_dst_sq_bit_x8 = _mm512_sllv_epi64(const_1_x8, slider_dst_sq_x8);

                let s_xq_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_queen_x8);
                let s_xr_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_rook_x8);
                let s_xb_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_bishop_x8);
                let s_xn_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_knight_x8);
                let s_xp_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_pawn_x8);
                let s_quiet_mask = _mm512_test_epi64_mask(slider_dst_sq_bit_x8, full_board_inv_x8);

                let slider_cap_mask =
                    _mm512_test_epi64_mask(slider_dst_sq_bit_x8, opponent_board_x8);

                let slider_full_move_x8 = _mm512_or_epi64(
                    _mm512_slli_epi64(slider_dst_sq_x8, 6),
                    _mm512_mask_or_epi64(
                        one_of_each_slider_index_x8,
                        slider_cap_mask,
                        one_of_each_slider_index_x8,
                        const_cap_flag_x8,
                    ),
                );

                // Convert full move to 16-bit format
                let s_mv_epi16_x8 = _mm512_cvtepi64_epi16(slider_full_move_x8);

                let mut acc = 0u8;

                capture_compress!(acc, BxQ, bishop_mask & s_xq_mask, s_mv_epi16_x8);
                capture_compress!(acc, RxQ, rook_mask & s_xq_mask, s_mv_epi16_x8);
                capture_compress!(acc, QxQ, queen_mask & s_xq_mask, s_mv_epi16_x8);
                capture_compress!(acc, BxR, bishop_mask & s_xr_mask, s_mv_epi16_x8);
                capture_compress!(acc, RxR, rook_mask & s_xr_mask, s_mv_epi16_x8);
                capture_compress!(acc, QxR, queen_mask & s_xr_mask, s_mv_epi16_x8);
                capture_compress!(acc, BxB, bishop_mask & s_xb_mask, s_mv_epi16_x8);
                capture_compress!(acc, RxB, rook_mask & s_xb_mask, s_mv_epi16_x8);
                capture_compress!(acc, QxB, queen_mask & s_xb_mask, s_mv_epi16_x8);
                capture_compress!(acc, BxN, bishop_mask & s_xn_mask, s_mv_epi16_x8);
                capture_compress!(acc, RxN, rook_mask & s_xn_mask, s_mv_epi16_x8);
                capture_compress!(acc, QxN, queen_mask & s_xn_mask, s_mv_epi16_x8);
                capture_compress!(acc, BxP, bishop_mask & s_xp_mask, s_mv_epi16_x8);
                capture_compress!(acc, RxP, rook_mask & s_xp_mask, s_mv_epi16_x8);
                capture_compress!(acc, QxP, queen_mask & s_xp_mask, s_mv_epi16_x8);
                quiet_compress!(acc, rook_mask & s_quiet_mask, s_mv_epi16_x8);
                quiet_compress!(acc, bishop_mask & s_quiet_mask, s_mv_epi16_x8);
                quiet_compress!(acc, queen_mask & s_quiet_mask, s_mv_epi16_x8);

                priority_move_check!(acc, s_mv_epi16_x8);

                non_slider_moves_x8 =
                    _mm512_xor_epi64(non_slider_moves_x8, non_slider_dst_sq_bit_x8);
                slider_moves_x8 = _mm512_xor_epi64(slider_moves_x8, slider_dst_sq_bit_x8);

                non_slider_dst_sq_x8 =
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_slider_moves_x8));
                slider_dst_sq_x8 =
                    _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(slider_moves_x8));

                let non_slider_dst_sq_mask =
                    _mm512_cmpneq_epi64_mask(non_slider_dst_sq_x8, const_n1_x8);

                let slider_dst_sq_mask = _mm512_cmpneq_epi64_mask(slider_dst_sq_x8, const_n1_x8);

                if (non_slider_dst_sq_mask | slider_dst_sq_mask) == 0 {
                    // No more moves left
                    break;
                }
            }

            // Pop pieces
            non_sliders_x8 = _mm512_xor_epi64(
                non_sliders_x8,
                _mm512_sllv_epi64(const_1_x8, one_of_each_non_slider_index_x8),
            );
            sliders_x8 = _mm512_xor_epi64(
                sliders_x8,
                _mm512_sllv_epi64(const_1_x8, one_of_each_slider_index_x8),
            );

            one_of_each_slider_index_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(sliders_x8));
            one_of_each_non_slider_index_x8 =
                _mm512_sub_epi64(const_63_x8, _mm512_lzcnt_epi64(non_sliders_x8));
            active_pieces_non_slider_mask =
                _mm512_cmpneq_epi64_mask(one_of_each_non_slider_index_x8, const_n1_x8);
            active_pieces_slider_mask =
                _mm512_cmpneq_epi64_mask(one_of_each_slider_index_x8, const_n1_x8);

            if (active_pieces_non_slider_mask | active_pieces_slider_mask) == 0 {
                break;
            }
        }

        // Castling moves
        if !CAPTURE_ONLY {
            let king_bitboard =
                bitboards[PieceIndex::WhiteKing as usize + ((b_move as usize) << 3)];
            let king_square = king_bitboard.trailing_zeros() as u16;

            let kc_move = ((king_square.wrapping_add(2)) << 6) | king_square | MV_FLAGS_CASTLE_KING;
            let kc = self.is_kingside_castle_allowed(b_move);
            *scratch.quiet_list.get_unchecked_mut(quiet_cursor as usize) = kc_move;
            quiet_cursor += kc as usize;

            tt_found |= (kc && tt_move == kc_move) as u8;
            beta1_found |= (kc && beta_1 == kc_move) as u8;
            beta2_found |= (kc && beta_2 == kc_move) as u8;

            let qc_move =
                ((king_square.wrapping_sub(2)) << 6) | king_square | MV_FLAGS_CASTLE_QUEEN;
            let qc = self.is_queenside_castle_allowed(b_move);
            *scratch.quiet_list.get_unchecked_mut(quiet_cursor as usize) = qc_move;
            quiet_cursor += qc as usize;

            tt_found |= (qc && tt_move == qc_move) as u8;
            beta1_found |= (qc && beta_1 == qc_move) as u8;
            beta2_found |= (qc && beta_2 == qc_move) as u8;
        }

        let mut num_captures = 0;
        for i in 0..32 {
            let cap_count =
                (scratch.capture_ptr[i] as usize - scratch.capture_list[i].as_ptr() as usize) >> 1;

            // Single list
            // move_list
            //     .get_unchecked_mut(num_captures..num_captures + cap_count)
            //     .copy_from_slice(&scratch[i].get_unchecked(0..cap_count));
            *move_list.get_unchecked_mut(num_captures + 0) =
                *scratch.capture_list[i].get_unchecked(0);
            *move_list.get_unchecked_mut(num_captures + 1) =
                *scratch.capture_list[i].get_unchecked(1);
            *move_list.get_unchecked_mut(num_captures + 2) =
                *scratch.capture_list[i].get_unchecked(2);
            *move_list.get_unchecked_mut(num_captures + 3) =
                *scratch.capture_list[i].get_unchecked(3);
            for j in 4..cap_count {
                std::hint::cold_path();
                *move_list.get_unchecked_mut(num_captures + j) =
                    *scratch.capture_list[i].get_unchecked(j);
            }

            num_captures += cap_count;
        }

        if !CAPTURE_ONLY {
            move_list
                .get_unchecked_mut(num_captures..num_captures + quiet_cursor)
                .copy_from_slice(&scratch.quiet_list.get_unchecked(0..quiet_cursor));
        }
        // println!("Copied {} captures", num_copied);
        // println!("Capture moves:");
        // for i in 0..num_copied {
        //     println!("[{}]: {}", i, util::move_string_dbg(capture_list[i] as u16));
        // }

        // capture_list[0..capture_cursors[CaptureMoves::PxQ as usize] as usize].copy_from_slice(
        //     &capture_moves[CaptureMoves::PxQ as usize]
        //         [0..capture_cursors[CaptureMoves::PxQ as usize] as usize],
        // );

        // for i in 0..30 {
        //     assert!(scratch2[i] <= 8);
        // }

        // for i in 0..30 {
        //     println!(
        //         "[{:?}]: {:?}",
        //         std::mem::transmute::<u8, CaptureMoves>(i),
        //         capture_moves[i as usize]
        //             .iter()
        //             .map(|mv| {
        //                 if *mv == 0 {
        //                     return "".to_string();
        //                 }
        //                 util::move_string_dbg(*mv as u16)
        //             })
        //             .collect::<Vec<_>>()
        //     );
        // }

        // for mv in quiet_moves {
        //     if mv == 0 {
        //         break;
        //     }
        //     println!("Quiet: {:?} ({})", mv, util::move_string_dbg(mv as u16));
        // }

        // (quiet_cursor as usize, num_captures as usize)

        // println!(
        //     "pv: {}, tt: {}, beta1: {}, beta2: {}",
        //     util::move_string_dbg(pv_move),
        //     util::move_string_dbg(tt_move),
        //     util::move_string_dbg(beta_1),
        //     util::move_string_dbg(beta_2)
        // );
        // println!(
        //     "pv_found: {}, tt_found: {}, beta1_found: {}, beta2_found: {}",
        //     pv_found, tt_found, beta1_found, beta2_found
        // );

        // for (idx, mv) in move_list
        //     .iter()
        //     .take(num_captures + quiet_cursor)
        //     .enumerate()
        // {
        //     for (idx2, mv2) in move_list
        //         .iter()
        //         .take(num_captures + quiet_cursor)
        //         .enumerate()
        //     {
        //         if idx == idx2 {
        //             continue;
        //         }
        //         assert!(
        //             *mv != *mv2,
        //             "Duplicate move found: {} ({}) (index {}) vs {} ({}) (index {}). {:?}. fen {}",
        //             util::move_string_dbg(*mv),
        //             *mv,
        //             idx,
        //             util::move_string_dbg(*mv2),
        //             *mv2,
        //             idx2,
        //             move_list
        //                 .iter()
        //                 .take(num_captures + quiet_cursor)
        //                 .map(|m| util::move_string_dbg(*m))
        //                 .collect::<Vec<_>>(),
        //             self.gen_fen()
        //         );
        //     }
        // }

        (
            num_captures + quiet_cursor,
            tt_found != 0,
            beta1_found != 0,
            beta2_found != 0,
        )
    }
}
