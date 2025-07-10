use crate::{
    constant::{PieceId, square_name},
    engine::{tables::Tables, *},
    ui::square_ui::SquareUi,
    window,
};

pub struct ChessUi {
    board: chess::Board,
    fen_input: String,
    from_square: Option<u8>,
    ask_promotion: Option<(u8, u8)>,
    tables: Tables,
    squares: Vec<SquareUi>,
}

impl ChessUi {
    pub fn new(fen: &str) -> Self {
        let mut board = chess::Board::new();
        board.load_fen(fen).unwrap();

        let mut squares = Vec::with_capacity(64);
        for rank in (0..8).rev() {
            for file in 0..8 {
                squares.push(SquareUi::new(rank, file));
            }
        }

        Self {
            board,
            fen_input: String::from(fen),
            from_square: None,
            ask_promotion: None,
            tables: Tables::new(),
            squares,
        }
    }

    pub fn draw(&mut self, ctx: window::DrawCtx) {
        let ui = ctx.ui;

        let [display_w, display_h] = ui.io().display_size;

        ui.window("chess")
            .bg_alpha(1.0)
            .resizable(false)
            .movable(false)
            .title_bar(false)
            .position([0.0, 0.0], imgui::Condition::Always)
            .size([display_w, display_h], imgui::Condition::Always)
            .build(|| {
                let [size_w, _] = ui.content_region_avail();
                let board_size = 0.7;

                let var_window_padding =
                    ui.push_style_var(imgui::StyleVar::WindowPadding([0.0, 0.0]));

                let mut board_cursor_xy: [f32; 2] = [0.0; 2];
                let mut square_wh = [0.0; 2];

                let mut moves = [0u16; 256];
                let move_count = self.board.gen_moves_slow(&self.tables, &mut moves);

                ui.child_window("board_container")
                    .size([size_w * board_size, -1.0])
                    .scroll_bar(false)
                    .build(|| {
                        ui.child_window("board")
                            .border(true)
                            .size([size_w * board_size, size_w * board_size])
                            .scroll_bar(false)
                            .build(|| {
                                let hovering_sq_index = (0..64).find_map(|square_index| {
                                    let [wnd_x, wnd_y] = ui.window_pos();
                                    let rank = square_index / 8;
                                    let file = square_index % 8;

                                    square_wh = SquareUi::calc_square_wh(ui);

                                    let x = wnd_x + file as f32 * square_wh[0];
                                    let y = wnd_y + (rank ^ 7) as f32 * square_wh[1];

                                    if ui.is_mouse_hovering_rect(
                                        [x, y],
                                        [x + square_wh[0], y + square_wh[1]],
                                    ) {
                                        Some(square_index as u8)
                                    } else {
                                        None
                                    }
                                });

                                board_cursor_xy = ui.cursor_screen_pos();

                                for rank in (0..8).rev() {
                                    for file in 0..8 {
                                        let square = &mut self.squares[rank * 8 + file];

                                        if self.ask_promotion.is_some() {
                                            square.reset_moving();
                                        } else if square.update(
                                            ui,
                                            &mut self.board,
                                            &mut self.from_square,
                                        ) {
                                            for mv_index in 0..move_count {
                                                let curmove = moves[mv_index];

                                                let from_sq =
                                                    if let Some(from_sq) = self.from_square {
                                                        from_sq as u16
                                                    } else {
                                                        continue;
                                                    };

                                                if curmove & 0x3F != from_sq
                                                    || square.sq_bit_index
                                                        != ((curmove >> 6) as u8 & 0x3F)
                                                {
                                                    continue;
                                                }

                                                if (curmove & chess::MV_FLAG_PROMOTION) != 0 {
                                                    self.ask_promotion =
                                                        Some((from_sq as u8, square.sq_bit_index));
                                                    self.from_square = None;
                                                    break;
                                                }

                                                self.board.make_move_slow(curmove, &self.tables);
                                                self.from_square = None;
                                            }
                                        }

                                        square.draw_bg(ui, &self.from_square);
                                        square.draw_texture(ui, &ctx.textures);

                                        // square.draw_highlights(self.from_square, &ctx.textures);

                                        // if (tables::EX_OUTER & (1 << square.sq_bit_index)) != 0 {
                                        //     square.draw_move_indicator();
                                        // }

                                        for mv_index in 0..move_count {
                                            if let Some(from_sq) = self.from_square {
                                                if moves[mv_index] & 0x3F == from_sq as u16
                                                    && square.sq_bit_index
                                                        == ((moves[mv_index] >> 6) as u8 & 0x3F)
                                                {
                                                    square.draw_move_indicator(ui);
                                                }
                                            }
                                        }

                                        // if self.board.is_square_attacked(
                                        //     square.sq_bit_index,
                                        //     self.board.b_move,
                                        //     &self.tables,
                                        // ) {
                                        //     square.draw_move_indicator(ui);
                                        // }

                                        // if let Some(ep_square) = self.board.en_passant {
                                        //     if square.sq_bit_index == ep_square {
                                        //         square.draw_move_indicator(ui);
                                        //     }
                                        // }

                                        if let Some(hovering_sq_index) = hovering_sq_index {}
                                    }
                                }

                                for rank in (0..8).rev() {
                                    for file in 0..8 {
                                        let square = &self.squares[rank * 8 + file];
                                        square.draw_highlights(ui, &ctx.textures);
                                    }
                                }
                            });

                        let [width_avail, _] = ui.content_region_avail();

                        let width_var = ui.push_item_width(width_avail);
                        if ui
                            .input_text("##fen_inp", &mut self.fen_input)
                            .auto_select_all(true)
                            .enter_returns_true(true)
                            .build()
                        {
                            if let Err(err) = self.board.load_fen(&self.fen_input) {
                                eprintln!("Failed to load FEN: {}", err);
                            } else {
                                println!("FEN loaded: {}", self.fen_input);
                                self.fen_input.clear();
                            }
                        }
                        width_var.end();
                    });

                var_window_padding.pop();

                ui.same_line();

                ui.child_window("side").border(true).build(|| {
                    macro_rules! display_bitboard {
                        ($name:ident) => {
                            ui.text(stringify!($name));
                            ui.text(format!(
                                "{:032b}",
                                self.board.board.bitboards[PieceId::$name as usize] >> 32
                            ));
                            ui.text(format!(
                                "{:032b}",
                                self.board.board.bitboards[PieceId::$name as usize] & 0xFFFFFFFF
                            ));
                            // ui.text(format!("{:016X}", self.board.pieces[PieceId::$name as usize]));
                        };
                    }

                    ui.text(format!("is_valid: {}", self.board.is_valid()));
                    ui.text(format!(
                        "ep_square: {:?}",
                        self.board.en_passant.and_then(|sq| Some(square_name(sq)))
                    ));
                    ui.text(format!(
                        "b_move: {}",
                        if self.board.b_move { "black" } else { "white" }
                    ));

                    ui.text(format!("Castles: {:04b}", self.board.castles));

                    ui.text(format!(
                        "half_moves: {}, full_moves: {}",
                        self.board.half_moves, self.board.full_moves
                    ));

                    ui.text(format!(
                        "In check: {}",
                        self.board.in_check_slow(&self.tables, self.board.b_move)
                    ));

                    ui.text("Castle moves:");
                    for mv_index in 0..move_count {
                        let curmove = moves[mv_index];

                        let from_sq = curmove & 0x3F;
                        let to_sq = (curmove >> 6) & 0x3F;
                        let flag = curmove & chess::MV_FLAGS;

                        if flag == chess::MV_FLAGS_CASTLE_KING
                            || flag == chess::MV_FLAGS_CASTLE_QUEEN
                        {
                            ui.text(format!(
                                "{} -> {}, flag: {:04b}",
                                square_name(from_sq as u8),
                                square_name(to_sq as u8),
                                flag
                            ));
                        }
                    }

                    // display_bitboard!(WhiteKing);
                    // display_bitboard!(WhiteQueen);
                    // display_bitboard!(WhiteRook);
                    // display_bitboard!(WhiteBishop);
                    // display_bitboard!(WhiteKnight);
                    // display_bitboard!(WhitePawn);

                    // ui.separator();

                    // display_bitboard!(BlackKing);
                    // display_bitboard!(BlackQueen);
                    // display_bitboard!(BlackRook);
                    // display_bitboard!(BlackBishop);
                    // display_bitboard!(BlackKnight);
                    // display_bitboard!(BlackPawn);
                });

                if let Some((from_sq, to_sq)) = self.ask_promotion {
                    ui.set_cursor_pos(board_cursor_xy);

                    let draw_list = ui.get_foreground_draw_list();

                    let rank_offset = 7 - to_sq / 8;
                    let file_offset = match to_sq % 8 {
                        4..=7 => (to_sq % 8) - 4,
                        val => val + 1,
                    };

                    let [selector_x, selector_y] = [
                        board_cursor_xy[0] + square_wh[0] * file_offset as f32,
                        board_cursor_xy[1] + square_wh[1] * rank_offset as f32,
                    ];

                    [
                        chess::MV_FLAGS_PR_QUEEN,
                        chess::MV_FLAGS_PR_ROOK,
                        chess::MV_FLAGS_PR_BISHOP,
                        chess::MV_FLAGS_PR_KNIGHT,
                    ]
                    .iter()
                    .enumerate()
                    .for_each(|(i, flag)| {
                        let texture_id = ctx.textures
                            [i + PieceId::WhiteQueen as usize + self.board.b_move as usize * 6];

                        let [option_x, option_y] =
                            [selector_x + square_wh[0] * i as f32, selector_y];

                        draw_list
                            .add_rect(
                                [option_x, option_y],
                                [option_x + square_wh[0], option_y + square_wh[1]],
                                [0.0, 0.0, 0.0, 0.5],
                            )
                            .filled(true)
                            .build();

                        draw_list
                            .add_image(
                                texture_id,
                                [option_x, option_y],
                                [option_x + square_wh[0], option_y + square_wh[1]],
                            )
                            .build();

                        if ui.is_mouse_clicked(imgui::MouseButton::Left)
                            && ui.is_mouse_hovering_rect(
                                [option_x, option_y],
                                [option_x + square_wh[0], option_y + square_wh[1]],
                            )
                        {
                            self.board.make_move_slow(
                                (from_sq as u16) | ((to_sq as u16) << 6) | *flag,
                                &self.tables,
                            );
                            self.ask_promotion = None;
                            self.from_square = None;
                        }
                    });
                }
            });
    }
}
