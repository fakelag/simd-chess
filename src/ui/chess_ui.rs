use crate::{
    constant::{PieceId, Side},
    engine::{tables::Tables, *},
    ui::square_ui::SquareUi,
    window,
};

pub struct ChessUi {
    board: chess::Board,
    fen_input: String,
    from_square: Option<u8>,
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

                                    let [square_w, square_h] = SquareUi::calc_square_wh(ui);

                                    let x = wnd_x + file as f32 * square_w;
                                    let y = wnd_y + (rank ^ 7) as f32 * square_h;

                                    if ui.is_mouse_hovering_rect(
                                        [x, y],
                                        [x + square_w, y + square_h],
                                    ) {
                                        Some(square_index as u8)
                                    } else {
                                        None
                                    }
                                });

                                let blockers = self
                                    .board
                                    .board
                                    .bitboards
                                    .into_iter()
                                    .reduce(|acc, bitboard| acc | bitboard)
                                    .unwrap();

                                let white_pieces = self
                                    .board
                                    .board
                                    .bitboards
                                    .into_iter()
                                    .take(PieceId::WhitePawn as usize + 1)
                                    .reduce(|acc, bitboard| acc | bitboard)
                                    .unwrap();

                                let mut moves = [0u16; 256];
                                let move_count =
                                    self.board.gen_moves_slow(&self.tables, &mut moves);

                                for rank in (0..8).rev() {
                                    for file in 0..8 {
                                        let square = &mut self.squares[rank * 8 + file];

                                        square.update(ui, &mut self.board, &mut self.from_square);

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

                                        if let Some(hovering_sq_index) = hovering_sq_index {
                                            // let hovering_sq_occupancy_mask =
                                            //     Tables::LT_BISHOP_OCCUPANCY_MASKS
                                            //         [hovering_sq_index as usize];

                                            // let slider_blockers =
                                            //     blockers & hovering_sq_occupancy_mask;

                                            // let slider_moves =
                                            //     self.tables.get_slider_move_mask::<false>(
                                            //         hovering_sq_index as usize,
                                            //         slider_blockers,
                                            //     );

                                            // if ((slider_moves & !white_pieces)
                                            //     & (1 << square.sq_bit_index))
                                            //     != 0
                                            // {
                                            //     square.draw_move_indicator(ui);
                                            // }

                                            // if (Tables::LT_BISHOP_OCCUPANCY_MASKS
                                            //     [hovering_sq_index as usize]
                                            //     & (1 << square.sq_bit_index))
                                            //     != 0
                                            // {
                                            //     square.draw_move_indicator();
                                            // }
                                        }
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

                    display_bitboard!(WhiteKing);
                    display_bitboard!(WhiteQueen);
                    display_bitboard!(WhiteRook);
                    display_bitboard!(WhiteBishop);
                    display_bitboard!(WhiteKnight);
                    display_bitboard!(WhitePawn);

                    ui.separator();

                    display_bitboard!(BlackKing);
                    display_bitboard!(BlackQueen);
                    display_bitboard!(BlackRook);
                    display_bitboard!(BlackBishop);
                    display_bitboard!(BlackKnight);
                    display_bitboard!(BlackPawn);
                });
            });
    }
}
