use crate::{constant::PieceId, engine::*, ui::square_ui::SquareUi, window};

pub struct ChessUi {
    board: chess::Board,
    fen_input: String,
    from_square: Option<u8>,
}

impl ChessUi {
    pub fn new(fen: &str) -> Self {
        let mut board = chess::Board::new();
        board.load_fen(fen).unwrap();
        Self {
            board,
            fen_input: String::from(fen),
            from_square: None,
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
                                let [wnd_x, wnd_y] = ui.window_pos();
                                let [size_w, size_h] = ui.content_region_avail();

                                let draw_list = ui.get_window_draw_list();
                                let square_h = size_h / 8.0;
                                let square_w = size_w / 8.0;
                                let square_wh = [square_w, square_h];

                                let hovering_sq_index = (0..64).find_map(|square_index| {
                                    let rank = square_index / 8;
                                    let file = square_index % 8;

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

                                for rank in (0..8).rev() {
                                    for file in 0..8 {
                                        let mut square = SquareUi::new(
                                            &mut self.board,
                                            [wnd_x, wnd_y],
                                            rank,
                                            file,
                                            square_wh,
                                            ui,
                                            &draw_list,
                                        );

                                        square.handle_move(&mut self.from_square);

                                        square.draw_bg();
                                        square.draw_texture(&ctx.textures);

                                        square.draw_highlights(self.from_square);

                                        if let Some(hovering_sq_index) = hovering_sq_index {
                                            if (tables::LT_KING_MOVES[hovering_sq_index as usize]
                                                & (1 << square.sq_bit_index))
                                                != 0
                                            {
                                                square.draw_move_indicator();
                                            }
                                        }
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
                                self.board.board.pieces[PieceId::$name as usize] >> 32
                            ));
                            ui.text(format!(
                                "{:032b}",
                                self.board.board.pieces[PieceId::$name as usize] & 0xFFFFFFFF
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
