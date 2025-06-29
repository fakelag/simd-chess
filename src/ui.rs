use crate::{chess, constant::PieceId, window};

const CLR_DARK_SQUARE: [f32; 3] = [0.81, 0.53, 0.28]; // [0.63, 0.42, 0.15];
const CLR_LIGHT_SQUARE: [f32; 3] = [0.99, 0.79, 0.61]; // [0.91, 0.65, 0.30];

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

                                for rank in (0..8).rev() {
                                    for file in 0..8 {
                                        let x = wnd_x + file as f32 * square_w;
                                        let y = wnd_y + (rank ^ 7) as f32 * square_h;

                                        let sq_min = [x, y];
                                        let sq_max = [x + square_w, y + square_h];
                                        let sq_bit = rank * 8 + file;

                                        let (primary_clr, secondary_clr) =
                                            if ((rank ^ 7) + file) % 2 == 0 {
                                                (CLR_LIGHT_SQUARE, CLR_DARK_SQUARE)
                                            } else {
                                                (CLR_DARK_SQUARE, CLR_LIGHT_SQUARE)
                                            };

                                        draw_list
                                            .add_rect(sq_min, sq_max, primary_clr)
                                            .filled(true)
                                            .build();

                                        let square_text =
                                            format!("{}{}", (b'a' + file as u8) as char, rank + 1);
                                        let square_text_width = ui.calc_text_size(&square_text)[0];

                                        draw_list.add_text(
                                            [x + square_w - square_text_width - 1.0, y + 1.0],
                                            secondary_clr,
                                            &square_text,
                                        );

                                        let is_hovering = ui.is_mouse_hovering_rect(sq_min, sq_max);

                                        let moved = if let Some(from_square) = self.from_square {
                                            if is_hovering
                                                && from_square != sq_bit
                                                && ui.is_mouse_clicked(imgui::MouseButton::Left)
                                            {
                                                self.board.move_piece_slow(
                                                    ((from_square as u16) & 0x3F)
                                                        | ((sq_bit as u16 & 0x3F) << 6),
                                                );
                                                self.from_square = None;
                                                true
                                            } else {
                                                false
                                            }
                                        } else {
                                            false
                                        };

                                        if let Some(piece_id) = self.board.piece_at_slow(rank, file)
                                        {
                                            if is_hovering
                                                && ui.is_mouse_clicked(imgui::MouseButton::Left)
                                                && !moved
                                            {
                                                if let Some(from_square) = self.from_square {
                                                    if from_square == sq_bit {
                                                        self.from_square = None;
                                                    } else {
                                                        self.from_square = Some(sq_bit);
                                                    }
                                                } else {
                                                    self.from_square = Some(sq_bit);
                                                }
                                            }

                                            if let Some(from_square) = self.from_square {
                                                if from_square == sq_bit {
                                                    draw_list
                                                        .add_rect(
                                                            sq_min,
                                                            sq_max,
                                                            [1.0, 0.0, 0.0, 0.5],
                                                        )
                                                        .filled(true)
                                                        .build();
                                                }
                                            }

                                            draw_list
                                                .add_image(
                                                    ctx.textures[piece_id as usize],
                                                    sq_min,
                                                    sq_max,
                                                )
                                                .build();
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
