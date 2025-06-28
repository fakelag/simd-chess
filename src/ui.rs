use crate::{chess, window};

const CLR_DARK_SQUARE: [f32; 3] = [0.63, 0.42, 0.15];
const CLR_LIGHT_SQUARE: [f32; 3] = [0.91, 0.65, 0.30];

pub struct ChessUi {
    board: chess::Board,
    fen_input: String,
}

impl ChessUi {
    pub fn new(fen: &str) -> Self {
        let mut board = chess::Board::new();
        board.load_fen(fen).unwrap();
        Self {
            board,
            fen_input: String::from(fen),
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

                                for rank in 0..8 {
                                    for file in 0..8 {
                                        let x = wnd_x + file as f32 * square_w;
                                        let y = wnd_y + rank as f32 * square_h;

                                        let color = if (rank + file) % 2 == 0 {
                                            CLR_LIGHT_SQUARE
                                        } else {
                                            CLR_DARK_SQUARE
                                        };

                                        draw_list
                                            .add_rect([x, y], [x + square_w, y + square_h], color)
                                            .filled(true)
                                            .build();

                                        if let Some(piece_id) =
                                            self.board.piece_at_slow(rank ^ 7, file)
                                        {
                                            draw_list
                                                .add_image(
                                                    ctx.textures[piece_id as usize],
                                                    [x.floor(), y.floor()],
                                                    [
                                                        (x + square_w).floor(),
                                                        (y + square_h).floor(),
                                                    ],
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
                            ui.text(format!("{:032b}", self.board.$name >> 32));
                            ui.text(format!("{:032b}", self.board.$name & 0xFFFFFFFF));
                            // ui.text(format!("{:016X}", self.board.$name));
                        };
                    }

                    display_bitboard!(w_king);
                    display_bitboard!(w_queen);
                    display_bitboard!(w_rook);
                    display_bitboard!(w_bishop);
                    display_bitboard!(w_knight);
                    display_bitboard!(w_pawn);

                    ui.separator();

                    display_bitboard!(b_king);
                    display_bitboard!(b_queen);
                    display_bitboard!(b_rook);
                    display_bitboard!(b_bishop);
                    display_bitboard!(b_knight);
                    display_bitboard!(b_pawn);
                });
            });
    }
}
