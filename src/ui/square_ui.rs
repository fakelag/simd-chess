use crate::{
    engine::chess,
    util::{PieceId, hex_to_f4_color, square_name},
};

const CLR_DARK_SQUARE: [f32; 4] = hex_to_f4_color(0xce8747, 1.0);
const CLR_LIGHT_SQUARE: [f32; 4] = hex_to_f4_color(0xfcc99b, 1.0);
const CLR_HIGHLIGHT: [f32; 4] = hex_to_f4_color(0xcb474b, 0.75);
const CLR_HIGHLIGHT_DARK: [f32; 4] = hex_to_f4_color(0x478ece, 1.0);
const CLR_HIGHLIGHT_LIGHT: [f32; 4] = hex_to_f4_color(0x9bcefc, 1.0);

pub struct SquareUi {
    sq_min: [f32; 2],
    sq_max: [f32; 2],
    sq_piece: Option<PieceId>,
    primary_clr: [f32; 4],
    secondary_clr: [f32; 4],
    highlight_clr: [f32; 4],

    is_hovering: bool,
    is_moving: bool,
    is_dragging: bool,
    mouse_xy: [f32; 2],

    pub sq_bit_index: u8,
}

impl SquareUi {
    pub fn new(rank: u8, file: u8) -> Self {
        let sq_bit_index = rank * 8 + file;

        let (primary_clr, secondary_clr, highlight_clr) = if ((rank ^ 7) + file) % 2 == 0 {
            (CLR_LIGHT_SQUARE, CLR_DARK_SQUARE, CLR_HIGHLIGHT_LIGHT)
        } else {
            (CLR_DARK_SQUARE, CLR_LIGHT_SQUARE, CLR_HIGHLIGHT_LIGHT)
        };

        Self {
            sq_min: [0.0; 2],
            sq_max: [0.0; 2],
            mouse_xy: [0.0; 2],
            sq_bit_index,
            sq_piece: None,
            primary_clr,
            secondary_clr,
            highlight_clr,
            is_hovering: false,
            is_moving: false,
            is_dragging: false,
        }
    }

    pub fn draw_bg(&self, ui: &imgui::Ui, sq_from: &Option<u8>) {
        let draw_list = ui.get_window_draw_list();

        draw_list
            .add_rect(self.sq_min, self.sq_max, self.primary_clr)
            .filled(true)
            .build();

        let square_text = square_name(self.sq_bit_index);
        let square_text_width = ui.calc_text_size(&square_text)[0];

        draw_list.add_text(
            [
                self.x() + self.sq_width() - square_text_width - 1.0,
                self.y() + 1.0,
            ],
            self.secondary_clr,
            &square_text,
        );

        if self.is_hovering && !self.is_moving && sq_from.is_some() {
            draw_list
                .add_rect(self.sq_min, self.sq_max, self.highlight_clr)
                .thickness(2.0)
                .build();
        }
    }

    pub fn calc_square_wh(ui: &imgui::Ui) -> [f32; 2] {
        let [content_avail_w, content_avail_h] = ui.content_region_avail();

        let square_h = content_avail_h / 8.0;
        let square_w = content_avail_w / 8.0;
        [square_w, square_h]
    }

    pub fn reset_moving(&mut self) {
        self.is_moving = false;
        self.is_dragging = false;
    }

    pub fn update(
        &mut self,
        ui: &imgui::Ui,
        board: &chess::ChessGame,
        sq_from: &mut Option<u8>,
    ) -> bool {
        self.reset_moving();
        self.mouse_xy = ui.io().mouse_pos;

        let sq_piece = board.piece_at_slow(1 << self.sq_bit_index);

        self.sq_piece = if sq_piece == 0 {
            None
        } else {
            Some(PieceId::from(sq_piece - 1))
        };

        self.is_hovering = ui.is_mouse_hovering_rect(self.sq_min, self.sq_max);

        let square_wh = Self::calc_square_wh(ui);

        let wnd_xy = ui.window_pos();

        let x = wnd_xy[0] + self.file() as f32 * square_wh[0];
        let y = wnd_xy[1] + (self.rank() ^ 7) as f32 * square_wh[1];

        self.sq_min = [x, y];
        self.sq_max = [x + square_wh[0], y + square_wh[1]];

        if let Some(sq_from) = sq_from {
            if self.sq_bit_index == *sq_from {
                self.is_moving = true;

                if self.sq_piece.is_some() && ui.is_mouse_down(imgui::MouseButton::Left) {
                    self.is_dragging = true;
                }
            }
        }

        if let Some(from_square) = sq_from {
            if self.is_hovering
                && *from_square != self.sq_bit_index
                && (ui.is_mouse_clicked(imgui::MouseButton::Left)
                    || ui.is_mouse_released(imgui::MouseButton::Left))
            {
                return true;
            }
        }

        if self.sq_piece.is_some() {
            if self.is_hovering && ui.is_mouse_clicked(imgui::MouseButton::Left) {
                if let Some(from_square) = sq_from {
                    if *from_square == self.sq_bit_index {
                        *sq_from = None;
                    } else {
                        *sq_from = Some(self.sq_bit_index);
                    }
                } else {
                    *sq_from = Some(self.sq_bit_index);
                }
            }
        }

        false
    }

    pub fn draw_highlights(&self, ui: &imgui::Ui, piece_tex: &[imgui::TextureId; 12]) {
        if self.is_moving {
            ui.get_window_draw_list()
                .add_rect(self.sq_min, self.sq_max, CLR_HIGHLIGHT)
                .filled(true)
                .build();

            if let Some(sq_piece) = self.sq_piece {
                if self.is_dragging {
                    let mouse_xy = self.mouse_xy;
                    let tex_min = [
                        mouse_xy[0] - self.sq_width() / 2.0,
                        mouse_xy[1] - self.sq_height() / 2.0,
                    ];
                    let tex_max = [
                        mouse_xy[0] + self.sq_width() / 2.0,
                        mouse_xy[1] + self.sq_height() / 2.0,
                    ];
                    ui.get_window_draw_list()
                        .add_image(piece_tex[sq_piece as usize], tex_min, tex_max)
                        .col([1.0, 1.0, 1.0, 0.75])
                        .build();
                }
            }
        }
    }

    pub fn draw_move_indicator(&self, ui: &imgui::Ui) {
        ui.get_window_draw_list()
            .add_circle(
                [
                    self.x() + self.sq_width() / 2.0,
                    self.y() + self.sq_height() / 2.0,
                ],
                self.sq_width() / 2.0 * 0.4,
                CLR_HIGHLIGHT,
            )
            .filled(true)
            .build();
    }

    pub fn draw_texture(&self, ui: &imgui::Ui, piece_tex: &[imgui::TextureId; 12]) {
        if let Some(sq_piece) = self.sq_piece {
            ui.get_window_draw_list()
                .add_image(piece_tex[sq_piece as usize], self.sq_min, self.sq_max)
                .build();
        }
    }

    fn x(&self) -> f32 {
        self.sq_min[0]
    }

    fn y(&self) -> f32 {
        self.sq_min[1]
    }

    fn rank(&self) -> u8 {
        self.sq_bit_index / 8
    }

    fn file(&self) -> u8 {
        self.sq_bit_index % 8
    }

    fn sq_width(&self) -> f32 {
        self.sq_max[0] - self.sq_min[0]
    }

    fn sq_height(&self) -> f32 {
        self.sq_max[1] - self.sq_min[1]
    }
}
