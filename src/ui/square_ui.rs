use crate::{chess, constant::PieceId};

const CLR_DARK_SQUARE: [f32; 3] = [0.81, 0.53, 0.28];
const CLR_LIGHT_SQUARE: [f32; 3] = [0.99, 0.79, 0.61];
const CLR_HIGHLIGHT: [f32; 4] = [1.0, 0.0, 0.0, 0.5];

pub struct SquareUi<'a> {
    ui: &'a imgui::Ui,
    draw_list: &'a imgui::DrawListMut<'a>,
    board: &'a mut chess::Board,
    sq_min: [f32; 2],
    sq_max: [f32; 2],
    sq_bit_index: u8,
    sq_piece: Option<PieceId>,
    is_hovering: bool,
    primary_clr: [f32; 3],
    secondary_clr: [f32; 3],
}

impl<'a> SquareUi<'a> {
    pub fn new(
        board: &'a mut chess::Board,
        wnd_xy: [f32; 2],
        rank: u8,
        file: u8,
        square_wh: [f32; 2],
        ui: &'a imgui::Ui,
        draw_list: &'a imgui::DrawListMut<'a>,
    ) -> Self {
        let x = wnd_xy[0] + file as f32 * square_wh[0];
        let y = wnd_xy[1] + (rank ^ 7) as f32 * square_wh[1];

        let sq_min = [x, y];
        let sq_max = [x + square_wh[0], y + square_wh[1]];
        let sq_bit_index = rank * 8 + file;
        let sq_piece = board.piece_at_slow(1 << sq_bit_index);

        let is_hovering = ui.is_mouse_hovering_rect(sq_min, sq_max);

        let (primary_clr, secondary_clr) = if ((rank ^ 7) + file) % 2 == 0 {
            (CLR_LIGHT_SQUARE, CLR_DARK_SQUARE)
        } else {
            (CLR_DARK_SQUARE, CLR_LIGHT_SQUARE)
        };

        Self {
            ui,
            draw_list,
            board,
            sq_min,
            sq_max,
            sq_bit_index,
            sq_piece: if sq_piece == 0 {
                None
            } else {
                Some(PieceId::from(sq_piece - 1))
            },
            is_hovering,
            primary_clr,
            secondary_clr,
        }
    }

    pub fn draw_bg(&self) {
        self.draw_list
            .add_rect(self.sq_min, self.sq_max, self.primary_clr)
            .filled(true)
            .build();

        let square_text = format!("{}{}", (b'a' + self.file() as u8) as char, self.rank() + 1);
        let square_text_width = self.ui.calc_text_size(&square_text)[0];

        self.draw_list.add_text(
            [
                self.x() + self.sq_width() - square_text_width - 1.0,
                self.y() + 1.0,
            ],
            self.secondary_clr,
            &square_text,
        );
    }

    pub fn handle_move(&mut self, sq_from: &mut Option<u8>) {
        if let Some(from_square) = sq_from {
            if self.is_hovering
                && *from_square != self.sq_bit_index
                && self.ui.is_mouse_clicked(imgui::MouseButton::Left)
            {
                self.board.move_piece_slow(
                    ((*from_square as u16) & 0x3F) | ((self.sq_bit_index as u16 & 0x3F) << 6),
                );
                *sq_from = None;
                return;
            }
        }

        if self.sq_piece.is_some() {
            if self.is_hovering && self.ui.is_mouse_clicked(imgui::MouseButton::Left) {
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
    }

    pub fn draw_highlights(&self, sq_from: Option<u8>) {
        if let Some(sq_from) = sq_from {
            if self.sq_bit_index == sq_from {
                self.draw_list
                    .add_rect(self.sq_min, self.sq_max, CLR_HIGHLIGHT)
                    .filled(true)
                    .build();
            }
        }

        if self.is_hovering {
            self.draw_list
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
    }

    pub fn draw_texture(&self, piece_tex: &[imgui::TextureId; 12]) {
        if let Some(sq_piece) = self.sq_piece {
            self.draw_list
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
