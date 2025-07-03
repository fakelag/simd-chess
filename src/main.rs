use winit::event_loop::{ControlFlow, EventLoop};

use crate::ui::chess_ui::ChessUi;

mod clipb;
mod constant;
mod engine;
mod ui;
mod window;

fn main() {
    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    let chess_ui = ChessUi::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    let mut app = window::App::new(chess_ui);
    event_loop.run_app(&mut app).unwrap();
}
