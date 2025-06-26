use winit::event_loop::{ControlFlow, EventLoop};

mod window;

fn draw(ui: &mut imgui::Ui) {
    let [display_w, display_h] = ui.io().display_size;

    ui.window("chess")
        .bg_alpha(1.0)
        .resizable(false)
        .movable(false)
        .title_bar(false)
        .position([0.0, 0.0], imgui::Condition::Always)
        .size([display_w, display_h], imgui::Condition::Always)
        .build(|| {
            ui.text("app");
        });
}

fn main() {
    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = window::App::new(Box::new(draw));
    event_loop.run_app(&mut app).unwrap();
}
