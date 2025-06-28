use clipboard::{ClipboardContext, ClipboardProvider};
use imgui::ClipboardBackend;

pub struct Clipboard {
    ctx: ClipboardContext,
}

impl Clipboard {
    pub fn new() -> Self {
        let ctx: ClipboardContext = ClipboardProvider::new().unwrap();
        Self { ctx }
    }
}

impl ClipboardBackend for Clipboard {
    fn get(&mut self) -> Option<String> {
        if let Ok(clipboard_text) = self.ctx.get_contents() {
            Some(clipboard_text)
        } else {
            None
        }
    }

    fn set(&mut self, value: &str) {
        _ = self
            .ctx
            .set_contents(value.to_owned())
            .map_err(|err| eprintln!("Failed to update clipboard text: {:?}", err));
    }
}
