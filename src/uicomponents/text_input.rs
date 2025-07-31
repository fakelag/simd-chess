pub struct ImguiTextInput {
    flags: imgui::InputTextFlags,
    pub buf: String,
}

impl ImguiTextInput {
    pub fn new(
        flags: imgui::InputTextFlags,
        default: Option<&'static str>,
        cap: Option<usize>,
    ) -> Self {
        let mut buf = String::with_capacity(cap.unwrap_or(256));

        if let Some(default) = default {
            buf.push_str(default);
        }
        Self { flags, buf }
    }

    pub fn draw(&mut self, title: Option<&str>, ui: &imgui::Ui, label: &str) -> Option<String> {
        if let Some(title) = title {
            ui.text(title);
        }
        let [width_avail, _] = ui.content_region_avail();

        let width_var = ui.push_item_width(width_avail);
        if ui
            .input_text(label, &mut self.buf)
            .flags(self.flags)
            .build()
        {
            width_var.end();
            return Some(self.buf.clone());
        }

        width_var.end();
        None
    }
}
