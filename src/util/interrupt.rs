use std::sync::atomic::{AtomicU32, Ordering};

static PRESSES: AtomicU32 = AtomicU32::new(0);

/// True once Ctrl+C (or Ctrl+Break) was pressed after `install`. Only the first press is handed to
/// the program; a second one falls through to the default handler, which kills the process.
pub fn requested() -> bool {
    PRESSES.load(Ordering::Relaxed) > 0
}

#[cfg(windows)]
pub fn install() -> std::io::Result<()> {
    const CTRL_C_EVENT: u32 = 0;
    const CTRL_BREAK_EVENT: u32 = 1;

    unsafe extern "system" fn handler(ctrl_type: u32) -> i32 {
        match ctrl_type {
            CTRL_C_EVENT | CTRL_BREAK_EVENT => {
                (PRESSES.fetch_add(1, Ordering::Relaxed) == 0) as i32
            }
            _ => 0,
        }
    }

    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn SetConsoleCtrlHandler(
            handler: Option<unsafe extern "system" fn(u32) -> i32>,
            add: i32,
        ) -> i32;
    }

    // Safety: the handler only touches an atomic and stays valid for the life of the process.
    if unsafe { SetConsoleCtrlHandler(Some(handler), 1) } == 0 {
        return Err(std::io::Error::last_os_error());
    }

    Ok(())
}

#[cfg(not(windows))]
pub fn install() -> std::io::Result<()> {
    Ok(())
}
