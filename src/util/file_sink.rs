use std::cell::RefCell;
use std::fs::File;
use std::io::{self, Write};
use std::rc::Rc;

/// `Write` target for sfbinpack writers. They emit their last chunk on drop and only print an
/// error if that fails, so the outcome is recorded here instead. Dropping the sink fsyncs the file.
pub struct FileSink {
    file: File,
    status: Rc<RefCell<SinkStatus>>,
}

#[derive(Default)]
pub struct SinkStatus {
    pub written: u64,
    pub error: Option<io::Error>,
}

impl FileSink {
    pub fn new(file: File) -> (Self, Rc<RefCell<SinkStatus>>) {
        let status = Rc::new(RefCell::new(SinkStatus::default()));
        let sink = Self {
            file,
            status: status.clone(),
        };
        (sink, status)
    }

    fn record(&self, e: &io::Error) {
        let mut status = self.status.borrow_mut();
        if status.error.is_none() {
            status.error = Some(io::Error::new(e.kind(), e.to_string()));
        }
    }
}

impl Write for FileSink {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let result = self.file.write(buf);
        match &result {
            Ok(n) => self.status.borrow_mut().written += *n as u64,
            Err(e) if e.kind() != io::ErrorKind::Interrupted => self.record(e),
            Err(_) => {}
        }
        result
    }

    fn flush(&mut self) -> io::Result<()> {
        self.file.flush()
    }
}

impl Drop for FileSink {
    fn drop(&mut self) {
        if let Err(e) = self.file.sync_all() {
            self.record(&e);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::test_dir::TestDir;

    #[test]
    fn test_counts_bytes_and_syncs_on_drop() {
        let dir = TestDir::new("file_sink_ok");
        let path = dir.path().join("out.bin");

        let (mut sink, status) = FileSink::new(File::create(&path).unwrap());
        sink.write_all(b"hello").unwrap();
        sink.write_all(b" world").unwrap();
        sink.flush().unwrap();
        drop(sink);

        let status = status.borrow();
        assert_eq!(status.written, 11);
        assert!(status.error.is_none());
        assert_eq!(std::fs::read(&path).unwrap(), b"hello world");
    }

    #[test]
    fn test_records_errors() {
        let dir = TestDir::new("file_sink_error");
        let path = dir.path().join("read_only.bin");
        std::fs::write(&path, b"x").unwrap();

        // No write access: the write and the sync on drop both fail.
        let (mut sink, status) = FileSink::new(File::open(&path).unwrap());
        assert!(sink.write(b"abc").is_err());
        assert!(status.borrow().error.is_some());
        drop(sink);

        let status = status.borrow();
        assert_eq!(status.written, 0);
        assert!(status.error.is_some());
        assert_eq!(std::fs::read(&path).unwrap(), b"x");
    }
}
