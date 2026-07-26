use std::fs::File;
use std::io::{BufWriter, Write};

pub struct ShardedFenWriter {
    base: String,
    shard_size: usize,
    shard_index: usize,
    in_shard: usize,
    writer: BufWriter<File>,
    buf: String,
}

impl ShardedFenWriter {
    const FLUSH_THRESHOLD: usize = 128 * 1024;

    pub fn new(base: &str, shard_size: usize) -> std::io::Result<Self> {
        let writer = BufWriter::new(File::create(Self::shard_path(base, shard_size, 0))?);
        Ok(Self {
            base: base.to_string(),
            shard_size,
            shard_index: 0,
            in_shard: 0,
            writer,
            buf: String::new(),
        })
    }

    fn shard_path(base: &str, shard_size: usize, i: usize) -> String {
        if shard_size == 0 {
            return base.to_string();
        }

        let path = std::path::Path::new(base);
        let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("fen");
        let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or(base);
        let name = format!("{stem}.{i}.{ext}");

        match path.parent() {
            Some(dir) if !dir.as_os_str().is_empty() => {
                dir.join(name).to_string_lossy().into_owned()
            }
            _ => name,
        }
    }

    pub fn write_line(&mut self, fen: &str) -> std::io::Result<()> {
        self.buf.push_str(fen);
        self.buf.push('\n');
        if self.buf.len() >= Self::FLUSH_THRESHOLD {
            self.flush_buf()?;
        }

        self.in_shard += 1;
        if self.shard_size != 0 && self.in_shard >= self.shard_size {
            self.rotate()?;
        }
        Ok(())
    }

    fn flush_buf(&mut self) -> std::io::Result<()> {
        self.writer.write_all(self.buf.as_bytes())?;
        self.buf.clear();
        Ok(())
    }

    fn rotate(&mut self) -> std::io::Result<()> {
        self.flush_buf()?;
        self.shard_index += 1;
        self.in_shard = 0;
        self.writer = BufWriter::new(File::create(Self::shard_path(
            &self.base,
            self.shard_size,
            self.shard_index,
        ))?);
        Ok(())
    }

    pub fn finish(&mut self) -> std::io::Result<()> {
        self.flush_buf()?;
        self.writer.flush()
    }
}
