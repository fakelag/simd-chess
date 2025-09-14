pub struct Annotator {
    binpack_path: String,
}

impl Annotator {
    pub fn new(binpack_path: String) -> Self {
        Self { binpack_path }
    }

    pub fn annotate(&mut self) -> anyhow::Result<()> {
        let mut reader =
            sfbinpack::CompressedTrainingDataEntryReader::new(&self.binpack_path).unwrap();

        while reader.has_next() {
            let entry = reader.next();

            println!("{} {} {}", entry.ply, entry.mv.as_uci(), entry.pos.fen());
        }

        Ok(())
    }
}
