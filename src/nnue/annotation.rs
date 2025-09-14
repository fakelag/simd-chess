use std::thread::JoinHandle;

type AnnotatableGame = Vec<sfbinpack::TrainingDataEntry>;

pub struct Annotator {
    binpack_path: String,
    handles: Vec<JoinHandle<()>>,
    tx_send: Option<crossbeam::channel::Sender<AnnotatableGame>>,
    // rx_recv: std::sync::mpsc::Receiver<AnnotatableGame>,
}

impl Annotator {
    pub fn new(threads: usize, binpack_path: String) -> Self {
        let (tx_send, rx_recv) = crossbeam::channel::bounded::<AnnotatableGame>(100);

        let handles = (0..threads)
            .into_iter()
            .map(|_i| {
                let rx_recv = rx_recv.clone();
                std::thread::spawn(move || {
                    while let Ok(entries) = rx_recv.recv() {
                        println!("Received game with {} entries", entries.len());
                    }
                })
            })
            .collect::<Vec<JoinHandle<()>>>();

        Self {
            handles,
            binpack_path,
            tx_send: Some(tx_send),
            // rx_recv,
        }
    }

    pub fn annotate(&mut self) -> anyhow::Result<()> {
        let st = std::time::Instant::now();

        let tx_send = self.tx_send.take().unwrap();

        let mut reader =
            sfbinpack::CompressedTrainingDataEntryReader::new(&self.binpack_path).unwrap();

        let mut prev_entry: Option<sfbinpack::TrainingDataEntry> = None;
        let mut counter = 1usize;

        let mut entries = vec![];

        while reader.has_next() {
            let entry = reader.next();

            if let Some(prev_entry) = &prev_entry {
                let is_cont = prev_entry.is_continuation(&entry);

                if !is_cont {
                    tx_send.send(entries).unwrap();
                    entries = vec![];
                }
            }

            if counter % 10000000 == 0 {
                println!(
                    "Read {} bytes: {:.02}% done",
                    reader.read_bytes(),
                    reader.read_bytes() as f64 / reader.file_size() as f64 * 100.0
                );
            }

            entries.push(entry);

            prev_entry = Some(entry);
            counter += 1;
        }

        println!("Total positions: {}", counter);
        println!("Time: {:?}", st.elapsed());

        drop(tx_send);

        for handle in self.handles.drain(..) {
            handle.join().unwrap();
        }

        Ok(())
    }
}
