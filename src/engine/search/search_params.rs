#[derive(Debug)]
pub struct SearchParams {
    pub depth: Option<u8>,
    pub btime: Option<u32>,
    pub wtime: Option<u32>,
    pub winc: Option<u32>,
    pub binc: Option<u32>,
    pub movestogo: Option<u8>,
    pub nodes: Option<u64>,
    pub mate: Option<u32>,
    pub movetime: Option<u32>,
    pub infinite: bool,
}

impl SearchParams {
    pub fn new() -> Self {
        SearchParams {
            depth: None,
            btime: None,
            wtime: None,
            winc: None,
            binc: None,
            movestogo: None,
            nodes: None,
            mate: None,
            movetime: None,
            infinite: false,
        }
    }
}

impl<'a> FromIterator<&'a str> for SearchParams {
    fn from_iter<I: IntoIterator<Item = &'a str>>(iter: I) -> Self {
        let mut search_params = SearchParams::new();
        let mut iter = iter.into_iter();

        loop {
            let command = iter.next();

            if command.is_none() {
                break;
            }

            macro_rules! next_int {
                ($input:expr,$ty:ty) => {
                    $input.next().and_then(|s| s.parse::<$ty>().ok())
                };
            }

            match command {
                Some("depth") => search_params.depth = next_int!(iter, u8),
                Some("btime") => search_params.btime = next_int!(iter, u32),
                Some("wtime") => search_params.wtime = next_int!(iter, u32),
                Some("winc") => search_params.winc = next_int!(iter, u32),
                Some("binc") => search_params.binc = next_int!(iter, u32),
                Some("movestogo") => search_params.movestogo = next_int!(iter, u8),
                Some("nodes") => search_params.nodes = next_int!(iter, u64),
                Some("mate") => search_params.mate = next_int!(iter, u32),
                Some("movetime") => search_params.movetime = next_int!(iter, u32),
                Some("infinite") => search_params.infinite = true,
                _ => break,
            }
        }

        search_params
    }
}
