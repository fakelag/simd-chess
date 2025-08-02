use crate::{
    engine::*,
    matchmaking::matchmaking::{Matchmaking, NEXT_MATCH_DELAY_SECONDS, VersusState},
    ui::square_ui::SquareUi,
    uicomponents::text_input::ImguiTextInput,
    util::{self, PieceId, square_name},
    window,
};

pub struct ChessUi {
    from_square: Option<u8>,
    ask_promotion: Option<(u8, u8)>,
    matchmaking: Matchmaking,
    squares: Vec<SquareUi>,

    input_fen: ImguiTextInput,
    input_white_engine: ImguiTextInput,
    input_black_engine: ImguiTextInput,
    input_num_games: ImguiTextInput,
    input_start_paused: bool,
    input_use_opening_book: bool,
}

fn draw_versus_stats(ui: &&mut imgui::Ui, matchmaking: &Matchmaking) {
    let stats = matchmaking.versus_stats();
    let white_engine = matchmaking.get_engine_for_side(util::Side::White).unwrap();
    let black_engine = matchmaking.get_engine_for_side(util::Side::Black).unwrap();

    let move_start_elapsed_ms = if let Some(move_start_time) = matchmaking.versus_move_start_time {
        move_start_time.elapsed().as_millis() as usize
    } else {
        0
    };

    let black_ms = matchmaking
        .versus_btime_ms
        .saturating_sub(move_start_elapsed_ms * matchmaking.board.b_move as usize);

    let white_ms = matchmaking
        .versus_wtime_ms
        .saturating_sub(move_start_elapsed_ms * !matchmaking.board.b_move as usize);

    ui.text(format!(
        "Matches: {}\n{} {} wins\n{} {} wins\nDraws: {}\nwtime: {}\nbtime: {}",
        stats.draws + stats.engine1_wins + stats.engine2_wins,
        stats.engine1_name,
        stats.engine1_wins,
        stats.engine2_name,
        stats.engine2_wins,
        stats.draws,
        util::time_format(white_ms as u64),
        util::time_format(black_ms as u64),
    ));

    ui.text(format!(
        "White Engine: {}\nBlack Engine: {}",
        white_engine.path, black_engine.path
    ));

    if let Some(opening) = &matchmaking.versus_current_opening {
        ui.text_wrapped(format!("Opening ({}): {}", opening.eco, opening.name));
    } else {
        ui.text_wrapped("Opening: None");
    }
}

impl ChessUi {
    pub fn new(fen: &'static str) -> Self {
        let mut squares = Vec::with_capacity(64);
        for rank in (0..8).rev() {
            for file in 0..8 {
                squares.push(SquareUi::new(rank, file));
            }
        }

        let matchmaking = Matchmaking::new(fen).unwrap();

        Self {
            matchmaking,
            squares,
            from_square: None,
            ask_promotion: None,
            input_fen: ImguiTextInput::new(
                imgui::InputTextFlags::AUTO_SELECT_ALL | imgui::InputTextFlags::ENTER_RETURNS_TRUE,
                Some("startpos moves e2e4"),
                Some(1024),
            ),
            input_white_engine: ImguiTextInput::new(
                imgui::InputTextFlags::AUTO_SELECT_ALL,
                Some("v6_psquare.exe"),
                None,
            ),
            input_black_engine: ImguiTextInput::new(
                imgui::InputTextFlags::AUTO_SELECT_ALL,
                Some("v5_tt.exe"),
                None,
            ),
            input_num_games: ImguiTextInput::new(
                imgui::InputTextFlags::AUTO_SELECT_ALL | imgui::InputTextFlags::CHARS_DECIMAL,
                Some("1000"),
                None,
            ),
            input_start_paused: false,
            input_use_opening_book: true,
        }
    }

    pub fn draw(&mut self, ctx: window::DrawCtx) {
        self.matchmaking.poll();

        let ui = ctx.ui;

        let [display_w, display_h] = ui.io().display_size;

        ui.window("chess")
            .bg_alpha(1.0)
            .resizable(false)
            .movable(false)
            .title_bar(false)
            .position([0.0, 0.0], imgui::Condition::Always)
            .size([display_w, display_h], imgui::Condition::Always)
            .build(|| {
                let [size_w, _] = ui.content_region_avail();
                let board_size = 0.7;

                let var_window_padding =
                    ui.push_style_var(imgui::StyleVar::WindowPadding([0.0, 0.0]));

                let mut board_cursor_xy: [f32; 2] = [0.0; 2];
                let mut square_wh = [0.0; 2];

                let mut hovering_sq_index = None;

                ui.child_window("board_container")
                    .size([size_w * board_size, -1.0])
                    .scroll_bar(false)
                    .build(|| {
                        ui.child_window("board")
                            .border(true)
                            .size([size_w * board_size, size_w * board_size])
                            .scroll_bar(false)
                            .build(|| {
                                square_wh = SquareUi::calc_square_wh(ui);

                                hovering_sq_index = (0..64).find_map(|square_index| {
                                    let [wnd_x, wnd_y] = ui.window_pos();
                                    let rank = square_index / 8;
                                    let file = square_index % 8;

                                    let x = wnd_x + file as f32 * square_wh[0];
                                    let y = wnd_y + (rank ^ 7) as f32 * square_wh[1];

                                    if ui.is_mouse_hovering_rect(
                                        [x, y],
                                        [x + square_wh[0], y + square_wh[1]],
                                    ) {
                                        Some(square_index as u8)
                                    } else {
                                        None
                                    }
                                });

                                if let Some(from_sq) = self.from_square {
                                    if !self
                                        .matchmaking
                                        .legal_moves
                                        .iter()
                                        .any(|&mv| mv & 0x3F == from_sq as u16)
                                    {
                                        self.from_square = None;
                                    }
                                }

                                board_cursor_xy = ui.cursor_screen_pos();

                                for rank in (0..8).rev() {
                                    for file in 0..8 {
                                        let square = &mut self.squares[rank * 8 + file];

                                        if self.ask_promotion.is_some() {
                                            square.reset_moving();
                                        } else if square.update(
                                            ui,
                                            &self.matchmaking.board,
                                            &mut self.from_square,
                                        ) {
                                            for mv_index in 0..self.matchmaking.legal_moves.len() {
                                                let curmove =
                                                    self.matchmaking.legal_moves[mv_index];

                                                let from_sq =
                                                    if let Some(from_sq) = self.from_square {
                                                        from_sq as u16
                                                    } else {
                                                        continue;
                                                    };

                                                if curmove & 0x3F != from_sq
                                                    || square.sq_bit_index
                                                        != ((curmove >> 6) as u8 & 0x3F)
                                                {
                                                    continue;
                                                }

                                                if (curmove & chess::MV_FLAG_PROMOTION) != 0 {
                                                    self.ask_promotion =
                                                        Some((from_sq as u8, square.sq_bit_index));
                                                    self.from_square = None;
                                                    break;
                                                }

                                                if let Err(err) = self
                                                    .matchmaking
                                                    .make_move_with_validation(curmove)
                                                {
                                                    panic!(
                                                        "Invalid move: {}: {}",
                                                        util::move_string(curmove),
                                                        err
                                                    );
                                                }
                                                self.from_square = None;
                                                break;
                                            }
                                        }

                                        square.draw_bg(ui, &self.from_square);
                                        square.draw_texture(ui, &ctx.textures);

                                        for mv_index in 0..self.matchmaking.legal_moves.len() {
                                            let curmove = self.matchmaking.legal_moves[mv_index];
                                            if let Some(from_sq) = self.from_square {
                                                if curmove & 0x3F == from_sq as u16
                                                    && square.sq_bit_index
                                                        == ((curmove >> 6) as u8 & 0x3F)
                                                {
                                                    square.draw_move_indicator(ui);
                                                }
                                            }
                                        }
                                    }
                                }

                                for rank in (0..8).rev() {
                                    for file in 0..8 {
                                        let square = &self.squares[rank * 8 + file];
                                        square.draw_highlights(ui, &ctx.textures);
                                    }
                                }
                            });

                        if let Some(position_str) = self.input_fen.draw(None, ui, "fen_inp") {
                            let mut moves = Vec::new();
                            let mut fen = String::new();

                            match util::parse_position(
                                &position_str,
                                &mut chess::ChessGame::new(),
                                &self.matchmaking.tables,
                                None,
                                Some(&mut moves),
                                Some(&mut fen),
                            ) {
                                Ok(_) => {
                                    if let Err(err) = self.matchmaking.load_fen(&fen) {
                                        eprintln!("Failed to load FEN: {}", err);
                                    } else {
                                        for mv in moves {
                                            if let Err(err) =
                                                self.matchmaking.make_move_with_validation(mv)
                                            {
                                                eprintln!(
                                                    "Failed to make move {}: {}",
                                                    util::move_string(mv),
                                                    err
                                                );
                                            }
                                        }
                                        // self.input_fen.buf.clear();
                                    }
                                    println!("Loaded position: {}", position_str);
                                }
                                Err(err) => {
                                    eprintln!("Failed to parse position: {}", err);
                                }
                            }
                        }
                    });

                var_window_padding.pop();

                ui.same_line();

                ui.child_window("side").border(true).build(|| {
                    ui.text(format!("is_valid: {}", self.matchmaking.board.is_valid()));
                    ui.text(format!(
                        "ep_square: {:?}",
                        self.matchmaking
                            .board
                            .en_passant
                            .and_then(|sq| Some(square_name(sq)))
                    ));
                    ui.text(format!(
                        "b_move: {}",
                        if self.matchmaking.board.b_move {
                            "black"
                        } else {
                            "white"
                        }
                    ));

                    ui.text(format!("Castles: {:04b}", self.matchmaking.board.castles));

                    ui.text(format!(
                        "zobrist_key: {:016X}",
                        self.matchmaking.board.zobrist_key
                    ));

                    ui.text(format!(
                        "half_moves: {}, full_moves: {}",
                        self.matchmaking.board.half_moves, self.matchmaking.board.full_moves
                    ));

                    ui.text(format!(
                        "In check: {}",
                        self.matchmaking
                            .board
                            .in_check_slow(&self.matchmaking.tables, self.matchmaking.board.b_move)
                    ));

                    if ui.button("Copy FEN") {
                        let fen = self.matchmaking.board.gen_fen();
                        ui.set_clipboard_text(fen);
                    }

                    ui.separator();

                    if let Some(hovering_sq_index) = hovering_sq_index {
                        ui.text(format!(
                            "Square: {} (index {})",
                            square_name(hovering_sq_index),
                            hovering_sq_index,
                        ));
                        let square_piece =
                            self.matchmaking.board.piece_at_slow(1 << hovering_sq_index);

                        if square_piece == 0 {
                            ui.text("No piece on this square");
                        } else {
                            ui.text(format!(
                                "Square bonus: {} (for {:?})",
                                tables::Tables::EVAL_TABLES_INV[square_piece - 1]
                                    [hovering_sq_index as usize],
                                PieceId::from(square_piece - 1)
                            ));
                        }
                    }
                    ui.separator();

                    match self.matchmaking.versus_state {
                        VersusState::Idle => {
                            self.input_white_engine
                                .draw(Some("White Engine"), ui, "w_engine_inp");
                            self.input_black_engine
                                .draw(Some("Black Engine"), ui, "b_engine_inp");
                            self.input_num_games
                                .draw(Some("Number of games"), ui, "num_games_inp");

                            ui.checkbox("Start paused", &mut self.input_start_paused);
                            ui.checkbox("Use opening book", &mut self.input_use_opening_book);

                            if ui.button("Start new Versus") {
                                if let Err(err) = self.matchmaking.versus_start(
                                    &self.input_white_engine.buf,
                                    &self.input_black_engine.buf,
                                    self.input_num_games.buf.parse().unwrap_or(1),
                                    self.input_start_paused,
                                    self.input_use_opening_book,
                                ) {
                                    eprintln!("Failed to spawn engines: {}", err);
                                }
                            }
                        }
                        VersusState::InProgress => {
                            ui.text("Versus match in progress");
                            draw_versus_stats(&ui, &self.matchmaking);

                            if ui.button("Pause Versus") {
                                self.matchmaking.versus_pause();
                            }
                            if ui.button("End Versus") {
                                self.matchmaking.versus_reset();
                            }
                        }
                        VersusState::Paused => {
                            ui.text("Versus match paused");
                            draw_versus_stats(&ui, &self.matchmaking);

                            if ui.button("Resume Versus") {
                                self.matchmaking.versus_resume();
                            }
                            if ui.button("Next Move") {
                                self.matchmaking.versus_step();
                            }
                            if ui.button("End Versus") {
                                self.matchmaking.versus_reset();
                            }
                        }
                        VersusState::Done => {
                            ui.text("Versus match done");
                            draw_versus_stats(&ui, &self.matchmaking);
                            if ui.button("Reset Versus") {
                                self.matchmaking.versus_reset();
                            }
                        }
                        VersusState::NextMatch(ended_at, _) => {
                            draw_versus_stats(&ui, &self.matchmaking);
                            ui.separator();
                            ui.text(format!(
                                "Next match starting in {} seconds",
                                NEXT_MATCH_DELAY_SECONDS
                                    .saturating_sub(ended_at.elapsed().as_secs())
                            ));
                        }
                    }

                    ui.separator();

                    match self.matchmaking.board.check_game_state(
                        &self.matchmaking.tables,
                        self.matchmaking.legal_moves.is_empty(),
                        self.matchmaking.board.b_move,
                    ) {
                        chess::GameState::Checkmate(side) => {
                            let engine_name = self
                                .matchmaking
                                .get_engine_for_side(side)
                                .and_then(|e| Some(e.path.clone()))
                                .unwrap_or_else(|| "none".to_string());

                            ui.text_wrapped(format!(
                                "Result: {} ({}) wins by checkmate",
                                if side == util::Side::White {
                                    "White"
                                } else {
                                    "Black"
                                },
                                engine_name
                            ));
                        }
                        chess::GameState::Stalemate => {
                            ui.text_wrapped("Result: Stalemate");
                        }
                        chess::GameState::DrawByFiftyMoveRule => {
                            ui.text_wrapped("Result: Draw by fifty-move rule");
                        }
                        chess::GameState::Ongoing => {}
                    }
                });

                if let Some((from_sq, to_sq)) = self.ask_promotion {
                    ui.set_cursor_pos(board_cursor_xy);

                    let draw_list = ui.get_foreground_draw_list();

                    let rank_offset = 7 - to_sq / 8;
                    let file_offset = match to_sq % 8 {
                        4..=7 => (to_sq % 8) - 4,
                        val => val + 1,
                    };

                    let [selector_x, selector_y] = [
                        board_cursor_xy[0] + square_wh[0] * file_offset as f32,
                        board_cursor_xy[1] + square_wh[1] * rank_offset as f32,
                    ];

                    [
                        chess::MV_FLAGS_PR_QUEEN,
                        chess::MV_FLAGS_PR_ROOK,
                        chess::MV_FLAGS_PR_BISHOP,
                        chess::MV_FLAGS_PR_KNIGHT,
                    ]
                    .iter()
                    .enumerate()
                    .for_each(|(i, flag)| {
                        let texture_id = ctx.textures[i
                            + PieceId::WhiteQueen as usize
                            + self.matchmaking.board.b_move as usize * 6];

                        let [option_x, option_y] =
                            [selector_x + square_wh[0] * i as f32, selector_y];

                        draw_list
                            .add_rect(
                                [option_x, option_y],
                                [option_x + square_wh[0], option_y + square_wh[1]],
                                [0.0, 0.0, 0.0, 0.5],
                            )
                            .filled(true)
                            .build();

                        draw_list
                            .add_image(
                                texture_id,
                                [option_x, option_y],
                                [option_x + square_wh[0], option_y + square_wh[1]],
                            )
                            .build();

                        if ui.is_mouse_clicked(imgui::MouseButton::Left)
                            && ui.is_mouse_hovering_rect(
                                [option_x, option_y],
                                [option_x + square_wh[0], option_y + square_wh[1]],
                            )
                        {
                            if let Err(err) = self.matchmaking.make_move_with_validation(
                                (from_sq as u16) | ((to_sq as u16) << 6) | *flag,
                            ) {
                                panic!(
                                    "Invalid promotion move: {}: {}",
                                    util::move_string(
                                        (from_sq as u16) | ((to_sq as u16) << 6) | *flag
                                    ),
                                    err
                                );
                            }
                            self.ask_promotion = None;
                            self.from_square = None;
                        }
                    });
                }
            });
    }
}
