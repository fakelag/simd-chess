use bullet::{
    LocalSettings, TrainingSchedule, TrainingSteps,
    game::{
        formats::sfbinpack::{
            chess::{r#move::MoveType, piecetype::PieceType},
            *,
        },
        inputs,
    },
    lr,
    nn::optimiser,
    trainer::{save::SavedFormat, settings::TestDataset},
    value::{ValueTrainerBuilder, loader},
    wdl,
};

use super::NnueConfig;

pub fn train(
    name: &str,
    binpack_paths: &[&str],
    out_path: &str,
    valid_set_path: Option<&str>,
    cfg: &NnueConfig,
) {
    let mut trainer = ValueTrainerBuilder::default()
        .dual_perspective()
        // clipping [-1.98, 1.98]
        .optimiser(optimiser::AdamW)
        .inputs(inputs::Chess768)
        .save_format(&[
            SavedFormat::id("l0w").quantise::<i16>(cfg.qa),
            SavedFormat::id("l0b").quantise::<i16>(cfg.qa),
            SavedFormat::id("l1w").quantise::<i16>(cfg.qb),
            SavedFormat::id("l1b").quantise::<i16>(cfg.qa * cfg.qb),
        ])
        // `target` == wdl * game_result + (1 - wdl) * sigmoid(search score in centipawns / SCALE)
        .loss_fn(|output, target| output.sigmoid().squared_error(target))
        .build(|builder, stm_inputs, ntm_inputs| {
            let l0 = builder.new_affine("l0", 768, cfg.hidden_size);
            let l1 = builder.new_affine("l1", 2 * cfg.hidden_size, 1);

            let stm_hidden = l0.forward(stm_inputs).screlu();
            let ntm_hidden = l0.forward(ntm_inputs).screlu();
            let hidden_layer = stm_hidden.concat(ntm_hidden);
            l1.forward(hidden_layer)
        });

    let schedule = TrainingSchedule {
        net_id: name.to_string(),
        eval_scale: cfg.quant_scale as f32,
        steps: TrainingSteps {
            batch_size: 16_384,
            batches_per_superbatch: 6104,
            start_superbatch: 1,
            end_superbatch: 40,
        },
        wdl_scheduler: wdl::ConstantWDL { value: 0.75 },
        lr_scheduler: lr::StepLR {
            start: 0.001,
            gamma: 0.1,
            step: 18,
        },
        save_rate: 10,
    };

    let settings = LocalSettings {
        threads: 4,
        test_set: valid_set_path.map(|path| TestDataset { freq: 100, path }),
        output_directory: out_path,
        batch_queue_size: 64,
    };

    let data_loader = {
        let buffer_size_mb = 1024;
        let threads = 4;
        fn filter(entry: &TrainingDataEntry) -> bool {
            entry.ply >= 16
                && !entry.pos.is_checked(entry.pos.side_to_move())
                && entry.score.unsigned_abs() <= 10000
                && entry.mv.mtype() == MoveType::Normal
                && entry.pos.piece_at(entry.mv.to()).piece_type() == PieceType::None
        }

        loader::SfBinpackLoader::new_concat_multiple(binpack_paths, buffer_size_mb, threads, filter)
    };

    println!("Starting training with datasets: {:?}", binpack_paths);

    std::thread::sleep(std::time::Duration::from_secs(1));

    trainer.run(&schedule, &settings, &data_loader);
}
