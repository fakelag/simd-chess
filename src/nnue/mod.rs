pub struct NnueConfig {
    pub hidden_size: usize,
    pub quant_scale: i32,
    pub qa: i16,
    pub qb: i16,
}

pub mod annotation;
pub mod nnue;
pub mod training;
