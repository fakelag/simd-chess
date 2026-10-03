use crate::engine::search::eval::Eval;

// name: type = default, min..=max, c_end;
tunables! {
    RFP_MAX_DEPTH:  u8    = 8,    4..=14,       0.5;
    RFP_MARGIN:     Eval  = 180,  60..=360,     15.0;

    NMP_MIN_DEPTH:  u8    = 3,    2..=6,        0.5;
    NMP_BASE_R:     u8    = 2,    1..=5,        0.5;
    NMP_DEPTH_DIV:  u8    = 3,    2..=6,        0.5;
    NMP_EVAL_R_DIV: Eval  = 192,  64..=512,     20.0;
    NMP_EVAL_R_MAX: u8    = 3,    0..=6,        0.5;

    LMR_BASE:       i32   = 75,   0..=150,      8.0;
    LMR_DIV:        i32   = 220,  120..=400,    12.0;
    LMR_MIN_MOVES:  usize = 3,    1..=6,        0.5;
    LMR_MIN_DEPTH:  u8    = 3,    2..=5,        0.5;
    LMR_HIST_DIV:   i32   = 8192, 2048..=16384, 600.0;
    LMR_HIST_MAX:   i32   = 2,    0..=4,        0.5;

    CORR_GRAIN:        Eval = 8,   2..=32,   1.5;
    CORR_W_PAWN:       Eval = 81,  32..=256, 12.0;
    CORR_W_NP_STM:     Eval = 48,  0..=160,  8.0;
    CORR_W_NP_NTM:     Eval = 24,  0..=128,  6.0;
    CORR_WEIGHT_SCALE: Eval = 128, 32..=512, 16.0;

    SEE_CAPTURE_MARGIN:  Eval = 100, 30..=200,   8.0;
    SEE_QUIET_MARGIN:    Eval = 12,  3..=30,     1.5;
    SEE_HISTORY_DIVISOR: Eval = 128, 32..=512,   24.0;
    SEE_QS_THRESHOLD:    Eval = -20, -120..=60,  9.0;

    SE_BETA_MARGIN:   Eval = 48, 16..=128, 6.0;
    SE_DOUBLE_MARGIN: Eval = 20, 0..=60, 3.0;
    ASP_WINDOW:       Eval = 17, 6..=60, 3.0;
}

#[cfg(not(feature = "spsa"))]
macro_rules! param {
    ($name:ident) => {
        $name
    };
}

#[cfg(feature = "spsa")]
macro_rules! param {
    ($name:ident) => {
        $name.get()
    };
}
pub(crate) use param;

#[cfg(feature = "spsa")]
pub const SPSA_R_END: f64 = 0.002;

#[cfg(feature = "spsa")]
pub struct TunableMeta {
    pub name: &'static str,
    pub default: i32,
    pub min: i32,
    pub max: i32,
    pub c_end: f64,
    value: std::sync::atomic::AtomicI32,
}

#[cfg(feature = "spsa")]
impl TunableMeta {
    pub fn value(&self) -> i32 {
        self.value.load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn set(&self, value: i32) -> anyhow::Result<()> {
        if !(self.min..=self.max).contains(&value) {
            return Err(anyhow::anyhow!(
                "{} = {} is outside [{}, {}]",
                self.name,
                value,
                self.min,
                self.max
            ));
        }
        self.value
            .store(value, std::sync::atomic::Ordering::Relaxed);
        Ok(())
    }
}

#[cfg(feature = "spsa")]
pub struct Tunable<T> {
    pub meta: TunableMeta,
    _ty: std::marker::PhantomData<fn() -> T>,
}

#[cfg(feature = "spsa")]
impl<T> Tunable<T> {
    const fn new(name: &'static str, default: i32, min: i32, max: i32, c_end: f64) -> Self {
        Self {
            meta: TunableMeta {
                name,
                default,
                min,
                max,
                c_end,
                value: std::sync::atomic::AtomicI32::new(default),
            },
            _ty: std::marker::PhantomData,
        }
    }
}

#[cfg(feature = "spsa")]
pub trait FromTunable {
    fn from_tunable(v: i32) -> Self;
}

#[cfg(feature = "spsa")]
macro_rules! impl_from_tunable {
    ($($ty:ty),+) => {
        $(
            impl FromTunable for $ty {
                #[inline(always)]
                fn from_tunable(v: i32) -> Self {
                    v as $ty
                }
            }
        )+
    };
}

#[cfg(feature = "spsa")]
impl_from_tunable!(u8, i16, i32, usize);

#[cfg(feature = "spsa")]
impl<T: FromTunable> Tunable<T> {
    #[inline(always)]
    pub fn get(&self) -> T {
        T::from_tunable(self.meta.value.load(std::sync::atomic::Ordering::Relaxed))
    }
}

macro_rules! tunables {
    ($($name:ident: $ty:ty = $default:literal, $min:literal..=$max:literal, $c_end:literal;)+) => {
        $(
            const _: () = assert!(
                $min <= $default
                    && $default <= $max
                    && $min as $ty as i32 == $min
                    && $max as $ty as i32 == $max
                    && $c_end > 0.0
            );

            #[cfg(not(feature = "spsa"))]
            pub const $name: $ty = $default;

            #[cfg(feature = "spsa")]
            pub static $name: Tunable<$ty> =
                Tunable::new(stringify!($name), $default, $min, $max, $c_end);
        )+

        #[cfg(feature = "spsa")]
        pub static ALL: &[&TunableMeta] = &[$(&$name.meta),+];
    };
}
use tunables;
