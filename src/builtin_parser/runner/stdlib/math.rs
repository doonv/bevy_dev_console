use crate::builtin_parser::{Environment, Float};
use crate::register;

macro_rules! float_calc_op {
    ($fn:ident, $name:expr) => {
        fn $fn(number: Float) -> Float {
            match number {
                Float::f32(number) => Float::f32(number.$fn()),
                Float::f64(number) => Float::f64(number.$fn()),
                Float::Unspecified(number) => Float::Unspecified(number.$fn()),
            }
        }
    };
}

float_calc_op!(sqrt, "square root");
float_calc_op!(sin, "sine");
float_calc_op!(cos, "cosine");
float_calc_op!(tan, "tangent");

float_calc_op!(abs, "absolute value");

float_calc_op!(ceil, "rounded-up value");
float_calc_op!(floor, "rounded-down value");
float_calc_op!(round, "rounded value");
float_calc_op!(trunc, "truncuated value");

pub fn register(env: &mut Environment) {
    register!(env => {
        fn sqrt;
        fn sin;
        fn cos;
        fn tan;

        fn abs;

        fn ceil;
        fn floor;
        fn round;
        fn trunc;
    });
}
