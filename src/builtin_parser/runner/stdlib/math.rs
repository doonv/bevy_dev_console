use crate::builtin_parser::Environment;
use crate::register;

// TODO: Add support for non-f64 ops

macro_rules! float_calc_op {
    ($fn:ident, $name:expr) => {
        fn $fn(number: f64) -> f64 {
            number.$fn()
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

fn pow(number: f64, power: f64) -> f64 {
    number.powf(power)
}

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


        fn pow;
    });
}
