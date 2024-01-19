use crate::builtin_parser::{Environment, Number, RunError, Spanned};
use crate::register;

macro_rules! float_calc_op {
    ($fn:ident, $name:expr) => {
        fn $fn(number: Spanned<Number>) -> Result<Number, RunError> {
            match number.value {
                Number::Float(number) => Ok(Number::Float(number.$fn())),
                Number::f32(number) => Ok(Number::f32(number.$fn())),
                Number::f64(number) => Ok(Number::f64(number.$fn())),
                _ => Err(RunError::Custom {
                    text: concat!("Cannot calculate the ", $name, " of a non-float value").into(),
                    span: number.span,
                }),
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
