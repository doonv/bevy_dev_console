use bevy::log::info;

use crate::parser::Environment;

use super::Value;

fn dbg(any: Value) {
    info!("{any:?}");
}
fn add(num1: f64, num2: f64) -> f64 {
    num1 + num2
}

pub fn register(environment: &mut Environment) {
    environment.register_fn("dbg", dbg);
    environment.register_fn("add", add);
}
