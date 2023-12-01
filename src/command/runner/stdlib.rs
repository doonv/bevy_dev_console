use std::cell::Ref;

use bevy::log::info;

use crate::command::{Environment, Spanned};

use super::{RunError, Value};

fn print(value: Spanned<Value>) -> Result<(), RunError> {
    match value.value {
        Value::String(string) => info!("{string}"),
        _ => {
            let string = value.value.try_format(value.span)?;
            info!("{string}");
        }
    }
    Ok(())
}

fn dbg(any: Value) {
    info!("Value::{any:?}");
}

fn ref_depth(value: Value) -> f64 {
    fn ref_depth_reference(value: Ref<Value>) -> f64 {
        match &*value {
            Value::Reference(reference) => {
                ref_depth_reference(reference.upgrade().unwrap().borrow()) + 1.0
            }
            _ => 0.0,
        }
    }

    match value {
        Value::Reference(reference) => {
            ref_depth_reference(reference.upgrade().unwrap().borrow()) + 1.0
        }
        _ => 0.0,
    }
}

pub fn register(environment: &mut Environment) {
    environment
        .register_fn("print", print)
        .register_fn("dbg", dbg)
        .register_fn("ref_depth", ref_depth);
}
