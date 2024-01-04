use crate::{builtin_parser::runner::environment::Variable, register};
use bevy::{ecs::world::World, log::info, reflect::TypeRegistration};
use std::{cell::Ref, ops::Range};

use super::{error::RunError, Environment, Spanned, Value};

fn print(
    value: Spanned<Value>,
    world: &mut World,
    registrations: &[&TypeRegistration],
) -> Result<(), RunError> {
    match value.value {
        Value::String(string) => info!("{string}"),
        _ => {
            let string = value.value.try_format(value.span, world, registrations)?;
            info!("{string}");
        }
    }
    Ok(())
}

fn dbg(any: Value) {
    info!("Value::{any:?}");
}

fn ref_depth(Spanned { span, value }: Spanned<Value>) -> Result<f64, RunError> {
    fn ref_depth_reference(value: Ref<Value>, span: Range<usize>) -> Result<f64, RunError> {
        Ok(match &*value {
            Value::Reference(reference) => {
                ref_depth_reference(
                    reference
                        .upgrade()
                        .ok_or(RunError::ReferenceToMovedData(span.clone()))?
                        .borrow(),
                    span,
                )? + 1.0
            }
            _ => 0.0,
        })
    }

    Ok(match value {
        Value::Reference(reference) => {
            ref_depth_reference(
                reference
                    .upgrade()
                    .ok_or(RunError::ReferenceToMovedData(span.clone()))?
                    .borrow(),
                span,
            )? + 1.0
        }
        _ => 0.0,
    })
}

fn print_env(env: &mut Environment) {
    for (name, variable) in env.iter() {
        match variable {
            Variable::Moved => info!("{name}: Moved"),
            Variable::Unmoved(rc) => info!("{name}: {:?}", rc.borrow_inner().borrow()),
            Variable::Function(_) => {}
        }
    }
}

/// Disposes of a [`Value`].
fn drop(_: Value) {}

pub fn register(environment: &mut Environment) {
    register!(environment => {
        fn print;
        fn dbg;
        fn ref_depth;
        fn drop;
        fn print_env;
    });
}
