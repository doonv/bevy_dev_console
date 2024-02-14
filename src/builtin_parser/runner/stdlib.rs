use crate::builtin_parser::runner::environment::Variable;
use crate::register;
use bevy::ecs::world::World;
use bevy::log::info;
use bevy::reflect::TypeRegistration;
use std::cell::Ref;
use std::ops::Range;

mod math;

use super::error::EvalError;
use super::{Environment, Spanned, Value};

fn print(
    value: Spanned<Value>,
    world: &mut World,
    registrations: &[&TypeRegistration],
) -> Result<(), EvalError> {
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

fn ref_depth(Spanned { span, value }: Spanned<Value>) -> Result<usize, EvalError> {
    fn ref_depth_reference(value: Ref<Value>, span: Range<usize>) -> Result<usize, EvalError> {
        Ok(match &*value {
            Value::Reference(reference) => {
                ref_depth_reference(
                    reference
                        .upgrade()
                        .ok_or(EvalError::ReferenceToMovedData(span.clone()))?
                        .borrow(),
                    span,
                )? + 1
            }
            _ => 0,
        })
    }

    Ok(match value {
        Value::Reference(reference) => {
            ref_depth_reference(
                reference
                    .upgrade()
                    .ok_or(EvalError::ReferenceToMovedData(span.clone()))?
                    .borrow(),
                span,
            )? + 1
        }
        _ => 0,
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

fn typeof_value(value: Value) -> String {
    value.kind().to_string()
}

/// Disposes of a [`Value`].
fn drop(_: Value) {}

pub fn register(environment: &mut Environment) {
    math::register(environment);

    register!(environment => {
        fn print;
        fn dbg;
        fn ref_depth;
        fn drop;
        fn print_env;
        fn typeof_value as "typeof";
    });
}
