//! A "standard library" for the builtin parser, this is a collection of functions that could
//! be useful for whatever you need to do with the builtin parser.

use crate::builtin_parser::runner::environment::Variable;
use crate::builtin_parser::runner::function::Function;
use crate::register;
use bevy::ecs::world::World;
use bevy::log::info;
use bevy::reflect::TypeRegistration;
use kinded::Kinded;
use std::cell::Ref;
use std::fmt::Write;
use std::ops::Range;

mod math;

use super::error::EvalError;
use super::{Environment, Spanned, Value};

fn print(
    values: Vec<Spanned<Value>>,
    world: &mut World,
    registrations: &[&TypeRegistration],
) -> Result<(), EvalError> {
    let mut output = String::new();
    for Spanned { span, value } in values {
        let string = match value {
            Value::String(string) => string,
            _ => value.try_format(span, world, registrations)?,
        };
        write!(output, "{string} ").unwrap();
    }
    info!("{output}");
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
    value.kind().as_str().to_owned()
}

/// Disposes of a [`Value`].
fn drop(_: Value) {}

fn alias(from: String, to: String, environment: &mut Environment) {
    environment.register_fn(
        to,
        move |arguments: Vec<Spanned<Value>>,
              environment: &mut Environment,
              world: &mut World,
              registrations: &[&TypeRegistration]| {
            environment.run_function(&from, arguments, world, registrations)
        },
    );
}

fn help(environment: &Environment) {
    struct Help<'e>(&'e Environment);
    impl<'e> std::fmt::Display for Help<'e> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for (name, variable) in self.0 {
                if let &Variable::Function(Function { argument_count, .. }) = variable {
                    write!(
                        f,
                        "\n    {name} - {argument_count} arg{} - TODO",
                        if argument_count == 1 { "" } else { "s" }
                    )?;
                }
            }
            Ok(())
        }
    }
    info!(
        "TODO: Add help text for functions and function signatures {}",
        Help(environment)
    );
}

pub(super) fn register(environment: &mut Environment) {
    math::register(environment);

    register!(environment => {
        fn print;
        fn dbg;
        fn ref_depth;
        fn drop;
        fn print_env;
        fn typeof_value as "typeof";
        fn alias;
        fn help;
    });
}
