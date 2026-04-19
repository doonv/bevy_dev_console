//! Executes the abstract syntax tree.

use environment::Environment;

use bevy::prelude::*;
use bevy::reflect::TypeRegistration;

use self::error::EvalError;

use super::Spanned;
use super::parser::{Ast, Expression};

pub(super) mod environment;
pub(super) mod error;
pub(super) mod eval;
pub(super) mod function;
pub(super) mod member;
pub(super) mod reflection;
pub(super) mod stdlib;
pub(super) mod unique_rc;
pub(super) mod value;

pub use eval::eval_expression;
pub use value::Value;

/// Temporary macro that prevents panicking by replacing the [`todo!`] panic with an error message.
macro_rules! todo_error {
    () => {
        return Err($crate::builtin_parser::EvalError::Custom {
            text: concat!("todo error invoked at ", file!(), ":", line!(), ":", column!()).into(),
            span: 0..0
        })
    };
    ($($arg:tt)+) => {
        return Err($crate::builtin_parser::EvalError::Custom {
            text: format!(concat!("todo error invoked at ", file!(), ":", line!(), ":", column!(), ": {}"), format_args!($($arg)+)).into(),
            span: 0..0
        })
    };
}
pub(crate) use todo_error;

/// Container for every value needed by evaluation functions.
pub struct EvalParams<'world, 'env, 'reg> {
    pub world: &'world mut World,
    pub environment: &'env mut Environment,
    pub registrations: &'reg [&'reg TypeRegistration],
}

pub fn eval(
    ast: Ast,
    world: &mut World,
    environment: &mut Environment,
    registry: &AppTypeRegistry,
) -> Result<Option<String>, EvalError> {
    let registry_read = registry.read();

    let registrations: Vec<_> = registry_read
        .iter()
        .filter(|registration| {
            world
                .components()
                .get_resource_id(registration.type_id())
                .is_some()
        })
        .collect();
    let mut last_value = None;
    for mut statement in ast {
        fn autoborrow(statement: Spanned<Expression>) -> Spanned<Expression> {
            let value = match statement.value {
                Expression::Variable(variable) => Expression::Borrow(Box::new(Spanned {
                    span: statement.span.clone(),
                    value: Expression::Variable(variable),
                })),
                Expression::Member { mut left, right } => {
                    *left = autoborrow(*left);
                    Expression::Member { left, right }
                }
                expr => expr,
            };
            Spanned {
                span: statement.span,
                value,
            }
        }
        // Automatically borrow variables to prevent move errors on `x`
        statement = autoborrow(statement);

        let span = statement.span.clone();
        let value = eval_expression(
            statement,
            EvalParams {
                world,
                environment,
                registrations: &registrations,
            },
        )?;

        last_value = match value {
            Value::None => None,
            value => Some(value.try_format(span, world, &registrations)?),
        }
    }

    Ok(last_value)
}
