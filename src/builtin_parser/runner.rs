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
#[macro_export]
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

/// Container for every value needed by evaluation functions.
pub struct EvalParams<'world, 'env, 'reg> {
    pub world: &'world mut World,
    pub environment: &'env mut Environment,
    pub registrations: &'reg [&'reg TypeRegistration],
}

#[derive(Debug)]
pub enum ExecutionError {
    NoEnvironment,
    NoTypeRegistry,
    Eval(EvalError),
}

impl std::fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoEnvironment => write!(
                f,
                "Environment resource doesn't exist, not executing command."
            ),
            Self::NoTypeRegistry => write!(
                f,
                "The AppTypeRegistry doesn't exist, not executing command. "
            ),
            Self::Eval(run_error) => <EvalError as std::fmt::Display>::fmt(run_error, f),
        }
    }
}

impl std::error::Error for ExecutionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ExecutionError::Eval(eval) => Some(eval),
            _ => None,
        }
    }
}

impl From<EvalError> for ExecutionError {
    fn from(value: EvalError) -> Self {
        Self::Eval(value)
    }
}

pub fn run(ast: Ast, world: &mut World) -> Result<Option<String>, ExecutionError> {
    // Temporarily remove the [`Environment`] resource to gain
    // mutability without needing a mutable reference.
    let mut environment = world
        .remove_non_send_resource::<Environment>()
        .ok_or(ExecutionError::NoEnvironment)?;

    // Same thing here (this time we are doing it because we are passing a `&mut World` to `eval_expression`)
    let Some(registry) = world.remove_resource::<AppTypeRegistry>() else {
        // Make sure to re-insert the resource on failure
        world.insert_non_send_resource(environment);

        return Err(ExecutionError::NoTypeRegistry);
    };

    let result = (|| {
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
            // Automatically borrow variables
            statement = autoborrow(statement);

            let span = statement.span.clone();
            let value = eval_expression(
                statement,
                EvalParams {
                    world,
                    environment: &mut environment,
                    registrations: &registrations,
                },
            )?;

            last_value = match value {
                Value::None => None,
                value => Some(value.try_format(span, world, &registrations)?),
            }
        }

        Ok(last_value)
    })();

    // Add back the resources
    world.insert_resource(registry);
    world.insert_non_send_resource(environment);

    result
}
