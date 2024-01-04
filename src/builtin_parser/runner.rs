//! Executes the abstract syntax tree.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use environment::Environment;

use crate::{command::CommandHints, ui::COMMAND_RESULT_NAME};

use self::{
    error::RunError,
    reflection::{object_to_dynamic_struct, CreateRegistration, IntoResource},
    unique_rc::{UniqueRc, WeakRef},
};

use super::{
    parser::{Ast, Expression, Operator},
    Number, SpanExtension, Spanned,
};
use bevy::{
    prelude::*,
    reflect::{DynamicEnum, Enum, ReflectMut, TypeInfo, TypeRegistration},
};

pub mod environment;
pub mod error;
pub mod reflection;
pub mod stdlib;
pub mod unique_rc;
pub mod value;

pub use value::Value;

/// Temporary macro that prevents panicking by replacing the [`todo!`] panic with an error message.
macro_rules! todo_error {
    () => {
        Err(RunError::Custom {
            text: "This error message is not yet implemented.".into(),
            span: 0..0
        })?
    };
    ($($arg:tt)+) => {
        Err(RunError::Custom {
            text: format!("This error message is not yet implemented: {}", format_args!($($arg)+)).into(),
            span: 0..0
        })?
    };
}

/// Container for every value needed by evaluation functions.
pub struct EvalParams<'world, 'env, 'reg> {
    world: &'world mut World,
    environment: &'env mut Environment,
    registrations: &'reg [&'reg TypeRegistration],
}

pub fn run(ast: Ast, world: &mut World) {
    // Temporarily remove the [`Environment`] resource to gain
    // mutability without needing a mutable reference.
    let Some(mut environment) = world.remove_non_send_resource::<Environment>() else {
        error!("Environment resource doesn't exist, not executing command.");
        return;
    };

    // Same thing here (this time we are doing it because we are passing a `&mut World` to `eval_expression`)
    let Some(registry) = world.remove_resource::<AppTypeRegistry>() else {
        error!("The AppTypeRegistry doesn't exist, not executing command. (What have you done to cause this? o_O)");
        return;
    };

    let Some(mut hints) = world.remove_resource::<CommandHints>() else {
        error!("CommandHints don't exist, not executing commands. (Seriously how did this happen)");
        return;
    };

    {
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

        for mut statement in ast {
            // Automatically borrow variables
            statement.value = match statement.value {
                Expression::Variable(variable) => Expression::Borrow(Box::new(Spanned {
                    span: statement.span.clone(),
                    value: Expression::Variable(variable),
                })),
                expr => expr,
            };

            let span = statement.span.clone();
            let value = eval_expression(
                statement,
                EvalParams {
                    world,
                    environment: &mut environment,
                    registrations: &registrations,
                },
            );

            match value {
                Ok(Value::None) => {}
                Ok(value) => match value.try_format(span, world, &registrations) {
                    Ok(value) => {
                        info!(name: COMMAND_RESULT_NAME, "{}{value}", crate::ui::COMMAND_RESULT_PREFIX)
                    }
                    Err(err) => error!("{err:?}"),
                },
                Err(err) => {
                    hints.push(err.hints());

                    error!("{}", err.message());
                }
            }
        }
    }

    // Add back the resources
    world.insert_resource(hints);
    world.insert_resource(registry);
    world.insert_non_send_resource(environment);
}

fn eval_expression(
    expr: Spanned<Expression>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<Value, RunError> {
    match expr.value {
        Expression::VarAssign {
            name,
            value: value_expr,
        } => match eval_path(
            *name,
            EvalParams {
                world,
                environment,
                registrations,
            },
        )?
        .value
        {
            Path::Variable(variable) => {
                let value = eval_expression(
                    *value_expr,
                    EvalParams {
                        world,
                        environment,
                        registrations,
                    },
                )?;

                *variable.upgrade().unwrap().borrow_mut() = value;

                Ok(Value::Reference(variable))
            }
            Path::NewVariable(variable) => {
                let value = eval_expression(
                    *value_expr,
                    EvalParams {
                        world,
                        environment,
                        registrations,
                    },
                )?;
                let rc = UniqueRc::new(value);
                let weak = rc.borrow();

                environment.set(variable, rc);

                Ok(Value::Reference(weak))
            }
            Path::Resource(resource) => {
                let registeration = registrations.create_registration(resource.id);
                let mut dyn_reflect = resource.mut_dyn_reflect(world, registeration);

                let reflect = dyn_reflect
                    .reflect_path_mut(resource.path.as_str())
                    .unwrap();

                match reflect.reflect_mut() {
                    ReflectMut::Enum(dyn_enum) => {
                        let TypeInfo::Enum(enum_info) = registeration.type_info() else {
                            unreachable!();
                        };
                        let Spanned { span, value } = *value_expr;
                        match value {
                            Expression::Variable(variable) => {
                                if enum_info.contains_variant(&variable) {
                                    let new_enum = DynamicEnum::new(variable, ());

                                    dyn_enum.set(Box::new(new_enum)).map_err(|new_enum| {
                                        RunError::IncompatibleReflectTypes {
                                            span,
                                            expected: dyn_enum.variant_name().to_string(),
                                            actual: new_enum
                                                .downcast::<DynamicEnum>()
                                                .unwrap()
                                                .variant_name()
                                                .to_string(),
                                        }
                                    })
                                } else {
                                    Err(RunError::EnumVariantNotFound {
                                        name: variable,
                                        span,
                                    })
                                }?
                            }
                            Expression::StructObject { name, map } => {
                                let map: HashMap<String, Value> = map
                                    .into_iter()
                                    .map(|(k, v)| {
                                        Ok((
                                            k,
                                            eval_expression(
                                                v,
                                                EvalParams {
                                                    world,
                                                    environment,
                                                    registrations,
                                                },
                                            )?,
                                        ))
                                    })
                                    .collect::<Result<_, _>>()?;
                                let new_enum =
                                    DynamicEnum::new(name, object_to_dynamic_struct(map));

                                let mut dyn_reflect =
                                    resource.mut_dyn_reflect(world, registrations);

                                let dyn_enum = dyn_reflect
                                    .reflect_path_mut(resource.path.as_str())
                                    .unwrap();

                                dyn_enum.apply(&new_enum);
                            }
                            _ => todo_error!(),
                        }
                    }
                    _ => {
                        let span = value_expr.span.clone();
                        let value = eval_expression(
                            *value_expr,
                            EvalParams {
                                world,
                                environment,
                                registrations,
                            },
                        )?;
                        let value_reflect = value.reflect();

                        let mut dyn_reflect = resource.mut_dyn_reflect(world, registrations);

                        let reflect = dyn_reflect
                            .reflect_path_mut(resource.path.as_str())
                            .unwrap();

                        reflect.set(value_reflect).map_err(|value_reflect| {
                            RunError::IncompatibleReflectTypes {
                                span,
                                expected: reflect.reflect_type_path().to_string(),
                                actual: value_reflect.reflect_type_path().to_string(),
                            }
                        })?;
                    }
                }

                Ok(Value::Resource(resource))
            }
        },
        Expression::String(string) => Ok(Value::String(string)),
        Expression::Number(number) => Ok(Value::Number(number)),
        Expression::Variable(variable) => {
            if registrations
                .iter()
                .any(|v| v.type_info().type_path_table().short_path() == variable)
            {
                Err(RunError::CannotMoveOutOfResource(Spanned {
                    span: expr.span,
                    value: variable,
                }))
            } else {
                environment.move_var(&variable, expr.span)
            }
        }
        Expression::BinaryOp {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(
                *left,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;
            let right = eval_expression(
                *right,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;

            match (left, right) {
                (Value::Number(left), Value::Number(right)) => Ok(Value::Number(match operator {
                    Operator::Add => Number::add(left, right, expr.span)?,
                    Operator::Sub => Number::sub(left, right, expr.span)?,
                    Operator::Mul => Number::mul(left, right, expr.span)?,
                    Operator::Div => Number::div(left, right, expr.span)?,
                    Operator::Mod => Number::rem(left, right, expr.span)?,
                })),
                (left, right) => todo_error!("{left:#?}, {right:#?}"),
            }
        }
        Expression::ForLoop {
            index_name,
            loop_count,
            block,
        } => todo_error!("for loop {index_name}, {loop_count}, {block:#?}"),
        Expression::Member { left, right } => eval_member_expression(
            *left,
            right,
            EvalParams {
                world,
                environment,
                registrations,
            },
        ),
        Expression::UnaryOp(sub_expr) => {
            let span = sub_expr.span.clone();
            let value = eval_expression(
                *sub_expr,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;

            if let Value::Number(number) = value {
                Ok(Value::Number(number.neg(span)?))
            } else {
                Err(RunError::ExpectedNumberAfterUnaryOperator(Spanned {
                    span,
                    value,
                }))
            }
        }
        Expression::StructObject { name, map } => {
            let hashmap = eval_object(
                map,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;
            Ok(Value::StructObject { name, map: hashmap })
        }
        Expression::Object(map) => {
            let hashmap = eval_object(
                map,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;
            Ok(Value::Object(hashmap))
        }
        Expression::Dereference(_inner) => Err(RunError::Custom {
            text: "Dereferencing without assignment is not possible at the moment. (TODO)".into(),
            span: expr.span,
        }),
        Expression::Borrow(inner) => {
            if let Expression::Variable(variable) = inner.value {
                if let Some(registration) = registrations
                    .iter()
                    .find(|v| v.type_info().type_path_table().short_path() == variable)
                {
                    Ok(Value::Resource(IntoResource::new(registration.type_id())))
                } else {
                    let rc = environment.get(&variable, inner.span)?;
                    let weak = rc.borrow();

                    Ok(Value::Reference(weak))
                }
            } else {
                Err(RunError::CannotBorrowValue(expr.span))
            }
        }
        Expression::None => Ok(Value::None),
        Expression::Boolean(bool) => Ok(Value::Boolean(bool)),
        Expression::Function { name, arguments } => {
            environment.function_scope(&name, move |environment, function| {
                (function.body)(
                    arguments,
                    EvalParams {
                        world,
                        environment,
                        registrations,
                    },
                )
            })
        }
    }
}

fn eval_member_expression(
    left: Spanned<Expression>,
    right: String,
    params: EvalParams,
) -> Result<Value, RunError> {
    let left_span = left.span.clone();
    let EvalParams {
        world,
        environment,
        registrations,
    } = params;
    // TODO: Add ability to borrow from a struct.
    let left = eval_expression(
        left,
        EvalParams {
            world,
            environment,
            registrations,
        },
    )?;

    match left {
        Value::Object(mut map) | Value::StructObject { mut map, .. } => {
            let value = map
                .remove(&right)
                .ok_or(RunError::FieldNotFoundInStruct(left_span))?;
            let value = Rc::try_unwrap(value).unwrap();

            Ok(value.into_inner())
        }
        Value::Resource(mut resource) => {
            resource.path.push('.');
            resource.path += &right;

            Ok(Value::Resource(resource))
        }
        _ => Err(RunError::CannotIndexValue(left_span)),
    }
}

enum Path {
    Variable(WeakRef<Value>),
    NewVariable(String),
    Resource(IntoResource),
}

fn eval_path(
    expr: Spanned<Expression>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<Spanned<Path>, RunError> {
    match expr.value {
        Expression::Variable(variable) => {
            if let Some(registration) = registrations
                .iter()
                .find(|v| v.type_info().type_path_table().short_path() == variable)
            {
                Ok(Spanned {
                    span: expr.span,
                    value: Path::Resource(IntoResource::new(registration.type_id())),
                })
            } else if let Ok(variable) = environment.get(&variable, expr.span.clone()) {
                Ok(Spanned {
                    span: expr.span,
                    value: Path::Variable(variable.borrow()),
                })
            } else {
                Ok(Spanned {
                    span: expr.span,
                    value: Path::NewVariable(variable),
                })
            }
        }
        Expression::Member { left, right } => {
            let left = eval_path(
                *left,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;

            match left.value {
                Path::Variable(variable) => {
                    todo_error!()
                }
                Path::Resource(mut resource) => {
                    resource.path.push('.');
                    resource.path += &right;

                    Ok(left.span.wrap(Path::Resource(resource)))
                }
                Path::NewVariable(name) => Err(RunError::VariableNotFound(left.span.wrap(name))),
            }
        }
        Expression::Dereference(inner) => {
            let path = eval_path(
                *inner,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;
            match path.value {
                Path::Variable(value) => {
                    let strong = value
                        .upgrade()
                        .ok_or(RunError::ReferenceToMovedData(path.span))?;
                    let borrow = strong.borrow();

                    if let Value::Reference(ref reference) = &*borrow {
                        Ok(expr.span.wrap(Path::Variable(reference.clone())))
                    } else {
                        Err(RunError::CouldntDereferenceValue(
                            expr.span.wrap(borrow.kind()),
                        ))
                    }
                }
                Path::NewVariable(_) => todo_error!(),
                Path::Resource(_) => todo_error!(),
            }
        }
        expr => todo_error!("{expr:#?}"),
    }
}

fn eval_object(
    map: HashMap<String, Spanned<Expression>>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<HashMap<String, Rc<RefCell<Value>>>, RunError> {
    let map = map
        .into_iter()
        .map(
            |(key, expr)| -> Result<(String, Rc<RefCell<Value>>), RunError> {
                Ok((
                    key,
                    Rc::new(RefCell::new(eval_expression(
                        expr,
                        EvalParams {
                            world,
                            environment,
                            registrations,
                        },
                    )?)),
                ))
            },
        )
        .collect::<Result<_, _>>()?;
    Ok(map)
}
