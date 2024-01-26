//! Executes the abstract syntax tree.

use environment::Environment;
use std::collections::HashMap;

use bevy::prelude::*;
use bevy::reflect::{
    DynamicEnum, DynamicTuple, ReflectMut, TypeInfo, TypeRegistration, VariantInfo,
};

use crate::builtin_parser::parser::access_unwrap;
use crate::command::CommandHints;
use crate::ui::COMMAND_RESULT_NAME;

use self::error::RunError;
use self::reflection::{object_to_dynamic_struct, CreateRegistration, IntoResource};
use self::unique_rc::{UniqueRc, WeakRef};

use super::parser::{Access, Ast, Expression, Operator};
use super::{Number, SpanExtension, Spanned};

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
            text: concat!("todo error invoked at ", file!(), ":", line!(), ":", column!()).into(),
            span: 0..0
        })?
    };
    ($($arg:tt)+) => {
        Err(RunError::Custom {
            text: format!(concat!("todo error invoked at ", file!(), ":", line!(), ":", column!(), " : {}"), format_args!($($arg)+)).into(),
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

                match variable.upgrade() {
                    Some(strong) => *strong.borrow_mut() = value,
                    None => todo_error!("cannot "),
                }

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
                            unreachable!()
                        };
                        let Spanned { span, value } = *value_expr;
                        match value {
                            Expression::Variable(name) => {
                                let variant_info = match enum_info.variant(&name) {
                                    Some(variant_info) => variant_info,
                                    None => {
                                        return Err(RunError::EnumVariantNotFound(span.wrap(name)))
                                    }
                                };
                                let VariantInfo::Unit(_) = variant_info else {
                                    return todo_error!("{variant_info:?}");
                                };

                                let new_enum = DynamicEnum::new(name, ());

                                dyn_enum.apply(&new_enum);
                            }
                            Expression::StructObject { name, map } => {
                                let variant_info = match enum_info.variant(&name) {
                                    Some(variant_info) => variant_info,
                                    None => {
                                        return Err(RunError::EnumVariantNotFound(span.wrap(name)))
                                    }
                                };
                                let VariantInfo::Struct(variant_info) = variant_info else {
                                    return todo_error!("{variant_info:?}");
                                };

                                let map: HashMap<_, _> = map
                                    .into_iter()
                                    .map(|(k, v)| {
                                        let ty = match variant_info.field(&k) {
                                            Some(field) => Ok(field.type_path_table().short_path()),
                                            None => Err(RunError::EnumVariantStructFieldNotFound {
                                                field_name: k.clone(),
                                                variant_name: name.clone(),
                                                span: span.clone(),
                                            }),
                                        }?;

                                        let span = v.span.clone();

                                        Ok((
                                            k,
                                            (
                                                eval_expression(
                                                    v,
                                                    EvalParams {
                                                        world,
                                                        environment,
                                                        registrations,
                                                    },
                                                )?,
                                                span,
                                                ty,
                                            ),
                                        ))
                                    })
                                    .collect::<Result<_, _>>()?;

                                let new_enum =
                                    DynamicEnum::new(name, object_to_dynamic_struct(map)?);

                                let mut dyn_reflect =
                                    resource.mut_dyn_reflect(world, registrations);

                                let dyn_enum = dyn_reflect
                                    .reflect_path_mut(resource.path.as_str())
                                    .unwrap();

                                dyn_enum.apply(&new_enum);
                            }
                            Expression::StructTuple { name, tuple } => {
                                let variant_info = match enum_info.variant(&name) {
                                    Some(variant_info) => variant_info,
                                    None => {
                                        return Err(RunError::EnumVariantNotFound(span.wrap(name)))
                                    }
                                };
                                let VariantInfo::Tuple(variant_info) = variant_info else {
                                    return todo_error!("{variant_info:?}");
                                };

                                let tuple = eval_tuple(
                                    tuple,
                                    EvalParams {
                                        world,
                                        environment,
                                        registrations,
                                    },
                                )?;

                                let mut dynamic_tuple = DynamicTuple::default();

                                for (index, element) in tuple.into_vec().into_iter().enumerate() {
                                    let ty = match variant_info.field_at(index) {
                                        Some(field) => Ok(field.type_path_table().short_path()),
                                        None => Err(RunError::EnumVariantTupleFieldNotFound {
                                            field_index: index,
                                            variant_name: name.clone(),
                                            span: span.clone(),
                                        }),
                                    }?;

                                    dynamic_tuple.insert_boxed(
                                        element.value.into_inner().reflect(element.span, ty)?,
                                    );
                                }

                                let new_enum = DynamicEnum::new(name, dynamic_tuple);

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
                        let ty = reflect.reflect_short_type_path().to_owned();
                        let value = eval_expression(
                            *value_expr,
                            EvalParams {
                                world,
                                environment,
                                registrations,
                            },
                        )?;
                        let value_reflect = value.reflect(span.clone(), &ty)?;

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
        Expression::Tuple(tuple) => {
            let tuple = eval_tuple(
                tuple,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;
            Ok(Value::Tuple(tuple))
        }
        Expression::StructTuple { name, tuple } => {
            let tuple = eval_tuple(
                tuple,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;
            Ok(Value::StructTuple { name, tuple })
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
        Expression::Dereference(inner) => {
            if let Expression::Variable(variable) = inner.value {
                let var = environment.get(&variable, inner.span)?;
                match &*var.borrow_inner().borrow() {
                    Value::Reference(reference) => {
                        let reference = reference
                            .upgrade()
                            .ok_or(RunError::ReferenceToMovedData(expr.span))?;
                        let owned = reference.borrow().clone();
                        Ok(owned)
                    }
                    value => Ok(value.clone()),
                }
            } else {
                Err(RunError::CannotDereferenceValue(
                    expr.span.wrap(inner.value.kind()),
                ))
            }
        }
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
                Err(RunError::CannotBorrowValue(
                    expr.span.wrap(inner.value.kind()),
                ))
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
    right: Spanned<Access>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<Value, RunError> {
    let span = left.span.start..right.span.end;
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
            access_unwrap!("an object", Field(field) = right => {
                let value = map
                    .remove(&field)
                    .ok_or(RunError::FieldNotFoundInStruct(span.wrap(field)))?;

                Ok(value.into_inner())
            })
        }
        Value::Resource(mut resource) => {
            access_unwrap!("a resource", Field(field) = right => {
                resource.path.push('.');
                resource.path += &field;

                Ok(Value::Resource(resource))
            })
        }
        _ => Err(RunError::CannotIndexValue(span.wrap(left))),
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
                Path::Variable(variable) => match &*variable.upgrade().unwrap().borrow() {
                    Value::Resource(resource) => {
                        if let Access::Field(right) = right.value {
                            let mut resource = resource.clone();

                            resource.path.push('.');
                            resource.path += &right;

                            Ok(left.span.wrap(Path::Resource(resource)))
                        } else {
                            Err(RunError::IncorrectAccessOperation {
                                span: right.span,
                                expected_access: &["a field access"],
                                expected_type: "a resource",
                                got: right.value,
                            })
                        }
                    }
                    Value::Object(object) | Value::StructObject { map: object, .. } => {
                        if let Access::Field(field) = right.value {
                            let span = left.span.join(right.span);
                            let weak = match object.get(&field) {
                                Some(rc) => rc.borrow(),
                                None => {
                                    return Err(RunError::FieldNotFoundInStruct(span.wrap(field)))
                                }
                            };

                            Ok(span.wrap(Path::Variable(weak)))
                        } else {
                            Err(RunError::IncorrectAccessOperation {
                                span: right.span,
                                expected_access: &["a field access"],
                                expected_type: "an object",
                                got: right.value,
                            })
                        }
                    }
                    Value::Tuple(tuple) | Value::StructTuple { tuple, .. } => {
                        if let Access::TupleIndex(index) = right.value {
                            let span = left.span.join(right.span);
                            let weak = match tuple.get(index) {
                                Some(Spanned { value: rc, span: _ }) => rc.borrow(),
                                None => {
                                    return Err(RunError::FieldNotFoundInTuple {
                                        span,
                                        field_index: index,
                                        tuple_size: tuple.len(),
                                    })
                                }
                            };

                            Ok(span.wrap(Path::Variable(weak)))
                        } else {
                            Err(RunError::IncorrectAccessOperation {
                                span: right.span,
                                expected_access: &["a tuple access"],
                                expected_type: "a tuple",
                                got: right.value,
                            })
                        }
                    }
                    value => todo_error!("{value:?}"),
                },
                Path::Resource(mut resource) => {
                    if let Access::Field(right) = right.value {
                        resource.path.push('.');
                        resource.path += &right;

                        Ok(left.span.wrap(Path::Resource(resource)))
                    } else {
                        Err(RunError::IncorrectAccessOperation {
                            span: right.span,
                            expected_access: &["a field"],
                            expected_type: "a resource",
                            got: right.value,
                        })
                    }
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
                        Err(RunError::CannotDereferenceValue(
                            expr.span.wrap(borrow.natural_kind()),
                        ))
                    }
                }
                Path::NewVariable(_) => todo_error!(),
                Path::Resource(_) => todo_error!(),
            }
        }
        expr => todo_error!("can't eval path of this expr: {expr:#?}"),
    }
}

fn eval_object(
    map: HashMap<String, Spanned<Expression>>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<HashMap<String, UniqueRc<Value>>, RunError> {
    let map = map
        .into_iter()
        .map(
            |(key, expr)| -> Result<(String, UniqueRc<Value>), RunError> {
                Ok((
                    key,
                    UniqueRc::new(eval_expression(
                        expr,
                        EvalParams {
                            world,
                            environment,
                            registrations,
                        },
                    )?),
                ))
            },
        )
        .collect::<Result<_, _>>()?;

    Ok(map)
}
fn eval_tuple(
    tuple: Vec<Spanned<Expression>>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<Box<[Spanned<UniqueRc<Value>>]>, RunError> {
    tuple
        .into_iter()
        .map(|expr| {
            let span = expr.span.clone();
            let value = UniqueRc::new(eval_expression(
                expr,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?);
            Ok(span.wrap(value))
        })
        .collect::<Result<_, _>>()
}
