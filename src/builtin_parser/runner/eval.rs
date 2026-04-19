//! Evaluation of expressions

use std::collections::HashMap;

use bevy::prelude::*;
use bevy::reflect::{DynamicEnum, DynamicTuple, ReflectMut, TypeInfo, VariantInfo};
use kinded::Kinded;

use crate::builtin_parser::number::Number;
use crate::builtin_parser::parser::{BinaryOperator, Expression, UnaryOperator};
use crate::builtin_parser::runner::value::ValueKind;
use crate::builtin_parser::{SpanExtension, Spanned};

use super::EvalParams;
use super::error::EvalError;
use super::member::{Path, eval_member_expression, eval_path};
use super::reflection::{CreateRegistration, IntoResource, object_to_dynamic_struct};
use super::unique_rc::UniqueRc;
use super::value::Value;

pub fn eval_expression(
    expr: Spanned<Expression>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<Value, EvalError> {
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
                    None => {
                        return Err(EvalError::Custom {
                            text: "cannot assign to moved variable".into(),
                            span: expr.span,
                        });
                    }
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
                let registration = registrations.create_registration(resource.id);
                let mut dyn_reflect = resource.mut_dyn_reflect(world, registration);

                let reflect = dyn_reflect
                    .reflect_path_mut(resource.path.as_str())
                    .unwrap();

                #[expect(clippy::single_match_else, reason = "more should be added later")]
                match reflect.reflect_mut() {
                    ReflectMut::Enum(dyn_enum) => {
                        let TypeInfo::Enum(enum_info) = registration.type_info() else {
                            unreachable!()
                        };
                        let Spanned { span, value } = *value_expr;
                        match value {
                            Expression::Variable(name) => {
                                let variant_info = match enum_info.variant(&name) {
                                    Some(variant_info) => variant_info,
                                    None => {
                                        return Err(EvalError::EnumVariantNotFound(
                                            span.wrap(name),
                                        ));
                                    }
                                };
                                let VariantInfo::Unit(_) = variant_info else {
                                    return Err(EvalError::Custom {
                                        text: format!("Enum variant {name} is not a unit variant")
                                            .into(),
                                        span,
                                    });
                                };

                                let new_enum = DynamicEnum::new(name, ());

                                dyn_enum.apply(&new_enum);
                            }
                            Expression::StructObject { name, map } => {
                                let variant_info = match enum_info.variant(&name) {
                                    Some(variant_info) => variant_info,
                                    None => {
                                        return Err(EvalError::EnumVariantNotFound(
                                            span.wrap(name),
                                        ));
                                    }
                                };
                                let VariantInfo::Struct(variant_info) = variant_info else {
                                    return Err(EvalError::Custom {
                                        text: format!(
                                            "Enum variant {name} is not a struct variant"
                                        )
                                        .into(),
                                        span,
                                    });
                                };

                                let map: HashMap<_, _> = map
                                    .into_iter()
                                    .map(|(k, v)| {
                                        let ty = match variant_info.field(&k) {
                                            Some(field) => Ok(field.type_path_table().short_path()),
                                            None => {
                                                Err(EvalError::EnumVariantStructFieldNotFound {
                                                    field_name: k.clone(),
                                                    variant_name: name.clone(),
                                                    span: span.clone(),
                                                })
                                            }
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
                                        return Err(EvalError::EnumVariantNotFound(
                                            span.wrap(name),
                                        ));
                                    }
                                };
                                let VariantInfo::Tuple(variant_info) = variant_info else {
                                    return Err(EvalError::Custom {
                                        text: format!("Enum variant {name} is not a tuple variant")
                                            .into(),
                                        span,
                                    });
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
                                        None => Err(EvalError::EnumVariantTupleFieldNotFound {
                                            field_index: index,
                                            variant_name: name.clone(),
                                            span: span.clone(),
                                        }),
                                    }?;

                                    dynamic_tuple.insert_boxed(
                                        element
                                            .value
                                            .into_inner()
                                            .reflect(element.span, ty)?
                                            .into_partial_reflect(),
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
                            _ => {
                                return Err(EvalError::Custom {
                                    text: "Unsupported enum variant assignment".into(),
                                    span,
                                });
                            }
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

                        reflect
                            .try_apply(value_reflect.as_partial_reflect())
                            .map_err(|apply_error| EvalError::ApplyError { apply_error, span })?;
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
                Err(EvalError::CannotMoveOutOfResource(Spanned {
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
                    BinaryOperator::Add => Number::add(left, right, expr.span)?,
                    BinaryOperator::Sub => Number::sub(left, right, expr.span)?,
                    BinaryOperator::Mul => Number::mul(left, right, expr.span)?,
                    BinaryOperator::Div => Number::div(left, right, expr.span)?,
                    BinaryOperator::Mod => Number::rem(left, right, expr.span)?,
                    BinaryOperator::And => Number::and(left, right, expr.span)?,
                    BinaryOperator::Xor => Number::xor(left, right, expr.span)?,
                    BinaryOperator::Or => Number::or(left, right, expr.span)?,
                })),
                (left, right) => Err(EvalError::Custom {
                    text: format!("Unsupported binary operation between {left:?} and {right:?}")
                        .into(),
                    span: expr.span,
                }),
            }
        }
        Expression::ForLoop {
            index_name,
            loop_count,
            block,
        } => Err(EvalError::Custom {
            text: format!(
                "For loops are not yet implemented: {index_name}, {loop_count}, {block:#?}"
            )
            .into(),
            span: expr.span,
        }),
        Expression::Member { left, right } => eval_member_expression(
            *left,
            right,
            EvalParams {
                world,
                environment,
                registrations,
            },
        ),
        Expression::UnaryOp { operator, operand } => {
            let value = eval_expression(
                *operand,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;
            match operator {
                UnaryOperator::Minus => {
                    if let Value::Number(number) = value {
                        Ok(Value::Number(number.neg(expr.span)?))
                    } else {
                        Err(EvalError::InvalidUnaryOperation {
                            span: expr.span,
                            operator,
                            operand: value.kind(),
                            accepted: &[ValueKind::AnyNumber],
                        })
                    }
                }
                UnaryOperator::Not => match value {
                    Value::Boolean(boolean) => Ok(Value::Boolean(!boolean)),
                    Value::Number(number) => Ok(Value::Number(number.not(expr.span)?)),
                    _ => Err(EvalError::InvalidUnaryOperation {
                        span: expr.span,
                        operator,
                        operand: value.kind(),
                        accepted: &[ValueKind::Boolean, ValueKind::AnyInteger],
                    }),
                },
            }
        }
        Expression::Dereference(inner) => {
            if let Expression::Variable(variable) = inner.value {
                let var = environment.get(&variable, inner.span)?;
                match &*var.borrow_inner().borrow() {
                    Value::Reference(reference) => {
                        let reference = reference
                            .upgrade()
                            .ok_or(EvalError::ReferenceToMovedData(expr.span))?;
                        let owned = reference.borrow().clone();
                        Ok(owned)
                    }
                    value => Ok(value.clone()),
                }
            } else {
                Err(EvalError::CannotDereferenceValueExpr(
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
                Err(EvalError::CannotBorrowValue(
                    expr.span.wrap(inner.value.kind()),
                ))
            }
        }
        Expression::None => Ok(Value::None),
        Expression::Boolean(bool) => Ok(Value::Boolean(bool)),
        Expression::Function { name, arguments } => {
            let args = arguments
                .into_iter()
                .map(|expr| {
                    Ok(Spanned {
                        span: expr.span.clone(),
                        value: eval_expression(
                            expr,
                            EvalParams {
                                world,
                                environment,
                                registrations,
                            },
                        )?,
                    })
                })
                .collect::<Result<Vec<_>, EvalError>>()?;

            environment.run_function(&name, args, world, registrations)
        }
    }
}

pub fn eval_object(
    map: HashMap<String, Spanned<Expression>>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<HashMap<String, UniqueRc<Value>>, EvalError> {
    let map = map
        .into_iter()
        .map(
            |(key, expr)| -> Result<(String, UniqueRc<Value>), EvalError> {
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

pub fn eval_tuple(
    tuple: Vec<Spanned<Expression>>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<Box<[Spanned<UniqueRc<Value>>]>, EvalError> {
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
