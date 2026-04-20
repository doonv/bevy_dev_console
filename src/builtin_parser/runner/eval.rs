//! Evaluation of expressions

use std::collections::HashMap;

use bevy::prelude::*;
use bevy::reflect::{DynamicEnum, DynamicTuple, ReflectMut, TypeInfo, VariantInfo};
use kinded::Kinded;

use crate::builtin_parser::number::Number;
use crate::builtin_parser::parser::{BinaryOperator, Expression, UnaryOperator};
use crate::builtin_parser::runner::value::ValueKind;
use crate::builtin_parser::{Diagnostic, ErrorExtension, SpanExtension, Spanned, StrongRef};

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
) -> Result<Value, Diagnostic<EvalError>> {
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
                        return Err(expr
                            .span
                            .wrap(EvalError::Custom("cannot assign to moved variable".into()))
                            .into());
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
                                        return Err(span
                                            .wrap(EvalError::EnumVariantNotFound(name))
                                            .into());
                                    }
                                };
                                let VariantInfo::Unit(_) = variant_info else {
                                    return Err(span
                                        .wrap(EvalError::Custom(
                                            format!("Enum variant {name} is not a unit variant")
                                                .into(),
                                        ))
                                        .into());
                                };

                                let new_enum = DynamicEnum::new(name, ());

                                dyn_enum.apply(&new_enum);
                            }
                            Expression::StructObject { name, map } => {
                                let variant_info = match enum_info.variant(&name) {
                                    Some(variant_info) => variant_info,
                                    None => {
                                        return Err(span
                                            .wrap(EvalError::EnumVariantNotFound(name))
                                            .into());
                                    }
                                };
                                let VariantInfo::Struct(variant_info) = variant_info else {
                                    return Err(span
                                        .wrap(EvalError::Custom(
                                            format!("Enum variant {name} is not a struct variant")
                                                .into(),
                                        ))
                                        .into());
                                };

                                let map: HashMap<_, _> = map
                                    .into_iter()
                                    .map(|(k, v)| {
                                        let ty = match variant_info.field(&k) {
                                            Some(field) => Ok(field.type_path_table().short_path()),
                                            None => Err(span.clone().diagnose(
                                                EvalError::EnumVariantStructFieldNotFound {
                                                    field_name: k.clone(),
                                                    variant_name: name.clone(),
                                                },
                                            )),
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
                                    .collect::<Result<_, Diagnostic<EvalError>>>()?;

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
                                        return Err(span
                                            .wrap(EvalError::EnumVariantNotFound(name))
                                            .into());
                                    }
                                };
                                let VariantInfo::Tuple(variant_info) = variant_info else {
                                    return Err(span
                                        .wrap(EvalError::Custom(
                                            format!("Enum variant {name} is not a tuple variant")
                                                .into(),
                                        ))
                                        .into());
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
                                        None => Err(span.clone().diagnose(
                                            EvalError::EnumVariantTupleFieldNotFound {
                                                field_index: index,
                                                variant_name: name.clone(),
                                            },
                                        )),
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
                                return Err(span
                                    .wrap(EvalError::Custom(
                                        "Unsupported enum variant assignment".into(),
                                    ))
                                    .into());
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
                            .map_err(|apply_error| {
                                span.diagnose(EvalError::ApplyError(apply_error))
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
                Err(expr
                    .span
                    .wrap(EvalError::CannotMoveOutOfResource(variable))
                    .into())
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
                (left, right) => Err(expr
                    .span
                    .wrap(EvalError::Custom(
                        format!("Unsupported binary operation between {left:?} and {right:?}")
                            .into(),
                    ))
                    .into()),
            }
        }
        Expression::ForLoop {
            index_name,
            loop_count,
            block,
        } => Err(expr
            .span
            .wrap(EvalError::Custom(
                format!(
                    "For loops are not yet implemented: {index_name}, {loop_count}, {block:#?}"
                )
                .into(),
            ))
            .into()),
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
                        Ok(Value::Number((-number).diagnosed(expr.span)?))
                    } else {
                        Err(expr.span.diagnose(EvalError::InvalidUnaryOperation {
                            operator,
                            operand: value.kind(),
                            accepted: &[ValueKind::AnyNumber],
                        }))
                    }
                }
                UnaryOperator::Not => match value {
                    Value::Boolean(boolean) => Ok(Value::Boolean(!boolean)),
                    Value::Number(number) => Ok(Value::Number((!number).diagnosed(expr.span)?)),
                    _ => Err(expr
                        .span
                        .wrap(EvalError::InvalidUnaryOperation {
                            operator,
                            operand: value.kind(),
                            accepted: &[ValueKind::Boolean, ValueKind::AnyInteger],
                        })
                        .into()),
                },
            }
        }
        Expression::Dereference(inner) => {
            if let Expression::Variable(variable) = inner.value {
                let var = environment.get_variable(&variable, inner.span)?;
                match &*var.borrow_inner().borrow() {
                    Value::Reference(reference) => {
                        let reference: StrongRef<Value> = reference
                            .upgrade()
                            .ok_or_else(|| expr.span.wrap(EvalError::ReferenceToMovedData))?;
                        let owned = reference.borrow().clone();
                        Ok(owned)
                    }
                    value => Ok(value.clone()),
                }
            } else {
                Err(expr
                    .span
                    .wrap(EvalError::CannotDereferenceValueExpr(inner.value.kind()))
                    .into())
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
                    let rc = environment.get_variable(&variable, inner.span)?;
                    let weak = rc.borrow();

                    Ok(Value::Reference(weak))
                }
            } else {
                Err(expr
                    .span
                    .wrap(EvalError::CannotBorrowValue(inner.value.kind()))
                    .into())
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
                .collect::<Result<Vec<_>, Diagnostic<EvalError>>>()?;

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
) -> Result<HashMap<String, UniqueRc<Value>>, Diagnostic<EvalError>> {
    let map = map
        .into_iter()
        .map(
            |(key, expr)| -> Result<(String, UniqueRc<Value>), Diagnostic<EvalError>> {
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
) -> Result<Box<[Spanned<UniqueRc<Value>>]>, Diagnostic<EvalError>> {
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
