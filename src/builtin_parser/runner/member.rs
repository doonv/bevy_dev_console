//! Evaluation for member expressions and paths

use crate::builtin_parser::parser::{access_unwrap, Access, Expression};
use crate::builtin_parser::{EvalError, SpanExtension, Spanned, WeakRef};

use super::reflection::IntoResource;
use super::{eval_expression, todo_error, EvalParams, Value};

/// Evaluate a member expression.
///
/// A member expression allows indexing of values to access their inner fields.
///
/// # Examples
///
/// ```text
/// $ {a: 5}.a
/// > 5
/// $ x = {key: "hi"}
/// $ &x.key
/// > "hi"
/// $ x
/// > {key: "hi"}
/// $ x.key
/// > "hi"
/// $ x
/// Error: Variable moved
/// ```
pub fn eval_member_expression(
    left: Spanned<Expression>,
    right: Spanned<Access>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<Value, EvalError> {
    let left_span = left.span.clone();
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
        Value::Reference(reference) => {
            let Some(strong) = reference.upgrade() else {
                return Err(EvalError::ReferenceToMovedData(left_span));
            };
            let reference = strong.borrow();
            match &&*reference {
                Value::Object(map) | Value::StructObject { map, .. } => {
                    access_unwrap!("an object reference", Field(field) = right => {
                        let value = map.get(&field).ok_or(EvalError::FieldNotFoundInStruct(span.wrap(field)))?;

                        Ok(Value::Reference(value.borrow()))
                    })
                }
                Value::Tuple(tuple) | Value::StructTuple { tuple, .. } => {
                    access_unwrap!("a tuple reference", TupleIndex(index) = right => {
                        let Spanned { span: _, value } =
                            tuple.get(index).ok_or(EvalError::FieldNotFoundInTuple {
                                span,
                                field_index: index,
                                tuple_size: tuple.len(),
                            })?;

                        Ok(Value::Reference(value.borrow()))
                    })
                }
                Value::Resource(resource) => {
                    access_unwrap!("a resource", Field(field) = right => {
                        let mut resource = resource.clone();

                        resource.path.push('.');
                        resource.path += &field;

                        Ok(Value::Resource(resource))
                    })
                }
                var => Err(EvalError::CannotIndexValue(left_span.wrap((*var).clone()))),
            }
        }
        Value::Object(mut map) | Value::StructObject { mut map, .. } => {
            access_unwrap!("an object", Field(field) = right => {
                let value = map
                    .remove(&field)
                    .ok_or(EvalError::FieldNotFoundInStruct(span.wrap(field)))?;

                Ok(value.into_inner())
            })
        }
        Value::Tuple(tuple) | Value::StructTuple { tuple, .. } => {
            access_unwrap!("a tuple reference", TupleIndex(field_index) = right => {
                let tuple_size = tuple.len();
                let Spanned { span: _, value } =
                    tuple
                        .into_vec()
                        .into_iter()
                        .nth(field_index)
                        .ok_or(EvalError::FieldNotFoundInTuple {
                            span,
                            field_index,
                            tuple_size,
                        })?;

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
        _ => Err(EvalError::CannotIndexValue(left_span.wrap(left))),
    }
}

pub enum Path {
    Variable(WeakRef<Value>),
    NewVariable(String),
    Resource(IntoResource),
}

/// Evaluate a path expression.
///
/// A path expression, in contrast to a member expression, is for creating new variables or assigning to existing ones.
///
/// # Examples
///
/// ```text
/// a -> Path::NewVariable("a")
/// a.b -> if a.b exists, returns Path::Variable(a.b)
/// MyResource.field -> appends "field" to the IntoResource path and returns Resource
/// a.b.c -> if a.b.c, returns Path::Variable(a.b.c) (wow look its recursive)
/// ```
pub fn eval_path(
    expr: Spanned<Expression>,
    EvalParams {
        world,
        environment,
        registrations,
    }: EvalParams,
) -> Result<Spanned<Path>, EvalError> {
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
                        access_unwrap!("a resource", Field(field) = right => {
                            let mut resource = resource.clone();

                            resource.path.push('.');
                            resource.path += &field;

                            Ok(left.span.wrap(Path::Resource(resource)))
                        })
                    }
                    Value::Object(object) | Value::StructObject { map: object, .. } => {
                        let span = left.span.start..right.span.end;
                        access_unwrap!("an object", Field(field) = right => {
                            let weak = match object.get(&field) {
                                Some(rc) => rc.borrow(),
                                None => {
                                    return Err(EvalError::FieldNotFoundInStruct(span.wrap(field)))
                                }
                            };

                            Ok(span.wrap(Path::Variable(weak)))
                        })
                    }
                    Value::Tuple(tuple) | Value::StructTuple { tuple, .. } => {
                        let span = left.span.start..right.span.end;
                        access_unwrap!("a tuple", TupleIndex(index) = right => {
                            let weak = match tuple.get(index) {
                                Some(Spanned { value: rc, span: _ }) => rc.borrow(),
                                None => {
                                    return Err(EvalError::FieldNotFoundInTuple {
                                        span,
                                        field_index: index,
                                        tuple_size: tuple.len(),
                                    })
                                }
                            };

                            Ok(span.wrap(Path::Variable(weak)))
                        })
                    }
                    value => todo_error!("{value:?}"),
                },
                Path::Resource(mut resource) => {
                    access_unwrap!("a resource", Field(field) = right => {
                        resource.path.push('.');
                        resource.path += &field;

                        Ok(left.span.wrap(Path::Resource(resource)))
                    })
                }
                Path::NewVariable(name) => Err(EvalError::VariableNotFound(left.span.wrap(name))),
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
                        .ok_or(EvalError::ReferenceToMovedData(path.span))?;
                    let borrow = strong.borrow();

                    if let Value::Reference(ref reference) = &*borrow {
                        Ok(expr.span.wrap(Path::Variable(reference.clone())))
                    } else {
                        Err(EvalError::CannotDereferenceValue(
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
