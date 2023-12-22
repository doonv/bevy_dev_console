//! Executes the abstract syntax tree.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use environment::Environment;

use self::reflection::{object_to_dynamic_struct, IntoRegistration, IntoResource};

use super::{
    parser::{Ast, Expression, Operator},
    Spanned,
};
use bevy::{
    prelude::*,
    reflect::{DynamicEnum, Enum, EnumInfo, ReflectMut, TypeInfo, TypeRegistration},
};
use logos::Span;
pub mod environment;
pub mod reflection;
pub mod stdlib;
pub mod value;

pub use value::Value;

// enum ReflectMutlect<'a> {
//     Ref(&'a mut dyn Reflect),
//     Mut(Mut<'a, dyn Reflect>)
// }
// impl<'a> ReflectMutlect<'a> {
//     pub fn to_ref(&'a mut self) -> &'a mut dyn Reflect {
//         match self {
//             ReflectMutlect::Ref(reference) => *reference,
//             ReflectMutlect::Mut(reference) => reference.as_reflect_mut()
//         }
//     }
// }

/// Container for every value needed by evaluation functions.
pub struct EvalParams<'world, 'env, 'reg> {
    world: &'world mut World,
    environment: &'env mut Environment,
    registrations: &'reg [&'reg TypeRegistration],
}

#[derive(Debug)]
pub enum RunError {
    Basic { text: String, span: Span },
    VariableNotFound(Span),
    ExpectedNumberAfterUnaryOperator(Value),
    InvalidVariantForResource(String, String),
    CannotIndexValue(Span),
    FieldNotFoundInStruct(Span),
    CouldntDereferenceValue(Span),
    ReferenceToMovedData(Span),
    VariableMoved(Span),
    CannotBorrowValue(Span),
    IncompatibleReflectTypes { expected: String, actual: String },
    EnumVariantNotFound { name: String },
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
                Ok(value) => match value.try_format(
                    span,
                    &world,
                    &registrations,
                ) {
                    Ok(value) => info!(name: "console_result", "> {value}"),
                    Err(err) => error!("{err:?}"),
                },
                Err(err) => error!("{err:?}"),
            }
        }
    }

    // Add back the resources
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
        } => match eval_path(*name, environment, registrations)?.value {
            Path::Variable(variable) => {
                let temp = variable.clone();
                let value = eval_expression(
                    *value_expr,
                    EvalParams {
                        world,
                        environment,
                        registrations,
                    },
                )?;
                *temp.borrow_mut() = value;

                Ok(Value::Reference(Rc::downgrade(&temp)))
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
                let rc = Rc::new(RefCell::new(value));
                let weak = Rc::downgrade(&rc);

                environment.set(variable, rc);

                Ok(Value::Reference(weak))
            }
            Path::Resource(resource) => {
                let registeration = registrations.into_registration(resource.id);
                let mut dyn_reflect = resource.mut_dyn_reflect(world, registeration);

                let reflect = dyn_reflect
                    .reflect_path_mut(resource.path.as_str())
                    .unwrap();

                match reflect.reflect_mut() {
                    ReflectMut::Enum(dyn_enum) => {
                        let TypeInfo::Enum(enum_info) = registeration.type_info() else {
                            unreachable!();
                        };
                        match value_expr.value {
                            Expression::Variable(variable) => {
                                if enum_info.contains_variant(&variable) {
                                    let new_enum = DynamicEnum::new(variable, ());

                                    dyn_enum.set(Box::new(new_enum)).map_err(|new_enum| {
                                        RunError::IncompatibleReflectTypes {
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
                                        name: variable
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
                            _ => todo!(),
                        }
                    }
                    _ => {
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
            if let Some(registration) = registrations
                .iter()
                .find(|v| v.type_info().type_path_table().short_path() == variable)
            {
                Ok(Value::Resource(IntoResource::new(registration.type_id())))
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
                    Operator::Add => left + right,
                    Operator::Sub => left - right,
                    Operator::Mul => left * right,
                    Operator::Div => left / right,
                    Operator::Mod => left % right,
                })),
                (left, right) => todo!("{left:#?}, {right:#?}"),
            }
        }
        Expression::ForLoop {
            index_name,
            loop_count,
            block,
        } => todo!("for loop {index_name}, {loop_count}, {block:#?}"),
        Expression::Member { left, right } => eval_member_expression(
            *left,
            right,
            EvalParams {
                world,
                environment,
                registrations,
            },
        ),
        Expression::UnaryOp(expr) => {
            let expr = eval_expression(
                *expr,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;

            match expr {
                Value::Number(number) => Ok(Value::Number(-number)),
                _ => Err(RunError::ExpectedNumberAfterUnaryOperator(expr)),
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
        Expression::Dereference(_inner) => {
            todo!()
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
                    let weak = Rc::downgrade(rc);

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

enum Path<'a> {
    Variable(&'a Rc<RefCell<Value>>),
    NewVariable(String),
    Resource(IntoResource),
}

fn eval_path<'a>(
    expr: Spanned<Expression>,
    environment: &'a Environment,
    registrations: &[&TypeRegistration],
) -> Result<Spanned<Path<'a>>, RunError> {
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
            } else {
                if let Ok(variable) = environment.get(&variable, expr.span.clone()) {
                    Ok(Spanned {
                        span: expr.span,
                        value: Path::Variable(variable),
                    })
                } else {
                    Ok(Spanned {
                        span: expr.span,
                        value: Path::NewVariable(variable),
                    })
                }
            }
        }
        Expression::Member { left, right } => {
            let left = eval_path(*left, environment, registrations)?;

            match left.value {
                Path::Variable(variable) => {
                    todo!()
                }
                Path::Resource(mut resource) => {
                    resource.path.push('.');
                    resource.path += &right;
                    Ok(Spanned {
                        span: left.span,
                        value: Path::Resource(resource),
                    })
                }
                Path::NewVariable(_) => Err(RunError::VariableNotFound(left.span)),
            }
        }
        _ => todo!(),
    }
}

// fn set_resource(
//     expr: Spanned<Expression>,
//     params: EvalParams,
//     var: String,
//     registration: &TypeRegistration,
// ) -> Result<Value, RunError> {
//     let EvalParams {
//         world,
//         environment,
//         registrations,
//     } = params;
//     match registration.type_info() {
//         // TypeInfo::Struct(_) => todo!(),
//         // TypeInfo::TupleStruct(_) => todo!(),
//         // TypeInfo::Tuple(_) => todo!(),
//         // TypeInfo::List(_) => todo!(),
//         // TypeInfo::Array(_) => todo!(),
//         // TypeInfo::Map(_) => todo!(),
//         TypeInfo::Enum(enum_info) => match expr.value {
//             Expression::Variable(value) => match enum_info.variant(&value) {
//                 Some(VariantInfo::Unit(_)) => {
//                     let mut reflect: Mut<dyn Reflect> =
//                         mut_dyn_reflect(world, registration).unwrap();
//                     let ReflectMut::Enum(enum_reflect) = reflect.reflect_mut() else {
//                         unreachable!()
//                     };

//                     let variant = DynamicEnum::new(value, DynamicVariant::Unit);

//                     enum_reflect.apply(&variant);

//                     Ok(Value::None)
//                 }
//                 Some(VariantInfo::Struct(_)) => Err(RunError::Basic {
//                     text: "Cannot set struct variant with identifier.".to_string(),
//                     span: expr.span,
//                 }),
//                 Some(VariantInfo::Tuple(_)) => Err(RunError::Basic {
//                     text: "Cannot set tuple variant with identifier.".to_string(),
//                     span: expr.span,
//                 }),
//                 None => Err(RunError::InvalidVariantForResource(var, value)),
//             },
//             Expression::StructObject { name, map } => match enum_info.variant(&name) {
//                 Some(VariantInfo::Unit(_)) => Err(RunError::Basic {
//                     text: "Cannot set unit variant with struct object.".to_string(),
//                     span: expr.span,
//                 }),
//                 Some(VariantInfo::Struct(_)) => {
//                     let map = eval_object(
//                         map,
//                         EvalParams {
//                             world,
//                             environment,
//                             registrations,
//                         },
//                     )?;
//                     let mut dyn_struct = DynamicStruct::default();
//                     for (key, value) in map.into_iter() {
//                         match Rc::try_unwrap(value).unwrap().into_inner() {
//                             Value::None => dyn_struct.insert(&key, ()),
//                             Value::Boolean(boolean) => dyn_struct.insert(&key, boolean),
//                             Value::Number(number) => dyn_struct.insert(&key, number),
//                             Value::String(string) => dyn_struct.insert(&key, string.clone()),
//                             Value::Reference(..) => todo!("todo reference"),
//                             Value::StructObject { .. } => todo!("todo structobject"),
//                             Value::Object(..) => todo!("todo object"),
//                             Value::Resource(..) => todo!("todo dynamicvalue"),
//                         }
//                     }

//                     dbg!("what");

//                     let mut reflect: Mut<'_, dyn Reflect> =
//                         mut_dyn_reflect(world, registration).unwrap();
//                     let ReflectMut::Enum(enum_reflect) = reflect.reflect_mut() else {
//                         unreachable!()
//                     };
//                     let variant = DynamicEnum::new(name, DynamicVariant::Struct(dyn_struct));
//                     dbg!("yay");

//                     enum_reflect.apply(&variant);

//                     Ok(Value::None)
//                 }
//                 Some(VariantInfo::Tuple(_)) => Err(RunError::Basic {
//                     text: "Cannot set tuple variant with struct object.".to_string(),
//                     span: expr.span,
//                 }),
//                 None => Err(RunError::InvalidVariantForResource(var, name)),
//             },
//             _ => todo!(),
//         },
//         // TypeInfo::Value(_) => todo!(),
//         _ => Err(RunError::Basic {
//             text: "Cannot set the value of this type.".to_string(),
//             span: expr.span,
//         }),
//     }
// }
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
