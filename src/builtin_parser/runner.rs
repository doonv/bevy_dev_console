//! Executes the abstract syntax tree.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use environment::Environment;

use super::{
    parser::{Ast, Expression, Operator},
    Spanned,
};
use bevy::reflect::ReflectRef;
use bevy::{
    prelude::*,
    reflect::{
        DynamicEnum, DynamicStruct, DynamicVariant, ReflectFromPtr, ReflectMut, TypeInfo,
        TypeRegistration, VariantInfo, VariantType,
    },
};
use logos::Span;

pub mod environment;
pub mod stdlib;
pub mod value;

pub use value::Value;

/// Container for every value needed by evaluation functions.
pub struct EvalParams<'a, 'b, 'c> {
    world: &'a mut World,
    environment: &'b mut Environment,
    registrations: &'c [&'c TypeRegistration],
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
                Ok(value) => match value.try_format(span) {
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
        Expression::VarAssign { name, value } => match name.value {
            Expression::Variable(var) => {
                if let Some(registration) = registrations
                    .iter()
                    .find(|v| v.type_info().type_path_table().short_path() == var)
                {
                    set_resource(
                        *value,
                        EvalParams {
                            world,
                            environment,
                            registrations,
                        },
                        var,
                        registration,
                    )
                } else {
                    let value = eval_expression(
                        *value,
                        EvalParams {
                            world,
                            environment,
                            registrations,
                        },
                    )?;
                    let rc = Rc::new(RefCell::new(value));
                    let weak = Rc::downgrade(&rc);

                    environment.set(var, rc);

                    Ok(Value::Reference(weak))
                }
            }
            _ => todo!(),
        },
        Expression::String(string) => Ok(Value::String(string)),
        Expression::Number(number) => Ok(Value::Number(number)),
        Expression::Variable(variable) => {
            if let Some(registration) = registrations
                .iter()
                .find(|v| v.type_info().type_path_table().short_path() == variable)
            {
                info!(name: "console_result", "> {}", fancy_debug_print(registration, world));

                Ok(Value::None)
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
        Expression::Member { left, right } => {
            let result = eval_member_expression(
                *left,
                right,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;
            match result {
                Reflectable::Value(value) => Ok(value),
                Reflectable::StructField(mut field) => {
                    let field = field.unwrap();
                    let field_ref = field.reflect_ref();
                    match field_ref {
                        ReflectRef::Struct(_) => todo!(),
                        ReflectRef::TupleStruct(_) => todo!(),
                        ReflectRef::Tuple(_) => todo!(),
                        ReflectRef::List(_) => todo!(),
                        ReflectRef::Array(_) => todo!(),
                        ReflectRef::Map(_) => todo!(),
                        ReflectRef::Enum(_) => todo!(),
                        ReflectRef::Value(value) => {
                            debug!("{value:?}");
                        }
                    }
                    Ok(Value::None)
                }
            }
        }
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
                let rc = environment.get(&variable, inner.span)?;
                let weak = Rc::downgrade(rc);

                Ok(Value::Reference(weak))
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

enum Reflectable<'a> {
    Value(Value),
    StructField(ReflectStructField<'a>),
}
struct ReflectStructField<'a> {
    string: String,
    mut_reflect: Mut<'a, dyn Reflect>,
}
impl<'a> ReflectStructField<'a> {
    fn unwrap(&'a mut self) -> &'a mut dyn Reflect {
        let ReflectMut::Struct(dyn_struct) = self.mut_reflect.reflect_mut() else {
            unreachable!()
        };
        let field = dyn_struct
            .field_mut(&self.string)
            .expect("no field on struct");

        field
    }
}
fn eval_member_expression<'a>(
    left: Spanned<Expression>,
    right: String,
    params: EvalParams<'a, 'a, 'a>,
) -> Result<Reflectable<'a>, RunError> {
    let left_span = left.span.clone();
    let EvalParams {
        world,
        environment,
        registrations,
    } = params;
    match left.value {
        Expression::Variable(variable) => {
            if let Some(registration) = registrations
                .iter()
                .find(|v| v.type_info().type_path_table().short_path() == variable)
            {
                let Some(var): Option<Mut<'a, dyn Reflect>> = mut_dyn_reflect(world, registration)
                else {
                    todo!()
                };
                let reflect = var.reflect_ref();

                match reflect {
                    ReflectRef::Struct(_) => Ok(Reflectable::StructField(ReflectStructField {
                        string: right,
                        mut_reflect: var,
                    })),
                    _ => todo!(),
                }
            } else {
                let reference = environment.get(&variable, left_span)?.borrow();
                match &*reference {
                    Value::Number(number) => return Ok(Reflectable::Value(Value::Number(*number))),
                    Value::Dynamic(value) => {
                        dbg!(value);
                        Ok(Reflectable::Value(Value::None))
                    }
                    Value::StructObject { map, .. } | Value::Object(map) => {
                        if let Some(value) = map.get(&right) {
                            let weak = Rc::downgrade(value);
                            Ok(Reflectable::Value(Value::Reference(weak)))
                        } else {
                            todo!()
                        }
                    }
                    value => todo!("{value:?}"),
                }
            }
        }
        _ => {
            let left = eval_expression(
                left,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )?;

            match left {
                Value::StructObject { map, .. } => {
                    if let Some(value) = map.get(&right) {
                        Ok(Reflectable::Value(Value::Reference(Rc::downgrade(value))))
                    } else {
                        Err(RunError::FieldNotFoundInStruct(left_span))
                    }
                }
                _ => Err(RunError::CannotIndexValue(left_span)),
            }
        }
    }
}

fn set_resource(
    expr: Spanned<Expression>,
    params: EvalParams,
    var: String,
    registration: &TypeRegistration,
) -> Result<Value, RunError> {
    let EvalParams {
        world,
        environment,
        registrations,
    } = params;
    match registration.type_info() {
        // TypeInfo::Struct(_) => todo!(),
        // TypeInfo::TupleStruct(_) => todo!(),
        // TypeInfo::Tuple(_) => todo!(),
        // TypeInfo::List(_) => todo!(),
        // TypeInfo::Array(_) => todo!(),
        // TypeInfo::Map(_) => todo!(),
        TypeInfo::Enum(enum_info) => match expr.value {
            Expression::Variable(value) => match enum_info.variant(&value) {
                Some(VariantInfo::Unit(_)) => {
                    let mut reflect: Mut<'_, dyn Reflect> =
                        mut_dyn_reflect(world, registration).unwrap();
                    let ReflectMut::Enum(enum_reflect) = reflect.reflect_mut() else {
                        unreachable!()
                    };

                    let variant = DynamicEnum::new(value, DynamicVariant::Unit);

                    enum_reflect.apply(&variant);

                    Ok(Value::None)
                }
                Some(VariantInfo::Struct(_)) => Err(RunError::Basic {
                    text: "Cannot set struct variant with identifier.".to_string(),
                    span: expr.span,
                }),
                Some(VariantInfo::Tuple(_)) => Err(RunError::Basic {
                    text: "Cannot set tuple variant with identifier.".to_string(),
                    span: expr.span,
                }),
                None => Err(RunError::InvalidVariantForResource(var, value)),
            },
            Expression::StructObject { name, map } => match enum_info.variant(&name) {
                Some(VariantInfo::Unit(_)) => Err(RunError::Basic {
                    text: "Cannot set unit variant with struct object.".to_string(),
                    span: expr.span,
                }),
                Some(VariantInfo::Struct(_)) => {
                    let map = eval_object(
                        map,
                        EvalParams {
                            world,
                            environment,
                            registrations,
                        },
                    )?;
                    let mut dyn_struct = DynamicStruct::default();
                    for (key, value) in map.into_iter() {
                        match Rc::try_unwrap(value).unwrap().into_inner() {
                            Value::None => dyn_struct.insert(&key, ()),
                            Value::Boolean(boolean) => dyn_struct.insert(&key, boolean),
                            Value::Number(number) => dyn_struct.insert(&key, number),
                            Value::String(string) => dyn_struct.insert(&key, string.clone()),
                            Value::Reference(..) => todo!("todo reference"),
                            Value::StructObject { .. } => todo!("todo structobject"),
                            Value::Object(..) => todo!("todo object"),
                            Value::Dynamic(..) => todo!("todo dynamicvalue"),
                        }
                    }

                    dbg!("what");

                    let mut reflect: Mut<'_, dyn Reflect> =
                        mut_dyn_reflect(world, registration).unwrap();
                    let ReflectMut::Enum(enum_reflect) = reflect.reflect_mut() else {
                        unreachable!()
                    };
                    let variant = DynamicEnum::new(name, DynamicVariant::Struct(dyn_struct));
                    dbg!("yay");

                    enum_reflect.apply(&variant);

                    Ok(Value::None)
                }
                Some(VariantInfo::Tuple(_)) => Err(RunError::Basic {
                    text: "Cannot set tuple variant with struct object.".to_string(),
                    span: expr.span,
                }),
                None => Err(RunError::InvalidVariantForResource(var, name)),
            },
            _ => todo!(),
        },
        // TypeInfo::Value(_) => todo!(),
        _ => Err(RunError::Basic {
            text: "Cannot set the value of this type.".to_string(),
            span: expr.span,
        }),
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

/// A massive function that takes in a type registration and the world and then
/// does all the hard work of printing out the type nicely.
fn fancy_debug_print(registration: &TypeRegistration, world: &mut World) -> String {
    let mut f = String::new();
    let type_info = registration.type_info();
    let Some(mut reflect) = mut_dyn_reflect(world, registration) else {
        return String::new();
    };
    let reflect = reflect.reflect_mut();
    match reflect {
        ReflectMut::Struct(struct_info) => {
            f += &format!("struct {} {{\n", type_info.type_path_table().short_path());
            for i in 0..struct_info.field_len() {
                let field = struct_info.field_at(i).unwrap();
                let field_name = struct_info.name_at(i).unwrap();
                f += &format!(
                    "\t{}: {} = {:?},\n",
                    field_name,
                    field.reflect_short_type_path(),
                    field
                );
            }
            f += "}";
        }
        ReflectMut::TupleStruct(_) => todo!(),
        ReflectMut::Tuple(_) => todo!(),
        ReflectMut::List(_) => todo!(),
        ReflectMut::Array(_) => todo!(),
        ReflectMut::Map(_) => todo!(),
        ReflectMut::Enum(set_variant_info) => {
            // Print out the enum types
            f += &format!("enum {} {{\n", type_info.type_path_table().short_path());
            let TypeInfo::Enum(enum_info) = registration.type_info() else {
                unreachable!()
            };
            for variant in enum_info.iter() {
                f += "\t";
                f += variant.name();
                match variant {
                    VariantInfo::Struct(variant) => {
                        f += " {\n";
                        for field in variant.iter() {
                            f += &format!(
                                "\t\t{}: {},\n",
                                field.name(),
                                field.type_path_table().short_path()
                            );
                        }
                        f += "\t}";
                    }
                    VariantInfo::Tuple(variant) => {
                        f += "(";
                        let mut iter = variant.iter();
                        if let Some(first) = iter.next() {
                            f += &format!("{}", first.type_path_table().short_path());
                            for field in iter {
                                f += &format!(", {}", field.type_path_table().short_path());
                            }
                        }
                        f += ")";
                    }
                    VariantInfo::Unit(_) => {}
                }
                f += ",\n";
            }
            // Print out the current value
            f += "} = ";
            f += set_variant_info.variant_name();
            match set_variant_info.variant_type() {
                VariantType::Struct => {
                    f += " {\n";
                    for field in set_variant_info.iter_fields() {
                        f += &format!("\t{}: {:?},\n", field.name().unwrap(), field.value());
                    }
                    f += "}";
                }
                VariantType::Tuple => todo!(),
                VariantType::Unit => {}
            }
        }
        ReflectMut::Value(_) => todo!(),
    }
    f
}

fn mut_dyn_reflect<'a>(
    world: &'a mut World,
    registration: &'a TypeRegistration,
) -> Option<Mut<'a, dyn Reflect>> {
    let Some(component_id) = world.components().get_resource_id(registration.type_id()) else {
        error!(
            "Couldn't get the component id of the {} resource.",
            registration.type_info().type_path()
        );
        return None;
    };
    let resource = world.get_resource_mut_by_id(component_id).unwrap();
    let reflect_from_ptr = registration.data::<ReflectFromPtr>().unwrap();
    // SAFETY: from the context it is known that `ReflectFromPtr` was made for the type of the `MutUntyped`
    let val: Mut<dyn Reflect> =
        resource.map_unchanged(|ptr| unsafe { reflect_from_ptr.as_reflect_mut(ptr) });
    Some(val)
}
