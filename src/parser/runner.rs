use std::borrow::BorrowMut;
use std::fmt::Debug as DebugTrait;
use std::rc::Weak;
use std::{cell::RefCell, rc::Rc};

use self::environment::Environment;

use super::{
    parser::{Expression, Operator, AST},
    Spanned,
};
use ahash::AHashMap;
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

/// A runtime value
#[derive(Debug)]
pub enum Value {
    /// Nothing at all
    None,
    /// A number, for simplicity only f64s are used.
    Number(f64),
    /// A string... there isn't much to say about this one.
    String(String),
    /// A reference.
    ///
    /// References are very similar to rust's ownership and borrowing.
    /// We achieve this by storing every variable as a [`Rc<RefCell<Value>>`],
    /// and having only the owner of the value have a strong reference,
    /// while every other value has a weak reference. This causes
    /// [`Rc::try_unwrap`] to succeed every time.
    ///
    /// This isn't partically efficent, so:
    /// TODO: Create a custom type this!
    Reference(Weak<RefCell<Value>>),
    StructObject {
        name: String,
        map: AHashMap<String, Rc<RefCell<Value>>>,
    },
    /// A reference to a dynamic value. (aka a reference.)
    DynamicValue(Box<dyn Reflect>),
}

impl Value {
    pub fn try_format(&self, span: Span) -> Result<String, RunError> {
        match self {
            Value::None => Ok(format!("()")),
            Value::Number(number) => Ok(format!("{number}")),
            Value::String(string) => Ok(format!("\"{string}\"")),
            Value::Reference(reference) => {
                if let Some(rc) = reference.upgrade() {
                    Ok(rc.borrow().try_format(span)?)
                } else {
                    Err(RunError::ReferenceToMovedData(span))
                }
            }
            Value::StructObject { name, map } => {
                let mut string = String::new();
                string += &format!("{name} {{");
                for (key, value) in map {
                    string += &format!("\n\t{key}: {},", value.borrow().try_format(span.clone())?);
                }
                if map.len() > 0 {
                    string.push('\n');
                }
                string.push('}');
                Ok(string)
            }
            Value::DynamicValue(value) => Ok(format!("{value:#?}")),
        }
    }
}

impl From<()> for Value {
    fn from((): ()) -> Self {
        Value::None
    }
}
impl From<f64> for Value {
    fn from(number: f64) -> Self {
        Value::Number(number)
    }
}
impl From<String> for Value {
    fn from(string: String) -> Self {
        Value::String(string)
    }
}

impl TryFrom<Value> for f64 {
    type Error = RunError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Number(number) = value {
            Ok(number)
        } else {
            todo!()
        }
    }
}
impl TryFrom<Value> for String {
    type Error = RunError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::String(string) = value {
            Ok(string)
        } else {
            todo!()
        }
    }
}

#[derive(Debug)]
pub enum RunError {
    VariableNotFound(Span),
    ExpectedNumberAfterUnaryOperator(Value),
    InvalidVariantForResource(String, String),
    Basic { text: String, span: Span },
    CannotIndexValue(Span),
    FieldNotFoundInStruct(Span),
    CouldntDereferenceValue(std::ops::Range<usize>),
    ReferenceToMovedData(std::ops::Range<usize>),
    VariableMoved(std::ops::Range<usize>),
}

pub fn run(ast: AST, world: &mut World) {
    // Temporarily remove the [`Environment`] resource to gain
    // mutability without needing a mutable reference.
    let Some(mut environment) = world.remove_non_send_resource::<Environment>() else {
        error!("Environment resource doesn't exist, not executing command.");
        return;
    };

    // Same thing here (this time we are doing it because we are passing a `&mut World` to `eval_expression`)
    let Some(registry) = world.remove_resource::<AppTypeRegistry>() else {
        error!("The AppTypeRegistry doesn't exist, not executing command. (What have you done to cause this?)");
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
            let value = eval_expression(statement, world, &mut environment, &registrations);

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
    world: &mut World,
    environment: &mut Environment,
    registrations: &[&TypeRegistration],
) -> Result<Value, RunError> {
    match expr.value {
        Expression::VarAssign { name, value } => match name.value {
            Expression::Variable(var) => {
                if let Some(registration) = registrations
                    .iter()
                    .find(|v| v.type_info().type_path_table().short_path() == &var)
                {
                    set_resource(world, registration, environment, registrations, value, var)
                } else {
                    let value = eval_expression(*value, world, environment, registrations)?;
                    let rc = Rc::new(RefCell::new(value));
                    let weak = Rc::downgrade(&rc);

                    environment.set(var, rc)?;

                    Ok(Value::Reference(weak))
                }
            }
            _ => todo!(),
        },
        Expression::String(string) => Ok(Value::String(string)),
        Expression::Number(number) => Ok(Value::Number(number)),
        Expression::Variable(variable) => {
            dbg!(&variable);
            if let Some(registration) = registrations
                .iter()
                .find(|v| v.type_info().type_path_table().short_path() == &variable)
            {
                info!(name: "console_result", "> {}", fancy_debug_print(registration, world));
                Ok(Value::None)
            } else {
                // let value = &*;
                match &*environment.get(&variable, expr.span.clone())?.borrow() {
                    Value::Number(number) => return Ok(Value::Number(*number)),
                    _ => {}
                }

                // Unwrapping will always succeed due to only the owner of the variable having
                // a strong reference. All other references are weak.
                let value = Rc::try_unwrap(environment.move_var(&variable, expr.span)?).unwrap();

                Ok(value.into_inner())
            }
        }
        Expression::BinaryOp {
            left,
            operator,
            right,
        } => {
            let left = eval_expression(*left, world, environment, registrations)?;
            let right = eval_expression(*right, world, environment, registrations)?;

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
        Expression::MemberExpression { left, right } => {
            eval_member_expression(*left, right, world, environment, registrations)
        }
        Expression::UnaryOp(expr) => {
            let expr = eval_expression(*expr, world, environment, registrations)?;

            match expr {
                Value::Number(number) => Ok(Value::Number(-number)),
                _ => Err(RunError::ExpectedNumberAfterUnaryOperator(expr)),
            }
        }
        Expression::StructObject { name, map } => {
            let hashmap = eval_object(map, world, environment, registrations)?;
            Ok(Value::StructObject { name, map: hashmap })
        }
        Expression::Dereference(inner) => {
            // if let Expression::Variable(variable) = inner.value {
            //     let cell = environment.get(&variable, inner.span.clone())?;
            //     // This line of code is stupid. However I believe that
            //     // Ref::leak (unstable) will give a less-shitty approach to this.
            //     if Rc::strong_count(&*cell.borrow()) == 1 {
            //         Ok(Rc::try_unwrap(
            //             environment
            //                 .remove(&variable, inner.span.clone())?
            //                 .into_inner(),
            //         )
            //         .unwrap())
            //     } else {
            //         Err(RunError::CouldntDereferenceValue(expr.span))
            //     }
            //     // let value = environment.get(&variable, inner.span)?.get_mut().;
            //     // if let Ok(value) = Rc::try_unwrap(value) {
            //     //     Ok(value)
            //     // } else {
            //     //     Err(RunError::CouldntDereferenceValue(expr.span))
            //     // }
            // } else {
            //     // Err()
            //     todo!()
            // }
            todo!()
        }
        Expression::Borrow(inner) => {
            if let Expression::Variable(variable) = inner.value {
                let rc = environment.get(&variable, inner.span)?;
                let weak = Rc::downgrade(rc);

                Ok(Value::Reference(weak))
            } else {
                todo!()
            }
        }
        Expression::None => Ok(Value::None),
    }
}

fn eval_member_expression(
    left: Spanned<Expression>,
    right: String,
    world: &mut World,
    environment: &mut Environment,
    registrations: &[&TypeRegistration],
) -> Result<Value, RunError> {
    let left_span = left.span.clone();
    match left.value {
        Expression::Variable(ident) => {
            if let Some(registration) = registrations
                .iter()
                .find(|v| v.type_info().type_path_table().short_path() == &ident)
            {
                let Some(mut var) = mut_dyn_reflect(world, registration) else {
                    return Ok(Value::None);
                };
                let reflect_mut = var.reflect_mut();

                match reflect_mut {
                    ReflectMut::Struct(dyn_struct) => {
                        let field = dyn_struct.field(&right).expect("no field on struct");

                        if let Some(number) = field.downcast_ref::<f64>() {
                            Ok(Value::Number(*number))
                        } else {
                            Ok(Value::DynamicValue(field.clone_value()))
                        }
                    }
                    ReflectMut::TupleStruct(_) => todo!(),
                    ReflectMut::Tuple(_) => todo!(),
                    ReflectMut::List(_) => todo!(),
                    ReflectMut::Array(_) => todo!(),
                    ReflectMut::Map(_) => todo!(),
                    ReflectMut::Enum(_) => todo!(),
                    ReflectMut::Value(_) => todo!(),
                }
            } else {
                todo!()
                // match &*environment.get(&variable, expr.span.clone())?.borrow() {
                //     Value::Number(number) => return Ok(Value::Number(*number)),
                //     _ => {}
                // }

                // // Unwrapping will always succeed due to only the owner of the variable having
                // // a strong reference. All other references are weak.
                // let value = Rc::try_unwrap(environment.move_var(&variable, expr.span)?).unwrap();

                // Ok(value.into_inner())
            }
        }
        _ => {
            let left = eval_expression(left, world, environment, registrations)?;

            match left {
                Value::StructObject { map, .. } => {
                    if let Some(value) = map.get(&right) {
                        Ok(Value::Reference(Rc::downgrade(value)))
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
    world: &mut World,
    registration: &TypeRegistration,
    environment: &mut Environment,
    registrations: &[&TypeRegistration],
    expr: Box<Spanned<Expression>>,
    var: String,
) -> Result<Value, RunError> {
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
                        get_mut_reflect(world, registration).unwrap();
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
                    let map = eval_object(map, world, environment, registrations)?;
                    let mut dyn_struct = DynamicStruct::default();
                    for (key, value) in map.into_iter() {
                        match Rc::try_unwrap(value).unwrap().into_inner() {
                            Value::None => {}
                            Value::Number(number) => dyn_struct.insert(&key, number),
                            Value::String(string) => dyn_struct.insert(&key, string.clone()),
                            Value::Reference(..) => todo!("todo reference"),
                            Value::StructObject { .. } => todo!("todo structobject"),
                            Value::DynamicValue(..) => todo!("todo dynamicvalue"),
                        }
                    }

                    dbg!("what");

                    let mut reflect: Mut<'_, dyn Reflect> =
                        get_mut_reflect(world, registration).unwrap();
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
    map: AHashMap<String, Spanned<Expression>>,
    world: &mut World,
    environment: &mut Environment,
    registrations: &[&TypeRegistration],
) -> Result<AHashMap<String, Rc<RefCell<Value>>>, RunError> {
    let map = map
        .into_iter()
        .map(
            |(key, expr)| -> Result<(String, Rc<RefCell<Value>>), RunError> {
                Ok((
                    key,
                    Rc::new(RefCell::new(eval_expression(
                        expr,
                        world,
                        environment,
                        registrations,
                    )?)),
                ))
            },
        )
        .collect::<Result<_, _>>()?;
    Ok(map)
}

fn get_mut_reflect<'a>(
    world: &'a mut World,
    registration: &TypeRegistration,
) -> Option<Mut<'a, dyn Reflect>> {
    if let Some(component_id) = world.components().get_resource_id(registration.type_id()) {
        let res = world.get_resource_mut_by_id(component_id).unwrap();
        let reflect_from_ptr = registration.data::<ReflectFromPtr>().unwrap();
        let val: Mut<'a, dyn Reflect> =
            res.map_unchanged(|ptr| unsafe { reflect_from_ptr.as_reflect_mut(ptr) });
        Some(val)
    } else {
        error!("Couldn't find component for resource registration");
        None
    }
}

// fn eval_variable_name(
//     name: Box<Spanned<Expression>>,
//     environment: &Environment,
// ) -> Result<&Rc<RefCell<Value>>, RunError> {
//     match name.value {
//         Expression::Variable(variable) => Ok(environment.get(&variable, name.span)?),
//         Expression::MemberExpression { left, right } => Ok(todo!()),
//         _ => Ok(todo!()),
//     }
// }

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
    registration: &TypeRegistration,
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
    let val: Mut<dyn Reflect> =
        resource.map_unchanged(|ptr| unsafe { reflect_from_ptr.as_reflect_mut(ptr) });
    Some(val)
}
