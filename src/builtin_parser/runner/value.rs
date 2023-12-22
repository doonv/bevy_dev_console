use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Weak;
use std::{cell::RefCell, rc::Rc};

use super::environment::{FunctionParameterData, ResultContainer};
use super::reflection::{mut_dyn_reflect, IntoResource};
use super::EvalParams;
use super::{super::Spanned, RunError};

use bevy::ecs::world::World;
use bevy::reflect::{GetPath, ReflectRef, TypeInfo, TypeRegistration, VariantInfo, VariantType, Reflect, DynamicStruct};

use logos::Span;

/// A runtime value
#[derive(Debug)]
pub enum Value {
    /// Nothing at all
    None,
    /// A number, for simplicity only f64s are used. (However this will probably change in the future)
    Number(f64),
    /// `true` or `false`. Thats it...
    Boolean(bool),
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
    /// A dynamic [`HashMap`].
    Object(HashMap<String, Rc<RefCell<Value>>>),
    /// An [`Object`](Value::Object) with a name attached to it.
    StructObject {
        /// The name of the struct
        name: String,
        /// The [`Object`](Value::Object) [`HashMap`].
        map: HashMap<String, Rc<RefCell<Value>>>,
    },
    /// A reference to a dynamic value. (aka a reference)
    Resource(IntoResource),
}

impl Value {
    /// Converts this value into a [`Box<dyn Reflect>`].
    pub fn reflect(self) -> Box<dyn Reflect> {
        match self {
            Value::None => Box::new(()),
            Value::Number(number) => Box::new(number),
            Value::Boolean(boolean) => Box::new(boolean),
            Value::String(string) => Box::new(string),
            Value::Reference(reference) => todo!(),
            Value::Object(object) => {
                let mut dyn_struct = DynamicStruct::default();

                for (name, value) in object {
                    dyn_struct.insert_boxed(&name, Rc::try_unwrap(value).unwrap().into_inner().reflect());
                }

                Box::new(dyn_struct)
            },
            Value::StructObject { name, map } =>  {
                let mut dyn_struct = DynamicStruct::default();

                for (name, value) in map {
                    dyn_struct.insert_boxed(&name, Rc::try_unwrap(value).unwrap().into_inner().reflect());
                }

                Box::new(dyn_struct)
            },
            Value::Resource(_) => todo!(),
        }
    }

    /// Attempts to format this [`Value`].
    ///
    /// Returns an error if the [`Value`] is a reference to moved data.
    pub fn try_format(
        &self,
        span: Span,
        EvalParams {
            world,
            environment,
            registrations,
        }: EvalParams,
    ) -> Result<String, RunError> {
        match self {
            Value::None => Ok(format!("()")),
            Value::Number(number) => Ok(format!("{number}")),
            Value::Boolean(bool) => Ok(format!("{bool}")),
            Value::String(string) => Ok(format!("\"{string}\"")),
            Value::Reference(reference) => {
                if let Some(rc) = reference.upgrade() {
                    Ok(rc.borrow().try_format(
                        span,
                        EvalParams {
                            world,
                            environment,
                            registrations,
                        },
                    )?)
                } else {
                    Err(RunError::ReferenceToMovedData(span))
                }
            }
            Value::Object(map) => {
                let mut string = String::new();
                string.push('{');
                for (key, value) in map {
                    string += &format!(
                        "\n\t{key}: {},",
                        value.borrow().try_format(
                            span.clone(),
                            EvalParams {
                                world,
                                environment,
                                registrations
                            }
                        )?
                    );
                }
                if !map.is_empty() {
                    string.push('\n');
                }
                string.push('}');
                Ok(string)
            }
            Value::StructObject { name, map } => {
                let mut string = String::new();
                string += &format!("{name} {{");
                for (key, value) in map {
                    string += &format!(
                        "\n\t{key}: {},",
                        value.borrow().try_format(
                            span.clone(),
                            EvalParams {
                                world,
                                environment,
                                registrations
                            }
                        )?
                    );
                }
                if !map.is_empty() {
                    string.push('\n');
                }
                string.push('}');
                Ok(string)
            }
            Value::Resource(resource) => Ok(fancy_debug_print(
                resource,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )),
        }
    }
}
/// A massive function that takes in a type registration and the world and then
/// does all the hard work of printing out the type nicely.
fn fancy_debug_print(resource: &IntoResource, params: EvalParams) -> String {
    let registration = params
        .registrations
        .iter()
        .find(|reg| reg.type_id() == resource.id)
        .expect("registration no longer exists");
    let mut dyn_reflect = mut_dyn_reflect(params.world, registration).unwrap();

    let reflect = dyn_reflect
        .reflect_path_mut(resource.path.as_str())
        .unwrap();

    let mut f = String::new();
    let reflect_ref = reflect.reflect_ref();
    match reflect_ref {
        ReflectRef::Struct(struct_info) => {
            f += &format!("struct {} {{\n", struct_info.reflect_short_type_path());
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
        ReflectRef::TupleStruct(_) => todo!(),
        ReflectRef::Tuple(_) => todo!(),
        ReflectRef::List(_) => todo!(),
        ReflectRef::Array(_) => todo!(),
        ReflectRef::Map(_) => todo!(),
        ReflectRef::Enum(set_variant_info) => {
            // Print out the enum types
            f += &format!("enum {} {{\n", set_variant_info.reflect_short_type_path());
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
        ReflectRef::Value(value) => {
            f += &format!("{value:?}");
        },
    }
    f
}

impl From<Value> for ResultContainer<Value, RunError> {
    fn from(value: Value) -> Self {
        ResultContainer(Ok(value))
    }
}
impl From<()> for Value {
    fn from((): ()) -> Self {
        Value::None
    }
}
impl From<()> for ResultContainer<Value, RunError> {
    fn from((): ()) -> Self {
        ResultContainer(Ok(Value::None))
    }
}
impl From<f64> for Value {
    fn from(number: f64) -> Self {
        Value::Number(number)
    }
}
impl From<f64> for ResultContainer<Value, RunError> {
    fn from(number: f64) -> Self {
        ResultContainer(Ok(Value::Number(number)))
    }
}
impl From<String> for Value {
    fn from(string: String) -> Self {
        Value::String(string)
    }
}

impl<'world, 'env, 'reg, 'world2, 'env2, 'reg2>
    TryFrom<FunctionParameterData<'world, 'env, 'reg, 'world2, 'env2, 'reg2>> for Spanned<Value>
{
    type Error = RunError;

    fn try_from(
        FunctionParameterData { value, .. }: FunctionParameterData,
    ) -> Result<Self, Self::Error> {
        Ok(value)
    }
}
impl<'world, 'env, 'reg, 'world2, 'env2, 'reg2>
    TryFrom<FunctionParameterData<'world, 'env, 'reg, 'world2, 'env2, 'reg2>> for Value
{
    type Error = RunError;

    fn try_from(
        FunctionParameterData { value, .. }: FunctionParameterData,
    ) -> Result<Self, Self::Error> {
        Ok(value.value)
    }
}
impl<'world, 'env, 'reg, 'world2, 'env2, 'reg2>
    TryFrom<FunctionParameterData<'world, 'env, 'reg, 'world2, 'env2, 'reg2>> for f64
{
    type Error = RunError;

    fn try_from(
        FunctionParameterData { value, .. }: FunctionParameterData,
    ) -> Result<Self, Self::Error> {
        if let Value::Number(number) = value.value {
            Ok(number)
        } else {
            todo!()
        }
    }
}

impl<'world, 'env, 'reg, 'world2, 'env2, 'reg2>
    TryFrom<FunctionParameterData<'world, 'env, 'reg, 'world2, 'env2, 'reg2>> for String
{
    type Error = RunError;

    fn try_from(
        FunctionParameterData { value, .. }: FunctionParameterData,
    ) -> Result<Self, Self::Error> {
        if let Value::String(string) = value.value {
            Ok(string)
        } else {
            todo!()
        }
    }
}

impl<'world, 'env, 'reg, 'world2, 'env2, 'reg2>
    TryFrom<FunctionParameterData<'world, 'env, 'reg, 'world2, 'env2, 'reg2>>
    for &'world mut World
{
    type Error = RunError;

    fn try_from(
        FunctionParameterData { world, .. }: FunctionParameterData<
            'world,
            'env,
            'reg,
            'world2,
            'env2,
            'reg2,
        >,
    ) -> Result<Self, Self::Error> {
        if let Some(world) = world.take() {
            Ok(world)
        } else {
            todo!()
        }
    }
}
