use std::cell::RefMut;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Weak;
use std::{cell::RefCell, rc::Rc};

use crate::builtin_parser::Environment;

use super::environment::{FunctionParam, ResultContainer};
use super::reflection::{mut_dyn_reflect, IntoRegistration, IntoResource};
use super::EvalParams;
use super::{super::Spanned, RunError};

use bevy::ecs::system::Resource;
use bevy::ecs::world::World;
use bevy::reflect::{
    DynamicStruct, GetPath, Reflect, ReflectRef, TypeInfo, TypeRegistration, VariantInfo,
    VariantType,
};

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
            Value::Object(object) | Value::StructObject { map: object, .. } => {
                let mut dyn_struct = DynamicStruct::default();

                for (name, value) in object {
                    dyn_struct
                        .insert_boxed(&name, Rc::try_unwrap(value).unwrap().into_inner().reflect());
                }

                Box::new(dyn_struct)
            }
            Value::Resource(_) => todo!(),
        }
    }

    /// Attempts to format this [`Value`].
    ///
    /// Returns an error if the [`Value`] is a reference to moved data.
    pub fn try_format(
        &self,
        span: Span,
        world: &World,
        registrations: &[&TypeRegistration],
    ) -> Result<String, RunError> {
        match self {
            Value::None => Ok(format!("()")),
            Value::Number(number) => Ok(format!("{number}")),
            Value::Boolean(bool) => Ok(format!("{bool}")),
            Value::String(string) => Ok(format!("\"{string}\"")),
            Value::Reference(reference) => {
                if let Some(rc) = reference.upgrade() {
                    Ok(rc.borrow().try_format(span, world, registrations)?)
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
                        value
                            .borrow()
                            .try_format(span.clone(), world, registrations)?
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
                        value
                            .borrow()
                            .try_format(span.clone(), world, registrations)?
                    );
                }
                if !map.is_empty() {
                    string.push('\n');
                }
                string.push('}');
                Ok(string)
            }
            Value::Resource(resource) => Ok(fancy_debug_print(resource, world, registrations)),
        }
    }
}
/// A massive function that takes in a type registration and the world and then
/// does all the hard work of printing out the type nicely.
fn fancy_debug_print(
    resource: &IntoResource,
    world: &World,
    registrations: &[&TypeRegistration],
) -> String {
    let registration = registrations.into_registration(resource.id);
    let dyn_reflect = resource.ref_dyn_reflect(world, registration);

    let reflect = dyn_reflect.reflect_path(resource.path.as_str()).unwrap();

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
        }
    }
    f
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
impl From<bool> for Value {
    fn from(boolean: bool) -> Self {
        Value::Boolean(boolean)
    }
}

impl From<HashMap<String, Rc<RefCell<Value>>>> for Value {
    fn from(hashmap: HashMap<String, Rc<RefCell<Value>>>) -> Self {
        Value::Object(hashmap)
    }
}
impl From<HashMap<String, Value>> for Value {
    fn from(hashmap: HashMap<String, Value>) -> Self {
        Value::Object(
            hashmap
                .into_iter()
                .map(|(k, v)| (k, Rc::new(RefCell::new(v))))
                .collect(),
        )
    }
}

impl FunctionParam for Spanned<Value> {
    type Item<'world, 'env, 'reg> = Self;
    const USES_VALUE: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, RunError> {
        Ok(value.unwrap())
    }
}
impl<T: TryFrom<Value, Error = RunError>> FunctionParam for Spanned<T> {
    type Item<'world, 'env, 'reg> = Self;
    const USES_VALUE: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, RunError> {
        let value = value.unwrap();
        Ok(Spanned {
            span: value.span,
            value: T::try_from(value.value)?,
        })
    }
}
impl FunctionParam for Value {
    type Item<'world, 'env, 'reg> = Self;
    const USES_VALUE: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, RunError> {
        Ok(value.unwrap().value)
    }
}

macro_rules! impl_function_param_for_value {
    (impl $type:ty: $value_pattern:pat => $return:expr) => {
        impl FunctionParam for $type {
            type Item<'world, 'env, 'reg> = Self;
            const USES_VALUE: bool = true;

            fn get<'world, 'env, 'reg>(
                value: Option<Spanned<Value>>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::Item<'world, 'env, 'reg>, RunError> {
                if let $value_pattern = value.unwrap().value {
                    Ok($return)
                } else {
                    todo!()
                }
            }
        }
        impl TryFrom<Value> for $type {
            type Error = RunError;

            fn try_from(value: Value) -> Result<Self, Self::Error> {
                if let $value_pattern = value {
                    Ok($return)
                } else {
                    todo!()
                }
            }
        }
    };
}
impl_function_param_for_value!(impl f64: Value::Number(number) => number);
impl_function_param_for_value!(impl bool: Value::Boolean(boolean) => boolean);
impl_function_param_for_value!(impl String: Value::String(string) => string);
impl_function_param_for_value!(impl HashMap<String, Rc<RefCell<Value>>>: Value::Object(object) => object);
impl_function_param_for_value!(impl HashMap<String, Value>: Value::Object(object) => {
    object.into_iter().map(|(k, v)| (k, Rc::try_unwrap(v).unwrap().into_inner())).collect()
});
impl_function_param_for_value!(impl Weak<RefCell<Value>>: Value::Reference(reference) => reference);

impl FunctionParam for &mut World {
    type Item<'world, 'env, 'reg> = &'world mut World;
    const USES_VALUE: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        world: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, RunError> {
        let Some(world) = world.take() else {
            // make this unreachable by checking the function when it gets registered
            todo!("world borrowed twice");
        };

        Ok(world)
    }
}

// This probably isn't a good idea. But eh who cares, more power to the user.
impl FunctionParam for &mut Environment {
    type Item<'world, 'env, 'reg> = &'env mut Environment;
    const USES_VALUE: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, RunError> {
        Ok(environment.take().unwrap())
    }
}

impl FunctionParam for &[&TypeRegistration] {
    type Item<'world, 'env, 'reg> = &'reg [&'reg TypeRegistration];
    const USES_VALUE: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        registrations: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, RunError> {
        Ok(registrations)
    }
}
