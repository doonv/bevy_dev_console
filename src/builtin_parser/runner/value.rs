use std::collections::HashMap;
use std::fmt::Debug;

use crate::builtin_parser::number::Number;
use crate::builtin_parser::{Environment, StrongRef, UniqueRc};

use super::super::Spanned;
use super::environment::FunctionParam;
use super::error::RunError;
use super::reflection::{CreateRegistration, IntoResource};
use super::unique_rc::WeakRef;

use bevy::ecs::world::World;
use bevy::reflect::{
    DynamicStruct, GetPath, Reflect, ReflectRef, TypeInfo, TypeRegistration, VariantInfo,
    VariantType,
};

use logos::Span;

/// A runtime value
#[derive(Debug, Clone)]
pub enum Value {
    /// Nothing at all
    None,
    /// A number.
    Number(Number),
    /// `true` or `false`. Thats it...
    Boolean(bool),
    /// A string... there isn't much to say about this one.
    String(String),
    /// A reference.
    ///
    /// References are very similar to rust's ownership and borrowing.
    /// We achieve this by storing every variable as a [`UniqueRc<T>`]
    /// (which is essentially just [`Rc<RefCell<T>>`] but having only
    /// the owner of the value have a strong reference, while every
    /// other value has a weak reference. This causes
    /// [`Rc::try_unwrap`] to succeed every time)
    Reference(WeakRef<Value>),
    /// A dynamic [`HashMap`].
    Object(HashMap<String, UniqueRc<Value>>),
    /// An [`Object`](Value::Object) with a name attached to it.
    StructObject {
        /// The name of the struct
        name: String,
        /// The [`Object`](Value::Object) [`HashMap`].
        map: HashMap<String, UniqueRc<Value>>,
    },
    /// A reference to a dynamic value. (aka a reference)
    Resource(IntoResource),
}

impl Value {
    /// Converts this value into a [`Box<dyn Reflect>`].
    ///
    /// `ty` is used for type inference.
    pub fn reflect(self, span: Span, ty: &str) -> Result<Box<dyn Reflect>, RunError> {
        match self {
            Value::None => Ok(Box::new(())),
            Value::Number(number) => number.reflect(span, ty),
            Value::Boolean(boolean) => Ok(Box::new(boolean)),
            Value::String(string) => Ok(Box::new(string)),
            Value::Reference(_reference) => Err(RunError::CannotReflectReference(span)),
            Value::Object(object) | Value::StructObject { map: object, .. } => {
                let mut dyn_struct = DynamicStruct::default();

                for (name, value) in object {
                    dyn_struct.insert_boxed(&name, value.into_inner().reflect(span.clone(), ty)?);
                }

                Ok(Box::new(dyn_struct))
            }
            Value::Resource(_) => Err(RunError::CannotReflectResource(span)),
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
                        value.borrow_inner().borrow().try_format(
                            span.clone(),
                            world,
                            registrations
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
                        value.borrow_inner().borrow().try_format(
                            span.clone(),
                            world,
                            registrations
                        )?
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

    /// Returns the kind of [`Value`] as a [string slice](str).
    /// Used for more natural sounding error messages.
    pub fn kind(&self) -> &'static str {
        match self {
            Value::None => "nothing",
            Value::Number(number) => number.kind(),
            Value::Boolean(_) => "a boolean",
            Value::String(_) => "a string",
            Value::Reference(_) => "a reference",
            Value::Object(_) => "a object",
            Value::StructObject { .. } => "a struct object",
            Value::Resource(_) => "a resource",
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
    const TAB: &str = "    ";
    let registration = registrations.create_registration(resource.id);
    let dyn_reflect = resource.ref_dyn_reflect(world, registration);

    let reflect = dyn_reflect.reflect_path(resource.path.as_str()).unwrap();

    fn debug_subprint(reflect: &dyn Reflect, indentation: usize) -> String {
        let mut f = String::new();
        let reflect_ref = reflect.reflect_ref();
        let identation_string = TAB.repeat(indentation);
        match reflect_ref {
            ReflectRef::Struct(struct_info) => {
                f += "{\n";
                for i in 0..struct_info.field_len() {
                    let field = struct_info.field_at(i).unwrap();
                    let field_name = struct_info.name_at(i).unwrap();

                    let field_value = debug_subprint(field, indentation + 1);
                    f += &format!(
                        "{identation_string}{TAB}{}: {} = {},\n",
                        field_name,
                        field.reflect_short_type_path(),
                        field_value
                    );
                }
                f += &identation_string;
                f += "}";
            }
            ReflectRef::TupleStruct(_) => todo!(),
            ReflectRef::Tuple(_) => todo!(),
            ReflectRef::List(_) => todo!(),
            ReflectRef::Array(_) => todo!(),
            ReflectRef::Map(_) => todo!(),
            ReflectRef::Enum(variant) => {
                // Print out the enum types
                f += variant.variant_name();

                match variant.variant_type() {
                    VariantType::Struct => {
                        f += " {\n";
                        for field in variant.iter_fields() {
                            f += &format!(
                                "{identation_string}{TAB}{}: {} = {},\n",
                                field.name().unwrap(),
                                field.value().reflect_short_type_path(),
                                debug_subprint(field.value(), indentation + 1)
                            );
                        }
                        f += &identation_string;
                        f += "}";
                    }
                    VariantType::Tuple => {
                        f += "(\n";
                        for field in variant.iter_fields() {
                            f += &format!(
                                "{identation_string}{TAB}{} = {},\n",
                                field.value().reflect_short_type_path(),
                                debug_subprint(field.value(), indentation + 1)
                            );
                        }
                        f += &identation_string;
                        f += ")";
                    }
                    VariantType::Unit => {}
                }
            }
            ReflectRef::Value(value) => {
                f += &format!("{value:?}");
            }
        }

        f
    }

    let mut f = String::new();
    let reflect_ref = reflect.reflect_ref();
    match reflect_ref {
        ReflectRef::Struct(struct_info) => {
            f += &format!("struct {} {{\n", struct_info.reflect_short_type_path());
            for i in 0..struct_info.field_len() {
                let field = struct_info.field_at(i).unwrap();
                let field_name = struct_info.name_at(i).unwrap();

                let field_value = debug_subprint(field, 1);
                f += &format!(
                    "{TAB}{}: {} = {},\n",
                    field_name,
                    field.reflect_short_type_path(),
                    field_value
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
                        f += TAB;
                        f += "}";
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

macro_rules! from_t {
    (impl $type:ty: $var:ident => $expr:expr) => {
        impl From<$type> for Value {
            fn from($var: $type) -> Self {
                $expr
            }
        }
    };
}
macro_rules! from_number {
    ($($number:ident),*$(,)?) => {
        $(
            from_t!(impl $number: number => Value::Number(Number::$number(number)));
        )*
    };
}

from_number!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, f32, f64);

from_t!(impl String: string => Value::String(string));
from_t!(impl bool: bool => Value::Boolean(bool));
from_t!(impl Number: number => Value::Number(number));
from_t!(impl HashMap<String, UniqueRc<Value>>: hashmap => Value::Object(hashmap));
from_t!(impl HashMap<String, Value>: hashmap => Value::Object(
    hashmap
        .into_iter()
        .map(|(k, v)| (k, UniqueRc::new(v)))
        .collect(),
));

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
impl<T: TryFrom<Spanned<Value>, Error = RunError>> FunctionParam for Spanned<T> {
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
            span: value.span.clone(),
            value: T::try_from(value)?,
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
                let value = value.unwrap();
                if let $value_pattern = value.value {
                    Ok($return)
                } else {
                    Err(RunError::IncompatibleFunctionParameter {
                        expected: stringify!($type),
                        actual: value.value.kind(),
                        span: value.span,
                    })
                }
            }
        }
        impl TryFrom<Spanned<Value>> for $type {
            type Error = RunError;

            fn try_from(value: Spanned<Value>) -> Result<Self, Self::Error> {
                if let $value_pattern = value.value {
                    Ok($return)
                } else {
                    todo!()
                }
            }
        }
    };
}
macro_rules! impl_function_param_for_numbers {
    ($generic:ident ($($number:ident),*$(,)?)) => {
        $(
            impl FunctionParam for $number {
                type Item<'world, 'env, 'reg> = Self;
                const USES_VALUE: bool = true;

                fn get<'world, 'env, 'reg>(
                    value: Option<Spanned<Value>>,
                    _: &mut Option<&'world mut World>,
                    _: &mut Option<&'env mut Environment>,
                    _: &'reg [&'reg TypeRegistration],
                ) -> Result<Self::Item<'world, 'env, 'reg>, RunError> {
                    let value = value.unwrap();
                    match value.value {
                        Value::Number(Number::$number(value)) => Ok(value),
                        Value::Number(Number::$generic(value)) => Ok(value as $number),
                        _ => Err(RunError::IncompatibleFunctionParameter {
                            expected: concat!("a ", stringify!($number)),
                            actual: value.value.kind(),
                            span: value.span,
                        })
                    }
                }
            }
            impl TryFrom<Spanned<Value>> for $number {
                type Error = RunError;

                fn try_from(value: Spanned<Value>) -> Result<Self, Self::Error> {
                    match value.value {
                        Value::Number(Number::$number(value)) => Ok(value),
                        Value::Number(Number::$generic(value)) => Ok(value as $number),
                        _ => Err(RunError::IncompatibleFunctionParameter {
                            expected: concat!("a ", stringify!($number)),
                            actual: value.value.kind(),
                            span: value.span
                        })
                    }
                }
            }
        )*
    };
}

impl_function_param_for_numbers!(Float(f32, f64));
impl_function_param_for_numbers!(Integer(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize));

impl_function_param_for_value!(impl bool: Value::Boolean(boolean) => boolean);
impl_function_param_for_value!(impl Number: Value::Number(number) => number);
impl_function_param_for_value!(impl String: Value::String(string) => string);
// impl_function_param_for_value!(impl HashMap<String, UniqueRc<Value>>: Value::Object(object) => object);
impl_function_param_for_value!(impl HashMap<String, Value>: Value::Object(object) => {
    object.into_iter().map(|(k, v)| (k, v.into_inner())).collect()
});
impl_function_param_for_value!(impl StrongRef<Value>: Value::Reference(reference) => reference.upgrade().unwrap());

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
