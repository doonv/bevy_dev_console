use std::collections::HashMap;
use std::rc::Weak;
use std::{cell::RefCell, rc::Rc};

use super::environment::{FunctionParameterData, ResultContainer};
use super::{super::Spanned, RunError};

use bevy::ecs::world::World;
use bevy::reflect::Reflect;

use logos::Span;

/// A runtime value
#[derive(Debug)]
pub enum Value {
    /// Nothing at all
    None,
    /// A number, for simplicity only f64s are used. (However this will probably change in the future)
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
    /// A dynamic [`HashMap].
    Object(HashMap<String, Rc<RefCell<Value>>>),
    /// An [`Object`](Value::Object) with a name attached to it.
    StructObject {
        /// The name of the struct
        name: String,
        /// The [`Object`](Value::Object) [`HashMap`].
        map: HashMap<String, Rc<RefCell<Value>>>,
    },
    /// A reference to a dynamic value. (aka a reference)
    Dynamic(Box<dyn Reflect>),
}

impl Value {
    /// Attempts to format this [`Value`].
    ///
    /// Returns an error if the [`Value`] is a reference to moved data.
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
            Value::Object(map) => {
                let mut string = String::new();
                string.push('{');
                for (key, value) in map {
                    string += &format!("\n\t{key}: {},", value.borrow().try_format(span.clone())?);
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
                    string += &format!("\n\t{key}: {},", value.borrow().try_format(span.clone())?);
                }
                if !map.is_empty() {
                    string.push('\n');
                }
                string.push('}');
                Ok(string)
            }
            Value::Dynamic(value) => Ok(format!("dyn {value:#?}")),
        }
    }
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
