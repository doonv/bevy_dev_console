use std::rc::Weak;
use std::{cell::RefCell, rc::Rc};

use super::environment::ResultContainer;
use super::RunError;

use super::super::Spanned;
use ahash::AHashMap;
use bevy::reflect::Reflect;

use logos::Span;

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
    Dynamic(Box<dyn Reflect>),
    Object(AHashMap<String, Rc<RefCell<Value>>>),
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
            Value::Object(map) => {
                let mut string = String::new();
                string.push('{');
                for (key, value) in map {
                    string += &format!("\n\t{key}: {},", value.borrow().try_format(span.clone())?);
                }
                if map.len() > 0 {
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
                if map.len() > 0 {
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

impl TryFrom<Spanned<Value>> for Value {
    type Error = RunError;

    fn try_from(value: Spanned<Value>) -> Result<Self, Self::Error> {
        Ok(value.value)
    }
}
impl TryFrom<Spanned<Value>> for f64 {
    type Error = RunError;

    fn try_from(value: Spanned<Value>) -> Result<Self, Self::Error> {
        if let Value::Number(number) = value.value {
            Ok(number)
        } else {
            todo!()
        }
    }
}
impl TryFrom<Spanned<Value>> for String {
    type Error = RunError;

    fn try_from(value: Spanned<Value>) -> Result<Self, Self::Error> {
        if let Value::String(string) = value.value {
            Ok(string)
        } else {
            todo!()
        }
    }
}
