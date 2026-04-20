//! Runtime values

use std::collections::HashMap;

use bevy::reflect::{DynamicStruct, DynamicTuple, PartialReflect};
use logos::Span;

use crate::builtin_parser::number::Number;
use crate::builtin_parser::{Diagnostic, SpanExtension, Spanned, UniqueRc};

use super::error::EvalError;
use super::reflection::IntoResource;
use super::unique_rc::WeakRef;

pub mod format;
pub mod function_param;
pub mod kind;

pub use kind::ValueKind;

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
    ///
    /// [`Rc::try_unwrap`]: std::rc::Rc::try_unwrap
    /// [`Rc<RefCell<T>>`]: std::rc::Rc
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
    /// A fixed size list of values that can have different types.
    Tuple(Box<[Spanned<UniqueRc<Value>>]>),
    /// A [`Tuple`](Value::Tuple) with a name attached to it.
    StructTuple {
        /// The name of the tuple
        name: String,
        /// The [`Object`](Value::Object) slice.
        tuple: Box<[Spanned<UniqueRc<Value>>]>,
    },
    /// A reference to a dynamic value. (aka a reference)
    Resource(IntoResource),
}

impl Value {
    /// Converts this value into a [`Box<dyn PartialReflect>`].
    ///
    /// `ty` is used for type inference.
    pub fn reflect(
        self,
        span: Span,
        ty: &str,
    ) -> Result<Box<dyn PartialReflect>, Diagnostic<EvalError>> {
        match self {
            Value::None => Ok(Box::new(())),
            Value::Number(number) => number
                .reflect(span, ty)
                .map(PartialReflect::into_partial_reflect),
            Value::Boolean(boolean) => Ok(Box::new(boolean)),
            Value::String(string) => Ok(Box::new(string)),
            Value::Reference(_reference) => Err(span.diagnose(EvalError::CannotReflectReference)),
            Value::Object(object) | Value::StructObject { map: object, .. } => {
                let mut dyn_struct = DynamicStruct::default();

                for (name, value) in object {
                    dyn_struct.insert_boxed(&name, value.into_inner().reflect(span.clone(), ty)?);
                }

                Ok(Box::new(dyn_struct))
            }
            Value::Tuple(tuple) | Value::StructTuple { tuple, .. } => {
                let mut dyn_tuple = DynamicTuple::default();

                for element in Vec::from(tuple).into_iter() {
                    dyn_tuple.insert_boxed(element.value.into_inner().reflect(element.span, ty)?);
                }

                Ok(Box::new(dyn_tuple))
            }
            Value::Resource(_) => Err(span.diagnose(EvalError::CannotReflectResource)),
        }
    }
}

macro_rules! from_t {
    (impl $type:ty: $var:pat => $expr:expr) => {
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

from_t!(impl (): () => Value::None);
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
