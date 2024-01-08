use std::fmt::Display;
use std::ops::*;

use bevy::reflect::Reflect;
use logos::Span;

use super::{RunError, SpanExtension, Spanned};

/// An enum that contains any type of number.
///
/// The [`Integer`](Number::Integer) and [`Float`](Number::Float) types
/// are generic types that then get downcasted when they first interact
/// with a concrete type. (i.e. calling a function, etc)
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs, non_camel_case_types)]
pub enum Number {
    /// Generic integer that can get downcasted.
    Integer(i128),
    /// Generic float that can get downcasted to a [`f64`] and [`f32`]
    Float(f64),

    u8(u8),
    u16(u16),
    u32(u32),
    u64(u64),
    usize(usize),
    i8(i8),
    i16(i16),
    i32(i32),
    i64(i64),
    isize(isize),
    f32(f32),
    f64(f64),
}

impl Number {
    /// Converts this into a [`Box<dyn Reflect>`](Reflect).
    pub fn reflect(self, span: Span, ty: &str) -> Result<Box<dyn Reflect>, RunError> {
        match self {
            Number::u8(number) => Ok(Box::new(number)),
            Number::u16(number) => Ok(Box::new(number)),
            Number::u32(number) => Ok(Box::new(number)),
            Number::u64(number) => Ok(Box::new(number)),
            Number::usize(number) => Ok(Box::new(number)),
            Number::i8(number) => Ok(Box::new(number)),
            Number::i16(number) => Ok(Box::new(number)),
            Number::i32(number) => Ok(Box::new(number)),
            Number::i64(number) => Ok(Box::new(number)),
            Number::isize(number) => Ok(Box::new(number)),
            Number::f32(number) => Ok(Box::new(number)),
            Number::f64(number) => Ok(Box::new(number)),
            Number::Integer(number) => match ty {
                "u8" => Ok(Box::new(number as u8)),
                "u16" => Ok(Box::new(number as u16)),
                "u32" => Ok(Box::new(number as u32)),
                "u64" => Ok(Box::new(number as u64)),
                "usize" => Ok(Box::new(number as usize)),
                "i8" => Ok(Box::new(number as i8)),
                "i16" => Ok(Box::new(number as i16)),
                "i32" => Ok(Box::new(number as i32)),
                "i64" => Ok(Box::new(number as i64)),
                "isize" => Ok(Box::new(number as isize)),
                ty => Err(RunError::IncompatibleReflectTypes {
                    expected: "integer".to_string(),
                    actual: ty.to_string(),
                    span,
                }),
            },
            Number::Float(number) => match ty {
                "f32" => Ok(Box::new(number as f32)),
                "f64" => Ok(Box::new(number)),
                ty => Err(RunError::IncompatibleReflectTypes {
                    expected: "float".to_string(),
                    actual: ty.to_string(),
                    span,
                }),
            },
        }
    }

    /// Returns a [`&'static str`](str) represents the kind of the number.
    pub fn kind(&self) -> &'static str {
        match self {
            Number::Float(_) => "a float",
            Number::Integer(_) => "an integer",
            Number::u8(_) => "a u8",
            Number::u16(_) => "a u16",
            Number::u32(_) => "a u32",
            Number::u64(_) => "a u64",
            Number::usize(_) => "a usize",
            Number::i8(_) => "a i8",
            Number::i16(_) => "a i16",
            Number::i32(_) => "a i32",
            Number::i64(_) => "a i64",
            Number::isize(_) => "a usize",
            Number::f32(_) => "a f32",
            Number::f64(_) => "a f64",
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Float(number) => write!(f, "{number} (float)"),
            Number::Integer(number) => write!(f, "{number} (integer)"),
            Number::u8(number) => write!(f, "{number} (u8)"),
            Number::u16(number) => write!(f, "{number} (u16)"),
            Number::u32(number) => write!(f, "{number} (u32)"),
            Number::u64(number) => write!(f, "{number} (u64)"),
            Number::usize(number) => write!(f, "{number} (usize)"),
            Number::i8(number) => write!(f, "{number} (i8)"),
            Number::i16(number) => write!(f, "{number} (i16)"),
            Number::i32(number) => write!(f, "{number} (i32)"),
            Number::i64(number) => write!(f, "{number} (i64)"),
            Number::isize(number) => write!(f, "{number} (isize)"),
            Number::f32(number) => write!(f, "{number} (f32)"),
            Number::f64(number) => write!(f, "{number} (f64)"),
        }
    }
}

macro_rules! impl_op {
    ($fn:ident, $op:tt) => {
        impl Number {
            #[doc = concat!("Performs the `", stringify!($op), "` calculation.")]
            pub fn $fn(left: Number, right: Number, span: Span) -> Result<Number, RunError> {
                match (left, right) {
                    (Number::u8(left), Number::u8(right)) => Ok(Number::u8(left $op right)),
                    (Number::u16(left), Number::u16(right)) => Ok(Number::u16(left $op right)),
                    (Number::u32(left), Number::u32(right)) => Ok(Number::u32(left $op right)),
                    (Number::u64(left), Number::u64(right)) => Ok(Number::u64(left $op right)),
                    (Number::usize(left), Number::usize(right)) => Ok(Number::usize(left $op right)),
                    (Number::i8(left), Number::i8(right)) => Ok(Number::i8(left $op right)),
                    (Number::i16(left), Number::i16(right)) => Ok(Number::i16(left $op right)),
                    (Number::i32(left), Number::i32(right)) => Ok(Number::i32(left $op right)),
                    (Number::i64(left), Number::i64(right)) => Ok(Number::i64(left $op right)),
                    (Number::isize(left), Number::isize(right)) => Ok(Number::isize(left $op right)),
                    (Number::f32(left), Number::f32(right)) => Ok(Number::f32(left $op right)),
                    (Number::f64(left), Number::f64(right)) => Ok(Number::f64(left $op right)),

                    (Number::Integer(left), Number::u8(right)) => Ok(Number::u8(left as u8 $op right)),
                    (Number::Integer(left), Number::u16(right)) => Ok(Number::u16(left as u16 $op right)),
                    (Number::Integer(left), Number::u32(right)) => Ok(Number::u32(left as u32 $op right)),
                    (Number::Integer(left), Number::u64(right)) => Ok(Number::u64(left as u64 $op right)),
                    (Number::Integer(left), Number::usize(right)) => Ok(Number::usize(left as usize $op right)),
                    (Number::Integer(left), Number::i8(right)) => Ok(Number::i8(left as i8 $op right)),
                    (Number::Integer(left), Number::i16(right)) => Ok(Number::i16(left as i16 $op right)),
                    (Number::Integer(left), Number::i32(right)) => Ok(Number::i32(left as i32 $op right)),
                    (Number::Integer(left), Number::i64(right)) => Ok(Number::i64(left as i64 $op right)),
                    (Number::Integer(left), Number::isize(right)) => Ok(Number::isize(left as isize $op right)),
                    (Number::Integer(left), Number::Integer(right)) => Ok(Number::Integer(left $op right)),
                    (Number::u8(left), Number::Integer(right)) => Ok(Number::u8(left $op right as u8)),
                    (Number::u16(left), Number::Integer(right)) => Ok(Number::u16(left $op right as u16)),
                    (Number::u32(left), Number::Integer(right)) => Ok(Number::u32(left $op right as u32)),
                    (Number::u64(left), Number::Integer(right)) => Ok(Number::u64(left $op right as u64)),
                    (Number::usize(left), Number::Integer(right)) => Ok(Number::usize(left $op right as usize)),
                    (Number::i8(left), Number::Integer(right)) => Ok(Number::i8(left $op right as i8)),
                    (Number::i16(left), Number::Integer(right)) => Ok(Number::i16(left $op right as i16)),
                    (Number::i32(left), Number::Integer(right)) => Ok(Number::i32(left $op right as i32)),
                    (Number::i64(left), Number::Integer(right)) => Ok(Number::i64(left $op right as i64)),
                    (Number::isize(left), Number::Integer(right)) => Ok(Number::isize(left $op right as isize)),

                    (Number::Float(left), Number::f32(right)) => Ok(Number::f32(left as f32 $op right)),
                    (Number::Float(left), Number::f64(right)) => Ok(Number::f64(left as f64 $op right)),
                    (Number::Float(left), Number::Float(right)) => Ok(Number::Float(left $op right)),
                    (Number::f32(left), Number::Float(right)) => Ok(Number::f32(left $op right as f32)),
                    (Number::f64(left), Number::Float(right)) => Ok(Number::f64(left $op right as f64)),
                    _ => Err(RunError::IncompatibleNumberTypes {
                        left: left.kind(),
                        right: right.kind(),
                        span
                    })
                }
            }
        }
    };
}

impl_op!(add, +);
impl_op!(sub, -);
impl_op!(mul, *);
impl_op!(div, /);
impl_op!(rem, %);

macro_rules! impl_op_spanned {
    ($trait:ident, $method:ident) => {
        impl $trait<Self> for Spanned<Number> {
            type Output = Result<Number, RunError>;
            fn $method(self, rhs: Self) -> Self::Output {
                let span = self.span.join(rhs.span);

                Number::$method(self.value, rhs.value, span)
            }
        }
    };
}

impl_op_spanned!(Add, add);
impl_op_spanned!(Sub, sub);
impl_op_spanned!(Mul, mul);
impl_op_spanned!(Rem, rem);

impl Number {
    /// Performs the unary `-` operation.
    pub fn neg(self, span: Span) -> Result<Number, RunError> {
        match self {
            Number::u8(_) => Err(RunError::CannotNegateUnsignedInteger(Spanned {
                span,
                value: self,
            })),
            Number::u16(_) => Err(RunError::CannotNegateUnsignedInteger(Spanned {
                span,
                value: self,
            })),
            Number::u32(_) => Err(RunError::CannotNegateUnsignedInteger(Spanned {
                span,
                value: self,
            })),
            Number::u64(_) => Err(RunError::CannotNegateUnsignedInteger(Spanned {
                span,
                value: self,
            })),
            Number::usize(_) => Err(RunError::CannotNegateUnsignedInteger(Spanned {
                span,
                value: self,
            })),
            Number::i8(number) => Ok(Number::i8(-number)),
            Number::i16(number) => Ok(Number::i16(-number)),
            Number::i32(number) => Ok(Number::i32(-number)),
            Number::i64(number) => Ok(Number::i64(-number)),
            Number::isize(number) => Ok(Number::isize(-number)),
            Number::f32(number) => Ok(Number::f32(-number)),
            Number::f64(number) => Ok(Number::f64(-number)),
            Number::Float(number) => Ok(Number::Float(-number)),
            Number::Integer(number) => Ok(Number::Integer(-number)),
        }
    }
}

macro_rules! from_primitive {
    ($primitive:ident) => {
        impl From<$primitive> for Number {
            fn from(value: $primitive) -> Self {
                Number::$primitive(value)
            }
        }
    };
}

from_primitive!(u8);
from_primitive!(u16);
from_primitive!(u32);
from_primitive!(u64);
from_primitive!(i8);
from_primitive!(i16);
from_primitive!(i32);
from_primitive!(i64);
from_primitive!(f32);
from_primitive!(f64);
