#![allow(missing_docs, non_camel_case_types)]

use std::fmt::{Debug, Display};
use std::ops::*;

use bevy::reflect::Reflect;
use kinded::Kinded;
use logos::Span;

use crate::builtin_parser::parser::BinaryOperator;
use crate::builtin_parser::{Diagnostic, ErrorExtension, YELLOW};

use super::runner::error::EvalError;
use super::{SpanExtension, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Kinded)]
#[kinded(skip_derive(Display))]
pub enum UnsignedInteger {
    u8(u8),
    u16(u16),
    u32(u32),
    u64(u64),
    usize(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Kinded)]
#[kinded(skip_derive(Display))]
pub enum SignedInteger {
    i8(i8),
    i16(i16),
    i32(i32),
    i64(i64),
    isize(isize),
    /// Generic integer that can get downcasted.
    Unspecified(i128),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Integer {
    Unsigned(UnsignedInteger),
    Signed(SignedInteger),
}

#[derive(Debug, Clone, Copy, PartialEq, Kinded)]
#[kinded(skip_derive(Display))]
pub enum Float {
    f32(f32),
    f64(f64),
    /// Generic float that can get downcasted to a [`f64`] and [`f32`]
    Unspecified(f64),
}

/// An enum that contains any type of number.
///
/// The [`Integer`](SignedInteger::Unspecified) and [`Float`](Float::Float) types
/// are generic types that then get downcasted when they first interact
/// with a concrete type. (i.e. calling a function, etc)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    Integer(Integer),
    Float(Float),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerKind {
    Unsigned(UnsignedIntegerKind),
    Signed(SignedIntegerKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberKind {
    Integer(IntegerKind),
    Float(FloatKind),
}

impl kinded::Kind for IntegerKind {
    fn all() -> &'static [Self] {
        &[]
    }
}

impl kinded::Kind for NumberKind {
    fn all() -> &'static [Self] {
        &[]
    }
}

impl Kinded for Integer {
    type Kind = IntegerKind;

    fn kind(&self) -> Self::Kind {
        match self {
            Integer::Unsigned(v) => IntegerKind::Unsigned(v.kind()),
            Integer::Signed(v) => IntegerKind::Signed(v.kind()),
        }
    }
}

impl Kinded for Number {
    type Kind = NumberKind;

    fn kind(&self) -> Self::Kind {
        match self {
            Number::Integer(v) => NumberKind::Integer(v.kind()),
            Number::Float(v) => NumberKind::Float(v.kind()),
        }
    }
}

impl Integer {
    /// Converts this into a [`Box<dyn Reflect>`](Reflect).
    pub fn reflect(self, span: Span, ty: &str) -> Result<Box<dyn Reflect>, Diagnostic<EvalError>> {
        match self {
            Integer::Unsigned(number) => match number {
                UnsignedInteger::u8(number) => Ok(Box::new(number)),
                UnsignedInteger::u16(number) => Ok(Box::new(number)),
                UnsignedInteger::u32(number) => Ok(Box::new(number)),
                UnsignedInteger::u64(number) => Ok(Box::new(number)),
                UnsignedInteger::usize(number) => Ok(Box::new(number)),
            },
            Integer::Signed(number) => match number {
                SignedInteger::i8(number) => Ok(Box::new(number)),
                SignedInteger::i16(number) => Ok(Box::new(number)),
                SignedInteger::i32(number) => Ok(Box::new(number)),
                SignedInteger::i64(number) => Ok(Box::new(number)),
                SignedInteger::isize(number) => Ok(Box::new(number)),
                SignedInteger::Unspecified(number) => match ty {
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
                    ty => Err(span.diagnose(EvalError::IncompatibleReflectTypes {
                        expected: "integer".to_owned(),
                        actual: ty.to_owned(),
                    })),
                },
            },
        }
    }
}

impl Number {
    /// Converts this into a [`Box<dyn Reflect>`](Reflect).
    pub fn reflect(self, span: Span, ty: &str) -> Result<Box<dyn Reflect>, Diagnostic<EvalError>> {
        match self {
            Number::Integer(integer) => integer.reflect(span, ty),
            Number::Float(number) => match number {
                Float::f32(number) => Ok(Box::new(number)),
                Float::f64(number) => Ok(Box::new(number)),
                Float::Unspecified(number) => match ty {
                    "f32" => Ok(Box::new(number as f32)),
                    "f64" => Ok(Box::new(number)),
                    ty => Err(span.diagnose(EvalError::IncompatibleReflectTypes {
                        expected: "float".to_owned(),
                        actual: ty.to_owned(),
                    })),
                },
            },
        }
    }
}

impl Display for UnsignedInteger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnsignedInteger::u8(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (u8)"),
            UnsignedInteger::u16(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (u16)"),
            UnsignedInteger::u32(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (u32)"),
            UnsignedInteger::u64(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (u64)"),
            UnsignedInteger::usize(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (usize)"),
        }
    }
}

impl Display for SignedInteger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignedInteger::i8(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (i8)"),
            SignedInteger::i16(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (i16)"),
            SignedInteger::i32(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (i32)"),
            SignedInteger::i64(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (i64)"),
            SignedInteger::isize(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (isize)"),
            SignedInteger::Unspecified(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (integer)"),
        }
    }
}

impl Display for Float {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Float::f32(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (f32)"),
            Float::f64(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (f64)"),
            Float::Unspecified(number) => write!(f, "{YELLOW}{number}{YELLOW:#} (float)"),
        }
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Integer::Unsigned(number) => Display::fmt(number, f),
            Integer::Signed(number) => Display::fmt(number, f),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Integer(number) => Display::fmt(number, f),
            Number::Float(number) => Display::fmt(number, f),
        }
    }
}

macro_rules! impl_kind_methods {
    ($kind:ident, $($variant:ident => ($str:expr, $natural:expr)),*$(,)?) => {
        impl $kind {
            /// Converts this kind into a [`&'static str`](str)
            /// You may want to use [`as_natural`](Self::as_natural)
            /// instead for more natural sounding error messages
            #[must_use]
            pub const fn as_str(&self) -> &'static str {
                match self {
                    $(Self::$variant => $str),*
                }
            }

            /// Returns the kind of value as a [string slice](str) with an `a` or `an` prepended to it.
            /// Used for more natural sounding error messages.
            #[must_use]
            pub const fn as_natural(&self) -> &'static str {
                match self {
                    $(Self::$variant => $natural),*
                }
            }
        }
        impl Display for $kind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if f.alternate() {
                    f.write_str(self.as_natural())
                } else {
                    f.write_str(self.as_str())
                }
            }
        }
    };
}

impl_kind_methods!(UnsignedIntegerKind,
    u8 => ("u8", "a u8"),
    u16 => ("u16", "a u16"),
    u32 => ("u32", "a u32"),
    u64 => ("u64", "a u64"),
    usize => ("usize", "a usize"),
);

impl_kind_methods!(SignedIntegerKind,
    i8 => ("i8", "a i8"),
    i16 => ("i16", "a i16"),
    i32 => ("i32", "a i32"),
    i64 => ("i64", "a i64"),
    isize => ("isize", "an isize"),
    Unspecified => ("integer", "an integer"),
);

impl_kind_methods!(FloatKind,
    f32 => ("f32", "a f32"),
    f64 => ("f64", "a f64"),
    Unspecified => ("float", "a float"),
);

impl IntegerKind {
    /// Converts this [`IntegerKind`] into a [`&'static str`](str)
    /// You may want to use [`as_natural`](Self::as_natural)
    /// instead for more natural sounding error messages
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Unsigned(kind) => kind.as_str(),
            Self::Signed(kind) => kind.as_str(),
        }
    }

    /// Returns the kind of [`Integer`] as a [string slice](str) with an `a` or `an` prepended to it.
    /// Used for more natural sounding error messages.
    #[must_use]
    pub const fn as_natural(&self) -> &'static str {
        match self {
            Self::Unsigned(kind) => kind.as_natural(),
            Self::Signed(kind) => kind.as_natural(),
        }
    }
}
impl Display for IntegerKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.write_str(self.as_natural())
        } else {
            f.write_str(self.as_str())
        }
    }
}

impl NumberKind {
    /// Converts this [`NumberKind`] into a [`&'static str`](str)
    /// You may want to use [`as_natural`](Self::as_natural)
    /// instead for more natural sounding error messages
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Integer(kind) => kind.as_str(),
            Self::Float(kind) => kind.as_str(),
        }
    }

    /// Returns the kind of [`Number`] as a [string slice](str) with an `a` or `an` prepended to it.
    /// Used for more natural sounding error messages.
    #[must_use]
    pub const fn as_natural(&self) -> &'static str {
        match self {
            Self::Integer(kind) => kind.as_natural(),
            Self::Float(kind) => kind.as_natural(),
        }
    }
}
impl Display for NumberKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.write_str(self.as_natural())
        } else {
            f.write_str(self.as_str())
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("cannot apply unary operator `-` to type `{0}`")]
pub struct CannotNegateUnsignedInteger(pub NumberKind);

#[derive(Debug, thiserror::Error)]
#[error("Incompatible number types; `{left}` and `{right}` are incompatible.")]
pub struct IncompatibleNumberTypes {
    pub left: NumberKind,
    pub right: NumberKind,
}

#[derive(Debug, thiserror::Error)]
#[error("cannot {operator} {left} by {right}")]
pub struct InvalidBinaryOperation {
    pub left: Number,
    pub right: Number,
    pub operator: BinaryOperator,
}

#[derive(Debug, thiserror::Error)]
#[error("integer value `{value}` out of range for type `{ty}`")]
pub struct ValueOutOfRange {
    pub value: i128,
    pub ty: NumberKind,
}

#[derive(thiserror::Error, Debug)]
pub enum NumberError {
    #[error(transparent)]
    InvalidBinaryOperation(#[from] InvalidBinaryOperation),
    #[error(transparent)]
    ValueOutOfRange(#[from] ValueOutOfRange),
    #[error(transparent)]
    IncompatibleNumberTypes(#[from] IncompatibleNumberTypes),
    #[error(transparent)]
    CannotNegateUnsignedInteger(#[from] CannotNegateUnsignedInteger),
    #[error(
        "cannot apply bitwise operator `{operator}` to type `{operand}`. only integers are supported."
    )]
    InvalidBitwiseOperation {
        operator: BinaryOperator,
        operand: NumberKind,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum IntegerOperationError {
    #[error(transparent)]
    IncompatibleTypes(#[from] IncompatibleNumberTypes),
    #[error(transparent)]
    InvalidBinaryOperation(#[from] InvalidBinaryOperation),
    #[error(transparent)]
    ValueOutOfRange(#[from] ValueOutOfRange),
}

impl From<IntegerOperationError> for NumberError {
    fn from(error: IntegerOperationError) -> Self {
        match error {
            IntegerOperationError::IncompatibleTypes(e) => Self::IncompatibleNumberTypes(e),
            IntegerOperationError::InvalidBinaryOperation(e) => Self::InvalidBinaryOperation(e),
            IntegerOperationError::ValueOutOfRange(e) => Self::ValueOutOfRange(e),
        }
    }
}

macro_rules! impl_unsigned_op {
    ($trait:ident, $method:ident, $checked:ident, $operator:ident) => {
        impl $trait for UnsignedInteger {
            type Output = Result<Self, IntegerOperationError>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::u8(l), Self::u8(r)) => l.$checked(r).map(Self::u8),
                    (Self::u16(l), Self::u16(r)) => l.$checked(r).map(Self::u16),
                    (Self::u32(l), Self::u32(r)) => l.$checked(r).map(Self::u32),
                    (Self::u64(l), Self::u64(r)) => l.$checked(r).map(Self::u64),
                    (Self::usize(l), Self::usize(r)) => l.$checked(r).map(Self::usize),
                    _ => {
                        return Err(IncompatibleNumberTypes {
                            left: self.kind().into(),
                            right: rhs.kind().into(),
                        }
                        .into());
                    }
                }
                .ok_or_else(|| {
                    InvalidBinaryOperation {
                        left: self.into(),
                        right: rhs.into(),
                        operator: BinaryOperator::$operator,
                    }
                    .into()
                })
            }
        }
    };
}

impl_unsigned_op!(Add, add, checked_add, Add);
impl_unsigned_op!(Sub, sub, checked_sub, Sub);
impl_unsigned_op!(Mul, mul, checked_mul, Mul);
impl_unsigned_op!(Div, div, checked_div, Div);
impl_unsigned_op!(Rem, rem, checked_rem, Mod);

macro_rules! impl_unsigned_bitwise_op {
    ($trait:ident, $method:ident) => {
        impl $trait for UnsignedInteger {
            type Output = Result<Self, IncompatibleNumberTypes>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::u8(l), Self::u8(r)) => Ok(Self::u8(l.$method(r))),
                    (Self::u16(l), Self::u16(r)) => Ok(Self::u16(l.$method(r))),
                    (Self::u32(l), Self::u32(r)) => Ok(Self::u32(l.$method(r))),
                    (Self::u64(l), Self::u64(r)) => Ok(Self::u64(l.$method(r))),
                    (Self::usize(l), Self::usize(r)) => Ok(Self::usize(l.$method(r))),
                    _ => Err(IncompatibleNumberTypes {
                        left: self.kind().into(),
                        right: rhs.kind().into(),
                    }),
                }
            }
        }
    };
}

impl_unsigned_bitwise_op!(BitAnd, bitand);
impl_unsigned_bitwise_op!(BitOr, bitor);
impl_unsigned_bitwise_op!(BitXor, bitxor);

impl Not for UnsignedInteger {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::u8(v) => Self::u8(!v),
            Self::u16(v) => Self::u16(!v),
            Self::u32(v) => Self::u32(!v),
            Self::u64(v) => Self::u64(!v),
            Self::usize(v) => Self::usize(!v),
        }
    }
}

macro_rules! impl_signed_op {
    ($trait:ident, $method:ident, $checked:ident, $operator:ident) => {
        impl $trait for SignedInteger {
            type Output = Result<Self, IntegerOperationError>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::i8(l), Self::i8(r)) => l.$checked(r).map(Self::i8),
                    (Self::i16(l), Self::i16(r)) => l.$checked(r).map(Self::i16),
                    (Self::i32(l), Self::i32(r)) => l.$checked(r).map(Self::i32),
                    (Self::i64(l), Self::i64(r)) => l.$checked(r).map(Self::i64),
                    (Self::isize(l), Self::isize(r)) => l.$checked(r).map(Self::isize),
                    (Self::Unspecified(l), Self::Unspecified(r)) => {
                        l.$checked(r).map(Self::Unspecified)
                    }
                    _ => {
                        return Err(IncompatibleNumberTypes {
                            left: self.kind().into(),
                            right: rhs.kind().into(),
                        }
                        .into());
                    }
                }
                .ok_or_else(|| {
                    InvalidBinaryOperation {
                        left: self.into(),
                        right: rhs.into(),
                        operator: BinaryOperator::$operator,
                    }
                    .into()
                })
            }
        }
    };
}

impl_signed_op!(Add, add, checked_add, Add);
impl_signed_op!(Sub, sub, checked_sub, Sub);
impl_signed_op!(Mul, mul, checked_mul, Mul);
impl_signed_op!(Div, div, checked_div, Div);
impl_signed_op!(Rem, rem, checked_rem, Mod);

macro_rules! impl_signed_bitwise_op {
    ($trait:ident, $method:ident) => {
        impl $trait for SignedInteger {
            type Output = Result<Self, IncompatibleNumberTypes>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::i8(l), Self::i8(r)) => Ok(Self::i8(l.$method(r))),
                    (Self::i16(l), Self::i16(r)) => Ok(Self::i16(l.$method(r))),
                    (Self::i32(l), Self::i32(r)) => Ok(Self::i32(l.$method(r))),
                    (Self::i64(l), Self::i64(r)) => Ok(Self::i64(l.$method(r))),
                    (Self::isize(l), Self::isize(r)) => Ok(Self::isize(l.$method(r))),
                    (Self::Unspecified(l), Self::Unspecified(r)) => {
                        Ok(Self::Unspecified(l.$method(r)))
                    }
                    _ => Err(IncompatibleNumberTypes {
                        left: self.kind().into(),
                        right: rhs.kind().into(),
                    }),
                }
            }
        }
    };
}

impl_signed_bitwise_op!(BitAnd, bitand);
impl_signed_bitwise_op!(BitOr, bitor);
impl_signed_bitwise_op!(BitXor, bitxor);

impl Not for SignedInteger {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::i8(v) => Self::i8(!v),
            Self::i16(v) => Self::i16(!v),
            Self::i32(v) => Self::i32(!v),
            Self::i64(v) => Self::i64(!v),
            Self::isize(v) => Self::isize(!v),
            Self::Unspecified(v) => Self::Unspecified(!v),
        }
    }
}

impl Neg for SignedInteger {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Self::i8(v) => Self::i8(-v),
            Self::i16(v) => Self::i16(-v),
            Self::i32(v) => Self::i32(-v),
            Self::i64(v) => Self::i64(-v),
            Self::isize(v) => Self::isize(-v),
            Self::Unspecified(v) => Self::Unspecified(-v),
        }
    }
}

macro_rules! impl_integer_op {
    ($trait:ident, $method:ident, $operator:ident) => {
        impl $trait for Integer {
            type Output = Result<Self, IntegerOperationError>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Signed(SignedInteger::Unspecified(l)), Self::Unsigned(r)) => {
                        let l_converted =
                            match r {
                                UnsignedInteger::u8(_) => u8::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u8(v))),
                                UnsignedInteger::u16(_) => u16::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u16(v))),
                                UnsignedInteger::u32(_) => u32::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u32(v))),
                                UnsignedInteger::u64(_) => u64::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u64(v))),
                                UnsignedInteger::usize(_) => usize::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::usize(v))),
                            }
                            .map_err(|_| ValueOutOfRange {
                                value: l,
                                ty: rhs.kind().into(),
                            })?;
                        l_converted.$method(rhs)
                    }
                    (Self::Unsigned(l), Self::Signed(SignedInteger::Unspecified(r))) => {
                        let r_converted =
                            match l {
                                UnsignedInteger::u8(_) => u8::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u8(v))),
                                UnsignedInteger::u16(_) => u16::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u16(v))),
                                UnsignedInteger::u32(_) => u32::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u32(v))),
                                UnsignedInteger::u64(_) => u64::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u64(v))),
                                UnsignedInteger::usize(_) => usize::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::usize(v))),
                            }
                            .map_err(|_| ValueOutOfRange {
                                value: r,
                                ty: l.kind().into(),
                            })?;
                        self.$method(r_converted)
                    }
                    (Self::Signed(SignedInteger::Unspecified(l)), Self::Signed(r)) => match r {
                        SignedInteger::Unspecified(r_val) => paste::paste! {
                            l.[<checked_ $method>](r_val)
                        }
                        .map(|v| Self::Signed(SignedInteger::Unspecified(v)))
                        .ok_or_else(|| {
                            InvalidBinaryOperation {
                                left: self.into(),
                                right: rhs.into(),
                                operator: BinaryOperator::$operator,
                            }
                            .into()
                        }),
                        _ => {
                            let l_converted =
                                match r {
                                    SignedInteger::i8(_) => i8::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::i8(v))),
                                    SignedInteger::i16(_) => i16::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::i16(v))),
                                    SignedInteger::i32(_) => i32::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::i32(v))),
                                    SignedInteger::i64(_) => i64::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::i64(v))),
                                    SignedInteger::isize(_) => isize::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::isize(v))),
                                    SignedInteger::Unspecified(_) => unreachable!(),
                                }
                                .map_err(|_| ValueOutOfRange {
                                    value: l,
                                    ty: r.kind().into(),
                                })?;
                            l_converted.$method(rhs)
                        }
                    },
                    (Self::Signed(l), Self::Signed(SignedInteger::Unspecified(r))) => match l {
                        SignedInteger::Unspecified(_) => unreachable!("Caught by arm above"),
                        _ => {
                            let r_converted =
                                match l {
                                    SignedInteger::i8(_) => i8::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::i8(v))),
                                    SignedInteger::i16(_) => i16::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::i16(v))),
                                    SignedInteger::i32(_) => i32::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::i32(v))),
                                    SignedInteger::i64(_) => i64::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::i64(v))),
                                    SignedInteger::isize(_) => isize::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::isize(v))),
                                    SignedInteger::Unspecified(_) => unreachable!(),
                                }
                                .map_err(|_| ValueOutOfRange {
                                    value: r,
                                    ty: l.kind().into(),
                                })?;
                            self.$method(r_converted)
                        }
                    },
                    (Self::Unsigned(l), Self::Unsigned(r)) => Ok(Self::Unsigned(l.$method(r)?)),
                    (Self::Signed(l), Self::Signed(r)) => Ok(Self::Signed(l.$method(r)?)),
                    _ => Err(IncompatibleNumberTypes {
                        left: self.kind().into(),
                        right: rhs.kind().into(),
                    }
                    .into()),
                }
            }
        }
    };
}

impl_integer_op!(Add, add, Add);
impl_integer_op!(Sub, sub, Sub);
impl_integer_op!(Mul, mul, Mul);
impl_integer_op!(Div, div, Div);
impl_integer_op!(Rem, rem, Mod);

macro_rules! impl_integer_bitwise_op {
    ($trait:ident, $method:ident) => {
        impl $trait for Integer {
            type Output = Result<Self, IntegerOperationError>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Signed(SignedInteger::Unspecified(l)), Self::Unsigned(r)) => {
                        let l_converted =
                            match r {
                                UnsignedInteger::u8(_) => u8::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u8(v))),
                                UnsignedInteger::u16(_) => u16::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u16(v))),
                                UnsignedInteger::u32(_) => u32::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u32(v))),
                                UnsignedInteger::u64(_) => u64::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u64(v))),
                                UnsignedInteger::usize(_) => usize::try_from(l)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::usize(v))),
                            }
                            .map_err(|_| ValueOutOfRange {
                                value: l,
                                ty: rhs.kind().into(),
                            })?;
                        l_converted.$method(rhs)
                    }
                    (Self::Unsigned(l), Self::Signed(SignedInteger::Unspecified(r))) => {
                        let r_converted =
                            match l {
                                UnsignedInteger::u8(_) => u8::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u8(v))),
                                UnsignedInteger::u16(_) => u16::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u16(v))),
                                UnsignedInteger::u32(_) => u32::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u32(v))),
                                UnsignedInteger::u64(_) => u64::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::u64(v))),
                                UnsignedInteger::usize(_) => usize::try_from(r)
                                    .map(|v| Integer::Unsigned(UnsignedInteger::usize(v))),
                            }
                            .map_err(|_| ValueOutOfRange {
                                value: r,
                                ty: l.kind().into(),
                            })?;
                        self.$method(r_converted)
                    }
                    (Self::Signed(SignedInteger::Unspecified(l)), Self::Signed(r)) => match r {
                        SignedInteger::Unspecified(r_val) => {
                            Ok(Self::Signed(SignedInteger::Unspecified(l.$method(r_val))))
                        }
                        _ => {
                            let l_converted =
                                match r {
                                    SignedInteger::i8(_) => i8::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::i8(v))),
                                    SignedInteger::i16(_) => i16::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::i16(v))),
                                    SignedInteger::i32(_) => i32::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::i32(v))),
                                    SignedInteger::i64(_) => i64::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::i64(v))),
                                    SignedInteger::isize(_) => isize::try_from(l)
                                        .map(|v| Integer::Signed(SignedInteger::isize(v))),
                                    SignedInteger::Unspecified(_) => unreachable!(),
                                }
                                .map_err(|_| ValueOutOfRange {
                                    value: l,
                                    ty: r.kind().into(),
                                })?;
                            l_converted.$method(rhs)
                        }
                    },
                    (Self::Signed(l), Self::Signed(SignedInteger::Unspecified(r))) => match l {
                        SignedInteger::Unspecified(_) => unreachable!("Caught by arm above"),
                        _ => {
                            let r_converted =
                                match l {
                                    SignedInteger::i8(_) => i8::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::i8(v))),
                                    SignedInteger::i16(_) => i16::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::i16(v))),
                                    SignedInteger::i32(_) => i32::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::i32(v))),
                                    SignedInteger::i64(_) => i64::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::i64(v))),
                                    SignedInteger::isize(_) => isize::try_from(r)
                                        .map(|v| Integer::Signed(SignedInteger::isize(v))),
                                    SignedInteger::Unspecified(_) => unreachable!(),
                                }
                                .map_err(|_| ValueOutOfRange {
                                    value: r,
                                    ty: l.kind().into(),
                                })?;
                            self.$method(r_converted)
                        }
                    },
                    (Self::Unsigned(l), Self::Unsigned(r)) => Ok(Self::Unsigned(l.$method(r)?)),
                    (Self::Signed(l), Self::Signed(r)) => Ok(Self::Signed(l.$method(r)?)),
                    _ => Err(IncompatibleNumberTypes {
                        left: self.kind().into(),
                        right: rhs.kind().into(),
                    }
                    .into()),
                }
            }
        }
    };
}

impl_integer_bitwise_op!(BitAnd, bitand);
impl_integer_bitwise_op!(BitOr, bitor);
impl_integer_bitwise_op!(BitXor, bitxor);

impl Not for Integer {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::Unsigned(v) => Self::Unsigned(!v),
            Self::Signed(v) => Self::Signed(!v),
        }
    }
}

impl Neg for Integer {
    type Output = Result<Self, CannotNegateUnsignedInteger>;
    fn neg(self) -> Self::Output {
        match self {
            Self::Unsigned(_) => Err(CannotNegateUnsignedInteger(self.kind().into())),
            Self::Signed(v) => Ok(Self::Signed(-v)),
        }
    }
}

macro_rules! impl_float_op {
    ($trait:ident, $method:ident) => {
        impl $trait for Float {
            type Output = Result<Self, IncompatibleNumberTypes>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::f32(l), Self::f32(r)) => Ok(Self::f32(l.$method(r))),
                    (Self::f64(l), Self::f64(r)) => Ok(Self::f64(l.$method(r))),
                    (Self::Unspecified(l), Self::Unspecified(r)) => {
                        Ok(Self::Unspecified(l.$method(r)))
                    }
                    (Self::Unspecified(l), Self::f32(r)) => Ok(Self::f32((l as f32).$method(r))),
                    (Self::Unspecified(l), Self::f64(r)) => Ok(Self::f64(l.$method(r))),
                    (Self::f32(l), Self::Unspecified(r)) => Ok(Self::f32(l.$method(r as f32))),
                    (Self::f64(l), Self::Unspecified(r)) => Ok(Self::f64(l.$method(r))),
                    _ => Err(IncompatibleNumberTypes {
                        left: self.kind().into(),
                        right: rhs.kind().into(),
                    }),
                }
            }
        }
    };
}

impl_float_op!(Add, add);
impl_float_op!(Sub, sub);
impl_float_op!(Mul, mul);
impl_float_op!(Div, div);
impl_float_op!(Rem, rem);

impl Neg for Float {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Self::f32(v) => Self::f32(-v),
            Self::f64(v) => Self::f64(-v),
            Self::Unspecified(v) => Self::Unspecified(-v),
        }
    }
}

macro_rules! impl_number_op {
    ($trait:ident, $method:ident, $operator:ident) => {
        impl $trait for Number {
            type Output = Result<Self, NumberError>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Integer(l), Self::Integer(r)) => Ok(Self::Integer(l.$method(r)?)),
                    (Self::Float(l), Self::Float(r)) => Ok(Self::Float(l.$method(r)?)),
                    _ => Err(IncompatibleNumberTypes {
                        left: self.kind(),
                        right: rhs.kind(),
                    }
                    .into()),
                }
            }
        }
    };
}

impl_number_op!(Add, add, Add);
impl_number_op!(Sub, sub, Sub);
impl_number_op!(Mul, mul, Mul);
impl_number_op!(Div, div, Div);
impl_number_op!(Rem, rem, Mod);

macro_rules! impl_number_bitwise_op {
    ($trait:ident, $method:ident, $operator:ident) => {
        impl $trait for Number {
            type Output = Result<Self, NumberError>;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Integer(l), Self::Integer(r)) => Ok(Self::Integer(l.$method(r)?)),
                    _ => Err(NumberError::InvalidBitwiseOperation {
                        operator: BinaryOperator::$operator,
                        operand: self.kind(),
                    }),
                }
            }
        }
    };
}

impl_number_bitwise_op!(BitAnd, bitand, And);
impl_number_bitwise_op!(BitOr, bitor, Or);
impl_number_bitwise_op!(BitXor, bitxor, Xor);

impl Not for Number {
    type Output = Result<Self, NumberError>;
    fn not(self) -> Self::Output {
        match self {
            Self::Integer(v) => Ok(Self::Integer(!v)),
            _ => Err(NumberError::InvalidBitwiseOperation {
                operator: crate::builtin_parser::parser::BinaryOperator::And, // Dummy
                operand: self.kind(),
            }),
        }
    }
}

impl Neg for Number {
    type Output = Result<Self, NumberError>;
    fn neg(self) -> Self::Output {
        match self {
            Self::Integer(v) => Ok(Self::Integer(
                v.neg().map_err(NumberError::CannotNegateUnsignedInteger)?,
            )),
            Self::Float(v) => Ok(Self::Float(-v)),
        }
    }
}

macro_rules! impl_op_spanned {
    ($trait:ident, $method:ident) => {
        impl $trait<Self> for Spanned<Number> {
            type Output = Result<Number, Diagnostic<EvalError>>;
            fn $method(self, rhs: Self) -> Self::Output {
                let span = self.span.join(&rhs.span);
                (self.value.$method(rhs.value)).diagnosed(span)
            }
        }
    };
}

impl_op_spanned!(Add, add);
impl_op_spanned!(Sub, sub);
impl_op_spanned!(Mul, mul);
impl_op_spanned!(Rem, rem);

impl_op_spanned!(BitAnd, bitand);
impl_op_spanned!(BitOr, bitor);
impl_op_spanned!(BitXor, bitxor);

macro_rules! from_primitive_integer {
    ($($primitive:ident => $group:ident, $enum:ident, $variant:ident),+) => {
        $(
            impl From<$primitive> for Number {
                fn from(value: $primitive) -> Self {
                    Number::Integer(Integer::$group($enum::$variant(value)))
                }
            }
            impl From<$primitive> for Integer {
                fn from(value: $primitive) -> Self {
                    Integer::$group($enum::$variant(value))
                }
            }
            impl From<$primitive> for $enum {
                fn from(value: $primitive) -> Self {
                    $enum::$variant(value)
                }
            }
        )+
    };
}

from_primitive_integer!(
    u8 => Unsigned, UnsignedInteger, u8,
    u16 => Unsigned, UnsignedInteger, u16,
    u32 => Unsigned, UnsignedInteger, u32,
    u64 => Unsigned, UnsignedInteger, u64,
    usize => Unsigned, UnsignedInteger, usize,
    i8 => Signed, SignedInteger, i8,
    i16 => Signed, SignedInteger, i16,
    i32 => Signed, SignedInteger, i32,
    i64 => Signed, SignedInteger, i64,
    isize => Signed, SignedInteger, isize
);

macro_rules! from_primitive_float {
    ($($primitive:ident => $enum:ident, $variant:ident),+) => {
        $(
            impl From<$primitive> for Number {
                fn from(value: $primitive) -> Self {
                    Number::Float($enum::$variant(value))
                }
            }
            impl From<$primitive> for Float {
                fn from(value: $primitive) -> Self {
                    Float::$variant(value)
                }
            }
        )+
    };
}

from_primitive_float!(
    f32 => Float, f32,
    f64 => Float, f64
);

impl From<Integer> for Number {
    fn from(value: Integer) -> Self {
        Number::Integer(value)
    }
}
impl From<UnsignedInteger> for Number {
    fn from(value: UnsignedInteger) -> Self {
        Number::Integer(Integer::Unsigned(value))
    }
}
impl From<SignedInteger> for Number {
    fn from(value: SignedInteger) -> Self {
        Number::Integer(Integer::Signed(value))
    }
}
impl From<UnsignedInteger> for Integer {
    fn from(value: UnsignedInteger) -> Self {
        Integer::Unsigned(value)
    }
}
impl From<SignedInteger> for Integer {
    fn from(value: SignedInteger) -> Self {
        Integer::Signed(value)
    }
}
impl From<Float> for Number {
    fn from(value: Float) -> Self {
        Number::Float(value)
    }
}

impl From<IntegerKind> for NumberKind {
    fn from(kind: IntegerKind) -> Self {
        NumberKind::Integer(kind)
    }
}
impl From<UnsignedIntegerKind> for IntegerKind {
    fn from(kind: UnsignedIntegerKind) -> Self {
        IntegerKind::Unsigned(kind)
    }
}
impl From<SignedIntegerKind> for IntegerKind {
    fn from(kind: SignedIntegerKind) -> Self {
        IntegerKind::Signed(kind)
    }
}
impl From<UnsignedIntegerKind> for NumberKind {
    fn from(kind: UnsignedIntegerKind) -> Self {
        NumberKind::Integer(IntegerKind::Unsigned(kind))
    }
}
impl From<SignedIntegerKind> for NumberKind {
    fn from(kind: SignedIntegerKind) -> Self {
        NumberKind::Integer(IntegerKind::Signed(kind))
    }
}
impl From<FloatKind> for NumberKind {
    fn from(kind: FloatKind) -> Self {
        NumberKind::Float(kind)
    }
}

impl From<Diagnostic<NumberError>> for Diagnostic<EvalError> {
    fn from(diagnostic: Diagnostic<NumberError>) -> Self {
        Diagnostic {
            spans: diagnostic.spans,
            error: Box::new(EvalError::Number(*diagnostic.error)),
        }
    }
}

impl From<CannotNegateUnsignedInteger> for EvalError {
    fn from(error: CannotNegateUnsignedInteger) -> Self {
        EvalError::Number(NumberError::CannotNegateUnsignedInteger(error))
    }
}
