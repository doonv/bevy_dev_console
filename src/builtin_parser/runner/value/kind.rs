//! Value kinds and related traits

use kinded::{Kind, Kinded};

use crate::builtin_parser::number::NumberKind;

use super::Value;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueKind {
    None,
    Number(NumberKind),
    AnyNumber,
    AnyInteger,
    AnyFloat,
    Boolean,
    String,
    Reference,
    Object,
    StructObject,
    Tuple,
    StructTuple,
    Resource,
}

impl Kinded for Value {
    type Kind = ValueKind;

    fn kind(&self) -> Self::Kind {
        match self {
            Self::None => ValueKind::None,
            Self::Number(number) => ValueKind::Number(number.kind()),
            Self::Boolean(..) => ValueKind::Boolean,
            Self::String(..) => ValueKind::String,
            Self::Reference(..) => ValueKind::Reference,
            Self::Object(..) => ValueKind::Object,
            Self::StructObject { .. } => ValueKind::StructObject,
            Self::Tuple(..) => ValueKind::Tuple,
            Self::StructTuple { .. } => ValueKind::StructTuple,
            Self::Resource(..) => ValueKind::Resource,
        }
    }
}

impl Kind for ValueKind {
    fn all() -> &'static [Self] {
        unimplemented!()
    }
}

impl ValueKind {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Number(number) => number.as_str(),
            Self::AnyNumber => "float/integer",
            Self::AnyInteger => "integer",
            Self::AnyFloat => "float",
            Self::Boolean => "boolean",
            Self::String => "string",
            Self::Reference => "reference",
            Self::Object => "object",
            Self::StructObject => "struct object",
            Self::Tuple => "tuple",
            Self::StructTuple => "struct tuple",
            Self::Resource => "resource",
        }
    }

    /// Returns the kind of [`Value`] as a [string slice](str) with an `a` or `an`  prepended to it.
    /// Used for more natural sounding error messages.
    pub const fn as_natural(self) -> &'static str {
        match self {
            Self::None => "nothing",
            Self::Number(number) => number.as_natural(),
            Self::AnyNumber => "any number",
            Self::AnyInteger => "any integer",
            Self::AnyFloat => "any float",
            Self::Boolean => "a boolean",
            Self::String => "a string",
            Self::Reference => "a reference",
            Self::Object => "a object",
            Self::StructObject => "a struct object",
            Self::Tuple => "a tuple",
            Self::StructTuple => "a struct tuple",
            Self::Resource => "a resource",
        }
    }
}

impl std::fmt::Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.write_str(self.as_natural())
        } else {
            f.write_str(self.as_str())
        }
    }
}
