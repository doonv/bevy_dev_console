use std::borrow::Cow;
use std::fmt;

use crate::builtin_parser::CannotNegateUnsignedInteger;
use crate::builtin_parser::number::{Number, NumberKind};
use crate::builtin_parser::parser::{AccessKind, BinaryOperator, ExpressionKind, UnaryOperator};
use crate::builtin_parser::runner::value::ValueKind;
use bevy::reflect::ApplyError;

/// An error occurring during the while evaluating the command.
///
/// TODO: This enormous enum should probably be split into smaller error types like `NumberError`, `EnvironmentError`, etc.
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs)]
pub enum EvalError {
    /// A custom text message. Contains very little contextual information, try to find an existing error instead.
    #[error("{0}")]
    Custom(Cow<'static, str>),

    #[error("cannot {operator} {left} by {right}")]
    InvalidBinaryOperation {
        left: Number,
        right: Number,
        operator: BinaryOperator,
    },

    #[error("Variable `{0}` not found.")]
    VariableNotFound(String),

    #[error(
        "cannot apply unary operator `{operator}` to type `{operand}`. the supported types are: {}",
        FancyJoin(accepted)
    )]
    InvalidUnaryOperation {
        operator: UnaryOperator,
        operand: ValueKind,
        accepted: &'static [ValueKind],
    },

    #[error("Cannot index `{0}` with a member expression.")]
    CannotIndexValue(ValueKind),

    #[error("Cannot access reference to moved data.")]
    ReferenceToMovedData,

    #[error("variable `{0}` was moved")]
    VariableMoved(String),

    #[error("Cannot dereference {0}.")]
    CannotDereferenceValue(ValueKind),

    #[error("Cannot dereference {0}.")]
    CannotDereferenceValueExpr(ExpressionKind),

    #[error("Cannot borrow {0}. Only variables can be borrowed.")]
    CannotBorrowValue(ExpressionKind),

    #[error("Cannot set incompatible reflect types. Expected `{expected}`, got `{actual}`")]
    IncompatibleReflectTypes { expected: String, actual: String },

    #[error("Enum variant `{0}` was not found.")]
    EnumVariantNotFound(String),

    #[error("cannot move out of resource `{0}`, try borrowing it instead.")]
    CannotMoveOutOfResource(String),

    #[error(transparent)]
    CannotNegateUnsignedInteger(#[from] CannotNegateUnsignedInteger),

    #[error("Incompatible number types; `{left}` and `{right}` are incompatible.")]
    IncompatibleNumberTypes { left: NumberKind, right: NumberKind },

    #[error("Mismatched function parameter type. Expected {expected:#} but got {actual:#}")]
    IncorrectFunctionParameterType {
        expected: ValueKind,
        actual: ValueKind,
    },

    #[error("Field `{field_name}` doesn't exist on struct variant `{variant_name}`.")]
    EnumVariantStructFieldNotFound {
        field_name: String,
        variant_name: String,
    },

    #[error("Expected `{0}` to be a variable, but got a function instead.")]
    ExpectedVariableGotFunction(String),

    #[error("Cannot reflect a reference. Try dereferencing it instead.")]
    CannotReflectReference,

    #[error("Cannot reflecting resources is not possible at the moment.")]
    CannotReflectResource,

    #[error("Field `{field_index}` doesn't exist on tuple variant `{variant_name}`.")]
    EnumVariantTupleFieldNotFound {
        field_index: usize,
        variant_name: String,
    },

    #[error(
        "Expected {got} access to access {expected_type} but got {:#}",
        format_expected_access(expected_access)
    )]
    IncorrectAccessOperation {
        expected_access: &'static [AccessKind],
        expected_type: &'static str,
        got: AccessKind,
    },

    #[error("Field {0} not found in struct")]
    FieldNotFoundInStruct(String),

    #[error("Field {field_index} is out of bounds for tuple of size {tuple_size}")]
    FieldNotFoundInTuple {
        field_index: usize,
        tuple_size: usize,
    },

    #[error("Error while applying value (todo make this error better): {0}")]
    ApplyError(ApplyError),

    #[error("integer value `{value}` out of range for type `{ty}`")]
    ValueOutOfRange { value: i128, ty: NumberKind },
}

fn format_expected_access(expected_access: &[AccessKind]) -> String {
    expected_access
        .iter()
        .map(|kind| kind.as_natural())
        .collect::<Vec<_>>()
        .join(" and ")
}

struct FancyJoin<'a, T: fmt::Display>(&'a [T]);

impl<'a, T: fmt::Display> fmt::Display for FancyJoin<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            if i == self.0.len() - 1 {
                f.write_str("and ")?;
            }
            if f.alternate() {
                write!(f, "{:#}", item)?;
            } else {
                write!(f, "{}", item)?;
            }
        }
        Ok(())
    }
}

impl From<&'static str> for EvalError {
    fn from(value: &'static str) -> Self {
        Self::Custom(value.into())
    }
}
