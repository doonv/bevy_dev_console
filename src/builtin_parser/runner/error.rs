use std::borrow::Cow;

use bevy::reflect::ApplyError;
use kinded::Kinded;
use logos::Span;

use crate::builtin_parser::Spanned;
use crate::builtin_parser::number::{Number, NumberKind};
use crate::builtin_parser::parser::{Access, AccessKind, ExpressionKind};
use crate::builtin_parser::runner::value::ValueKind;

use super::Value;

/// An error occurring during the while evaluating the command.
///
/// TODO: This enormous enum should probably be split into smaller error types like `NumberError`, `EnvironmentError`, etc.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum EvalError {
    /// A custom text message. Contains very little contextual information, try to find an existing error instead.
    Custom {
        /// The text of the message
        text: Cow<'static, str>,
        span: Span,
    },
    InvalidOperation {
        left: Number,
        right: Number,
        operation: &'static str,
        span: Span,
    },
    VariableNotFound(Spanned<String>),
    ExpectedNumberAfterUnaryOperator(Spanned<Value>),
    CannotIndexValue(Spanned<Value>),
    ReferenceToMovedData(Span),
    VariableMoved(Spanned<String>),
    CannotDereferenceValue(Spanned<ValueKind>),
    CannotDereferenceValueExpr(Spanned<ExpressionKind>),
    CannotBorrowValue(Spanned<ExpressionKind>),
    IncompatibleReflectTypes {
        expected: String,
        actual: String,
        span: Span,
    },
    EnumVariantNotFound(Spanned<String>),
    CannotMoveOutOfResource(Spanned<String>),
    CannotNegateUnsignedInteger(Spanned<NumberKind>),
    IncompatibleNumberTypes {
        left: NumberKind,
        right: NumberKind,
        span: Span,
    },
    IncorrectFunctionParameterType {
        expected: ValueKind,
        actual: ValueKind,
        span: Span,
    },
    EnumVariantStructFieldNotFound {
        field_name: String,
        variant_name: String,
        span: Span,
    },
    ExpectedVariableGotFunction(Spanned<String>),
    CannotReflectReference(Span),
    CannotReflectResource(Span),
    EnumVariantTupleFieldNotFound {
        span: Span,
        field_index: usize,
        variant_name: String,
    },
    IncorrectAccessOperation {
        span: Span,
        expected_access: &'static [AccessKind],
        expected_type: &'static str,
        got: Access,
    },
    FieldNotFoundInStruct(Spanned<String>),
    FieldNotFoundInTuple {
        span: Span,
        field_index: usize,
        tuple_size: usize,
    },
    ApplyError {
        apply_error: ApplyError,
        span: Span,
    },
    ValueOutOfRange {
        span: Span,
        value: i128,
        ty: NumberKind,
    },
}

impl EvalError {
    /// Get all the locations of the error in the source.
    #[must_use]
    pub fn spans(&self) -> Vec<Span> {
        use EvalError as E;

        match self {
            E::Custom { span, .. }
            | E::VariableNotFound(Spanned { span, .. })
            | E::ExpectedNumberAfterUnaryOperator(Spanned { span, .. })
            | E::CannotIndexValue(Spanned { span, .. })
            | E::FieldNotFoundInStruct(Spanned { span, value: _ })
            | E::CannotDereferenceValue(Spanned { span, .. })
            | E::CannotDereferenceValueExpr(Spanned { span, .. })
            | E::ReferenceToMovedData(span)
            | E::VariableMoved(Spanned { span, .. })
            | E::CannotBorrowValue(Spanned { span, .. })
            | E::IncompatibleReflectTypes { span, .. }
            | E::EnumVariantNotFound(Spanned { span, .. })
            | E::EnumVariantStructFieldNotFound { span, .. }
            | E::EnumVariantTupleFieldNotFound { span, .. }
            | E::CannotMoveOutOfResource(Spanned { span, .. })
            | E::CannotNegateUnsignedInteger(Spanned { span, .. })
            | E::IncompatibleNumberTypes { span, .. }
            | E::IncorrectFunctionParameterType { span, .. }
            | E::ExpectedVariableGotFunction(Spanned { span, .. })
            | E::CannotReflectReference(span)
            | E::CannotReflectResource(span)
            | E::InvalidOperation { span, .. }
            | E::IncorrectAccessOperation { span, .. }
            | E::FieldNotFoundInTuple { span, .. }
            | E::ApplyError { span, .. }
            | E::ValueOutOfRange { span, .. } => vec![span.clone()],
        }
    }
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use EvalError as E;

        match self {
            E::Custom { text, .. } => f.write_str(text),
            E::VariableNotFound(Spanned { value, .. }) => {
                write!(f, "Variable `{value}` not found.")
            }
            E::ExpectedNumberAfterUnaryOperator(Spanned { value, .. }) => write!(
                f,
                "Expected a number after unary operator (-) but got {} instead.",
                value.kind()
            ),
            E::CannotIndexValue(Spanned { span: _, value }) => {
                write!(f, "Cannot index {} with a member expression.", value.kind())
            }
            E::ReferenceToMovedData(_) => write!(f, "Cannot access reference to moved data."),
            E::VariableMoved(Spanned { value, .. }) => {
                write!(f, "Variable `{value}` was moved.")
            }
            E::CannotDereferenceValue(Spanned { value: kind, .. }) => {
                write!(f, "Cannot dereference {kind}.")
            }
            E::CannotDereferenceValueExpr(Spanned { value: kind, .. }) => {
                write!(f, "Cannot dereference {kind}.")
            }
            E::CannotBorrowValue(Spanned { value: kind, .. }) => {
                write!(f, "Cannot borrow {kind}. Only variables can be borrowed.")
            }
            E::IncompatibleReflectTypes {
                expected, actual, ..
            } => write!(
                f,
                "Cannot set incompatible reflect types. Expected `{expected}`, got `{actual}`"
            ),
            E::EnumVariantNotFound(Spanned { value: name, .. }) => {
                write!(f, "Enum variant `{name}` was not found.")
            }
            E::EnumVariantStructFieldNotFound {
                field_name,
                variant_name,
                ..
            } => write!(
                f,
                "Field `{field_name}` doesn't exist on struct variant `{variant_name}`."
            ),
            E::EnumVariantTupleFieldNotFound {
                field_index,
                variant_name,
                ..
            } => write!(
                f,
                "Field `{field_index}` doesn't exist on tuple variant `{variant_name}`."
            ),
            E::CannotMoveOutOfResource(Spanned { value, .. }) => write!(
                f,
                "cannot move out of resource `{value}`, try borrowing it instead."
            ),
            E::CannotNegateUnsignedInteger(Spanned { value, .. }) => {
                write!(f, "cannot apply unary operator `-` to type `{value}`",)
            }
            E::IncompatibleNumberTypes { left, right, .. } => write!(
                f,
                "Incompatible number types; `{left}` and `{right}` are incompatible."
            ),
            E::IncorrectFunctionParameterType {
                expected, actual, ..
            } => write!(
                f,
                "Mismatched function parameter type. Expected {expected:#} but got {actual:#}"
            ),
            E::ExpectedVariableGotFunction(Spanned { value, .. }) => write!(
                f,
                "Expected `{value}` to be a variable, but got a function instead."
            ),
            E::CannotReflectReference(_) => {
                write!(
                    f,
                    "Cannot reflect a reference. Try dereferencing it instead."
                )
            }
            E::CannotReflectResource(_) => {
                write!(
                    f,
                    "Cannot reflecting resources is not possible at the moment."
                )
            }
            E::InvalidOperation {
                left,
                right,
                operation,
                span: _,
            } => write!(f, "cannot {operation} {left} by {right}"),
            E::IncorrectAccessOperation {
                expected_access,
                expected_type,
                got,
                span: _,
            } => {
                let expected_access = expected_access
                    .iter()
                    .map(|kind| kind.as_natural())
                    .collect::<Vec<_>>()
                    .join(" and ");
                write!(
                    f,
                    "Expected {expected_access} access to access {expected_type} but got {:#}",
                    got.kind()
                )
            }
            E::FieldNotFoundInStruct(Spanned { span: _, value }) => {
                write!(f, "Field {value} not found in struct")
            }
            E::FieldNotFoundInTuple {
                field_index,
                tuple_size,
                span: _,
            } => write!(
                f,
                "Field {field_index} is out of bounds for tuple of size {tuple_size}"
            ),
            E::ApplyError {
                apply_error,
                span: _,
            } => {
                write!(
                    f,
                    "Error while applying value (todo make this error better): {apply_error}"
                )
            }
            E::ValueOutOfRange { span: _, value, ty } => {
                write!(f, "integer value `{value}` out of range for type `{ty}`")
            }
        }
    }
}

impl std::error::Error for EvalError {}
