use std::borrow::Cow;

use logos::Span;

use crate::builtin_parser::number::Number;
use crate::builtin_parser::parser::Access;
use crate::builtin_parser::Spanned;
use crate::command::{CommandHint, CommandHintColor};

use super::Value;

/// An error occurring during the while executing the [`AST`](Ast) of the command.
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
    CannotDereferenceValue(Spanned<&'static str>),
    CannotBorrowValue(Spanned<&'static str>),
    IncompatibleReflectTypes {
        expected: String,
        actual: String,
        span: Span,
    },
    EnumVariantNotFound(Spanned<String>),
    CannotMoveOutOfResource(Spanned<String>),
    CannotNegateUnsignedInteger(Spanned<Number>),
    IncompatibleNumberTypes {
        left: &'static str,
        right: &'static str,
        span: Span,
    },
    IncompatibleFunctionParameter {
        expected: &'static str,
        actual: &'static str,
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
        expected_access: &'static [&'static str],
        expected_type: &'static str,
        got: Access,
    },
    FieldNotFoundInStruct(Spanned<String>),
    FieldNotFoundInTuple {
        span: Span,
        field_index: usize,
        tuple_size: usize,
    },
}

impl EvalError {
    /// Get all the locations of the error in the source.
    pub fn spans(&self) -> Vec<Span> {
        use EvalError as E;

        match self {
            E::Custom { span, .. } => vec![span.clone()],
            E::VariableNotFound(Spanned { span, .. }) => vec![span.clone()],
            E::ExpectedNumberAfterUnaryOperator(Spanned { span, .. }) => vec![span.clone()],
            E::CannotIndexValue(Spanned { span, .. }) => vec![span.clone()],
            E::FieldNotFoundInStruct(Spanned { span, value: _ }) => vec![span.clone()],
            E::CannotDereferenceValue(Spanned { span, .. }) => vec![span.clone()],
            E::ReferenceToMovedData(span) => vec![span.clone()],
            E::VariableMoved(Spanned { span, .. }) => vec![span.clone()],
            E::CannotBorrowValue(Spanned { span, .. }) => vec![span.clone()],
            E::IncompatibleReflectTypes { span, .. } => vec![span.clone()],
            E::EnumVariantNotFound(Spanned { span, .. }) => vec![span.clone()],
            E::EnumVariantStructFieldNotFound { span, .. } => vec![span.clone()],
            E::EnumVariantTupleFieldNotFound { span, .. } => vec![span.clone()],
            E::CannotMoveOutOfResource(Spanned { span, .. }) => vec![span.clone()],
            E::CannotNegateUnsignedInteger(Spanned { span, .. }) => vec![span.clone()],
            E::IncompatibleNumberTypes { span, .. } => vec![span.clone()],
            E::IncompatibleFunctionParameter { span, .. } => vec![span.clone()],
            E::ExpectedVariableGotFunction(Spanned { span, .. }) => vec![span.clone()],
            E::CannotReflectReference(span) => vec![span.clone()],
            E::CannotReflectResource(span) => vec![span.clone()],
            E::InvalidOperation { span, .. } => vec![span.clone()],
            E::IncorrectAccessOperation { span, .. } => vec![span.clone()],
            E::FieldNotFoundInTuple { span, .. } => vec![span.clone()],
        }
    }
    /// Returns all the hints for this error.
    pub fn hints(&self) -> Vec<CommandHint> {
        self.spans()
            .into_iter()
            .map(|span| CommandHint::new(span, CommandHintColor::Error, self.to_string()))
            .collect()
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
                value.natural_kind()
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
                "Cannot move out of resource `{value}`, try borrowing it instead."
            ),
            E::CannotNegateUnsignedInteger(Spanned { value, .. }) => write!(
                f,
                "Unsigned integers cannot be negated. (Type: {})",
                value.natural_kind()
            ),
            E::IncompatibleNumberTypes { left, right, .. } => write!(
                f,
                "Incompatible number types; `{left}` and `{right}` are incompatible."
            ),
            E::IncompatibleFunctionParameter {
                expected, actual, ..
            } => write!(
                f,
                "Mismatched function parameter type. Expected {expected} but got {actual}"
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
            } => write!(f, "Invalid operation: Cannot {operation} {left} by {right}"),
            E::IncorrectAccessOperation {
                expected_access,
                expected_type,
                got,
                span: _,
            } => write!(
                f,
                "Expected {} access to access {expected_type} but got {}",
                expected_access.join(" and "),
                got.natural_kind()
            ),
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
        }
    }
}

impl std::error::Error for EvalError {}
