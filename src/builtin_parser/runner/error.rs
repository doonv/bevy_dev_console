use std::borrow::Cow;

use logos::Span;

use crate::builtin_parser::number::Number;
use crate::builtin_parser::Spanned;
use crate::command::{CommandHint, CommandHintColor};

use super::Value;

/// An error occuring during the while executing the [`AST`](Ast) of the command.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum RunError {
    /// A custom text message. Contains very little contextual information, try to find an existing error instead.
    Custom {
        /// The text of the message
        text: Cow<'static, str>,
        span: Span,
    },
    VariableNotFound(Spanned<String>),
    ExpectedNumberAfterUnaryOperator(Spanned<Value>),
    InvalidVariantForResource(String, String),
    CannotIndexValue(Span),
    FieldNotFoundInStruct(Span),
    CouldntDereferenceValue(Span),
    ReferenceToMovedData(Span),
    VariableMoved(Spanned<String>),
    CannotBorrowValue(Span),
    IncompatibleReflectTypes {
        expected: String,
        actual: String,
        span: Span,
    },
    EnumVariantNotFound {
        name: String,
        span: Span,
    },
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
}

impl RunError {
    /// Get all the locations of the error in the source.
    pub fn spans(&self) -> Vec<Span> {
        use RunError::*;

        match self {
            Custom { span, .. } => vec![span.clone()],
            VariableNotFound(Spanned { span, .. }) => vec![span.clone()],
            ExpectedNumberAfterUnaryOperator(Spanned { span, .. }) => vec![span.clone()],
            InvalidVariantForResource(_, _) => todo!(),
            CannotIndexValue(span) => vec![span.clone()],
            FieldNotFoundInStruct(span) => vec![span.clone()],
            CouldntDereferenceValue(span) => vec![span.clone()],
            ReferenceToMovedData(span) => vec![span.clone()],
            VariableMoved(Spanned { span, .. }) => vec![span.clone()],
            CannotBorrowValue(span) => vec![span.clone()],
            IncompatibleReflectTypes { span, .. } => vec![span.clone()],
            EnumVariantNotFound { span, .. } => vec![span.clone()],
            EnumVariantStructFieldNotFound { span, .. } => vec![span.clone()],
            CannotMoveOutOfResource(Spanned { span, .. }) => vec![span.clone()],
            CannotNegateUnsignedInteger(Spanned { span, .. }) => vec![span.clone()],
            IncompatibleNumberTypes { span, .. } => vec![span.clone()],
            IncompatibleFunctionParameter { span, .. } => vec![span.clone()],
            ExpectedVariableGotFunction(Spanned { span, .. }) => vec![span.clone()],
        }
    }
    /// Returns all the hints for this error.
    pub fn hints(&self) -> Vec<CommandHint> {
        self.spans()
            .into_iter()
            .map(|span| CommandHint::new(span, CommandHintColor::Error, self.message()))
            .collect()
    }
    /// A summary message explaining this error.
    pub fn message(&self) -> Cow<'static, str> {
        use RunError::*;

        match self {
            Custom { text, .. } => text.clone(),
            VariableNotFound(Spanned { value, .. }) => {
                format!("Variable `{value}` not found.").into()
            }
            ExpectedNumberAfterUnaryOperator(Spanned { value, .. }) => format!(
                "Expected a number after unary operator (-) but got {} instead.",
                value.kind()
            )
            .into(),
            InvalidVariantForResource(_, _) => todo!(),
            CannotIndexValue(_) => todo!(),
            FieldNotFoundInStruct(_) => todo!(),
            CouldntDereferenceValue(_) => "Can't dereference this type.".into(),
            ReferenceToMovedData(_) => todo!(),
            VariableMoved(Spanned { value, .. }) => format!("Variable `{value}` was moved.").into(),
            CannotBorrowValue(_) => todo!(),
            IncompatibleReflectTypes {
                expected, actual, ..
            } => format!(
                "Cannot set incompatible reflect types. Expected `{expected}`, got `{actual}`"
            )
            .into(),
            EnumVariantNotFound { name, span } => todo!(),
            EnumVariantStructFieldNotFound {
                field_name,
                variant_name,
                ..
            } => format!("Field `{field_name}` doesn't exist on struct variant `{variant_name}`.")
                .into(),
            CannotMoveOutOfResource(Spanned { value, .. }) => {
                format!("Cannot move out of resource `{value}`, try borrowing it instead.").into()
            }
            CannotNegateUnsignedInteger(Spanned { value, .. }) => format!(
                "Unsigned integers cannot be negated. (Type: {})",
                value.kind()
            )
            .into(),
            IncompatibleNumberTypes { left, right, .. } => {
                format!("Incompatible number types; `{left}` and `{right}` are incompatible.")
                    .into()
            }
            IncompatibleFunctionParameter {
                expected, actual, ..
            } => {
                format!("Mismatched function paramater type. Expected {expected} but got {actual}")
                    .into()
            }
            ExpectedVariableGotFunction(Spanned { value, .. }) => {
                format!("Expected `{value}` to be a variable, but got a function instead.").into()
            }
        }
    }
}
