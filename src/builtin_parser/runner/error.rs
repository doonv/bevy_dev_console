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
    CannotIndexValue(Span),
    FieldNotFoundInStruct(Span),
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
        field_index: usize,
        variant_name: String,
        span: std::ops::Range<usize>,
    },
}

impl RunError {
    /// Get all the locations of the error in the source.
    pub fn spans(&self) -> Vec<Span> {
        use RunError::*;

        match self {
            Custom { span, .. } => vec![span.clone()],
            VariableNotFound(Spanned { span, .. }) => vec![span.clone()],
            ExpectedNumberAfterUnaryOperator(Spanned { span, .. }) => vec![span.clone()],
            CannotIndexValue(span) => vec![span.clone()],
            FieldNotFoundInStruct(span) => vec![span.clone()],
            CannotDereferenceValue(Spanned { span, .. }) => vec![span.clone()],
            ReferenceToMovedData(span) => vec![span.clone()],
            VariableMoved(Spanned { span, .. }) => vec![span.clone()],
            CannotBorrowValue(Spanned { span, .. }) => vec![span.clone()],
            IncompatibleReflectTypes { span, .. } => vec![span.clone()],
            EnumVariantNotFound(Spanned { span, .. }) => vec![span.clone()],
            EnumVariantStructFieldNotFound { span, .. } => vec![span.clone()],
            EnumVariantTupleFieldNotFound { span, .. } => vec![span.clone()],
            CannotMoveOutOfResource(Spanned { span, .. }) => vec![span.clone()],
            CannotNegateUnsignedInteger(Spanned { span, .. }) => vec![span.clone()],
            IncompatibleNumberTypes { span, .. } => vec![span.clone()],
            IncompatibleFunctionParameter { span, .. } => vec![span.clone()],
            ExpectedVariableGotFunction(Spanned { span, .. }) => vec![span.clone()],
            CannotReflectReference(span) => vec![span.clone()],
            CannotReflectResource(span) => vec![span.clone()],
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
                value.natural_kind()
            )
            .into(),
            CannotIndexValue(_) => todo!(),
            FieldNotFoundInStruct(_) => todo!(),
            ReferenceToMovedData(_) => todo!(),
            VariableMoved(Spanned { value, .. }) => format!("Variable `{value}` was moved.").into(),
            CannotDereferenceValue(Spanned { value: kind, .. }) => {
                format!("Cannot dereference {kind}.").into()
            }
            CannotBorrowValue(Spanned { value: kind, .. }) => {
                format!("Cannot borrow {kind}.").into()
            }
            IncompatibleReflectTypes {
                expected, actual, ..
            } => format!(
                "Cannot set incompatible reflect types. Expected `{expected}`, got `{actual}`"
            )
            .into(),
            EnumVariantNotFound(Spanned { value: name, .. }) => {
                format!("Enum variant `{name}` was not found.").into()
            }
            EnumVariantStructFieldNotFound {
                field_name,
                variant_name,
                ..
            } => format!("Field `{field_name}` doesn't exist on struct variant `{variant_name}`.")
                .into(),
            EnumVariantTupleFieldNotFound {
                field_index,
                variant_name,
                ..
            } => format!("Field `{field_index}` doesn't exist on tuple variant `{variant_name}`.")
                .into(),
            CannotMoveOutOfResource(Spanned { value, .. }) => {
                format!("Cannot move out of resource `{value}`, try borrowing it instead.").into()
            }
            CannotNegateUnsignedInteger(Spanned { value, .. }) => format!(
                "Unsigned integers cannot be negated. (Type: {})",
                value.natural_kind()
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
            CannotReflectReference(_) => {
                "Cannot reflect a reference. Try dereferencing it instead.".into()
            }
            CannotReflectResource(_) => {
                "Cannot reflecting resources is not possible at the moment.".into()
            }
        }
    }
}
