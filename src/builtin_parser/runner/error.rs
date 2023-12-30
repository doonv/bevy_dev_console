use std::borrow::Cow;

use logos::Span;

use crate::{
    builtin_parser::{number::Number, Spanned},
    command::{CommandHint, CommandHintColor},
};

use super::Value;

/// An error occuring during the while executing the [`AST`](Ast) of the command.
#[derive(Debug)]
pub enum RunError {
    /// A custom text message. Contains very little contextual information, try to find an existing error instead.
    Custom {
        /// The text of the message
        text: String,
        span: Span,
    },
    VariableNotFound(Span),
    ExpectedNumberAfterUnaryOperator(Spanned<Value>),
    InvalidVariantForResource(String, String),
    CannotIndexValue(Span),
    FieldNotFoundInStruct(Span),
    CouldntDereferenceValue(Span),
    ReferenceToMovedData(Span),
    VariableMoved(Span),
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
        span: Span
    },
}

impl RunError {
    pub fn spans(&self) -> Vec<Span> {
        use RunError::*;

        match self {
            Custom { span, .. } => vec![span.clone()],
            VariableNotFound(span) => vec![span.clone()],
            ExpectedNumberAfterUnaryOperator(Spanned { span, .. }) => vec![span.clone()],
            InvalidVariantForResource(_, _) => todo!(),
            CannotIndexValue(span) => vec![span.clone()],
            FieldNotFoundInStruct(span) => vec![span.clone()],
            CouldntDereferenceValue(span) => vec![span.clone()],
            ReferenceToMovedData(span) => vec![span.clone()],
            VariableMoved(span) => vec![span.clone()],
            CannotBorrowValue(span) => vec![span.clone()],
            IncompatibleReflectTypes { span, .. } => vec![span.clone()],
            EnumVariantNotFound { span, .. } => vec![span.clone()],
            CannotMoveOutOfResource(Spanned { span, .. }) => vec![span.clone()],
            CannotNegateUnsignedInteger(Spanned { span, .. }) => vec![span.clone()],
            IncompatibleNumberTypes { span, .. } => vec![span.clone()],
        }
    }
    pub fn hints(&self) -> Vec<CommandHint> {
        self.spans()
            .into_iter()
            .map(|span| CommandHint::new(span, CommandHintColor::Error, self.message()))
            .collect()
    }
    pub fn message(&self) -> Cow<'static, str> {
        use RunError::*;

        match self {
            Custom { text, .. } => text.clone().into(),
            VariableNotFound(_) => "Variable not found.".into(),
            ExpectedNumberAfterUnaryOperator(Spanned { value, .. }) => format!(
                "Expected a number after unary operator (-) but got {} instead.",
                value.kind()
            )
            .into(),
            InvalidVariantForResource(_, _) => todo!(),
            CannotIndexValue(_) => todo!(),
            FieldNotFoundInStruct(_) => todo!(),
            CouldntDereferenceValue(_) => todo!(),
            ReferenceToMovedData(_) => todo!(),
            VariableMoved(_) => todo!(),
            CannotBorrowValue(_) => todo!(),
            IncompatibleReflectTypes {
                expected,
                actual,
                span,
            } => todo!(),
            EnumVariantNotFound { name, span } => todo!(),
            CannotMoveOutOfResource(Spanned { value, .. }) => {
                format!("Cannot move out of resource `{value}`, try borrowing it instead.").into()
            }
            CannotNegateUnsignedInteger(Spanned { value, .. }) => format!(
                "Unsigned integers cannot be negated. (Type: {})",
                value.kind()
            )
            .into(),
            IncompatibleNumberTypes {
                left,
                right,
                ..
            } => format!("Incompatible number types; `{left}` and `{right}` are incompatible.")
                .into(),
        }
    }
}
