use crate::builtin_parser::lexer::{FailedToLexCharacter, Token};
use crate::builtin_parser::number::NumberKind;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("unknown token character: {0}")]
    FailedToLexCharacters(String),

    #[error("expected more tokens, got nothing.")]
    ExpectedMoreTokens,

    #[error("expected token {expected:?}, got token {got:?} instead.")]
    ExpectedTokenButGot { expected: Token, got: Token },

    #[error(
        "expected a semicolon or endline after a complete statement, but got more tokens than expected."
    )]
    ExpectedEndline(Token),

    #[error("expected a literal token, got {0:?} which is not a valid literal.")]
    ExpectedLiteral(Token),

    #[error(
        "invalid suffix `{0}` for number literal. the suffix must be one of the numeric types (`u32`, `isize`, `f32`, etc.)"
    )]
    InvalidSuffixForNumber(String),

    #[error(
        "invalid suffix `{0}` for float literal. the suffix must be one of the float types (`f32`, `f64`)"
    )]
    InvalidSuffixForFloat(String),

    #[error(r#""{0}" is an invalid suffix for a float. the valid suffixes are "f32" and "f64"."#)]
    IntegerSuffixOnFloat(String),

    #[error("{number} cannot be represented as {number_kind:#} as it is too small.")]
    NegativeIntOverflow {
        number: String,
        number_kind: NumberKind,
    },

    #[error("{number} cannot be represented as {number_kind:#} as it is too large.")]
    PositiveIntOverflow {
        number: String,
        number_kind: NumberKind,
    },

    #[error(
        "expected a continuation to the object declaration (such as a comma or a closing bracket), but got {0:?} instead."
    )]
    ExpectedObjectContinuation(Option<Result<Token, FailedToLexCharacter>>),

    #[error(
        "expected an identifier or integer when accessing member of variable, got {got:?} instead."
    )]
    ExpectedIndexer { got: Token },

    #[error("{feature} are not yet supported. {}", format_issue(*issue))]
    UnsupportedFeature { feature: &'static str, issue: u8 },

    #[error("unexpected closing delimiter: `{delimiter}`")]
    MismatchedDelimiter { delimiter: char },
}

pub(crate) fn format_issue(issue: u8) -> String {
    if issue != 0 {
        format!("see bevy_dev_console issue #{issue}")
    } else {
        String::new()
    }
}
