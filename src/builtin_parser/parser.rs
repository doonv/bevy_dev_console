//! Generates an abstract syntax tree from a list of tokens.

use kinded::Kinded;
use logos::Span;
use std::collections::HashMap;
use std::fmt::Display;
use std::num::IntErrorKind;

use crate::builtin_parser::number::{
    Float, NumberKind, SignedInteger, SignedIntegerKind, UnsignedInteger, UnsignedIntegerKind,
};

use super::lexer::{FailedToLexCharacter, Token, TokenStream};
use super::number::Number;
use super::runner::environment::Function;
use super::{Diagnostic, Environment, SpanExtension, Spanned};

/// An [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
///
/// This type represents a list of expressions, which is what makes up a command.
pub type Ast = Vec<Spanned<Expression>>;

macro_rules! expect {
    ($tokens:ident, $($token:tt)+) => {
        match $tokens.next() {
            Some(Ok($($token)+)) => ($($token)+) ,
            Some(Ok(token)) => {
                return Err($tokens.span().wrap(ParseError::ExpectedTokenButGot {
                    expected: $($token)+,
                    got: token,
                }).into())
            }
            Some(Err(FailedToLexCharacter)) => {
                return Err($tokens.span().diagnose(ParseError::FailedToLexCharacters($tokens.slice().to_owned())))
            }
            None => return Err($tokens.span().diagnose(ParseError::ExpectedMoreTokens)),
        }
    };
}

/// A type that represents an expression.
#[derive(Debug, Clone, Kinded)]
pub enum Expression {
    // Primitives
    None,
    Boolean(bool),
    Number(Number),
    Variable(String),
    String(String),
    Borrow(Box<Spanned<Expression>>),
    Dereference(Box<Spanned<Expression>>),
    Object(HashMap<String, Spanned<Expression>>),
    StructObject {
        name: String,
        map: HashMap<String, Spanned<Expression>>,
    },
    Tuple(Vec<Spanned<Expression>>),
    StructTuple {
        name: String,
        tuple: Vec<Spanned<Expression>>,
    },

    // Expressions
    BinaryOp {
        left: Box<Spanned<Expression>>,
        operator: BinaryOperator,
        right: Box<Spanned<Expression>>,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Spanned<Expression>>,
    },
    Member {
        left: Box<Spanned<Expression>>,
        right: Spanned<Access>,
    },

    // Statement-like
    VarAssign {
        name: Box<Spanned<Expression>>,
        value: Box<Spanned<Expression>>,
    },
    Function {
        name: String,
        arguments: Vec<Spanned<Expression>>,
    },
    ForLoop {
        index_name: String,
        loop_count: u64,
        block: Ast,
    },
}

impl ExpressionKind {
    pub const fn as_natural(&self) -> &'static str {
        match self {
            ExpressionKind::None => "nothing",
            ExpressionKind::Boolean => "a boolean",
            ExpressionKind::Number => "a number",
            ExpressionKind::Variable => "a variable name",
            ExpressionKind::String => "a string",
            ExpressionKind::Borrow => "a borrow",
            ExpressionKind::Dereference => "a dereference",
            ExpressionKind::Object => "an object",
            ExpressionKind::StructObject => "a struct object",
            ExpressionKind::Tuple => "a tuple",
            ExpressionKind::StructTuple => "a struct tuple",

            ExpressionKind::BinaryOp => "a binary operation",
            ExpressionKind::UnaryOp => "a unary operation",
            ExpressionKind::Member => "a member expression",
            ExpressionKind::VarAssign => "a variable assignment",
            ExpressionKind::Function => "a function call",
            ExpressionKind::ForLoop => "a for loop",
        }
    }
}

/// A singular element access within a [`Expression::Member`].
///
/// Based on `bevy_reflect`'s `Access`.
#[derive(Debug, Clone, Kinded)]
pub enum Access {
    /// A name-based field access on a struct.
    Field(String),
    /// An index-based access on a tuple.
    TupleIndex(usize),
    // /// An index-based access on a list.
    // ListIndex(usize),
}
impl AccessKind {
    /// Returns the kind of [`Access`] as a [string slice](str) with an `a` or `an` prepended to it.
    /// Used for more natural sounding error messages.
    pub const fn as_natural(self) -> &'static str {
        match self {
            AccessKind::Field => "a field",
            AccessKind::TupleIndex => "a tuple",
        }
    }
}

/// Get the access if its of a certain type, if not, return a [`EvalError`](super::runner::error::EvalError).
///
/// For examples, take a look at existing uses in the code.
macro_rules! access_unwrap {
    ($expected:literal, $($variant:ident($variant_inner:ident))|+ = $val:expr => $block:block) => {{
        let Spanned { span, value } = $val;
        if let $(Access::$variant($variant_inner))|+ = value $block else {
            use $crate::builtin_parser::parser::AccessKind;
            use $crate::builtin_parser::runner::error::EvalError;

            Err(span.diagnose(
                EvalError::IncorrectAccessOperation {
                    expected_access: &[$(AccessKind::$variant),+],
                    expected_type: $expected,
                    got: value.kind(),
                },
            ))?
        }
    }};
}
pub(crate) use access_unwrap;

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    And,
    Xor,
    Or,
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Mod => write!(f, "%"),
            BinaryOperator::And => write!(f, "&"),
            BinaryOperator::Xor => write!(f, "^"),
            BinaryOperator::Or => write!(f, "|"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    /// `-x`
    Minus,
    /// `!true`
    Not,
}
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Minus => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

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
        number_kind: crate::builtin_parser::number::NumberKind,
    },

    #[error("{number} cannot be represented as {number_kind:#} as it is too large.")]
    PositiveIntOverflow {
        number: String,
        number_kind: crate::builtin_parser::number::NumberKind,
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

fn format_issue(issue: u8) -> String {
    if issue != 0 {
        format!("see bevy_dev_console issue #{issue}")
    } else {
        String::new()
    }
}

const FLOAT_PARSE_EXPECT_REASON: &str =
    "Float parsing errors are handled by the lexer, and floats cannot overflow.";

pub fn parse(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Ast, Diagnostic<ParseError>> {
    let mut ast = Vec::new();

    while tokens.peek().is_some() {
        ast.push(parse_expression(tokens, environment)?);

        match tokens.next() {
            Some(Ok(Token::SemiColon)) => continue,
            Some(Ok(token)) => {
                return Err(tokens
                    .span()
                    .wrap(ParseError::ExpectedEndline(token))
                    .into());
            }
            Some(Err(FailedToLexCharacter)) => {
                return Err(tokens
                    .span()
                    .wrap(ParseError::FailedToLexCharacters(tokens.slice().to_owned()))
                    .into());
            }
            None => break,
        }
    }

    Ok(ast)
}

fn parse_expression(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Spanned<Expression>, Diagnostic<ParseError>> {
    match tokens.peek() {
        Some(Ok(Token::Loop)) => Err(tokens
            .peek_span()
            .wrap(ParseError::UnsupportedFeature {
                feature: "infinite loops",
                issue: 8,
            })
            .into()),
        Some(Ok(Token::While)) => Err(tokens
            .peek_span()
            .wrap(ParseError::UnsupportedFeature {
                feature: "while loops",
                issue: 8,
            })
            .into()),
        Some(Ok(Token::For)) => Err(tokens
            .peek_span()
            .wrap(ParseError::UnsupportedFeature {
                feature: "for loops",
                issue: 8,
            })
            .into()),
        Some(Ok(Token::If)) => Err(tokens
            .skip_one()
            .span_until(Token::RightBracket)
            .wrap(ParseError::UnsupportedFeature {
                feature: "if statements",
                issue: 0,
            })
            .into()),
        Some(Ok(_)) => {
            let expr = parse_additive(tokens, environment)?;

            match tokens.peek() {
                Some(Ok(Token::Equals)) => Ok(parse_var_assign(expr, tokens, environment)?),
                _ => Ok(expr),
            }
        }
        Some(Err(FailedToLexCharacter)) => Err(tokens
            .peek_span()
            .wrap(ParseError::FailedToLexCharacters(tokens.slice().to_owned()))
            .into()),
        None => Err(tokens
            .peek_span()
            .wrap(ParseError::ExpectedMoreTokens)
            .into()),
    }
}

fn _parse_block(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Ast, Diagnostic<ParseError>> {
    expect!(tokens, Token::LeftBracket);
    let ast = parse(tokens, environment)?;
    expect!(tokens, Token::RightBracket);

    Ok(ast)
}

fn parse_additive(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Spanned<Expression>, Diagnostic<ParseError>> {
    let mut node = parse_multiplicitive(tokens, environment)?;

    while let Some(Ok(Token::Plus | Token::Minus)) = tokens.peek() {
        let operator = match tokens.next() {
            Some(Ok(Token::Plus)) => BinaryOperator::Add,
            Some(Ok(Token::Minus)) => BinaryOperator::Sub,
            _ => unreachable!(),
        };

        let right = parse_multiplicitive(tokens, environment)?;

        node = Spanned {
            span: node.span.join(&right.span),
            value: Expression::BinaryOp {
                left: Box::new(node),
                operator,
                right: Box::new(right),
            },
        };
    }

    Ok(node)
}
fn parse_multiplicitive(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Spanned<Expression>, Diagnostic<ParseError>> {
    let mut node = parse_and(tokens, environment)?;

    while let Some(Ok(Token::Asterisk | Token::Slash | Token::Modulo)) = tokens.peek() {
        let operator = match tokens.next() {
            Some(Ok(Token::Asterisk)) => BinaryOperator::Mul,
            Some(Ok(Token::Slash)) => BinaryOperator::Div,
            Some(Ok(Token::Modulo)) => BinaryOperator::Mod,
            _ => unreachable!(),
        };

        let right = parse_and(tokens, environment)?;

        node = Spanned {
            span: node.span.start..right.span.end,
            value: Expression::BinaryOp {
                left: Box::new(node),
                operator,
                right: Box::new(right),
            },
        };
    }

    Ok(node)
}
macro_rules! parse_bitwise {
    ($op:ident, $token:ident: $name:ident => $next:ident) => {
        fn $name(
            tokens: &mut TokenStream,
            environment: &Environment,
        ) -> Result<Spanned<Expression>, Diagnostic<ParseError>> {
            let mut node = $next(tokens, environment)?;

            while let Some(Ok(Token::$token)) = tokens.peek() {
                tokens.next();
                let right = $next(tokens, environment)?;

                node = Spanned {
                    span: node.span.start..right.span.end,
                    value: Expression::BinaryOp {
                        left: Box::new(node),
                        operator: BinaryOperator::$op,
                        right: Box::new(right),
                    },
                };
            }

            Ok(node)
        }
    };
}
parse_bitwise!(And, Ampersand: parse_and => parse_xor);
parse_bitwise!(Xor, Xor: parse_xor => parse_or);
parse_bitwise!(Or, Pipe: parse_or => parse_value);

fn parse_value(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Spanned<Expression>, Diagnostic<ParseError>> {
    /// Parses a literal (value without member expressions)
    fn parse_literal(
        tokens: &mut TokenStream,
        environment: &Environment,
    ) -> Result<Spanned<Expression>, Diagnostic<ParseError>> {
        match tokens.next() {
            Some(Ok(Token::LeftParen)) => {
                let start = tokens.span().start;
                if let Some(Ok(Token::RightParen)) = tokens.peek() {
                    tokens.next();
                    Ok(Spanned {
                        span: start..tokens.span().end,
                        value: Expression::None,
                    })
                } else {
                    let expr = parse_expression(tokens, environment)?;
                    if let Some(Ok(Token::Comma)) = tokens.peek() {
                        let mut tuple = vec![expr];

                        while let Some(Ok(Token::Comma)) = tokens.peek() {
                            tokens.next();
                            let expr = parse_expression(tokens, environment)?;

                            tuple.push(expr);
                        }

                        expect!(tokens, Token::RightParen);

                        Ok(Spanned {
                            span: start..tokens.span().end,
                            value: Expression::Tuple(tuple),
                        })
                    } else {
                        expect!(tokens, Token::RightParen);

                        Ok(expr)
                    }
                }
            }
            Some(Ok(Token::Identifier)) => {
                let start = tokens.span().start;
                let name = tokens.slice().to_owned();

                match tokens.peek() {
                    Some(Ok(Token::LeftParen)) => {
                        tokens.next();

                        let expr = parse_expression(tokens, environment)?;

                        let mut tuple = vec![expr];

                        while let Some(Ok(Token::Comma)) = tokens.peek() {
                            tokens.next();
                            let expr = parse_expression(tokens, environment)?;

                            tuple.push(expr);
                        }

                        expect!(tokens, Token::RightParen);

                        Ok(Spanned {
                            span: start..tokens.span().end,
                            value: Expression::StructTuple { name, tuple },
                        })
                    }
                    Some(Ok(Token::LeftBracket)) => {
                        tokens.next();

                        let map = parse_object(tokens, environment)?;

                        Ok(Spanned {
                            span: tokens.span(),
                            value: Expression::StructObject { name, map },
                        })
                    }
                    _ => {
                        if let Some(Function { argument_count, .. }) =
                            environment.get_function(&name)
                        {
                            let mut arguments = Vec::new();
                            for _ in 0..(*argument_count) {
                                let expr = parse_expression(tokens, environment)?;
                                arguments.push(expr);
                            }
                            while !matches!(
                                tokens.peek(),
                                Some(Ok(Token::SemiColon
                                    | Token::RightBrace
                                    | Token::RightBracket
                                    | Token::RightParen))
                            ) && let Ok(additional) = parse_expression(tokens, environment)
                            {
                                arguments.push(additional);
                            }
                            Ok(Spanned {
                                span: start..tokens.span().end,
                                value: Expression::Function { name, arguments },
                            })
                        } else {
                            Ok(tokens.span().wrap(Expression::Variable(name)))
                        }
                    }
                }
            }
            Some(Ok(Token::LeftBracket)) => {
                let map = parse_object(tokens, environment)?;

                Ok(Spanned {
                    span: tokens.span(),
                    value: Expression::Object(map),
                })
            }
            Some(Ok(Token::String)) => {
                let slice = tokens.slice();
                let string = slice[1..slice.len() - 1].to_owned();
                Ok(tokens.span().wrap(Expression::String(string)))
            }
            Some(Ok(token @ (Token::Minus | Token::Not))) => {
                let expr = parse_literal(tokens, environment)?;
                Ok(tokens.span().wrap(Expression::UnaryOp {
                    operator: match token {
                        Token::Minus => UnaryOperator::Minus,
                        Token::Not => UnaryOperator::Not,
                        _ => unreachable!(),
                    },
                    operand: Box::new(expr),
                }))
            }
            Some(Ok(Token::Ampersand)) => {
                let expr = parse_literal(tokens, environment)?;

                Ok(tokens.span().wrap(Expression::Borrow(Box::new(expr))))
            }
            Some(Ok(Token::Asterisk)) => {
                let expr = parse_literal(tokens, environment)?;
                Ok(tokens.span().wrap(Expression::Dereference(Box::new(expr))))
            }
            Some(Ok(Token::IntegerNumber)) => {
                parse_number(tokens).map(|s| s.map(Expression::Number))
            }
            Some(Ok(Token::FloatNumber)) => {
                let (number, suffix) = split_number(tokens);
                let number: Number = match suffix {
                    "u8" | "u16" | "u32" | "u64" | "usize" | "i8" | "i16" | "i32" | "i64"
                    | "isize" => Err(Diagnostic::single(
                        tokens.span().add(number.len()..0),
                        ParseError::IntegerSuffixOnFloat(suffix.to_owned()),
                    ))?,
                    "f32" => {
                        Number::Float(Float::f32(number.parse().expect(FLOAT_PARSE_EXPECT_REASON)))
                    }
                    "f64" => {
                        Number::Float(Float::f64(number.parse().expect(FLOAT_PARSE_EXPECT_REASON)))
                    }
                    "" => Number::Float(Float::Unspecified(
                        number.parse().expect(FLOAT_PARSE_EXPECT_REASON),
                    )),
                    _ => {
                        return Err(tokens
                            .span()
                            .add(number.len()..0)
                            .wrap(ParseError::InvalidSuffixForFloat(suffix.to_owned()))
                            .into());
                    }
                };
                let start_span = tokens.span().end;

                Ok(Spanned {
                    span: start_span..tokens.span().end,
                    value: Expression::Number(number),
                })
            }
            Some(Ok(Token::True)) => Ok(tokens.span().wrap(Expression::Boolean(true))),
            Some(Ok(Token::False)) => Ok(tokens.span().wrap(Expression::Boolean(false))),
            Some(Ok(Token::LeftBrace)) => Err(tokens
                .span_until(Token::RightBrace)
                .wrap(ParseError::UnsupportedFeature {
                    feature: "lists and vectors",
                    issue: 10,
                })
                .into()),
            Some(Ok(Token::Pipe)) => Err(tokens
                .span_until(Token::Pipe)
                .wrap(ParseError::UnsupportedFeature {
                    feature: "closures",
                    issue: 12,
                })
                .into()),
            Some(Ok(Token::RightBrace | Token::RightBracket | Token::RightParen)) => Err(tokens
                .span()
                .wrap(ParseError::MismatchedDelimiter {
                    delimiter: tokens.slice().chars().next().unwrap(),
                })
                .into()),
            Some(Ok(token)) => Err(tokens
                .span()
                .wrap(ParseError::ExpectedLiteral(token))
                .into()),
            Some(Err(FailedToLexCharacter)) => Err(tokens
                .span()
                .wrap(ParseError::FailedToLexCharacters(tokens.slice().to_owned()))
                .into()),
            None => Err(tokens.span().diagnose(ParseError::ExpectedMoreTokens)),
        }
    }

    let mut expr = parse_literal(tokens, environment)?;
    // If theres a dot after the expression, do a member expression:
    while let Some(Ok(Token::Dot)) = tokens.peek() {
        tokens.next(); // Skip the dot
        match tokens.next() {
            Some(Ok(Token::Identifier)) => {
                let right = tokens.slice().to_owned();
                expr = Spanned {
                    span: expr.span.start..tokens.span().end,
                    value: Expression::Member {
                        left: Box::new(expr),
                        right: tokens.span().wrap(Access::Field(right)),
                    },
                };
            }
            Some(Ok(Token::IntegerNumber)) => {
                let right = tokens.slice().parse().map_err(map_parseint_error(
                    tokens.span(),
                    tokens.slice(),
                    UnsignedIntegerKind::usize.into(),
                ))?;

                expr = Spanned {
                    span: expr.span.start..tokens.span().end,
                    value: Expression::Member {
                        left: Box::new(expr),
                        right: tokens.span().wrap(Access::TupleIndex(right)),
                    },
                };
            }
            Some(Ok(token)) => {
                return Err(tokens
                    .span()
                    .wrap(ParseError::ExpectedIndexer { got: token })
                    .into());
            }
            Some(Err(FailedToLexCharacter)) => {
                return Err(tokens
                    .span()
                    .wrap(ParseError::FailedToLexCharacters(tokens.slice().to_owned()))
                    .into());
            }
            None => return Err(tokens.span().diagnose(ParseError::ExpectedMoreTokens)),
        }
    }
    Ok(expr)
}

fn map_parseint_error<'s>(
    span: Span,
    slice: &'s str,
    number_kind: crate::builtin_parser::number::NumberKind,
) -> impl FnOnce(std::num::ParseIntError) -> Diagnostic<ParseError> + 's {
    move |error| {
        let error = match error.kind() {
            IntErrorKind::PosOverflow => ParseError::PositiveIntOverflow {
                number: slice.to_owned(),
                number_kind,
            },
            IntErrorKind::NegOverflow => ParseError::NegativeIntOverflow {
                number: slice.to_owned(),
                number_kind,
            },
            IntErrorKind::Empty | IntErrorKind::InvalidDigit | IntErrorKind::Zero => unreachable!(
                "Lexer makes sure other errors aren't possible. Create an bevy_dev_console issue!"
            ),
            _ => unimplemented!(), // Required due to IntErrorKind being #[non_exhaustive]
        };
        span.diagnose(error)
    }
}

fn parse_number(tokens: &mut TokenStream) -> Result<Spanned<Number>, Diagnostic<ParseError>> {
    let (number, suffix) = split_number(tokens);
    let map = |s: NumberKind| map_parseint_error(tokens.span(), tokens.slice(), s);

    let number = match suffix {
        "u8" => UnsignedInteger::u8(
            number
                .parse()
                .map_err(map(UnsignedIntegerKind::u8.into()))?,
        )
        .into(),
        "u16" => UnsignedInteger::u16(
            number
                .parse()
                .map_err(map(UnsignedIntegerKind::u16.into()))?,
        )
        .into(),
        "u32" => UnsignedInteger::u32(
            number
                .parse()
                .map_err(map(UnsignedIntegerKind::u32.into()))?,
        )
        .into(),
        "u64" => UnsignedInteger::u64(
            number
                .parse()
                .map_err(map(UnsignedIntegerKind::u64.into()))?,
        )
        .into(),
        "usize" => UnsignedInteger::usize(
            number
                .parse()
                .map_err(map(UnsignedIntegerKind::usize.into()))?,
        )
        .into(),
        "i8" => {
            SignedInteger::i8(number.parse().map_err(map(SignedIntegerKind::i8.into()))?).into()
        }
        "i16" => {
            SignedInteger::i16(number.parse().map_err(map(SignedIntegerKind::i16.into()))?).into()
        }
        "i32" => {
            SignedInteger::i32(number.parse().map_err(map(SignedIntegerKind::i32.into()))?).into()
        }
        "isize" => SignedInteger::isize(
            number
                .parse()
                .map_err(map(SignedIntegerKind::isize.into()))?,
        )
        .into(),
        "f32" => Number::Float(Float::f32(number.parse().expect(FLOAT_PARSE_EXPECT_REASON))),
        "f64" => Number::Float(Float::f64(number.parse().expect(FLOAT_PARSE_EXPECT_REASON))),
        "" => SignedInteger::Unspecified(number.parse().unwrap()).into(),
        _ => {
            return Err(tokens
                .span()
                .add(number.len()..0)
                .wrap(ParseError::InvalidSuffixForNumber(suffix.to_owned()))
                .into());
        }
    };
    Ok(Spanned {
        span: tokens.span(),
        value: number,
    })
}

fn split_number<'s>(tokens: &'s TokenStream<'_>) -> (&'s str, &'s str) {
    let s = tokens.slice();
    let i = s
        .as_bytes()
        .iter()
        .position(|b| b.is_ascii_alphabetic() || *b == b'_')
        .unwrap_or(s.len());

    let (number, suffix) = s.split_at(i);
    (number, suffix)
}

fn parse_var_assign(
    name: Spanned<Expression>,
    tokens: &mut TokenStream<'_>,
    environment: &Environment,
) -> Result<Spanned<Expression>, Diagnostic<ParseError>> {
    tokens.next(); // We already know that the next token is an equals

    let value = parse_expression(tokens, environment)?;

    Ok(Spanned {
        span: name.span.start..value.span.end,
        value: Expression::VarAssign {
            name: Box::new(name),
            value: Box::new(value),
        },
    })
}

/// Parses an object.
///
/// - `{}`
/// - `{test: 4}`
/// - `{str: "sup!", num: -6.2}`
fn parse_object(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<HashMap<String, Spanned<Expression>>, Diagnostic<ParseError>> {
    let mut map = HashMap::new();
    while let Some(Ok(Token::Identifier)) = tokens.peek() {
        tokens.next();
        let ident = tokens.slice().to_owned();
        expect!(tokens, Token::Colon);
        let expr = parse_expression(tokens, environment)?;
        map.insert(ident, expr);
        match tokens.peek() {
            Some(Ok(Token::RightBracket)) => break,
            Some(Ok(Token::Comma)) => {
                tokens.next();
            }
            token => Err(tokens
                .span()
                .diagnose(ParseError::ExpectedObjectContinuation(token.clone())))?,
        }
    }
    expect!(tokens, Token::RightBracket);
    Ok(map)
}

#[cfg(test)]
mod tests {
    use crate::builtin_parser::Spanned;
    use crate::builtin_parser::parser::Expression;
    use logos::Span;

    use super::super::Environment;
    use super::super::lexer::TokenStream;
    use super::Expression::*;
    use super::parse;
    use crate::builtin_parser::Integer::*;
    use crate::builtin_parser::Number;
    use crate::builtin_parser::SignedInteger::*;
    use crate::builtin_parser::parser::BinaryOperator::*;
    use std::assert_matches;

    fn setup(src: &str) -> std::vec::IntoIter<Spanned<Expression>> {
        let mut lexer = TokenStream::new(src);
        let environment = Environment::default();

        let ast = parse(&mut lexer, &environment).unwrap();

        ast.into_iter()
    }

    #[test]
    fn var_assign() {
        let mut stmts = setup("x = 1 + 2 - 30 + y");

        assert_matches!(
            stmts.next(),
            Some(Spanned {
                span: Span { start: 0, end: 18 },
                value:
                    VarAssign {
                        name:
                            box Spanned {
                                span: Span { start: 0, end: 1 },
                                value: Variable(_),
                            },
                        value:
                            box Spanned {
                                span: Span { start: 4, end: 18 },
                                value:
                                    BinaryOp {
                                        left:
                                            box Spanned {
                                                span: Span { start: 4, end: 14 },
                                                value:
                                                    BinaryOp {
                                                        left:
                                                            box Spanned {
                                                                span: Span { start: 4, end: 9 },
                                                                value:
                                                                    BinaryOp {
                                                                        left:
                                                                            box Spanned {
                                                                                span:
                                                                                    Span {
                                                                                        start: 4,
                                                                                        end: 5,
                                                                                    },
                                                                                value:
                                                                                    Number(Number::Integer(Signed(Unspecified(1)))),
                                                                            },
                                                                        operator: Add,
                                                                        right:
                                                                            box Spanned {
                                                                                span:
                                                                                    Span {
                                                                                        start: 8,
                                                                                        end: 9,
                                                                                    },
                                                                                value:
                                                                                    Number(Number::Integer(Signed(Unspecified(2)))),
                                                                            },
                                                                    },
                                                            },
                                                        operator: Sub,
                                                        right:
                                                            box Spanned {
                                                                span: Span { start: 12, end: 14 },
                                                                value: Number(Number::Integer(Signed(Unspecified(30)))),
                                                            },
                                                    },
                                            },
                                        operator: Add,
                                        right:
                                            box Spanned {
                                                span: Span { start: 17, end: 18 },
                                                value: Variable(_),
                                            },
                                    },
                            },
                    },
            })
        );
        assert!(stmts.next().is_none());
    }
}
