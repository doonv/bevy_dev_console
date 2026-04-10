//! Generates an abstract syntax tree from a list of tokens.

use kinded::Kinded;
use logos::Span;
use std::collections::HashMap;
use std::num::IntErrorKind;

use super::lexer::{FailedToLexCharacter, Token, TokenStream};
use super::number::Number;
use super::runner::environment::Function;
use super::{Environment, SpanExtension, Spanned};

/// An [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
///
/// This type represents a list of expressions, which is what makes up a command.
pub type Ast = Vec<Spanned<Expression>>;

macro_rules! expect {
    ($tokens:ident, $($token:tt)+) => {
        match $tokens.next() {
            Some(Ok($($token)+)) => ($($token)+) ,
            Some(Ok(token)) => {
                return Err(ParseError::ExpectedTokenButGot {
                    expected: $($token)+,
                    got: token,
                    span: $tokens.span(),
                })
            }
            Some(Err(FailedToLexCharacter)) => {
                return Err(ParseError::FailedToLexCharacters($tokens.span().wrap($tokens.slice().to_string())))
            }
            None => return Err(ParseError::ExpectedMoreTokens($tokens.span())),
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
        operator: Operator,
        right: Box<Spanned<Expression>>,
    },
    UnaryOp(Box<Spanned<Expression>>),
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
    pub const fn as_natural(&self) -> &'static str {
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
        let val = $val;
        if let $(Access::$variant($variant_inner))|+ = val.value $block else {
            use $crate::builtin_parser::parser::AccessKind;
            use $crate::builtin_parser::runner::error::EvalError;

            // We have to put this in a `const` first to avoid a
            // `temporary value dropped while borrowed` error.
            const EXPECTED_ACCESS: &[AccessKind] = &[$(AccessKind::$variant),+];
            Err(EvalError::IncorrectAccessOperation {
                span: val.span,
                expected_access: EXPECTED_ACCESS,
                expected_type: $expected,
                got: val.value,
            })?
        }
    }};
}
pub(crate) use access_unwrap;

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, Kinded)]
pub enum UnsupportedFeature {
    InfiniteLoops,
    WhileLoops,
    ForLoops,
    IfStatements,
    ListsAndVectors,
    Closures,
}

impl UnsupportedFeature {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::InfiniteLoops => "infinite loops",
            Self::WhileLoops => "while loops",
            Self::ForLoops => "for loops",
            Self::IfStatements => "if statements",
            Self::ListsAndVectors => "lists/vectors",
            Self::Closures => "closures",
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    FailedToLexCharacters(Spanned<String>),
    ExpectedMoreTokens(Span),
    ExpectedTokenButGot {
        expected: Token,
        got: Token,
        span: Span,
    },
    ExpectedEndline(Spanned<Token>),
    ExpectedLiteral(Spanned<Token>),
    InvalidSuffixForNumber(Spanned<String>, bool),
    InvalidSuffixForFloat(Spanned<String>),
    NegativeIntOverflow {
        span: Span,
        number: String,
        number_kind: crate::builtin_parser::number::NumberKind,
    },
    PositiveIntOverflow {
        span: Span,
        number: String,
        number_kind: crate::builtin_parser::number::NumberKind,
    },
    ExpectedObjectContinuation(Spanned<Option<Result<Token, FailedToLexCharacter>>>),
    ExpectedIndexer {
        got: Token,
        span: Span,
    },
    UnsupportedFeature {
        ty: UnsupportedFeature,
        span: Span,
        issue: u8,
    },
    MismatchedDelimiter {
        delimiter: char,
        span: Span,
    },
}

impl ParseError {
    pub fn span(&self) -> Span {
        use ParseError as E;
        match self {
            E::FailedToLexCharacters(Spanned { span, value: _ }) => span,
            E::ExpectedMoreTokens(span) => span,
            E::ExpectedTokenButGot { span, .. } => span,
            E::ExpectedEndline(Spanned { span, value: _ }) => span,
            E::ExpectedLiteral(Spanned { span, value: _ }) => span,
            E::InvalidSuffixForNumber(Spanned { span, .. }, ..) => span,
            E::InvalidSuffixForFloat(Spanned { span, value: _ }) => span,
            E::PositiveIntOverflow { span, .. } => span,
            E::NegativeIntOverflow { span, .. } => span,
            E::ExpectedObjectContinuation(Spanned { span, value: _ }) => span,
            E::ExpectedIndexer { got: _, span } => span,
            E::UnsupportedFeature { span, .. } => span,
            E::MismatchedDelimiter { span, .. } => span,
        }
        .clone()
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseError as E;
        match self {
            E::FailedToLexCharacters(Spanned { span: _, value }) => {
                write!(f, "unknown token character: {value}")
            }
            E::ExpectedMoreTokens(_) => write!(f, "expected more tokens, got nothing."),
            E::ExpectedTokenButGot {
                expected,
                got,
                span: _,
            } => write!(f, "expected token {expected:?}, got token {got:?} instead."),
            E::ExpectedEndline(_) => write!(
                f,
                "expected a semicolon or endline after a complete statement, but got more tokens than expected."
            ),
            E::ExpectedLiteral(Spanned { span: _, value }) => write!(
                f,
                "expected a literal token, got {value:?} which is not a valid literal."
            ),
            E::InvalidSuffixForNumber(Spanned { span: _, value }, is_float) => {
                write!(f, "invalid suffix `{value}` for number literal. ")?;
                if *is_float {
                    write!(
                        f,
                        "the suffix must be one of the float types (`f32`, `f64`)"
                    )
                } else {
                    write!(
                        f,
                        "the suffix must be one of the numeric types (`u32`, `isize`, `f32`, etc.)"
                    )
                }
            }
            E::InvalidSuffixForFloat(Spanned {
                span: _,
                value: suffix,
            }) => write!(
                f,
                r#""{suffix}" is an invalid suffix for a float. the valid suffixes are "f32" and "f64"."#
            ),
            E::NegativeIntOverflow {
                span: _,
                number,
                number_kind,
            } => write!(
                f,
                "{number} cannot be represented as {number_kind:#} as it is too small."
            ),
            E::PositiveIntOverflow {
                span: _,
                number,
                number_kind,
            } => write!(
                f,
                "{number} cannot be represented as {number_kind:#} as it is too large."
            ),
            E::ExpectedObjectContinuation(Spanned {
                span: _,
                value: got,
            }) => write!(
                f,
                "expected a continuation to the object declaration (such as a comma or a closing bracket), but got {got:?} instead."
            ),
            E::ExpectedIndexer { got, span: _ } => write!(
                f,
                "expected an identifier or integer when accessing member of variable, got {got:?} instead."
            ),
            &E::UnsupportedFeature { ty, span: _, issue } => {
                write!(f, "{} are not yet supported. ", ty.as_str())?;
                if issue != 0 {
                    write!(f, "see bevy_dev_console issue #{issue}")?;
                }
                Ok(())
            }
            E::MismatchedDelimiter { delimiter, span: _ } => {
                write!(f, "unexpected closing delimiter: `{delimiter}`")
            }
        }
    }
}
impl std::error::Error for ParseError {}

const FLOAT_PARSE_EXPECT_REASON: &str =
    "Float parsing errors are handled by the lexer, and floats cannot overflow.";

pub fn parse(tokens: &mut TokenStream, environment: &Environment) -> Result<Ast, ParseError> {
    let mut ast = Vec::new();

    while tokens.peek().is_some() {
        ast.push(parse_expression(tokens, environment)?);

        match tokens.next() {
            Some(Ok(Token::SemiColon)) => continue,
            Some(Ok(token)) => return Err(ParseError::ExpectedEndline(tokens.span().wrap(token))),
            Some(Err(FailedToLexCharacter)) => {
                return Err(ParseError::FailedToLexCharacters(
                    tokens.span().wrap(tokens.slice().to_string()),
                ));
            }
            None => break,
        }
    }

    Ok(ast)
}

fn parse_expression(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Spanned<Expression>, ParseError> {
    match tokens.peek() {
        Some(Ok(Token::Loop)) => Err(ParseError::UnsupportedFeature {
            ty: UnsupportedFeature::InfiniteLoops,
            span: tokens.peek_span(),
            issue: 8,
        }),
        Some(Ok(Token::While)) => Err(ParseError::UnsupportedFeature {
            ty: UnsupportedFeature::WhileLoops,
            span: tokens.peek_span(),
            issue: 8,
        }),
        Some(Ok(Token::For)) => Err(ParseError::UnsupportedFeature {
            ty: UnsupportedFeature::ForLoops,
            span: tokens.peek_span(),
            issue: 8,
        }),
        Some(Ok(Token::If)) => Err(ParseError::UnsupportedFeature {
            ty: UnsupportedFeature::IfStatements,
            span: tokens.discard().span_until(Token::RightBracket),
            issue: 0,
        }),
        Some(Ok(_)) => {
            let expr = parse_additive(tokens, environment)?;

            match tokens.peek() {
                Some(Ok(Token::Equals)) => Ok(parse_var_assign(expr, tokens, environment)?),
                _ => Ok(expr),
            }
        }
        Some(Err(FailedToLexCharacter)) => Err(ParseError::FailedToLexCharacters(
            tokens.peek_span().wrap(tokens.slice().to_string()),
        )),
        None => Err(ParseError::ExpectedMoreTokens(tokens.peek_span())),
    }
}

fn _parse_block(tokens: &mut TokenStream, environment: &Environment) -> Result<Ast, ParseError> {
    expect!(tokens, Token::LeftBracket);
    let ast = parse(tokens, environment)?;
    expect!(tokens, Token::RightBracket);
    Ok(ast)
}

fn parse_additive(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Spanned<Expression>, ParseError> {
    let mut node = parse_multiplicitive(tokens, environment)?;

    while let Some(Ok(Token::Plus | Token::Minus)) = tokens.peek() {
        let operator = match tokens.next() {
            Some(Ok(Token::Plus)) => Operator::Add,
            Some(Ok(Token::Minus)) => Operator::Sub,
            _ => unreachable!(),
        };

        let right = parse_multiplicitive(tokens, environment)?;

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
fn parse_multiplicitive(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Spanned<Expression>, ParseError> {
    let mut node = parse_value(tokens, environment)?;

    while let Some(Ok(Token::Asterisk | Token::Slash | Token::Modulo)) = tokens.peek() {
        let operator = match tokens.next() {
            Some(Ok(Token::Asterisk)) => Operator::Mul,
            Some(Ok(Token::Slash)) => Operator::Div,
            Some(Ok(Token::Modulo)) => Operator::Mod,
            _ => unreachable!(),
        };

        let right = parse_value(tokens, environment)?;

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

fn parse_value(
    tokens: &mut TokenStream,
    environment: &Environment,
) -> Result<Spanned<Expression>, ParseError> {
    /// Parses a literal (value without member expressions)
    fn parse_literal(
        tokens: &mut TokenStream,
        environment: &Environment,
    ) -> Result<Spanned<Expression>, ParseError> {
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
                let name = tokens.slice().to_string();

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
                            dbg!(argument_count);

                            let mut arguments = Vec::new();
                            for _ in 0..(*argument_count) {
                                let expr = parse_expression(tokens, environment)?;
                                arguments.push(expr);
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
                let string = slice[1..slice.len() - 1].to_string();
                Ok(tokens.span().wrap(Expression::String(string)))
            }
            Some(Ok(Token::Minus)) => {
                let expr = parse_literal(tokens, environment)?;
                Ok(tokens.span().wrap(Expression::UnaryOp(Box::new(expr))))
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
                    | "isize" => Err(ParseError::InvalidSuffixForFloat(
                        tokens.span().add(number.len()..0).wrap(suffix.to_owned()),
                    ))?,
                    "f32" => Number::f32(number.parse().expect(FLOAT_PARSE_EXPECT_REASON)),
                    "f64" => Number::f64(number.parse().expect(FLOAT_PARSE_EXPECT_REASON)),
                    _ => {
                        return Err(ParseError::InvalidSuffixForNumber(
                            tokens.span().add(number.len()..0).wrap(suffix.to_owned()),
                            true,
                        ));
                    }
                };
                let start_span = tokens.span().end;

                tokens.next();

                Ok(Spanned {
                    span: start_span..tokens.span().end,
                    value: Expression::Number(number),
                })
            }
            Some(Ok(Token::True)) => Ok(tokens.span().wrap(Expression::Boolean(true))),
            Some(Ok(Token::False)) => Ok(tokens.span().wrap(Expression::Boolean(false))),
            Some(Ok(Token::LeftBrace)) => Err(ParseError::UnsupportedFeature {
                ty: UnsupportedFeature::ListsAndVectors,
                span: tokens.span_until(Token::RightBrace),
                issue: 10,
            }),
            Some(Ok(Token::Pipe)) => Err(ParseError::UnsupportedFeature {
                ty: UnsupportedFeature::Closures,
                span: tokens.span_until(Token::Pipe),
                issue: 12,
            }),
            Some(Ok(Token::RightBrace | Token::RightBracket | Token::RightParen)) => {
                Err(ParseError::MismatchedDelimiter {
                    delimiter: tokens.slice().chars().next().unwrap(),
                    span: tokens.span(),
                })
            }
            Some(Ok(token)) => Err(ParseError::ExpectedLiteral(tokens.span().wrap(token))),
            Some(Err(FailedToLexCharacter)) => Err(ParseError::FailedToLexCharacters(
                tokens.span().wrap(tokens.slice().to_string()),
            )),
            None => Err(ParseError::ExpectedMoreTokens(tokens.span())),
        }
    }

    let mut expr = parse_literal(tokens, environment)?;
    // If theres a dot after the expression, do a member expression:
    while let Some(Ok(Token::Dot)) = tokens.peek() {
        tokens.next(); // Skip the dot
        match tokens.next() {
            Some(Ok(Token::Identifier)) => {
                let right = tokens.slice().to_string();
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
                    crate::builtin_parser::number::NumberKind::usize,
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
                return Err(ParseError::ExpectedIndexer {
                    got: token,
                    span: tokens.span(),
                });
            }
            Some(Err(FailedToLexCharacter)) => {
                return Err(ParseError::FailedToLexCharacters(
                    tokens.span().wrap(tokens.slice().to_string()),
                ));
            }
            None => return Err(ParseError::ExpectedMoreTokens(tokens.span())),
        }
    }
    Ok(expr)
}

fn map_parseint_error<'s>(
    span: Span,
    slice: &'s str,
    number_kind: crate::builtin_parser::number::NumberKind,
) -> impl FnOnce(std::num::ParseIntError) -> ParseError + 's {
    move |error| match error.kind() {
        IntErrorKind::PosOverflow => ParseError::PositiveIntOverflow {
            span,
            number: slice.to_string(),
            number_kind,
        },
        IntErrorKind::NegOverflow => ParseError::NegativeIntOverflow {
            span,
            number: slice.to_string(),
            number_kind,
        },
        IntErrorKind::Empty | IntErrorKind::InvalidDigit | IntErrorKind::Zero => unreachable!(
            "Lexer makes sure other errors aren't possible. Create an bevy_dev_console issue!"
        ),
        _ => unimplemented!(), // Required due to IntErrorKind being #[non_exhaustive]
    }
}

fn parse_number(tokens: &mut TokenStream) -> Result<Spanned<Number>, ParseError> {
    let (number, suffix) = split_number(tokens);
    let map = |s| map_parseint_error(tokens.span(), tokens.slice(), s);
    use crate::builtin_parser::number::NumberKind;
    let number = match suffix {
        "u8" => Number::u8(number.parse().map_err(map(NumberKind::u8))?),
        "u16" => Number::u16(number.parse().map_err(map(NumberKind::u16))?),
        "u32" => Number::u32(number.parse().map_err(map(NumberKind::u32))?),
        "u64" => Number::u64(number.parse().map_err(map(NumberKind::u64))?),
        "usize" => Number::usize(number.parse().map_err(map(NumberKind::usize))?),
        "i8" => Number::i8(number.parse().map_err(map(NumberKind::i8))?),
        "i16" => Number::i16(number.parse().map_err(map(NumberKind::i16))?),
        "i32" => Number::i32(number.parse().map_err(map(NumberKind::i32))?),
        "isize" => Number::isize(number.parse().map_err(map(NumberKind::isize))?),
        "f32" => Number::f32(number.parse().expect(FLOAT_PARSE_EXPECT_REASON)),
        "f64" => Number::f64(number.parse().expect(FLOAT_PARSE_EXPECT_REASON)),
        "" => Number::Integer(number.parse().unwrap()),
        _ => {
            return Err(ParseError::InvalidSuffixForNumber(
                tokens.span().add(number.len()..0).wrap(suffix.to_owned()),
                false,
            ));
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
) -> Result<Spanned<Expression>, ParseError> {
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
) -> Result<HashMap<String, Spanned<Expression>>, ParseError> {
    let mut map = HashMap::new();
    while let Some(Ok(Token::Identifier)) = tokens.peek() {
        tokens.next();
        let ident = tokens.slice().to_string();
        expect!(tokens, Token::Colon);
        let expr = parse_expression(tokens, environment)?;
        map.insert(ident, expr);
        match tokens.peek() {
            Some(Ok(Token::RightBracket)) => break,
            Some(Ok(Token::Comma)) => {
                tokens.next();
            }
            token => Err(ParseError::ExpectedObjectContinuation(
                tokens.span().wrap(token.clone()),
            ))?,
        }
    }
    expect!(tokens, Token::RightBracket);
    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::super::Environment;
    use super::super::lexer::TokenStream;
    use super::parse;

    #[test]
    fn var_assign() {
        let mut lexer = TokenStream::new("x = 1 + 2 - 30 + y");
        let environment = Environment::default();

        let ast = parse(&mut lexer, &environment);

        assert!(ast.is_ok());

        // TODO: figure out how to assert ast
    }
}
