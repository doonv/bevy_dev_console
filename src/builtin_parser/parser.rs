//! Generates an abstract syntax tree from a list of tokens.

use logos::Span;
use std::collections::HashMap;
use std::num::IntErrorKind;

use super::lexer::{FailedToLexCharacter, Token, TokenStream};
use super::number::Number;
use super::runner::environment::Function;
use super::{Environment, Spanned};

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
                return Err(ParseError::FailedToLexCharacter($tokens.span()))
            }
            None => return Err(ParseError::ExpectedMoreTokens($tokens.span())),
        }
    };
}

/// A type that repreesnts an expression.
#[derive(Debug, Clone)]
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

/// A singular element access within a [`Expression::Member`].
///
/// Based on `bevy_reflect`'s `Access`.
#[derive(Debug, Clone)]
pub enum Access {
    /// A name-based field access on a struct.
    Field(String),
    /// An index-based access on a tuple.
    TupleIndex(usize),
    // /// An index-based access on a list.
    // ListIndex(usize),
}
pub enum AccessKind {
    Field,
    TupleIndex,
}
impl Access {
    pub const fn kind(&self) -> AccessKind {
        match self {
            Access::Field(_) => AccessKind::Field,
            Access::TupleIndex(_) => AccessKind::TupleIndex,
        }
    }
    /// Returns the kind of [`Access`] as a [string slice](str) with an `a` or `an` prepended to it.
    /// Used for more natural sounding error messages.
    pub const fn natural_kind(&self) -> &'static str {
        self.kind().natural()
    }
}

impl AccessKind {
    /// Returns the kind of [`Access`] as a [string slice](str) with an `a` or `an` prepended to it.
    /// Used for more natural sounding error messages.
    pub const fn natural(&self) -> &'static str {
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
            const EXPECTED_ACCESS: &[&str] = &[$(AccessKind::$variant.natural()),+];
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

impl Expression {
    pub const fn kind(&self) -> &'static str {
        match self {
            Expression::None => "nothing",
            Expression::Boolean(..) => "a boolean",
            Expression::Number(..) => "a number",
            Expression::Variable(..) => "a variable name",
            Expression::String(..) => "a string",
            Expression::Borrow(..) => "a borrow",
            Expression::Dereference(..) => "a dereference",
            Expression::Object(..) => "an object",
            Expression::StructObject { .. } => "a struct object",
            Expression::Tuple(..) => "a tuple",
            Expression::StructTuple { .. } => "a struct tuple",

            Expression::BinaryOp { .. } => "a binary operation",
            Expression::UnaryOp(..) => "a unary operation",
            Expression::Member { .. } => "a member expression",
            Expression::VarAssign { .. } => "a variable assignment",
            Expression::Function { .. } => "a function call",
            Expression::ForLoop { .. } => "a for loop",
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum ParseError {
    FailedToLexCharacter(Span),
    ExpectedMoreTokens(Span),
    ExpectedTokenButGot {
        expected: Token,
        got: Token,
        span: Span,
    },
    ExpectedEndline(Spanned<Token>),
    ExpectedLiteral(Spanned<Token>),
    InvalidSuffixForDecimal(Spanned<String>),
    NegativeIntOverflow(Span),
    PositiveIntOverflow(Span),
    ExpectObjectContinuation(Spanned<Option<Result<Token, FailedToLexCharacter>>>),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::FailedToLexCharacter(_) => write!(f, "Invalid character"),
            ParseError::ExpectedMoreTokens(_) => write!(f, "Expected more tokens, got nothing."),
            ParseError::ExpectedTokenButGot {
                expected,
                got,
                span: _,
            } => write!(f, "Expected token {expected:?}, got token {got:?} instead."),
            ParseError::ExpectedEndline(_) => write!(f, "Expected a semicolon or endline after a complete statement, but got more tokens than expected."),
            ParseError::ExpectedLiteral(Spanned { span: _, value }) => write!(f, "Expected a literal token, got {value:?} which is not a valid literal."),
            ParseError::InvalidSuffixForDecimal(_) => write!(f, ""),
            ParseError::NegativeIntOverflow(_) => todo!(),
            ParseError::PositiveIntOverflow(_) => todo!(),
            ParseError::ExpectObjectContinuation(_) => todo!(),
        }
    }
}

const FLOAT_PARSE_EXPECT_REASON: &str = "Float parse errors only occur d";
const NUMBER_TYPE_WILDCARD_UNREACHABLE_REASON: &str =
    "Lexer gurantees `NumberType`'s slice to be included one of the match arms.";

pub fn parse(tokens: &mut TokenStream, environment: &Environment) -> Result<Ast, ParseError> {
    let mut ast = Vec::new();

    while tokens.peek().is_some() {
        ast.push(parse_expression(tokens, environment)?);

        match tokens.next() {
            Some(Ok(Token::SemiColon)) => continue,
            Some(Ok(token)) => return Err(ParseError::ExpectedEndline(tokens.wrap_span(token))),
            Some(Err(FailedToLexCharacter)) => {
                return Err(ParseError::FailedToLexCharacter(tokens.span()))
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
        Some(Ok(Token::For)) => {
            let start = tokens.span().start;
            tokens.next();

            let index_name = tokens.slice().to_string();

            tokens.next();

            match tokens.next() {
                Some(Ok(Token::In)) => {}
                _ => todo!(),
            }

            let loop_count = match parse_additive(tokens, environment)?.value {
                Expression::Number(Number::u8(number)) => number as u64,
                Expression::Number(Number::u16(number)) => number as u64,
                Expression::Number(Number::u32(number)) => number as u64,
                Expression::Number(Number::u64(number)) => number,
                Expression::Number(Number::i8(number)) => number as u64,
                Expression::Number(Number::i16(number)) => number as u64,
                Expression::Number(Number::i32(number)) => number as u64,
                Expression::Number(Number::i64(number)) => number as u64,
                t => todo!("{t:?}"),
            };

            let block = parse_block(tokens, environment)?;
            let end = tokens.span().end;

            Ok(Spanned {
                value: Expression::ForLoop {
                    index_name,
                    loop_count,
                    block,
                },
                span: start..end,
            })
        }
        Some(Ok(_)) => {
            let expr = parse_additive(tokens, environment)?;

            match tokens.peek() {
                Some(Ok(Token::Equals)) => Ok(parse_var_assign(expr, tokens, environment)?),
                _ => Ok(expr),
            }
        }
        Some(Err(FailedToLexCharacter)) => {
            Err(ParseError::FailedToLexCharacter(tokens.peek_span()))
        }
        None => Err(ParseError::ExpectedMoreTokens(tokens.peek_span())),
    }
}

fn parse_block(tokens: &mut TokenStream, environment: &Environment) -> Result<Ast, ParseError> {
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
            Some(Ok(Token::Identifer)) => {
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
                            Ok(tokens.wrap_span(Expression::Variable(name)))
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
                Ok(tokens.wrap_span(Expression::String(string)))
            }
            Some(Ok(Token::Minus)) => {
                let expr = parse_literal(tokens, environment)?;
                Ok(tokens.wrap_span(Expression::UnaryOp(Box::new(expr))))
            }
            Some(Ok(Token::Ampersand)) => {
                let expr = parse_literal(tokens, environment)?;

                Ok(tokens.wrap_span(Expression::Borrow(Box::new(expr))))
            }
            Some(Ok(Token::Asterisk)) => {
                let expr = parse_literal(tokens, environment)?;
                Ok(tokens.wrap_span(Expression::Dereference(Box::new(expr))))
            }
            Some(Ok(Token::IntegerNumber)) => {
                parse_number(tokens).map(|s| s.map(Expression::Number))
            }
            Some(Ok(Token::FloatNumber)) => {
                if let Some(Ok(Token::NumberType)) = tokens.peek() {
                    let number: Number = match tokens.peek_slice() {
                        "u8" | "u16" | "u32" | "u64" | "usize" | "i8" | "i16" | "i32" | "i64"
                        | "isize" => Err(ParseError::InvalidSuffixForDecimal(
                            tokens.wrap_span(tokens.slice().to_string()),
                        ))?,
                        "f32" => {
                            Number::f32(tokens.slice().parse().expect(FLOAT_PARSE_EXPECT_REASON))
                        }
                        "f64" => {
                            Number::f64(tokens.slice().parse().expect(FLOAT_PARSE_EXPECT_REASON))
                        }
                        _ => unreachable!("{NUMBER_TYPE_WILDCARD_UNREACHABLE_REASON}"),
                    };
                    let start_span = tokens.span().end;

                    tokens.next();

                    Ok(Spanned {
                        span: start_span..tokens.span().end,
                        value: Expression::Number(number),
                    })
                } else {
                    let number = Number::Float(tokens.slice().parse().unwrap());

                    Ok(Spanned {
                        span: tokens.span(),
                        value: Expression::Number(number),
                    })
                }
            }
            Some(Ok(Token::True)) => Ok(tokens.wrap_span(Expression::Boolean(true))),
            Some(Ok(Token::False)) => Ok(tokens.wrap_span(Expression::Boolean(false))),
            Some(Ok(token)) => Err(ParseError::ExpectedLiteral(tokens.wrap_span(token))),
            Some(Err(FailedToLexCharacter)) => Err(ParseError::FailedToLexCharacter(tokens.span())),
            None => Err(ParseError::ExpectedMoreTokens(tokens.span())),
        }
    }

    let mut expr = parse_literal(tokens, environment)?;
    // If theres a dot after the expression, do a member expression:
    while let Some(Ok(Token::Dot)) = tokens.peek() {
        tokens.next(); // Skip the dot
        match tokens.next() {
            Some(Ok(Token::Identifer)) => {
                let right = tokens.slice().to_string();
                expr = Spanned {
                    span: expr.span.start..tokens.span().end,
                    value: Expression::Member {
                        left: Box::new(expr),
                        right: tokens.wrap_span(Access::Field(right)),
                    },
                };
            }
            Some(Ok(Token::IntegerNumber)) => {
                let right = tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?;
                expr = Spanned {
                    span: expr.span.start..tokens.span().end,
                    value: Expression::Member {
                        left: Box::new(expr),
                        right: tokens.wrap_span(Access::TupleIndex(right)),
                    },
                };
            }
            _ => todo!(),
        }
    }
    Ok(expr)
}

fn map_parseint_error(span: Span) -> impl Fn(std::num::ParseIntError) -> ParseError {
    move |error| match error.kind() {
        IntErrorKind::PosOverflow => ParseError::PositiveIntOverflow(span.clone()),
        IntErrorKind::NegOverflow => ParseError::NegativeIntOverflow(span.clone()),
        IntErrorKind::Empty | IntErrorKind::InvalidDigit | IntErrorKind::Zero => unreachable!(
            "Lexer makes sure other errors aren't possible. Create an bevy_dev_console issue!"
        ),
        _ => unimplemented!(), // Required due to IntErrorKind being #[non_exhaustive]
    }
}

fn parse_number(tokens: &mut TokenStream) -> Result<Spanned<Number>, ParseError> {
    if let Some(Ok(Token::NumberType)) = tokens.peek() {
        let number: Number = match tokens.peek_slice() {
            "u8" => Number::u8(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "u16" => Number::u16(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "u32" => Number::u32(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "u64" => Number::u64(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "usize" => Number::usize(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "i8" => Number::i8(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "i16" => Number::i16(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "i32" => Number::i32(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "isize" => Number::isize(
                tokens
                    .slice()
                    .parse()
                    .map_err(map_parseint_error(tokens.span()))?,
            ),
            "f32" => Number::f32(tokens.slice().parse().expect(FLOAT_PARSE_EXPECT_REASON)),
            "f64" => Number::f64(tokens.slice().parse().expect(FLOAT_PARSE_EXPECT_REASON)),
            _ => unreachable!("{}", NUMBER_TYPE_WILDCARD_UNREACHABLE_REASON),
        };
        let start_span = tokens.span().end;
        tokens.next();

        Ok(Spanned {
            span: start_span..tokens.span().end,
            value: number,
        })
    } else {
        let number = Number::Integer(tokens.slice().parse().unwrap());

        Ok(Spanned {
            span: tokens.span(),
            value: number,
        })
    }
}

fn parse_var_assign(
    name: Spanned<Expression>,
    tokens: &mut TokenStream<'_>,
    environment: &Environment,
) -> Result<Spanned<Expression>, ParseError> {
    tokens.next(); // We already know that the next token is an equals

    let value = parse_additive(tokens, environment)?;

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
    while let Some(Ok(Token::Identifer)) = tokens.peek() {
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
            token => Err(ParseError::ExpectObjectContinuation(
                tokens.wrap_span(token.clone()),
            ))?,
        }
    }
    expect!(tokens, Token::RightBracket);
    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::super::lexer::TokenStream;
    use super::super::Environment;
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
