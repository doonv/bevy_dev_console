//! Generates an abstract syntax tree from a list of tokens.

mod ast;
mod error;
#[cfg(test)]
mod tests;

pub use ast::*;
pub use error::*;

use logos::Span;
use std::collections::HashMap;
use std::num::IntErrorKind;

use crate::builtin_parser::number::{
    Float, NumberKind, SignedInteger, SignedIntegerKind, UnsignedInteger, UnsignedIntegerKind,
};

use super::lexer::{FailedToLexCharacter, Token, TokenStream};
use super::number::Number;
use super::runner::environment::Function;
use super::{Diagnostic, Environment, SpanExtension, Spanned};

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
