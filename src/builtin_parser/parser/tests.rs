use crate::builtin_parser::Spanned;
use crate::builtin_parser::parser::Expression;
use logos::Span;

use super::super::Environment;
use super::super::lexer::TokenStream;
use crate::builtin_parser::Number;
use crate::builtin_parser::parser::BinaryOperator::*;
use std::assert_matches;

fn setup(src: &str) -> std::vec::IntoIter<Spanned<Expression>> {
    let mut lexer = TokenStream::new(src);
    let environment = Environment::default();

    let ast = super::parse(&mut lexer, &environment).unwrap();

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
                Expression::VarAssign {
                    name:
                        box Spanned {
                            span: Span { start: 0, end: 1 },
                            value: Expression::Variable(_),
                        },
                    value:
                        box Spanned {
                            span: Span { start: 4, end: 18 },
                            value:
                                Expression::BinaryOp {
                                    left:
                                        box Spanned {
                                            span: Span { start: 4, end: 14 },
                                            value:
                                                Expression::BinaryOp {
                                                    left:
                                                        box Spanned {
                                                            span: Span { start: 4, end: 9 },
                                                            value:
                                                                Expression::BinaryOp {
                                                                    left:
                                                                        box Spanned {
                                                                            span:
                                                                                Span {
                                                                                    start: 4,
                                                                                    end: 5,
                                                                                },
                                                                            value:
                                                                                Expression::Number(Number::Integer(crate::builtin_parser::number::Integer::Signed(crate::builtin_parser::number::SignedInteger::Unspecified(1)))),
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
                                                                                Expression::Number(Number::Integer(crate::builtin_parser::number::Integer::Signed(crate::builtin_parser::number::SignedInteger::Unspecified(2)))),
                                                                        },
                                                                },
                                                        },
                                                    operator: Sub,
                                                    right:
                                                        box Spanned {
                                                            span: Span { start: 12, end: 14 },
                                                            value: Expression::Number(Number::Integer(crate::builtin_parser::number::Integer::Signed(crate::builtin_parser::number::SignedInteger::Unspecified(30)))),
                                                        },
                                                },
                                        },
                                    operator: Add,
                                    right:
                                        box Spanned {
                                            span: Span { start: 17, end: 18 },
                                            value: Expression::Variable(_),
                                        },
                                },
                        },
                },
        })
    );
    assert!(stmts.next().is_none());
}
