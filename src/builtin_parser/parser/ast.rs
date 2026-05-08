use kinded::Kinded;
use std::collections::HashMap;
use std::fmt::Display;

use crate::builtin_parser::Spanned;
use crate::builtin_parser::number::Number;

/// An [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
///
/// This type represents a list of expressions, which is what makes up a command.
pub type Ast = Vec<Spanned<Expression>>;

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
