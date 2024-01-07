//! [`bevy_dev_console`](crate)'s built-in command parser.
//!
//! Currently the built-in command parser is in very early development.
//! It's purpose is to provide a simple, yet powerful method of modifying
//! the game world via commands.

use bevy::prelude::*;
use logos::Span;

use crate::command::{CommandParser, DefaultCommandParser};

use self::lexer::TokenStream;
use self::parser::parse;

pub(crate) mod lexer;
pub(crate) mod number;
pub(crate) mod parser;
pub(crate) mod runner;

pub use number::*;
pub use runner::environment::Environment;
pub use runner::error::RunError;
pub use runner::unique_rc::*;
pub use runner::Value;

/// Additonal traits for span.
pub trait SpanExtension {
    /// Wrap this value with a [`Spanned`].
    fn wrap<T>(self, value: T) -> Spanned<T>;
    /// Combine two [`Span`]s into one.
    fn join(self, span: Self) -> Self;
}
impl SpanExtension for Span {
    fn wrap<T>(self, value: T) -> Spanned<T> {
        Spanned { span: self, value }
    }
    fn join(self, span: Self) -> Self {
        self.start..span.end
    }
}

/// Wrapper around `T` that stores a [Span] (A location in the source code)
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    /// The location of `T` in the source/command.
    pub span: Span,
    /// The value of `T`.
    pub value: T,
}

impl Default for DefaultCommandParser {
    fn default() -> Self {
        Self(Box::new(BuiltinCommandParser))
    }
}

/// [`bevy_dev_console`](crate)'s built-in command parser.
///
/// See the [module level documentation for more](self).
#[derive(Default)]
pub struct BuiltinCommandParser;
impl CommandParser for BuiltinCommandParser {
    fn parse(&self, command: &str, world: &mut World) {
        let mut tokens = TokenStream::new(command);

        let environment = world.remove_non_send_resource::<Environment>().unwrap();
        let ast = parse(&mut tokens, &environment);
        world.insert_non_send_resource(environment);

        match ast {
            Ok(ast) => {
                runner::run(ast, world);
            }
            Err(err) => error!("{err:#?}"),
        }
    }
}
