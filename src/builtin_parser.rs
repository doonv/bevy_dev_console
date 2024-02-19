//! [`bevy_dev_console`](crate)'s built-in command parser.
//!
//! Currently the built-in command parser is in very early development.
//! It's purpose is to provide a simple, yet powerful method of modifying
//! the game world via commands.

use bevy::prelude::*;
use logos::Span;

use crate::builtin_parser::runner::ExecutionError;
use crate::command::{CommandHints, CommandParser, DefaultCommandParser};

pub(crate) mod lexer;
pub(crate) mod number;
pub(crate) mod parser;
pub(crate) mod runner;

pub use number::*;
pub use runner::environment::Environment;
pub use runner::error::EvalError;
pub use runner::unique_rc::*;
pub use runner::Value;

/// Additonal traits for span.
pub trait SpanExtension {
    /// Wrap this value with a [`Spanned`].
    #[must_use]
    fn wrap<T>(self, value: T) -> Spanned<T>;
    /// Combine two [`Span`]s into one.
    #[must_use]
    fn join(self, span: Self) -> Self;
}
impl SpanExtension for Span {
    #[inline]
    fn wrap<T>(self, value: T) -> Spanned<T> {
        Spanned { span: self, value }
    }
    #[inline]
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
impl<T> Spanned<T> {
    /// Maps a `Spanned<T>` to `Spanned<U>` by applying a function to a
    /// contained `T` value, leaving the [`Span`] value untouched.
    #[must_use]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
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
        let mut tokens = lexer::TokenStream::new(command);

        let environment = world.non_send_resource::<Environment>();
        let ast = parser::parse(&mut tokens, environment);

        dbg!(&ast);
        match ast {
            Ok(ast) => match runner::run(ast, world) {
                Ok(()) => (),
                Err(error) => {
                    if let ExecutionError::Eval(eval_error) = &error {
                        world
                            .resource_mut::<CommandHints>()
                            .push(eval_error.hints());
                    }

                    error!("{error}")
                }
            },
            Err(err) => {
                world.resource_mut::<CommandHints>().push([err.hint()]);

                error!("{err}")
            }
        }
    }
}
