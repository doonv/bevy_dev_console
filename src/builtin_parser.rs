//! [`bevy_dev_console`](crate)'s built-in command parser.
//!
//! Currently the built-in command parser is in very early development.
//! It's purpose is to provide a simple, yet powerful method of modifying
//! the game world via commands.

use bevy::prelude::*;
use logos::Span;

use crate::builtin_parser::runner::ExecutionError;
use crate::command::{
    format_command_with_hints, CommandParser, DefaultCommandParser, COMMAND_MESSAGE_NAME,
    COMMAND_MESSAGE_PREFIX,
};

#[cfg(feature = "builtin-parser-completions")]
use crate::command::CompletionSuggestion;

#[cfg(feature = "builtin-parser-completions")]
pub(crate) mod completions;
pub(crate) mod lexer;
pub(crate) mod number;
pub(crate) mod parser;
pub(crate) mod runner;

pub use number::*;
pub use runner::Value;
pub use runner::environment::Environment;
pub use runner::error::EvalError;
pub use runner::unique_rc::*;

/// Additional traits for span.
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
    /// Maps a [`Spanned<T>`] to [`Spanned<U>`] by applying a function to the
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

        match ast {
            Ok(ast) => match runner::run(ast, world) {
                Ok(()) => {
                    info!(name: COMMAND_MESSAGE_NAME, "{COMMAND_MESSAGE_PREFIX}{command}");
                }
                Err(error) => {
                    let spans = if let ExecutionError::Eval(eval_error) = &error {
                        eval_error.spans()
                    } else {
                        vec![]
                    };
                    let highlighted = format_command_with_hints(command, &spans);
                    info!(name: COMMAND_MESSAGE_NAME, "{COMMAND_MESSAGE_PREFIX}{highlighted}");
                    error!("{error}");
                }
            },
            Err(err) => {
                let highlighted = format_command_with_hints(command, &[err.span()]);
                info!(name: COMMAND_MESSAGE_NAME, "{COMMAND_MESSAGE_PREFIX}{highlighted}");
                error!("{err}");
            }
        }
        #[cfg(feature = "builtin-parser-completions")]
        {
            *world.resource_mut() =
                completions::store_in_cache(world.non_send_resource::<Environment>());
        }
    }

    #[cfg(feature = "builtin-parser-completions")]
    fn completion(&self, command: &str, world: &World) -> Vec<CompletionSuggestion> {
        use fuzzy_matcher::FuzzyMatcher;

        use crate::builtin_parser::completions::EnvironmentCache;

        let matcher = fuzzy_matcher::skim::SkimMatcherV2::default();
        let environment_cache = world.resource::<EnvironmentCache>();

        let mut names: Vec<_> = environment_cache
            .function_names
            .iter()
            .chain(environment_cache.variable_names.iter())
            .cloned()
            .chain(
                world
                    .resource::<AppTypeRegistry>()
                    .read()
                    .iter()
                    .filter(|&v| world.components().get_resource_id(v.type_id()).is_some())
                    .map(|v| v.type_info().type_path_table().short_path().to_owned()),
            )
            .map(|name| (matcher.fuzzy_indices(&name, command), name))
            .filter_map(|(fuzzy, name)| fuzzy.map(|v| (v, name)))
            .collect();

        names.sort_by_key(|((score, _), _)| std::cmp::Reverse(*score));
        names.truncate(crate::ui::MAX_COMPLETION_SUGGESTIONS);

        names
            .into_iter()
            .map(|((_, indices), name)| CompletionSuggestion {
                suggestion: name,
                highlighted_indices: indices,
            })
            .collect()
    }
}
