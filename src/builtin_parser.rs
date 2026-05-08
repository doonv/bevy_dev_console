//! [`bevy_dev_console`](crate)'s built-in command parser.
//!
//! The built-in parser provides an experimental mini-language that can evaluate expressions, call commands, and modify resources.
//!
//! It's purpose is to provide a simple, yet powerful method of modifying
//! the game world via commands.
//!
//! Take a look at the [docs] for how to use it.

use std::fmt::Display;

use bevy::prelude::*;
use logos::Span;
use smallvec::SmallVec;

use crate::builtin_parser::parser::ParseError;
use crate::command::{
    COMMAND_MESSAGE_NAME, COMMAND_RESULT_NAME, CommandParser, DefaultCommandParser,
    format_command_with_hints,
};

/// Prefix for log messages that show a previous command.
const COMMAND_MESSAGE_PREFIX: &str = "$ ";
/// Prefix for log messages that show the result of a command.
const COMMAND_RESULT_PREFIX: &str = "> ";

#[cfg(feature = "builtin-parser-completions")]
use crate::command::CompletionSuggestion;

#[cfg(feature = "builtin-parser-completions")]
pub(crate) mod completions;
pub(crate) mod lexer;
pub(crate) mod number;
pub(crate) mod parser;
pub(crate) mod runner;

#[doc = include_str!("./builtin_parser/docs.md")]
#[cfg(doc)]
pub mod docs {}

/// A macro to test example usages of the builtin parser.
// #[cfg(doctest)] // This doesn't work right now, see https://github.com/rust-lang/rust/issues/67295
#[doc(hidden)]
#[macro_export]
macro_rules! test_builtin_parser {
    (
        $(  {
            $dollar:tt $($expr:expr)+
            $(; $($result:tt)+)?
        }
        )+
    ) => {{
        use bevy::prelude::World;
        use bevy_dev_console::builtin_parser::*;
        use bevy::prelude::AppTypeRegistry;

        fn strip_ansi(ansi: &str) -> String {
            ansitok::parse_ansi(ansi)
                .filter_map(|element| {
                    if let ansitok::ElementKind::Text = element.kind() {
                        Some(&ansi[element.range()])
                    } else {
                        None
                    }
                })
                .collect()
        }
        let mut world = World::new();
        let mut environment = Environment::default();
        let registry = AppTypeRegistry::default();

        $(
            let cmd = stringify!($($expr)+);
            let result = run(
                cmd,
                &mut world,
                &mut environment,
                &registry,
            );
            $(
                bevy_dev_console::test_builtin_parser!(
                    (result, cmd);
                    $($result)+
                );
            )?
        )+
    }};
    (
        ($got:ident, $cmd:ident);
        $( > $($expected:tt)+)?
    ) => {
        $( assert_eq!(strip_ansi(&$got.expect($cmd).unwrap()), stringify!($($expected)+)); )?
    };
    (
        ($got:ident, $cmd:ident);
        err $expected:literal
    ) => {
        assert_eq!(strip_ansi(&$got.unwrap_err().to_string()), $expected.trim().trim_start_matches("ERROR "));
    };
}

pub use number::*;
pub use runner::Value;
pub use runner::environment::Environment;
pub use runner::error::EvalError;
pub use runner::unique_rc::*;

const fn color(color: anstyle::AnsiColor) -> anstyle::Style {
    anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(color)))
}

pub(crate) const DARK: anstyle::Style = color(anstyle::AnsiColor::BrightBlack);
pub(crate) const MEMBER: anstyle::Style = color(anstyle::AnsiColor::Red);
pub(crate) const VALUE: anstyle::Style = color(anstyle::AnsiColor::Yellow);
pub(crate) const TYPE: anstyle::Style = color(anstyle::AnsiColor::BrightYellow);
pub(crate) const STRING: anstyle::Style = color(anstyle::AnsiColor::Green);
pub(crate) const FUNCTION: anstyle::Style = color(anstyle::AnsiColor::Blue);
pub(crate) const KEYWORD: anstyle::Style = color(anstyle::AnsiColor::Magenta);
pub(crate) const VARIANT: anstyle::Style = color(anstyle::AnsiColor::Cyan);

/// Additional methods for [`Span`].
pub trait SpanExtension {
    /// Wrap this value with a [`Spanned`].
    #[must_use]
    fn wrap<T>(self, value: T) -> Spanned<T>;

    /// Combine two [`Span`]s into one.
    #[must_use]
    fn join(&self, span: &Self) -> Self;

    /// Wrap an error in a [`Diagnostic`] with this [`Span`].
    fn diagnose<E: std::error::Error>(self, error: E) -> Diagnostic<E>;

    /// Adds the left and right values of the provided [`Span`] to this [`Span`].
    #[must_use]
    fn add(self, range: Span) -> Self;
}
impl SpanExtension for Span {
    #[inline]
    fn wrap<T>(self, value: T) -> Spanned<T> {
        Spanned { span: self, value }
    }

    #[inline]
    fn join(&self, span: &Self) -> Self {
        debug_assert!(self.start <= span.end);

        self.start..span.end
    }

    #[inline]
    fn diagnose<E: std::error::Error>(self, error: E) -> Diagnostic<E> {
        Diagnostic::single(self, error)
    }

    fn add(self, range: Span) -> Self {
        Span {
            start: self.start + range.start,
            end: self.end + range.end,
        }
    }
}

pub trait ErrorExtension<T, E: std::error::Error> {
    fn diagnosed(self, span: Span) -> Result<T, Diagnostic<E>>;
}
impl<T, E: Into<EvalError>> ErrorExtension<T, EvalError> for Result<T, E> {
    fn diagnosed(self, span: Span) -> Result<T, Diagnostic<EvalError>> {
        self.map_err(|e| span.diagnose(e.into()))
    }
}
impl<T, E: Into<ParseError>> ErrorExtension<T, ParseError> for Result<T, E> {
    fn diagnosed(self, span: Span) -> Result<T, Diagnostic<ParseError>> {
        self.map_err(|e| span.diagnose(e.into()))
    }
}

/// Wrapper around `T` that stores a [Span].
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

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
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

        // Can't `resource_scope` the environment because it's a non send resource.
        let mut environment = world.remove_non_send_resource::<Environment>().unwrap();
        let ast = parser::parse(&mut tokens, &environment);
        world.resource_scope(|world, registry: Mut<AppTypeRegistry>| {
            match ast {
                Ok(ast) => match runner::eval(ast, world, &mut environment, &registry) {
                    Ok(value) => {
                        info!(name: COMMAND_MESSAGE_NAME, "{DARK}{COMMAND_MESSAGE_PREFIX}{DARK:#}{command}");
                        if let Some(value) = value {
                            info!(name: COMMAND_RESULT_NAME, "{DARK}{COMMAND_RESULT_PREFIX}{DARK:#}{value}");
                        }
                    }
                    Err(error) => {
                        let highlighted = format_command_with_hints(command, &error.spans);
                        info!(name: COMMAND_MESSAGE_NAME, "{DARK}{COMMAND_MESSAGE_PREFIX}{DARK:#}{highlighted}");
                        error!("{error}");
                    }
                },
                Err(err) => {
                    let highlighted = format_command_with_hints(command, &err.spans);
                    info!(name: COMMAND_MESSAGE_NAME, "{DARK}{COMMAND_MESSAGE_PREFIX}{DARK:#}{highlighted}");
                    error!("{err}");
                }
            }
        });
        #[cfg(feature = "builtin-parser-completions")]
        {
            *world.resource_mut() = completions::store_in_cache(&environment);
        }
        world.insert_non_send_resource(environment);
    }

    #[cfg(feature = "builtin-parser-completions")]
    fn completion(&self, command: &str, world: &World) -> Vec<CompletionSuggestion> {
        use crate::builtin_parser::completions::EnvironmentCache;
        use fuzzy_matcher::FuzzyMatcher;

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

#[must_use]
#[derive(thiserror::Error, Debug)]
pub struct Diagnostic<E: std::error::Error> {
    pub spans: SmallVec<[Span; 1]>,
    #[source]
    pub error: Box<E>,
}
impl<E: std::error::Error> Diagnostic<E> {
    pub fn empty(error: E) -> Self {
        Self {
            spans: SmallVec::new(),
            error: Box::new(error),
        }
    }
    pub fn single(span: Span, error: E) -> Self {
        Self {
            spans: SmallVec::from_buf([span]),
            error: Box::new(error),
        }
    }
}

impl<E: std::error::Error> Display for Diagnostic<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.error, f)
    }
}
impl<E: std::error::Error> From<Spanned<E>> for Diagnostic<E> {
    fn from(Spanned { span, value }: Spanned<E>) -> Self {
        Self::single(span, value)
    }
}
impl<T: Into<EvalError>> From<T> for Diagnostic<EvalError> {
    fn from(value: T) -> Self {
        Diagnostic::empty(value.into())
    }
}

#[derive(thiserror::Error, Debug)]
pub enum RunError {
    #[error(transparent)]
    Parse(Diagnostic<ParseError>),
    #[error(transparent)]
    Eval(Diagnostic<EvalError>),
}

pub fn run(
    command: &str,
    world: &mut World,
    environment: &mut Environment,
    registry: &AppTypeRegistry,
) -> Result<Option<String>, RunError> {
    let mut tokens = lexer::TokenStream::new(command);

    let ast = parser::parse(&mut tokens, environment).map_err(RunError::Parse)?;
    let value = runner::eval(ast, world, environment, registry).map_err(RunError::Eval)?;

    Ok(value)
}
