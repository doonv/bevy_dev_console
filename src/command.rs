//! Command execution functionality.

use bevy::prelude::*;
use std::ops::Range;

/// Identifier for log messages that show a previous command.
pub const COMMAND_MESSAGE_NAME: &str = "console_command";
/// Identifier for log messages that show the result of a command.
pub const COMMAND_RESULT_NAME: &str = "console_result";

/// Formats a command with ANSI highlights for errors.
#[must_use]
pub fn format_command_with_hints(command: &str, spans: &[Range<usize>]) -> String {
    let mut result = String::new();
    let mut last_end = 0;

    let mut sorted_spans = spans.to_vec();
    sorted_spans.sort_by_key(|s| s.start);

    for span in sorted_spans {
        if span.start > last_end {
            result.push_str(&command[last_end..span.start]);
        }

        const RED_UNDERLINE: anstyle::Style = anstyle::Style::new()
            .effects(anstyle::Effects::CURLY_UNDERLINE)
            .underline_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Red)));

        let highlighted = format!("{RED_UNDERLINE}{}{RED_UNDERLINE:#}", &command[span.clone()]);
        result.push_str(&highlighted);
        last_end = span.end;
    }
    if last_end < command.len() {
        result.push_str(&command[last_end..]);
    }
    result
}

/// The command parser currently being used by the dev console.
#[derive(Resource)]
pub struct DefaultCommandParser(pub Box<dyn CommandParser>);

impl DefaultCommandParser {
    /// Shortcut method for calling [`parser.0.parse(command, world)`](CommandParser::parse).
    #[inline]
    pub fn parse(&self, command: &str, world: &mut World) {
        self.0.parse(command, world);
    }
    /// Shortcut method for calling [`parser.0.completion(command, world)`](CommandParser::completion).
    #[inline]
    #[must_use]
    #[cfg(feature = "completions")]
    pub fn completion(&self, keyword: &str, world: &World) -> Vec<CompletionSuggestion> {
        self.0.completion(keyword, world)
    }
}
impl<Parser: CommandParser> From<Parser> for DefaultCommandParser {
    fn from(value: Parser) -> Self {
        Self(Box::new(value))
    }
}
impl From<Box<dyn CommandParser>> for DefaultCommandParser {
    fn from(value: Box<dyn CommandParser>) -> Self {
        Self(value)
    }
}

/// The trait that all [`CommandParser`]s implement.
/// You can take a look at the [builtin parser](crate::builtin_parser) for an advanced example.
///
/// ```
/// # use bevy::ecs::world::World;
/// # use bevy_dev_console::command::CommandParser;
/// # use bevy::log::info;
/// # use bevy_dev_console::command::COMMAND_RESULT_NAME;
///
/// pub struct MyCustomParser;
/// impl CommandParser for MyCustomParser {
///     fn parse(&self, command: &str, world: &mut World) {
///         // The `name: COMMAND_RESULT_NAME` tells the console this is a result from
///         // the parser and then formats it accordingly.
/// #       // TODO: figure out better solution for this
///         info!(name: COMMAND_RESULT_NAME, "You just entered the command {command}")
///     }
/// }
/// ```
pub trait CommandParser: Send + Sync + 'static {
    /// This method is called by the console when a command is ran.
    fn parse(&self, command: &str, world: &mut World);
    /// This method is called by the console when the command is changed.
    #[must_use]
    #[cfg(feature = "completions")]
    fn completion(&self, keyword: &str, world: &World) -> Vec<CompletionSuggestion> {
        let _ = (keyword, world);
        Vec::new()
    }
}

/// A suggestion for autocomplete.
#[cfg(feature = "completions")]
pub struct CompletionSuggestion {
    /// The suggestion string
    pub suggestion: String,
    /// The character indices of the [`suggestion`](Self::suggestion) to highlight.
    pub highlighted_indices: Vec<usize>,
}

pub(crate) struct ExecuteCommand(pub String);
impl Command for ExecuteCommand {
    fn apply(self, world: &mut World) {
        match world.remove_resource::<DefaultCommandParser>() {
            Some(parser) => {
                parser.parse(&self.0, world);
                world.insert_resource(parser);
            }
            _ => {
                error!("Default command parser doesn't exist, cannot execute command.");
            }
        }
    }
}

#[derive(Resource, Default, Deref, DerefMut)]
#[cfg(feature = "completions")]
pub(crate) struct AutoCompletions(pub(crate) Vec<CompletionSuggestion>);
