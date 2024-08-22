//! Command execution functionality.

use std::borrow::Cow;
use std::ops::Range;

use bevy::ecs::world::Command;
use bevy::prelude::*;

/// The command parser currently being used by the dev console.
#[derive(Resource)]
pub struct DefaultCommandParser(pub Box<dyn CommandParser>);

impl DefaultCommandParser {
    /// Shortcut method for calling `parser.0.parse(command, world)`.
    #[inline]
    pub fn parse(&self, command: &str, world: &mut World) {
        self.0.parse(command, world)
    }
    /// Shortcut method for calling `parser.0.completion(command, world)`.
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

/// A hint displayed to the user when they make a mistake.
#[derive(Debug, Clone)]
pub struct CommandHint {
    /// The color of the hint.
    pub color: CommandHintColor,
    /// The location of the hint in the command.
    pub span: Range<usize>,
    /// Additional information about the hint when hovered over.
    /// (Doesn't do anything atm)
    pub description: Cow<'static, str>,
}
impl CommandHint {
    /// Creates a new [`CommandHint`].
    pub fn new(
        span: Range<usize>,
        color: CommandHintColor,
        description: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self {
            color,
            span,
            description: description.into(),
        }
    }
}

/// The color of a [`CommandHint`], may either be a standard color or a [`Custom`](CommandHintColor::Custom) [`Color`].
#[derive(Debug, Clone)]
pub enum CommandHintColor {
    /// An error marks bad code that cannot be recovered from.
    ///
    /// Usually colored red.
    Error,
    /// A warning marks code that could cause problems in the future.
    ///
    /// Usually colored yellow.
    Warning,
    /// A hint marks code that is questionable, but is otherwise fine.
    ///
    /// Usually colored blue.
    Hint,
    /// This marks code that could be improved.
    ///
    /// Usually colored green.
    Help,
    /// A custom color of your choice! This is usually not recommended as
    /// you're much better off using the standard colors.
    Custom(Color),
}

/// A resource where hints (errors/warnings/etc) are stored
/// to be displayed in the developer console.
#[derive(Resource, Debug, Default, Deref)]
pub struct CommandHints {
    #[deref]
    hints: Vec<Vec<CommandHint>>,
    hint_added: bool,
}
impl CommandHints {
    /// Push a list of hints. This should be done once per command call.
    pub fn push(&mut self, hints: impl Into<Vec<CommandHint>>) {
        if self.hint_added {
            warn!(
                "Hints were added twice! Hint 1: {:?}, Hint 2: {:?}",
                self.hints.last(),
                hints.into()
            )
        } else {
            self.hint_added = true;
            self.hints.push(hints.into());
        }
    }
    pub(crate) fn reset_hint_added(&mut self) {
        if !self.hint_added {
            self.push([]);
        }
        self.hint_added = false;
    }
}

/// The trait that all [`CommandParser`]s implement.
/// You can take a look at the [builtin parser](crate::builtin_parser) for an advanced example.
///
/// ```
/// # use bevy::ecs::world::World;
/// # use bevy_dev_console::command::CommandParser;
/// # use bevy::log::info;
/// # use bevy_dev_console::ui::COMMAND_RESULT_NAME;
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
    #[inline]
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
        if let Some(parser) = world.remove_resource::<DefaultCommandParser>() {
            parser.parse(&self.0, world);
            world.insert_resource(parser);
        } else {
            error!("Default command parser doesn't exist, cannot execute command.");
        }
    }
}

#[derive(Resource, Default, Deref, DerefMut)]
#[cfg(feature = "completions")]
pub struct AutoCompletions(pub(crate) Vec<CompletionSuggestion>);
#[cfg(feature = "completions")]
pub(crate) struct UpdateAutoComplete(pub String);
#[cfg(feature = "completions")]
impl Command for UpdateAutoComplete {
    fn apply(self, world: &mut World) {
        if let Some(parser) = world.remove_resource::<DefaultCommandParser>() {
            let completions = parser.completion(&self.0, world);
            world.resource_mut::<AutoCompletions>().0 = completions;
            world.insert_resource(parser);
        }
    }
}
