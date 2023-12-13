//! Command execution functionality.

use bevy::{ecs::system::Command, prelude::*};

/// The command parser currrently being used by the dev console.
#[derive(Resource)]
pub struct DefaultCommandParser(pub Box<dyn CommandParser>);

impl DefaultCommandParser {
    /// Shortcut method for calling `parser.0.parse(command, world)`.
    pub fn parse(&self, command: &str, world: &mut World) {
        self.0.parse(command, world)
    }
}

/// The trait that all [`CommandParser`]s implement.
/// You can take a look at the [builtin parser](crate::builtin_parser) for an example.
///
/// ```
/// # use bevy::ecs::world::World;
/// # use bevy_dev_console::command::CommandParser;
/// # use bevy::log::info;
///
/// pub struct MyCustomParser;
/// impl CommandParser for MyCustomParser {
///     fn parse(&self, command: &str, world: &mut World) {
///         // The `name: "console_result"` tells the console this is a result from
///         // the parser and then formats it accordingly.
///         info!(name: "console_result", "You just entered the command {command}")
///     }
/// }
/// ```
pub trait CommandParser: Send + Sync {
    /// The method called by the developer console when a command is ran.
    fn parse(&self, command: &str, world: &mut World);
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
