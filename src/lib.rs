//! `bevy_dev_console` is a [Source](https://en.wikipedia.org/wiki/Source_(game_engine))-like
//! developer console plugin for the [Bevy Game Engine](https://github.com/bevyengine/bevy).
//!
//! `bevy_dev_console` is currently in its early development stages.
//! Expect breaking changes in the near future (espically when using the built-in command parser).
//! For this reason its only available as a git package at the moment.
//!
//! ## Example
//!
//! ```no_run
//! use bevy::prelude::*;
//! use bevy_dev_console::prelude::*;
//!
//! fn main() {
//!     App::new()
//!         .add_plugins((
//!             ConsoleLogPlugin::default(),
//!             DefaultPlugins.build().disable::<bevy::log::LogPlugin>(),
//!             DevConsolePlugin,
//!         ))
//!         .run();
//! }
//! ```

use bevy::prelude::*;
use bevy_egui::EguiPlugin;
use ui::ConsoleUiState;

#[cfg(feature = "builtin-parser")]
pub mod builtin_parser;
pub mod command;
mod logging;
pub mod prelude;
mod ui;

/// Adds a Developer Console to your Bevy application.
///
/// Requires [ConsoleLogPlugin](logging::log_plugin::ConsoleLogPlugin).
pub struct DevConsolePlugin;
impl Plugin for DevConsolePlugin {
    fn build(&self, app: &mut App) {
        if !app.is_plugin_added::<EguiPlugin>() {
            app.add_plugins(EguiPlugin);
        }

        #[cfg(feature = "builtin-parser")]
        {
            app.init_non_send_resource::<builtin_parser::Environment>();
            app.init_resource::<command::DefaultCommandParser>();
        }

        app.init_resource::<ConsoleUiState>()
            .add_systems(Update, ui::ui);
    }
}
