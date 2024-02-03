#![doc = include_str!("../README.md")]
#![cfg_attr(test, feature(box_patterns))]

use bevy::prelude::*;
use bevy_egui::EguiPlugin;
use command::CommandHints;
use config::ConsoleConfig;
use ui::ConsoleUiState;

#[cfg(feature = "builtin-parser")]
pub mod builtin_parser;
pub mod command;
pub mod config;
mod logging;
pub mod prelude;
pub mod ui;

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
            .init_resource::<CommandHints>()
            .init_resource::<ConsoleConfig>()
            .register_type::<ConsoleConfig>()
            .add_systems(
                Update,
                (
                    ui::read_logs,
                    ui::open_close_ui,
                    ui::render_ui.run_if(|s: Res<ConsoleUiState>| s.open),
                ),
            );
    }
}
