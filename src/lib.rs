#![doc = include_str!("../README.md")]

use bevy::prelude::*;
use bevy_egui::EguiPlugin;
use command::CommandHints;
use config::ConsoleConfig;
use ui::ConsoleUiState;

#[cfg(feature = "builtin-parser")]
pub mod builtin_parser;
pub mod command;
pub mod config;
pub mod logging;
pub mod prelude;
pub mod ui;

/// Adds a Developer Console to your Bevy application.
///
/// Requires [custom_log_layer](logging::custom_log_layer).
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
            #[cfg(feature = "builtin-parser-completions")]
            app.init_resource::<builtin_parser::completions::EnvironmentCache>();
        }
        #[cfg(feature = "completions")]
        app.init_resource::<command::AutoCompletions>();

        app.init_resource::<ConsoleUiState>()
            .init_resource::<CommandHints>()
            .init_resource::<ConsoleConfig>()
            .register_type::<ConsoleConfig>()
            .add_systems(
                Update,
                (
                    ui::read_logs,
                    (
                        ui::open_close_ui,
                        ui::render_ui.run_if(|s: Res<ConsoleUiState>| s.open),
                    )
                        .chain(),
                ),
            );
    }
}
