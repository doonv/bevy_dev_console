use bevy::prelude::*;
use bevy_egui::EguiPlugin;
use command::Environment;
use ui::ConsoleUiState;

mod logging;
mod command;
pub mod prelude;
mod ui;

pub struct DevConsolePlugin;
impl Plugin for DevConsolePlugin {
    fn build(&self, app: &mut App) {
        if !app.is_plugin_added::<EguiPlugin>() {
            app.add_plugins(EguiPlugin);
        }

        app.init_resource::<ConsoleUiState>()
            .init_non_send_resource::<Environment>()
            .add_systems(Update, ui::ui);
    }
}
