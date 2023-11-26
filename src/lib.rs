use bevy::prelude::*;
use bevy_egui::EguiPlugin;
use parser::Environment;
use ui::ConsoleUiState;

mod logging;
mod parser;
pub mod prelude;
mod ui;

pub struct DevConsolePlugin;
impl Plugin for DevConsolePlugin {
    fn build(&self, app: &mut App) {
        if app.is_plugin_added::<EguiPlugin>() == false {
            app.add_plugins(EguiPlugin);
        }

        app.init_resource::<ConsoleUiState>()
            .init_non_send_resource::<Environment>()
            .add_systems(Update, ui::ui);
    }
}
