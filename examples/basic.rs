//! A simple example showing how to setup the developer console plugin.

use bevy::log::{DEFAULT_FILTER, LogPlugin};
use bevy::prelude::*;
use bevy_dev_console::prelude::*;

fn main() {
    App::new()
        .add_plugins((
            // Add the log plugin with the custom log layer
            DefaultPlugins.set(LogPlugin {
                // Add a filter to the log plugin that shows all log levels from this example
                filter: format!("{DEFAULT_FILTER},{}=trace", module_path!()),
                ..console_log_plugin()
            }),
            // Add the dev console plugin itself.
            DevConsolePlugin,
        ))
        .add_systems(Startup, test)
        .run();
}

fn test(mut commands: Commands) {
    commands.spawn(Camera2d);

    trace!("tracing");
    debug!("solving {}...", nu_ansi_term::Color::Red.paint("issues"));
    info!("hello :)");
    warn!("spooky warning");
    error!("scary error");
}
