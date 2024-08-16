//! A simple example showing how to setup the developer console plugin.

use bevy::log::LogPlugin;
use bevy::prelude::*;
use bevy_dev_console::prelude::*;

fn main() {
    App::new()
        .add_plugins((
            // Add the log plugin with the custom log layer
            DefaultPlugins.set(LogPlugin {
                custom_layer: custom_log_layer,
                // Add a filter to the log plugin that shows all log levels from this example
                filter: format!("wgpu=error,naga=warn,{}=trace", module_path!()),
                ..default()
            }),
            // Add the dev console plugin itself.
            DevConsolePlugin,
        ))
        .add_systems(Startup, test)
        .run();
}

fn test() {
    trace!("tracing");
    debug!("solving issues...");
    info!("hello :)");
    warn!("spooky warning");
    error!("scary error");
}
