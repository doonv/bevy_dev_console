//! A simple example

use bevy::log::{Level, LogPlugin};
use bevy::prelude::*;
use bevy_dev_console::prelude::*;

fn main() {
    App::new()
        .add_plugins((
            // Start capturing logs before the default plugins initiate.
            // Also append a filter that shows `TRACE` logs from this module.
            ConsoleLogPlugin::default().append_filter(module_path!(), Level::TRACE),
            // Add the default plugins without the LogPlugin.
            // Not removing the LogPlugin will cause a panic!
            DefaultPlugins.build().disable::<LogPlugin>(),
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
