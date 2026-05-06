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
        .add_systems(Update, loopa)
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

fn loopa(time: Res<Time>, mut timer: Local<Option<Timer>>) {
    if timer
        .get_or_insert(Timer::from_seconds(0.5, TimerMode::Repeating))
        .tick(time.delta())
        .just_finished()
    {
        info!("a");
    }
}
