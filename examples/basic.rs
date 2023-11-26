use bevy::{log::LogPlugin, prelude::*};
use bevy_dev_console::prelude::*;

fn main() {
    App::new()
        .add_plugins((
            bevy_dev_console::prelude::LogPlugin::default(),
            DefaultPlugins.build().disable::<LogPlugin>(),
            DevConsolePlugin,
        ))
        .add_systems(Startup, test)
        .run();
}

pub fn test() {
    trace!("tracing");
    debug!("solving issues...");
    info!("hello :)");
    warn!("spooky warning");
    error!("scary error");
}
