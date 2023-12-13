//! A simple exmaple

use bevy::{
    log::{Level, LogPlugin},
    prelude::*,
};
use bevy_dev_console::{
    builtin_parser::{Environment, Spanned, Value},
    prelude::*,
    register,
};

// Declare the functions we want to create:

// Basic function
fn time_since_epoch() {
    let time = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap();
    info!("The unix epoch was {} seconds ago", time.as_secs());
}

// Function with parameters and return value
fn add(num1: f64, num2: f64) -> f64 {
    num1 + num2
}

// Function with any value + span
fn print_debug_info(value: Spanned<Value>) {
    info!(
        "Location in command: {:?}, Value: {:?}",
        value.span, value.value
    )
}

// For more example take a look at the standard library.

// Register our functions by creating and inserting our own environment
fn custom_environment() -> Environment {
    let mut environment = Environment::default();

    // The register macro allows us to easily add functions to the environment.
    register!(&mut environment => {
        fn time_since_epoch;
        fn add;
        fn print_debug_info;
    });

    environment
}

fn main() {
    App::new()
        // Insert our new environment
        .insert_non_send_resource(custom_environment())
        .add_plugins((
            ConsoleLogPlugin::default().append_filter(module_path!(), Level::TRACE),
            DefaultPlugins.build().disable::<LogPlugin>(),
            DevConsolePlugin,
        ))
        .run();
}
