//! An example showing how to create custom functions

use bevy::prelude::*;
use bevy_dev_console::builtin_parser::{
    Environment, EvalError, Number, SpanExtension, Spanned, Value,
};
use bevy_dev_console::prelude::*;
use bevy_dev_console::register;
use web_time as time;

// Declare the functions we want to create:

/// Basic function
fn time_since_epoch() {
    let time = time::SystemTime::now()
        .duration_since(time::UNIX_EPOCH)
        .unwrap();
    info!("The unix epoch was {} seconds ago", time.as_secs());
}

/// Function with parameters and return value.
///
/// Note that this will cause an error if an integer is passed onto this function.
fn add(num1: f64, num2: f64) -> f64 {
    num1 + num2
}

/// A "Generic" function that works with any [`Number`] type.
fn add_generic(num1: Spanned<Number>, num2: Spanned<Number>) -> Result<Number, EvalError> {
    Number::add(num1.value, num2.value, num1.span.join(num2.span))
}

/// Function with any value + span
fn print_debug_info(value: Spanned<Value>) {
    info!(
        "Location in command: {:?}, Value: {:?}",
        value.span, value.value
    )
}

#[derive(Resource)]
struct MyCounter(u32);

/// Function with [`World`]
fn add_to_global_counter(num: u32, world: &mut World) -> u32 {
    let mut counter = world.resource_mut::<MyCounter>();

    counter.0 += num;

    counter.0
}

// Function with reference
fn toggle_bool(value: &mut bool) {
    *value = !*value;
}

// For more examples take a look at the standard library.

// Register our functions by creating and inserting our own environment
fn custom_environment() -> Environment {
    let mut environment = Environment::default();

    // The register macro allows us to easily add functions to the environment.
    register!(&mut environment => {
        fn time_since_epoch;
        fn add;
        fn add_generic;
        fn print_debug_info;
        fn add_to_global_counter;
        fn toggle_bool;
    });

    environment
}

fn main() {
    App::new()
        .insert_resource(MyCounter(0))
        // Insert our new environment
        .insert_non_send_resource(custom_environment())
        .add_plugins((DefaultPlugins.set(console_log_plugin()), DevConsolePlugin))
        .add_systems(Startup, spawn_camera)
        .run();
}

fn spawn_camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}
