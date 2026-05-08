//! An example showing how to create custom functions

use std::time;

use bevy::prelude::*;
use bevy_dev_console::builtin_parser::{
    Diagnostic, Environment, EvalError, Number, Spanned, Value,
};
use bevy_dev_console::prelude::*;
use bevy_dev_console::register;

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
fn add_generic(
    num1: Spanned<Number>,
    num2: Spanned<Number>,
) -> Result<Number, Diagnostic<EvalError>> {
    num1 + num2 // Adding two Spanned Numbers automatically returns the correct diagnostic for you.
}

/// Function with any value + span
fn print_debug_info(Spanned { span, value }: Spanned<Value>) {
    info!("Location of command: {span:?}, Value: {value:?}");
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

// Variable argument function
fn count_args(args: Vec<i32>) -> usize {
    args.len()
}

// For more examples take a look at the [standard library](https://github.com/doonv/bevy_dev_console/blob/master/src/builtin_parser/runner/stdlib.rs).

// Register our functions by creating and inserting our own environment
fn custom_environment() -> Environment {
    let mut environment = Environment::default();

    // The register macro allows us to easily add functions to
    // the environment without needing to specify the name twice.
    register!(&mut environment => {
        fn time_since_epoch;
        fn add;
        fn add_generic;
        fn print_debug_info;
        fn add_to_global_counter;
        fn toggle_bool;
        fn count_args;
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
