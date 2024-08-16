//! An example showing how to create custom functions

use bevy::log::LogPlugin;
use bevy::prelude::*;
use bevy_dev_console::builtin_parser::{Environment, EvalError, Number, Spanned, StrongRef, Value};
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
fn increment_global_counter(world: &mut World) -> u32 {
    world.resource_scope(|_, mut counter: Mut<MyCounter>| {
        counter.0 += 1;

        counter.0
    })
}

// Function with reference (Syntax subject to change soon)
fn increment_number(number: Spanned<StrongRef<Value>>) -> Result<(), EvalError> {
    let span = number.span;
    let mut reference = number.value.borrow_mut();
    if let Value::Number(number) = &mut *reference {
        *number = Number::add(*number, Number::Integer(1), span).unwrap();
        Ok(())
    } else {
        Err(EvalError::Custom {
            text: "Oh nooo".into(),
            span,
        })
    }
}

// For more examples take a look at the standard library.

// Register our functions by creating and inserting our own environment
fn custom_environment() -> Environment {
    let mut environment = Environment::default();

    // The register macro allows us to easily add functions to the environment.
    register!(&mut environment => {
        fn time_since_epoch;
        fn add;
        fn print_debug_info;
        fn increment_global_counter;
        fn increment_number;
    });

    environment
}

fn main() {
    App::new()
        .insert_resource(MyCounter(0))
        // Insert our new environment
        .insert_non_send_resource(custom_environment())
        .add_plugins((
            DefaultPlugins.set(LogPlugin {
                custom_layer: custom_log_layer,
                ..default()
            }),
            DevConsolePlugin,
        ))
        .run();
}
