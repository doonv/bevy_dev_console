//! Example of modifying resources via the console via reflection.
//!
//! To use, start by typing `MyStruct` into the dev console. Then try modifying it!
//!
//! **Warning:** This is very experimental, might not work.

use bevy::prelude::*;
use bevy_dev_console::prelude::*;

#[derive(Resource, Reflect, Default, Debug)]
enum MyEnum {
    #[default]
    None,
    Numero1,
    Structio {
        a: f64,
        b: String,
    },
    Tupleo(String, f64),
}

#[derive(Resource, Reflect, Default, Debug)]
struct MyStruct {
    number: f64,
    string: String,
    struct_in_struct: SubStruct,
    tuple: (i32, u8),
}

#[derive(Reflect, Default, Debug)]
struct SubStruct {
    boolean: bool,
    enume: MyEnum,
}

fn main() {
    App::new()
        .init_resource::<MyEnum>()
        .insert_resource(MyStruct {
            number: 5.6,
            string: "hi there :)".to_owned(),
            struct_in_struct: SubStruct {
                boolean: false,
                enume: MyEnum::Tupleo("nooo".to_owned(), 5.),
            },
            tuple: (-5, 255),
        })
        .add_plugins((DefaultPlugins.set(console_log_plugin()), DevConsolePlugin))
        .add_systems(Startup, spawn_camera)
        .run();
}

fn spawn_camera(mut commands: Commands) {
    commands.spawn(Camera2d);
}
