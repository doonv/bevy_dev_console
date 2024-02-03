//! Example of modifying resources via the console via reflection.
//!
//! **Warning:** This is very experimental, might not work.

use bevy::log::LogPlugin;
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
        .register_type::<MyEnum>()
        .init_resource::<MyEnum>()
        .insert_resource(MyStruct {
            number: 5.6,
            string: "hi there :)".to_string(),
            struct_in_struct: SubStruct {
                boolean: false,
                enume: MyEnum::Tupleo("nooo".to_string(), 5.),
            },
            tuple: (-5, 255),
        })
        .register_type::<MyStruct>()
        .add_plugins((
            ConsoleLogPlugin::default(),
            DefaultPlugins.build().disable::<LogPlugin>(),
            DevConsolePlugin,
        ))
        .run();
}
