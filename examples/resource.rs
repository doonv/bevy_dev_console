use bevy::{log::LogPlugin, prelude::*};
use bevy_dev_console::prelude::*;

#[derive(Resource, Reflect, Default, Debug)]
enum MyEnum {
    #[default]
    None,
    Numero1,
    Structio {
        a: f64,
        b: f64,
    },
    Tupleo(String, f64),
}

#[derive(Resource, Reflect, Default, Debug)]
struct MyStruct {
    number1: f64,
    number2: f64,
    string: String,
}

fn main() {
    App::new()
        .register_type::<MyEnum>()
        .init_resource::<MyEnum>()
        .register_type::<MyStruct>()
        .insert_resource(MyStruct {
            number1: 52138.0,
            number2: -123.8,
            string: "hi there :)".to_string(),
        })
        .add_plugins((
            bevy_dev_console::prelude::LogPlugin::default(),
            DefaultPlugins.build().disable::<LogPlugin>(),
            DevConsolePlugin,
        ))
        .run();
}
