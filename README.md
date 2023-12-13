# bevy_dev_console

`bevy_dev_console` is a [Source](https://en.wikipedia.org/wiki/Source_(game_engine))-like developer console plugin for the [Bevy Game Engine](https://github.com/bevyengine/bevy).

![Image of the developer console](doc/console.png)

> **Warning:** `bevy_dev_console` is currently in its early development stages. Expect breaking changes in the near future (especially when using the built-in command parser). For this reason its only available as a git package at the moment.

## Usage

1. Add the `bevy_dev_console` git package.
```bash
cargo add --git https://github.com/doonv/bevy_dev_console.git
```
2. Import the `prelude`.
```rs
use bevy_dev_console::prelude::*;
```
3. Add the plugins.
```rs
App::new()
    .add_plugins((
        // Start capturing logs before the default plugins initiate.
        ConsoleLogPlugin::default(),
        // Add the default plugins without the LogPlugin.
        // Not removing the LogPlugin will cause a panic!
        DefaultPlugins.build().disable::<LogPlugin>(),
        // Add the dev console plugin itself.
        DevConsolePlugin,
    ))
```
4. That should be it! You can now press the `` ` `` or `~` key on your keyboard and it should open the console!

## Features

`builtin-parser (default)` allows you to optionally remove the built-in parser and replace it with your own.

## Bevy Compatibility


| bevy   | bevy_dev_console |
| ------ | ---------------- |
| 0.12.* | 0.1.0            |