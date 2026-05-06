//! `bevy_dev_console` is an experimental developer console plugin for the [Bevy Game Engine](https://bevy.org).
//!
//! ![Image of the developer console](https://raw.githubusercontent.com/doonv/bevy_dev_console/master/doc/console.png)
//!
//! <div class="warning">
//!     <b>Warning</b>
//!     <br>
//!     <code>bevy_dev_console</code> is currently in its early development stages. Expect breaking changes in the near future
//!     (especially when using the built-in command parser). For this reason its only available as a git package at the moment.
//! </div>
//!
//! ## Features
//!
//! - Log viewing
//!   - View all the hidden data from any log message by hovering over it.
//! - Powerful Built-in parser language built specifically for `bevy_dev_console`. Take a look at the [docs](builtin_parser::docs) for how to use it!
//!   - Calculations
//!   - Variables
//!     - Uses a simplified version of ownership and borrowing
//!   - Standard library (Doesn't have much at the moment)
//!   - - [Custom native functions](https://github.com/doonv/bevy_dev_console/blob/master/examples/custom_functions.rs) are written just like regular rust functions! ([`World`] access included!)
//!   - [Many types](https://github.com/doonv/bevy_dev_console/wiki/Built%E2%80%90in-Parser#types)
//!   - Resource viewing and modification
//!     - Enums
//!     - Structs
//!   - ~~Entity queries~~ [*Coming Soon...*](https://github.com/doonv/bevy_dev_console/issues/3) (Syntax suggestions would be appreciated!)
//!   - ...and more!
//!
//! ## Usage
//!
//! 1. Add the `bevy_dev_console` git package.
//!
//!     ```bash
//!     cargo add --git https://github.com/doonv/bevy_dev_console.git
//!     ```
//!
//! 2. Import the [`prelude`].
//!
//!     ```rust
//!     use bevy_dev_console::prelude::*;
//!     ```
//!
//! 3. Add the plugins.
//!
//!     ```rust,no_run
//!     use bevy::prelude::*;
//!     use bevy_dev_console::prelude::*;
//!
//!     App::new()
//!         .add_plugins((
//!             DefaultPlugins.set(console_log_plugin()), // Don't forget to set the LogPlugin
//!             DevConsolePlugin,
//!         ))
//!         .run();
//!     ```
//!
//! 4. That should be it! You can now press the <kbd>\`</kbd> / <kbd>~</kbd> key on your keyboard and it should open the console!
//!
//! ## Toggleable Features
//!
//! `builtin-parser` **(default)** - Includes the default parser. Disabling this allows you to remove the built-in parser and replace it with your own (or you could do nothing and make the console into a log reader).
//!
//! ## Bevy Compatibility
//!
//! | bevy   | bevy_dev_console |
//! | ------ | ---------------- |
//! | 0.18.* | git (master)     |

// #![warn(clippy::pedantic)]
// #![warn(clippy::nursery)]
// #![allow(clippy::needless_pass_by_value)]
// #![allow(clippy::too_many_lines)]
// #![allow(clippy::use_self)]
// #![allow(clippy::items_after_statements)]
#![allow(internal_features)]
#![cfg_attr(any(docsrs, docsrs_dep), feature(rustdoc_internals))]
#![cfg_attr(test, feature(box_patterns))]

use bevy::prelude::*;
use bevy_egui::prelude::*;
use config::ConsoleConfig;
use ui::ConsoleUiState;

#[cfg(debug_assertions)]
use crate::logging::LogMessage;

#[cfg(feature = "builtin-parser")]
pub mod builtin_parser;
pub mod command;
pub mod config;
pub mod logging;
pub mod prelude;
pub mod ui;

/// Adds a Developer Console to your Bevy application.
///
/// ## Usage
///
/// ```rust,no_run
/// # use bevy::prelude::*;
/// use bevy_dev_console::prelude::*;
///
/// App::new()
///     .add_plugins((
///         DefaultPlugins.set(console_log_plugin()), // Don't forget to set the LogPlugin
///         DevConsolePlugin
///     ))
///     .run();
/// ```
pub struct DevConsolePlugin;
impl Plugin for DevConsolePlugin {
    fn build(&self, app: &mut App) {
        if !app.is_plugin_added::<EguiPlugin>() {
            app.add_plugins(EguiPlugin::default());
        }

        #[cfg(feature = "builtin-parser")]
        {
            app.init_non_send_resource::<builtin_parser::Environment>();
            app.init_resource::<command::DefaultCommandParser>();

            #[cfg(feature = "builtin-parser-completions")]
            app.init_resource::<builtin_parser::completions::EnvironmentCache>();
        }

        #[cfg(feature = "completions")]
        app.init_resource::<command::AutoCompletions>();

        app.init_resource::<ConsoleUiState>()
            .init_resource::<ConsoleConfig>()
            .add_systems(Update, ui::read_logs)
            .add_systems(
                EguiPrimaryContextPass,
                ui::render_ui_system.run_if(toggle()),
            );
    }

    fn finish(&self, app: &mut App) {
        debug_assert!(
            app.world().contains_resource::<Messages<LogMessage>>(),
            "`LogMessage` message not initialized. `DevConsolePlugin` requires `bevy::log::LogPlugin::custom_layer` be \
            set to `bevy_dev_console::logging::console_log_layer`. See `bevy_dev_consoles`'s examples for more info."
        );
    }
}

fn toggle() -> impl FnMut(Res<ButtonInput<KeyCode>>, Res<ConsoleConfig>) -> bool + Clone {
    let mut active = false;
    move |inputs: Res<ButtonInput<KeyCode>>, config: Res<ConsoleConfig>| {
        active ^= inputs.just_pressed(config.open_key);
        active
    }
}
