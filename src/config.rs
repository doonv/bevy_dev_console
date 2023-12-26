//! Configuration structs for the developer console.

use bevy::prelude::*;
use bevy_egui::egui::FontId;

/// The configuration of the developer console.
#[derive(Resource)]
pub struct ConsoleConfig {
    /// The colors used in the developer console.
    pub colors: ConsoleColors,
    /// The key used to open the developer console.
    pub open_key: KeyCode,
    /// The font used in the developer console.
    pub font: FontId,
}

impl Default for ConsoleConfig {
    fn default() -> Self {
        Self {
            colors: ConsoleColors::ONE_DARK,
            open_key: KeyCode::Grave,
            font: FontId::monospace(14.0),
        }
    }
}

/// The colors used by the text in the developer console.
pub struct ConsoleColors {
    /// The color of the "error" level.
    ///
    /// Designates very serious errors.
    pub error: Color,
    /// The color of the "warn" level.
    ///
    /// Designates hazardous situations.
    pub warning: Color,
    /// The color of the "info" level.
    ///
    /// Designates useful information.
    pub info: Color,
    /// The color of the "debug" level.
    ///
    /// Designates lower priority information.
    pub debug: Color,
    /// The color of the "trace" level.
    ///
    /// Designates very low priority, often extremely verbose, information.
    pub trace: Color,
}

impl ConsoleColors {
    /// Atom's iconic One Dark theme.
    pub const ONE_DARK: Self = Self {
        error: Color::rgb(0.91, 0.46, 0.5),
        warning: Color::rgb(0.82, 0.56, 0.32),
        info: Color::rgb(0.55, 0.76, 0.4),
        debug: Color::rgb(0.29, 0.65, 0.94),
        trace: Color::rgb(0.78, 0.45, 0.89),
    };
}
