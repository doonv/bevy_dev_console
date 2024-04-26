//! Configuration structs for the developer console.

use bevy::log::Level;
use bevy::prelude::*;
use bevy_egui::egui::{Color32, FontId, TextFormat};

/// The configuration of the developer console.
#[derive(Resource, Reflect, Debug)]
pub struct ConsoleConfig {
    /// The colors used in the developer console.
    pub theme: ConsoleTheme,
    /// The key used to open the developer console.
    pub open_key: KeyCode,
    /// The key used to submit a command to the command parser.
    pub submit_key: KeyCode
}

impl Default for ConsoleConfig {
    fn default() -> Self {
        Self {
            theme: ConsoleTheme::ONE_DARK,
            open_key: KeyCode::Backquote,
            submit_key: KeyCode::Enter
        }
    }
}

/// The colors used by the text in the developer console.
#[derive(Reflect, Debug)]
pub struct ConsoleTheme {
    /// The font used in the developer console.
    #[reflect(ignore)]
    pub font: FontId,
    /// The default color of text.
    pub text_color: Color,
    /// The color of dark text.
    pub dark: Color,
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

/// Helper trait that allows conversion between [`bevy::Color`](Color) and [`egui::Color32`].
pub trait ToColor32 {
    /// Convert this [`bevy::Color`](Color) to a [`egui::Color32`].
    fn to_color32(&self) -> Color32;
}
impl ToColor32 for Color {
    fn to_color32(&self) -> Color32 {
        let [r, g, b, a] = self.as_rgba_u8();

        Color32::from_rgba_unmultiplied(r, g, b, a)
    }
}

macro_rules! define_text_format_method {
    ($name:ident, $color:ident) => {
        #[doc = concat!("Returns a [`TextFormat`] colored with [`Self::", stringify!($color), "`]")]
        pub fn $name(&self) -> TextFormat {
            TextFormat {
                color: self.$color.to_color32(),
                ..self.format_text()
            }
        }
    };
}

impl ConsoleTheme {
    /// Atom's iconic One Dark theme.
    pub const ONE_DARK: Self = Self {
        font: FontId::monospace(14.0),
        dark: Color::rgb(0.42, 0.44, 0.48),
        text_color: Color::rgb(0.67, 0.7, 0.75),
        error: Color::rgb(0.91, 0.46, 0.5),
        warning: Color::rgb(0.82, 0.56, 0.32),
        info: Color::rgb(0.55, 0.76, 0.4),
        debug: Color::rgb(0.29, 0.65, 0.94),
        trace: Color::rgb(0.78, 0.45, 0.89),
    };
    /// High contrast theme, might help some people.
    pub const HIGH_CONTRAST: Self = Self {
        font: FontId::monospace(14.0),
        dark: Color::rgb(0.5, 0.5, 0.5),
        text_color: Color::rgb(1.0, 1.0, 1.0),
        error: Color::rgb(1.0, 0.0, 0.0),
        warning: Color::rgb(1.0, 1.0, 0.0),
        info: Color::rgb(0.0, 1.0, 0.0),
        debug: Color::rgb(0.25, 0.25, 1.0),
        trace: Color::rgb(1.0, 0.0, 1.0),
    };

    /// Returns a [`Color32`] based on the `level`
    pub fn color_level(&self, level: Level) -> Color32 {
        match level {
            Level::ERROR => self.error.to_color32(),
            Level::WARN => self.warning.to_color32(),
            Level::INFO => self.info.to_color32(),
            Level::DEBUG => self.debug.to_color32(),
            Level::TRACE => self.trace.to_color32(),
        }
    }

    /// Returns a [`TextFormat`] with a color based on the [`Level`] and the [`ConsoleTheme`].
    pub fn format_level(&self, level: Level) -> TextFormat {
        TextFormat {
            color: self.color_level(level),
            ..self.format_text()
        }
    }

    /// Returns a [`TextFormat`] with the default font and color.
    pub fn format_text(&self) -> TextFormat {
        TextFormat {
            font_id: self.font.clone(),
            color: self.text_color.to_color32(),

            ..default()
        }
    }

    define_text_format_method!(format_dark, dark);
    define_text_format_method!(format_error, error);
    define_text_format_method!(format_warning, warning);
    define_text_format_method!(format_info, info);
    define_text_format_method!(format_debug, debug);
    define_text_format_method!(format_trace, trace);
}
