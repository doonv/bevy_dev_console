use ansitok::{AnsiColor, ElementKind, Output, VisualAttribute};
use bevy::log::error_once;
use bevy_egui::egui::text::LayoutJob;
use bevy_egui::egui::{Color32, Stroke};

use crate::config::{ConsoleConfig, ToColor32};

pub fn ansi_to_layout_job(input: &str, config: &ConsoleConfig, job: &mut LayoutJob) {
    let ConsoleConfig { theme, .. } = config;
    // Maintain the current style state
    let mut current_format = theme.format_text();
    let color4bit = |c| {
        match c {
            31 => theme.error,
            32 => theme.info,
            33 => theme.warning,
            34 => theme.debug,
            35 => theme.trace,

            _ => todo!(),
        }
        .to_color32()
    };
    let ansi_to_color32 = |ansi| match ansi {
        AnsiColor::Bit4(c) => color4bit(c),
        AnsiColor::Bit8(c) => match c {
            0..=7 => color4bit(30 + c),
            _ => todo!(),
        },
        AnsiColor::Bit24 { r, g, b } => Color32::from_rgb(r, g, b),
    };

    for element in ansitok::parse_ansi(input) {
        let text = &input[element.range()];
        match element.kind() {
            ElementKind::Sgr => match ansitok::parse_ansi_sgr(text).next().unwrap() {
                Output::Escape(esc) => match esc {
                    VisualAttribute::Bold => todo!(),
                    VisualAttribute::Faint => todo!(),
                    VisualAttribute::Italic => current_format.italics = true,
                    VisualAttribute::FgColor(ansi_color) => {
                        current_format.color = ansi_to_color32(ansi_color);
                        if current_format.underline.width > 0.0 {
                            current_format.underline = Stroke::new(1.0, current_format.color);
                        }
                    }
                    VisualAttribute::BgColor(ansi_color) => {
                        current_format.background = ansi_to_color32(ansi_color)
                    }
                    VisualAttribute::UndrColor(ansi_color) => {
                        current_format.underline = Stroke::new(1.0, ansi_to_color32(ansi_color))
                    }
                    VisualAttribute::Underline => {
                        current_format.underline = Stroke::new(1.0, current_format.color);
                    }
                    VisualAttribute::Reset(_) => current_format = theme.format_text(),
                    _ => error_once!("Unrecognized ansi visual attribute type: {esc:?}"),
                },
                Output::Text(text) => match &text[0..2] {
                    // 4: is not part of the ansi spec but it is used in many terminal emulators,
                    // we use it for errors in bevy_dev_console
                    // however, ansitok doesn't support parsing it.
                    "4:" => {
                        // egui doesn't support fancy underline styles
                        current_format.underline = Stroke::new(1.0, current_format.color);
                    }
                    _ => unreachable!("found text in sgr: {text}"),
                },
            },
            ElementKind::Osc => todo!("implement Osc ansi sequences"),
            ElementKind::Text => job.append(text, 0.0, current_format.clone()),
            _ => {} // ignore
        }
    }
}
