//! The module that handles the user interface of the console.
//!
//! Made with [`bevy_egui`]
use bevy::prelude::*;
use bevy::utils::tracing::Level;
use bevy_egui::{egui::Margin, *};

use crate::{logging::log_plugin::LogMessage, parser::ExecuteConsoleCommand};

#[derive(Default, Resource)]
pub struct ConsoleUiState {
    open: bool,
    log: Vec<LogMessage>,
    command: String,
}

pub fn ui(
    mut commands: Commands,
    mut contexts: EguiContexts,
    mut state: ResMut<ConsoleUiState>,
    key: Res<Input<KeyCode>>,
    mut logs: EventReader<LogMessage>,
) {
    for log_message in logs.read() {
        state.log.push(log_message.clone());
    }

    if key.just_pressed(KeyCode::Grave) {
        state.open = !state.open;
    }

    if key.just_pressed(KeyCode::Return) && !state.command.trim().is_empty() {
        info!(name: "console_command", "$ {}", state.command.trim());
        // Get the owned command by replacing it with an empty string
        let command = std::mem::replace(&mut state.command, String::new());
        commands.add(ExecuteConsoleCommand(command));
    }

    if state.open {
        egui::Window::new("Developer Console")
            .collapsible(false)
            .default_width(900.)
            .show(contexts.ctx_mut(), |ui| {
                // A General rule when creating layouts in egui is to place elements which fill remaining space last.
                // Since immediate mode ui can't predict the final sizes of widgets until they've already been drawn

                // Thus we create a bottom panel first, where our text edit and submit button resides.
                egui::TopBottomPanel::bottom("bottom panel")
                    .frame(egui::Frame::none().outer_margin(Margin {
                        left: 0.0,
                        right: 5.0,
                        top: 5. + 6.,
                        bottom: 5.0,
                    }))
                    .show_inside(ui, |ui| {
                        //We can use a right to left layout, so we can place the text input last and tell it to fill all remaining space
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            let _ = ui.button("Submit");
                            // ui.button is a shorthand command, a similar command exists for text edits, but this is how to manually construct a widget.
                            // doing this also allows access to more options of the widget, rather than being stuck with the default the shorthand picks.
                            let text_edit = egui::TextEdit::singleline(&mut state.command)
                                .desired_width(ui.available_width())
                                .margin(egui::Vec2::splat(4.0))
                                .lock_focus(true);
                            ui.add(text_edit);
                        });
                    });
                // Now we can fill the remaining minutespace with a scrollarea, which has only the vertical scrollbar enabled and expands to be as big as possible.
                egui::ScrollArea::new([false, true])
                    .auto_shrink([false, false])
                    .show(ui, |ui| {
                        const CONSOLE_FONT_SIZE: f32 = 14.0;
                        const FONT_ID: egui::FontId = egui::FontId::monospace(CONSOLE_FONT_SIZE);
                        ui.vertical(|ui| {
                            for LogMessage {
                                message,
                                level,
                                name,
                                time,
                                ..
                            } in &state.log
                            {
                                let time = chrono::DateTime::<chrono::Local>::from(*time);
                                ui.horizontal_wrapped(|ui| {
                                    ui.label(
                                        egui::RichText::new(time.format("%H:%M").to_string())
                                            .font(FONT_ID),
                                    );
                                    if *name == "console_command" || *name == "console_result" {
                                        ui.label(
                                            egui::RichText::new(message)
                                                .color(egui::Color32::from_gray(190))
                                                .font(FONT_ID),
                                        );
                                        return;
                                    }
                                    let level_color = match *level {
                                        Level::TRACE => egui::Color32::from_rgb(74, 165, 240),
                                        Level::DEBUG => egui::Color32::from_rgb(200, 114, 226),
                                        Level::INFO => egui::Color32::from_rgb(140, 194, 101),
                                        Level::WARN => egui::Color32::from_rgb(209, 143, 82),
                                        Level::ERROR => egui::Color32::from_rgb(231, 118, 128),
                                    };
                                    ui.label(
                                        egui::RichText::new(level.as_str())
                                            .color(level_color)
                                            .font(FONT_ID),
                                    );
                                    ui.label(
                                        egui::RichText::new(message)
                                            .color(egui::Color32::LIGHT_GRAY)
                                            .font(FONT_ID),
                                    );
                                });
                            }
                        });
                    });
            });
    }
}
