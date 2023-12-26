//! The module that handles the user interface of the console.
//!
//! Made with [`bevy_egui`].

use bevy::prelude::*;
use bevy::utils::tracing::Level;
use bevy_egui::{
    egui::{text::LayoutJob, Color32, Stroke, TextFormat},
    *,
};
use chrono::TimeZone;
use web_time::SystemTime;

use crate::{
    command::{CommandHints, ExecuteCommand},
    logging::log_plugin::LogMessage,
    prelude::ConsoleConfig,
};

#[derive(Default, Resource)]
pub(crate) struct ConsoleUiState {
    open: bool,
    log: Vec<(LogMessage, bool)>,
    command: String,
}

fn system_time_to_chrono_utc(t: SystemTime) -> chrono::DateTime<chrono::Utc> {
    let dur = t.duration_since(web_time::SystemTime::UNIX_EPOCH).unwrap();
    let (sec, nsec) = (dur.as_secs() as i64, dur.subsec_nanos());

    chrono::Utc.timestamp_opt(sec, nsec).unwrap()
}

pub(crate) fn ui(
    mut commands: Commands,
    mut contexts: EguiContexts,
    mut state: ResMut<ConsoleUiState>,
    key: Res<Input<KeyCode>>,
    mut logs: EventReader<LogMessage>,
    hints: Res<CommandHints>,
    config: Res<ConsoleConfig>,
) {
    for log_message in logs.read() {
        state.log.push((log_message.clone(), true));
    }

    if key.just_pressed(KeyCode::Grave) {
        state.open = !state.open;
    }

    let mut submit_command = |command: &mut String| {
        if !command.trim().is_empty() {
            info!(name: "console_command", "$ {}", command.trim());
            // Get the owned command string by replacing it with an empty string
            let command = std::mem::take(command);
            commands.add(ExecuteCommand(command));
        }
    };

    if key.just_pressed(KeyCode::Return) {
        submit_command(&mut state.command);
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
                    .frame(egui::Frame::none().outer_margin(egui::Margin {
                        left: 0.0,
                        right: 5.0,
                        top: 5. + 6.,
                        bottom: 5.0,
                    }))
                    .show_inside(ui, |ui| {
                        //We can use a right to left layout, so we can place the text input last and tell it to fill all remaining space
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            if ui.button("Submit").clicked() {
                                submit_command(&mut state.command);
                            }
                            // ui.button is a shorthand command, a similar command exists for text edits, but this is how to manually construct a widget.
                            // doing this also allows access to more options of the widget, rather than being stuck with the default the shorthand picks.
                            let text_edit = egui::TextEdit::singleline(&mut state.command)
                                .desired_width(ui.available_width())
                                .margin(egui::Vec2::splat(4.0))
                                .font(config.font.clone())
                                .lock_focus(true);

                            ui.add(text_edit);
                        });
                    });
                // Now we can fill the remaining minutespace with a scrollarea, which has only the vertical scrollbar enabled and expands to be as big as possible.
                egui::ScrollArea::new([false, true])
                    .auto_shrink([false, true])
                    .show(ui, |ui| {
                        ui.vertical(|ui| {
                            for (id, (message, is_new)) in state.log.iter_mut().enumerate() {
                                add_log(ui, id, message, is_new, &hints, &config);
                            }
                        });
                    });
            });
    }
}

fn add_log(
    ui: &mut egui::Ui,
    id: usize,
    LogMessage {
        message,
        name,
        target,
        level,
        module_path,
        file,
        line,
        time,
    }: &LogMessage,
    is_new: &mut bool,
    hints: &CommandHints,
    config: &ConsoleConfig,
) {
    let text_format = TextFormat {
        font_id: config.font.clone(),
        color: egui::Color32::LIGHT_GRAY,
        ..default()
    };

    let mut command_index = 0;

    ui.push_id(id, |ui| {
        let time_utc = system_time_to_chrono_utc(*time);
        let time: chrono::DateTime<chrono::Local> = time_utc.into();
        let res = ui
            .horizontal_wrapped(|ui| {
                ui.label(
                    egui::RichText::new(time.format("%H:%M").to_string()).font(config.font.clone()),
                );
                if *name == "console_command" || *name == "console_result" {
                    if *name == "console_command" {
                        if let Some(hints) = hints.get(command_index) {
                            let mut layout = LayoutJob::default();
                            let hint = &hints[0];
                            layout.append(&message[..hint.span.start], 0., text_format.clone());
                            layout.append(
                                &message[hint.span.clone()],
                                0.,
                                TextFormat {
                                    underline: Stroke::new(2.0, Color32::RED),
                                    ..text_format.clone()
                                },
                            );
                            layout.append(&message[hint.span.end..], 0., text_format);

                            ui.label(layout);
                            return;
                        }
                        command_index += 1;
                    }
                    ui.label(
                        egui::RichText::new(message.as_str())
                            .color(egui::Color32::from_gray(190))
                            .font(config.font.clone()),
                    );
                    return;
                }
                let level_color = match *level {
                    Level::TRACE => egui::Color32::from_rgb(200, 114, 226),
                    Level::DEBUG => egui::Color32::from_rgb(74, 165, 240),
                    Level::INFO => egui::Color32::from_rgb(140, 194, 101),
                    Level::WARN => egui::Color32::from_rgb(209, 143, 82),
                    Level::ERROR => egui::Color32::from_rgb(231, 118, 128),
                };
                ui.label(
                    egui::RichText::new(level.as_str())
                        .color(level_color)
                        .font(config.font.clone()),
                );
                ui.label(
                    egui::RichText::new(message.as_str())
                        .color(egui::Color32::LIGHT_GRAY)
                        .font(config.font.clone()),
                );
            })
            .response;
        if *is_new {
            res.scroll_to_me(Some(egui::Align::Max));
            *is_new = false;
        }
        res.on_hover_ui(|ui| {
            ui.horizontal(|ui| {
                ui.label(egui::RichText::new("Time:").color(egui::Color32::WHITE));
                ui.label(
                    egui::RichText::new(time.format("%x %X %:z").to_string())
                        .color(egui::Color32::GRAY),
                );
            });
            ui.horizontal(|ui| {
                ui.label(egui::RichText::new("Time (UTC):").color(egui::Color32::WHITE));
                ui.label(
                    egui::RichText::new(
                        time_utc.to_rfc3339_opts(chrono::SecondsFormat::Micros, true),
                    )
                    .color(egui::Color32::GRAY),
                );
            });
            ui.horizontal(|ui| {
                ui.label(egui::RichText::new("Name:").color(egui::Color32::WHITE));
                ui.label(egui::RichText::new(*name).color(egui::Color32::GRAY));
            });
            ui.horizontal(|ui| {
                ui.label(egui::RichText::new("Target:").color(egui::Color32::WHITE));
                ui.label(egui::RichText::new(*target).color(egui::Color32::GRAY));
            });
            ui.horizontal(|ui| {
                ui.label(egui::RichText::new("Module Path:").color(egui::Color32::WHITE));
                if let Some(module_path) = module_path {
                    ui.label(egui::RichText::new(*module_path).color(egui::Color32::GRAY));
                } else {
                    ui.label(egui::RichText::new("Unknown").color(egui::Color32::GRAY));
                }
            });
            ui.horizontal(|ui| {
                ui.label(egui::RichText::new("File: ").color(egui::Color32::WHITE));

                if let (Some(file), Some(line)) = (file, line) {
                    ui.label(
                        egui::RichText::new(format!("{file}:{line}")).color(egui::Color32::GRAY),
                    );
                } else {
                    ui.label(egui::RichText::new("Unknown").color(egui::Color32::GRAY));
                }
            });
        });
    });
}
