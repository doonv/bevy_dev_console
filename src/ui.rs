//! The module that handles the user interface of the console.
//!
//! Made with [`bevy_egui`].

use bevy::prelude::*;
use bevy_egui::egui::text::LayoutJob;
use bevy_egui::egui::{Stroke, TextFormat};
use bevy_egui::*;
use chrono::prelude::*;
use web_time::SystemTime;

use crate::command::{CommandHints, ExecuteCommand};
use crate::config::ToColor32;
use crate::logging::log_plugin::LogMessage;
use crate::prelude::ConsoleConfig;

/// Prefix for log messages that show a previous command.
pub const COMMAND_MESSAGE_PREFIX: &str = "$ ";
/// Prefix for log messages that show the result of a command.
pub const COMMAND_RESULT_PREFIX: &str = "> ";
/// Identifier for log messages that show a previous command.
pub const COMMAND_MESSAGE_NAME: &str = "console_command";
/// Identifier for log messages that show the result of a command.
pub const COMMAND_RESULT_NAME: &str = "console_result";

#[derive(Default, Resource)]
pub(crate) struct ConsoleUiState {
    /// Wherever the console is open or not.
    pub(crate) open: bool,
    /// Whether we have set focus this open or not.
    pub(crate) text_focus: bool,
    /// A list of all log messages received plus an
    /// indicator indicating if the message is new.
    pub(crate) log: Vec<(LogMessage, bool)>,
    /// The command in the text bar.
    pub(crate) command: String,
}

fn system_time_to_chrono_utc(t: SystemTime) -> chrono::DateTime<chrono::Utc> {
    let dur = t.duration_since(web_time::SystemTime::UNIX_EPOCH).unwrap();
    let (sec, nsec) = (dur.as_secs() as i64, dur.subsec_nanos());

    chrono::Utc.timestamp_opt(sec, nsec).unwrap()
}

pub(crate) fn read_logs(mut logs: EventReader<LogMessage>, mut state: ResMut<ConsoleUiState>) {
    for log_message in logs.read() {
        state.log.push((log_message.clone(), true));
    }
}

pub(crate) fn open_close_ui(
    mut state: ResMut<ConsoleUiState>,
    key: Res<ButtonInput<KeyCode>>,
    config: Res<ConsoleConfig>,
) {
    if key.just_pressed(config.open_key) {
        state.open = !state.open;
        state.text_focus = false;
    }
}

pub(crate) fn render_ui(
    mut contexts: EguiContexts,
    mut commands: Commands,
    mut state: ResMut<ConsoleUiState>,
    key: Res<ButtonInput<KeyCode>>,
    mut hints: ResMut<CommandHints>,
    config: Res<ConsoleConfig>,
) {
    let mut submit_command = |command: &mut String| {
        if !command.trim().is_empty() {
            info!(name: COMMAND_MESSAGE_NAME, "{COMMAND_MESSAGE_PREFIX}{}", command.trim());
            // Get the owned command string by replacing it with an empty string
            let command = std::mem::take(command);
            commands.add(ExecuteCommand(command));
        }
    };

    if key.just_pressed(config.submit_key) {
        submit_command(&mut state.command);
    }

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
                    let text_edit_id = egui::Id::new("text_edit");

                    //We can use a right to left layout, so we can place the text input last and tell it to fill all remaining space
                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        if ui.button("Submit").clicked() {
                            submit_command(&mut state.command);

                            // Return keyboard focus to the text edit control.
                            ui.ctx().memory_mut(|mem| mem.request_focus(text_edit_id));
                        }
                        // ui.button is a shorthand command, a similar command exists for text edits, but this is how to manually construct a widget.
                        // doing this also allows access to more options of the widget, rather than being stuck with the default the shorthand picks.
                        let text_edit = egui::TextEdit::singleline(&mut state.command)
                            .id(text_edit_id)
                            .desired_width(ui.available_width())
                            .margin(egui::Vec2::splat(4.0))
                            .font(config.theme.font.clone())
                            .lock_focus(true);

                        ui.add(text_edit);

                        // Each time we open the console, we want to set focus to the text edit control.
                        if !state.text_focus {
                            state.text_focus = true;
                            ui.ctx().memory_mut(|mem| mem.request_focus(text_edit_id));
                        }
                    });
                });
            // Now we can fill the remaining minutespace with a scrollarea, which has only the vertical scrollbar enabled and expands to be as big as possible.
            egui::ScrollArea::new([false, true])
                .auto_shrink([false, true])
                .show(ui, |ui| {
                    ui.vertical(|ui| {
                        let mut command_index = 0;

                        for (id, (message, is_new)) in state.log.iter_mut().enumerate() {
                            add_log(
                                ui,
                                id,
                                message,
                                is_new,
                                &mut hints,
                                &config,
                                &mut command_index,
                            );
                        }
                    });
                });
        });
}

fn add_log(
    ui: &mut egui::Ui,
    id: usize,
    event: &LogMessage,
    is_new: &mut bool,
    hints: &mut CommandHints,
    config: &ConsoleConfig,
    command_index: &mut usize,
) {
    ui.push_id(id, |ui| {
        let time_utc = system_time_to_chrono_utc(event.time);
        let time: DateTime<chrono::Local> = time_utc.into();

        let text = format_line(time, config, event, *is_new, command_index, hints);
        let label = ui.label(text);

        if *is_new {
            label.scroll_to_me(Some(egui::Align::Max));
            *is_new = false;
        }
        label.on_hover_ui(|ui| {
            let mut text = LayoutJob::default();
            text.append("Time: ", 0.0, config.theme.format_text());
            text.append(
                &time.format("%x %X %:z").to_string(),
                0.0,
                config.theme.format_dark(),
            );
            text.append("\nTime (UTC): ", 0.0, config.theme.format_text());
            text.append(
                &time_utc.to_rfc3339_opts(chrono::SecondsFormat::Micros, true),
                0.0,
                config.theme.format_dark(),
            );
            text.append("\nName: ", 0.0, config.theme.format_text());
            text.append(event.name, 0.0, config.theme.format_dark());
            text.append("\nTarget : ", 0.0, config.theme.format_text());
            text.append(event.target, 0.0, config.theme.format_dark());
            text.append("\nModule Path: ", 0.0, config.theme.format_text());
            if let Some(module_path) = event.module_path {
                text.append(module_path, 0.0, config.theme.format_dark());
            } else {
                text.append("(Unknown)", 0.0, config.theme.format_dark());
            }
            text.append("\nFile: ", 0.0, config.theme.format_text());
            if let (Some(file), Some(line)) = (event.file, event.line) {
                text.append(&format!("{file}:{line}"), 0.0, config.theme.format_dark());
            } else {
                text.append("(Unknown)", 0.0, config.theme.format_dark());
            }

            ui.label(text);
        });
    });
}

fn format_line(
    time: DateTime<chrono::Local>,
    config: &ConsoleConfig,
    LogMessage {
        message,
        name,
        level,
        ..
    }: &LogMessage,
    new: bool,
    command_index: &mut usize,
    hints: &mut CommandHints,
) -> LayoutJob {
    let mut text = LayoutJob::default();
    text.append(
        &time.format("%H:%M ").to_string(),
        0.0,
        config.theme.format_dark(),
    );
    match *name {
        COMMAND_MESSAGE_NAME => {
            if new {
                hints.reset_hint_added();
            }
            let hints = &hints[*command_index];

            *command_index += 1;

            let message_stripped = message
                .strip_prefix(COMMAND_MESSAGE_PREFIX)
                .unwrap_or(message);
            text.append(COMMAND_MESSAGE_PREFIX, 0.0, config.theme.format_dark());
            // TODO: Handle more than just the first element
            if let Some(hint) = hints.first() {
                text.append(
                    &message_stripped[..hint.span.start],
                    0.,
                    config.theme.format_text(),
                );
                text.append(
                    &message_stripped[hint.span.start..hint.span.end],
                    0.,
                    TextFormat {
                        underline: Stroke::new(1.0, config.theme.error.to_color32()),
                        ..config.theme.format_text()
                    },
                );
                text.append(
                    &message_stripped[hint.span.end..],
                    0.,
                    config.theme.format_text(),
                );
                return text;
            }
            text.append(message_stripped, 0.0, config.theme.format_text());
            text
        }
        COMMAND_RESULT_NAME => {
            text.append(COMMAND_RESULT_PREFIX, 0.0, config.theme.format_dark());
            text.append(
                message
                    .strip_prefix(COMMAND_RESULT_PREFIX)
                    .unwrap_or(message),
                0.0,
                config.theme.format_text(),
            );
            text
        }
        _ => {
            text.append(level.as_str(), 0.0, config.theme.format_level(*level));
            text.append(&format!(" {message}"), 0.0, config.theme.format_text());

            text
        }
    }
}
