//! The module that handles the user interface of the console.
//!
//! Made with [`bevy_egui`].

use bevy::prelude::*;
use bevy_egui::{
    egui::{text::LayoutJob, Stroke, TextFormat},
    *,
};
use chrono::prelude::*;
use web_time::SystemTime;

use crate::{
    command::{CommandHints, ExecuteCommand},
    config::ToColor32,
    logging::log_plugin::LogMessage,
    prelude::ConsoleConfig,
};

#[derive(Default, Resource)]
pub(crate) struct ConsoleUiState {
    /// Whever the console is open or not.
    pub(crate) open: bool,
    /// A list of all log messages receieved plus an
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
    key: Res<Input<KeyCode>>,
    config: Res<ConsoleConfig>,
) {
    if key.just_pressed(config.open_key) {
        state.open = !state.open;
    }
}

pub(crate) fn render_ui(
    mut contexts: EguiContexts,
    mut commands: Commands,
    mut state: ResMut<ConsoleUiState>,
    key: Res<Input<KeyCode>>,
    mut hints: ResMut<CommandHints>,
    config: Res<ConsoleConfig>,
) {
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
                            .font(config.theme.font.clone())
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
                            add_log(ui, id, message, is_new, &mut hints, &config);
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
) {
    let mut command_index = 0;

    ui.push_id(id, |ui| {
        let time_utc = system_time_to_chrono_utc(event.time);
        let time: DateTime<chrono::Local> = time_utc.into();

        let text = format_line(time, config, event, *is_new, &mut command_index, hints);
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
    if *name == "console_command" || *name == "console_result" {
        if *name == "console_command" {
            if new {
                hints.reset_hint_added();
            }
            if let Some(hints) = hints.get(*command_index) {
                let hint = &hints[0];
                text.append(&message[..hint.span.start], 0., config.theme.format_text());
                text.append(
                    &message[hint.span.clone()],
                    0.,
                    TextFormat {
                        underline: Stroke::new(2.0, config.theme.error.to_color32()),
                        ..config.theme.format_text()
                    },
                );
                text.append(&message[hint.span.end..], 0., config.theme.format_text());
                return text;
            }
            *command_index += 1;
        }
        text.append(message.as_str(), 0.0, config.theme.format_text());

        return text;
    }
    text.append(level.as_str(), 0.0, config.theme.format_level(*level));
    text.append(&format!(" {message}"), 0.0, config.theme.format_text());

    text
}
