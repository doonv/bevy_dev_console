//! The module that handles the user interface of the console.
//!
//! Made with [`bevy_egui`].

use bevy::prelude::*;
use bevy_egui::egui::text::LayoutJob;
use bevy_egui::prelude::*;
use chrono::prelude::*;

use crate::command::{COMMAND_MESSAGE_NAME, COMMAND_RESULT_NAME, ExecuteCommand};
use crate::logging::LogMessage;
use crate::prelude::ConsoleConfig;

#[cfg(feature = "completions")]
use crate::command::AutoCompletions;
use crate::ui::ansi::ansi_to_layout_job;

#[cfg(feature = "completions")]
mod completions;
#[cfg(feature = "completions")]
pub use completions::MAX_COMPLETION_SUGGESTIONS;

mod ansi;

#[derive(Default, Resource)]
pub(crate) struct ConsoleUiState {
    /// Whether we have set focus this open or not.
    pub text_focus: bool,
    /// A list of all log messages received plus an
    /// indicator indicating if the message is new.
    pub log: Vec<LogMessage>,
    /// The command currently in the text bar.
    pub command: String,
    #[cfg(feature = "completions")]
    pub selected_completion: usize,
}

pub(crate) fn read_logs(mut logs: MessageReader<LogMessage>, mut state: ResMut<ConsoleUiState>) {
    for log_message in logs.read() {
        state.log.push(log_message.clone());
    }
}

pub(crate) fn render_ui_system(
    mut contexts: EguiContexts,
    mut commands: Commands,
    mut state: ResMut<ConsoleUiState>,
    key: Res<ButtonInput<KeyCode>>,
    config: Res<ConsoleConfig>,
    #[cfg(feature = "completions")] completions: Res<AutoCompletions>,
) {
    egui::Window::new("Developer Console")
        .collapsible(false)
        .default_width(900.)
        .show(contexts.ctx_mut().unwrap(), |ui| {
            render_ui(ui, &mut commands, &mut state, &key, &config, &completions);
        });
}

/// The function that renders the UI of the developer console.
pub(crate) fn render_ui(
    ui: &mut egui::Ui,
    commands: &mut Commands,
    state: &mut ConsoleUiState,
    key: &ButtonInput<KeyCode>,
    config: &ConsoleConfig,
    #[cfg(feature = "completions")] completions: &AutoCompletions,
) {
    fn submit_command(command: &mut String, commands: &mut Commands) {
        if !command.trim().is_empty() {
            // Get the owned command string by replacing it with an empty string
            let command = std::mem::take(command);
            commands.queue(ExecuteCommand(command));
        }
    }

    if key.just_pressed(config.submit_key) {
        submit_command(&mut state.command, commands);
    }

    completions::change_selected_completion(ui, state, completions);

    // A General rule when creating layouts in egui is to place elements which fill remaining space last.
    // Since immediate mode ui can't predict the final sizes of widgets until they've already been drawn

    // Thus we create a bottom panel first, where our text edit and submit button resides.
    egui::TopBottomPanel::bottom("bottom panel")
        .frame(egui::Frame::NONE.outer_margin(egui::Margin {
            left: 5,
            right: 5,
            top: 5 + 6,
            bottom: 5,
        }))
        .show_inside(ui, |ui| {
            let text_edit_id = egui::Id::new("text_edit");

            // We can use a right to left layout, so we can place the text input last and tell it to fill all remaining space
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                if ui.button("Submit").clicked() {
                    submit_command(&mut state.command, commands);

                    // Return keyboard focus to the text edit control.
                    ui.ctx().memory_mut(|mem| mem.request_focus(text_edit_id));
                }

                #[cfg_attr(not(feature = "completions"), allow(unused_variables))]
                let text_edit = egui::TextEdit::singleline(&mut state.command)
                    .id(text_edit_id)
                    .desired_width(ui.available_width())
                    .margin(egui::Vec2::splat(4.0))
                    .font(config.theme.font.clone())
                    .lock_focus(true)
                    .show(ui);

                // Display completions if the "completions" feature is enabled
                #[cfg(feature = "completions")]
                completions::completions(
                    text_edit,
                    text_edit_id,
                    state,
                    ui,
                    commands,
                    completions,
                    config,
                );

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
        .stick_to_bottom(true)
        .show(ui, |ui| {
            ui.vertical(|ui| {
                for (id, message) in state.log.iter_mut().enumerate() {
                    add_log(ui, id, message, config);
                }
            });
        });
}

fn add_log(ui: &mut egui::Ui, id: usize, event: &LogMessage, config: &ConsoleConfig) {
    ui.push_id(id, |ui| {
        let time_utc = event.time;
        let time: DateTime<chrono::Local> = time_utc.into();

        let text = format_line(time, config, event);
        let label = ui.label(text);

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

            text.append("\nTarget: ", 0.0, config.theme.format_text());
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
) -> LayoutJob {
    let mut text = LayoutJob::default();
    text.append(
        &time.format("%H:%M ").to_string(),
        0.0,
        config.theme.format_dark(),
    );
    match *name {
        COMMAND_MESSAGE_NAME | COMMAND_RESULT_NAME => {}
        _ => {
            text.append(level.as_str(), 0.0, config.theme.format_level(*level));
            text.append(" ", 0.0, config.theme.format_text());
        }
    }
    ansi_to_layout_job(message, config, &mut text);
    text
}
