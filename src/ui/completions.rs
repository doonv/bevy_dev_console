use bevy::ecs::system::Commands;
use bevy_egui::egui;

use crate::command::{AutoCompletions, CompletionSuggestion, UpdateAutoComplete};
use crate::prelude::ConsoleConfig;

use super::ConsoleUiState;

/// The max amount of completion suggestions shown at once.
pub const MAX_COMPLETION_SUGGESTIONS: usize = 6;

pub fn completions(
    text_edit: egui::text_edit::TextEditOutput,
    text_edit_id: egui::Id,
    state: &mut ConsoleUiState,
    ui: &mut egui::Ui,
    mut commands: Commands,
    completions: &AutoCompletions,
    config: &ConsoleConfig,
) {
    let text_edit_complete_id = ui.make_persistent_id("text_edit_complete");

    if let Some(cursor_range) = text_edit.state.cursor.char_range() {
        let [primary, secondary] = cursor_range.sorted();

        fn non_keyword(character: char) -> bool {
            !(character.is_alphanumeric() || character == '_')
        }

        let cursor_index = (|| {
            // Convert the cursor's char index into a byte index
            // aswell as returning the character at the cursor's position position
            let (primary_index, char) = state
                .command
                .char_indices()
                .nth(primary.index.saturating_sub(1))?;

            if non_keyword(char) {
                return None;
            }

            Some(primary_index)
        })();
        if text_edit.response.changed() {
            state.selected_completion = 0;
        }
        // todo check cursor position changed https://github.com/emilk/egui/discussions/4540
        // if text_edit.response.changed() {
        if true {
            if let Some(cursor_index) = cursor_index {
                ui.memory_mut(|mem| {
                    if !completions.is_empty() {
                        mem.open_popup(text_edit_complete_id)
                    }
                });
                let before_cursor = &state.command[..=cursor_index];
                let keyword_before = match before_cursor.rfind(non_keyword) {
                    // If found, return the slice from the end of the non-alphanumeric character to the cursor position
                    Some(index) => &before_cursor[(index + 1)..],
                    // If not found, the whole substring is a word
                    None => before_cursor,
                };
                commands.add(UpdateAutoComplete(keyword_before.to_owned()));
            } else {
                ui.memory_mut(|mem| {
                    if mem.is_popup_open(text_edit_complete_id) {
                        mem.close_popup();
                    }
                });
            }
        }
        if let Some(cursor_index) = cursor_index {
            if ui.input(|i| i.key_pressed(egui::Key::Tab)) {
                // Remove the old text
                let before_cursor = &state.command[..=cursor_index];
                let index_before = match before_cursor.rfind(non_keyword) {
                    Some(index) => index + 1,
                    None => 0,
                };
                let after_cursor = &state.command[cursor_index..];
                match after_cursor.find(non_keyword) {
                    Some(characters_after) => state
                        .command
                        .drain(index_before..cursor_index + characters_after),
                    None => state.command.drain(index_before..),
                };
                // Add the completed text
                let completed_text = &completions.0[state.selected_completion].suggestion;

                state.command.insert_str(index_before, completed_text);

                // Set the cursor position
                let mut text_edit_state = text_edit.state;
                let mut cursor_range = egui::text::CCursorRange::two(primary, secondary);

                cursor_range.primary.index +=
                    completed_text.len() - (cursor_index - index_before) - 1;
                cursor_range.secondary.index +=
                    completed_text.len() - (cursor_index - index_before) - 1;

                text_edit_state.cursor.set_char_range(Some(cursor_range));
                egui::TextEdit::store_state(ui.ctx(), text_edit_id, text_edit_state);
            }
        }
    }
    egui::popup_below_widget(
        ui,
        text_edit_complete_id,
        &text_edit.response,
        egui::PopupCloseBehavior::CloseOnClickOutside,
        |ui| {
            ui.vertical(|ui| {
                for (
                    i,
                    CompletionSuggestion {
                        suggestion,
                        highlighted_indices,
                    },
                ) in completions.iter().take(6).enumerate()
                {
                    let mut layout = egui::text::LayoutJob::default();
                    for (i, _) in suggestion.char_indices() {
                        layout.append(
                            &suggestion[i..=i],
                            0.0,
                            if highlighted_indices.contains(&i) {
                                config.theme.format_bold()
                            } else {
                                config.theme.format_text()
                            },
                        );
                    }
                    let res = ui.label(layout);
                    if i == state.selected_completion {
                        res.highlight();
                    }
                }
            })
        },
    );
}

/// Also consumes the up and down arrow keys.
pub fn change_selected_completion(
    ui: &mut egui::Ui,
    state: &mut ConsoleUiState,
    completions: &[CompletionSuggestion],
) {
    if ui.input_mut(|i| i.consume_key(egui::Modifiers::NONE, egui::Key::ArrowUp)) {
        state.selected_completion = state.selected_completion.saturating_sub(1);
    }
    if ui.input_mut(|i| i.consume_key(egui::Modifiers::NONE, egui::Key::ArrowDown)) {
        state.selected_completion = state
            .selected_completion
            .saturating_add(1)
            .min(completions.len() - 1);
    }
}
