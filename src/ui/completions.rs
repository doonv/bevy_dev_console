use bevy::prelude::*;
use bevy_egui::egui::{self, LayerId, Popup, PopupAnchor};

use crate::command::{AutoCompletions, CompletionSuggestion, DefaultCommandParser};
use crate::prelude::ConsoleConfig;

use super::ConsoleUiState;

/// The max amount of completion suggestions shown at once.
pub const MAX_COMPLETION_SUGGESTIONS: usize = 6;

pub fn completions(
    text_edit: egui::text_edit::TextEditOutput,
    text_edit_id: egui::Id,
    state: &mut ConsoleUiState,
    ui: &egui::Ui,
    commands: &mut Commands,
    completions: &AutoCompletions,
    config: &ConsoleConfig,
) {
    let mut popup_open = false;

    if let Some(cursor_range) = text_edit.state.cursor.char_range() {
        let [primary, secondary] = cursor_range.sorted_cursors();

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
                ui.memory_mut(|_| {
                    if !completions.is_empty() {
                        popup_open = true;
                    }
                });
                let before_cursor = &state.command[..=cursor_index];
                let keyword_before = before_cursor
                    .rfind(non_keyword)
                    .map_or(before_cursor, |index| &before_cursor[(index + 1)..])
                    .to_owned();
                commands.queue(move |world: &mut World| {
                    world.resource_scope(|world, parser: Mut<DefaultCommandParser>| {
                        let completions: Vec<CompletionSuggestion> =
                            parser.completion(&keyword_before, world);
                        world.resource_mut::<AutoCompletions>().0 = completions;
                    });
                });
            } else {
                ui.memory_mut(|_| popup_open = false);
            }
        }
        if let Some(cursor_index) = cursor_index
            && let Some(suggestion) = &completions.0.get(state.selected_completion)
            && ui.input(|i| i.key_pressed(egui::Key::Tab))
        {
            // Remove the old text
            let before_cursor = &state.command[..=cursor_index];
            let index_before = before_cursor
                .rfind(non_keyword)
                .map_or(0, |index| index + 1);
            let after_cursor = &state.command[cursor_index..];
            match after_cursor.find(non_keyword) {
                Some(characters_after) => state
                    .command
                    .drain(index_before..cursor_index + characters_after),
                None => state.command.drain(index_before..),
            };
            // Add the completed text
            let completed_text = &suggestion.suggestion;
            state.command.insert_str(index_before, completed_text);

            // Set the cursor position
            let mut text_edit_state = text_edit.state;

            let mut cursor_range = egui::text::CCursorRange::two(primary, secondary);

            cursor_range.primary.index += completed_text.len() - (cursor_index - index_before) - 1;
            cursor_range.secondary.index +=
                completed_text.len() - (cursor_index - index_before) - 1;

            text_edit_state.cursor.set_char_range(Some(cursor_range));
            egui::TextEdit::store_state(ui.ctx(), text_edit_id, text_edit_state);
        }
    }
    Popup::new(
        ui.make_persistent_id("text_edit_complete"),
        ui.ctx().clone(),
        PopupAnchor::ParentRect(text_edit.response.rect),
        LayerId::debug(),
    )
    .open(popup_open)
    .show(|ui| {
        ui.set_width(200.0);
        ui.vertical(|ui| {
            for (
                i,
                CompletionSuggestion {
                    suggestion,
                    highlighted_indices,
                },
            ) in completions
                .iter()
                .take(MAX_COMPLETION_SUGGESTIONS)
                .enumerate()
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
                let res = ui.add(egui::Label::new(layout).wrap_mode(egui::TextWrapMode::Truncate));
                if i == state.selected_completion {
                    res.highlight();
                }
            }
        })
    });
}

/// Also consumes the up and down arrow keys.
pub fn change_selected_completion(
    ui: &egui::Ui,
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
