//! Custom [LogPlugin](bevy::log::LogPlugin) functionality.

#[cfg(target_os = "android")]
mod android_tracing;
pub(crate) mod log_plugin;
