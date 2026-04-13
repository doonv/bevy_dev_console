//! Logging capturing and storage.

use bevy::log::tracing::Subscriber;
use bevy::log::tracing_subscriber::field::Visit;
use bevy::log::tracing_subscriber::{Layer, fmt};
use bevy::log::{BoxedFmtLayer, BoxedLayer, Level, LogPlugin, tracing_subscriber as subscriber};
use bevy::prelude::*;
use std::sync::mpsc;
use web_time::SystemTime;

/// Convince function for setting the [`custom_layer`](LogPlugin::custom_layer) and
/// [`fmt_layer`](LogPlugin::fmt_layer) of a [`LogPlugin`] with [`console_log_layer`] and [`colored_fmt_layer`].
#[must_use]
pub fn console_log_plugin() -> LogPlugin {
    LogPlugin {
        custom_layer: console_log_layer,
        fmt_layer: colored_fmt_layer,
        ..default()
    }
}

/// A [`LogPlugin::custom_layer`] that implements the log reading
/// functionality for the developer console.
#[must_use]
pub fn console_log_layer(app: &mut App) -> Option<BoxedLayer> {
    let (sender, receiver) = mpsc::channel();
    app.add_message::<LogMessage>();
    app.insert_non_send_resource(CapturedLogEvents(receiver));
    app.add_systems(PostUpdate, transfer_log_events);

    Some(Box::new(LogCaptureLayer { sender }))
}

/// A [`LogPlugin::fmt_layer`] that disables [ANSI sanitization](fmt::Layer::with_ansi_sanitization).
///
/// Allows for colored text at the cost of a minor security vulnerability. The extent of it is that
/// malicious ANSI codes can clear the screen, or embed malicious links, not much more than that.
///
/// See [tokio-rs/tracing#3378](https://github.com/tokio-rs/tracing/issues/3378) for more info.
#[must_use]
pub fn colored_fmt_layer(_: &mut App) -> Option<BoxedFmtLayer> {
    Some(Box::new(
        fmt::Layer::default()
            .with_ansi_sanitization(false)
            .with_writer(std::io::stderr),
    ))
}

/// A [`tracing`](bevy::log::tracing) log message event.
///
/// This is used to transfer the log data from `tracing` to the `bevy_dev_console` UI.
#[derive(Message, Debug, Clone)]
pub(crate) struct LogMessage {
    /// The message contents.
    pub message: String,

    /// The name of the span described by this metadata.
    pub name: &'static str,

    /// The part of the system that the span that this
    /// metadata describes occurred in.
    pub target: &'static str,

    /// The level of verbosity of the described span.
    pub level: Level,

    /// The name of the Rust module where the span occurred,
    /// or `None` if this could not be determined.
    pub module_path: Option<&'static str>,

    /// The name of the source code file where the span occurred,
    ///  or `None` if this could not be determined.
    pub file: Option<&'static str>,

    /// The line number in the source code file where the span occurred,
    /// or `None` if this could not be determined.
    pub line: Option<u32>,

    /// The time the log occurred.
    pub time: SystemTime,
}

/// Transfers information from the [`CapturedLogEvents`] resource to [`Events<LogMessage>`](LogMessage).
fn transfer_log_events(
    receiver: NonSend<CapturedLogEvents>,
    mut log_events: MessageWriter<LogMessage>,
) {
    log_events.write_batch(receiver.0.try_iter());
}

/// This struct temporarily stores [`LogMessage`]s before they are
/// written to [`MessageWriter<LogMessage>`] by [`transfer_log_events`].
struct CapturedLogEvents(mpsc::Receiver<LogMessage>);

/// A [`Layer`] that captures log events and saves them to [`CapturedLogEvents`].
struct LogCaptureLayer {
    sender: mpsc::Sender<LogMessage>,
}
impl<S: Subscriber> Layer<S> for LogCaptureLayer {
    fn on_event(
        &self,
        event: &bevy::log::tracing::Event<'_>,
        _ctx: subscriber::layer::Context<'_, S>,
    ) {
        let mut message = None;
        event.record(&mut LogEventVisitor(&mut message));
        if let Some(message) = message {
            let metadata = event.metadata();
            self.sender
                .send(LogMessage {
                    message,
                    name: metadata.name(),
                    target: metadata.target(),
                    level: *metadata.level(),
                    module_path: metadata.module_path(),
                    file: metadata.file(),
                    line: metadata.line(),
                    time: SystemTime::now(),
                })
                .expect("CapturedLogEvents resource no longer exists!");
        }
    }
}

/// A [`Visit`]or that records log messages that are transferred to [`LogCaptureLayer`].
struct LogEventVisitor<'a>(&'a mut Option<String>);
impl Visit for LogEventVisitor<'_> {
    fn record_debug(
        &mut self,
        field: &bevy::log::tracing::field::Field,
        value: &dyn std::fmt::Debug,
    ) {
        // Only log out messages
        if field.name() == "message" {
            *self.0 = Some(format!("{value:?}"));
        }
    }
}
