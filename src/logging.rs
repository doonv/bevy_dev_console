//! Custom [LogPlugin](bevy::log::LogPlugin) functionality.

use bevy::log::{BoxedLayer, Level};
use bevy::prelude::*;
use bevy::utils::tracing::Subscriber;
use std::sync::mpsc;
use tracing_subscriber::field::Visit;
use tracing_subscriber::Layer;
use web_time::SystemTime;

/// A function that implements the log reading functionality for the
/// developer console via [`LogPlugin::custom_layer`](bevy::log::LogPlugin::custom_layer).
pub fn custom_log_layer(app: &mut App) -> Option<BoxedLayer> {
    Some(Box::new(create_custom_log_layer(app)))
}

fn create_custom_log_layer(app: &mut App) -> LogCaptureLayer {
    let (sender, receiver) = mpsc::channel();
    app.add_event::<LogMessage>();
    app.insert_non_send_resource(CapturedLogEvents(receiver));
    app.add_systems(PostUpdate, transfer_log_events);

    LogCaptureLayer { sender }
}

/// A [`tracing`](bevy::utils::tracing) log message event.
///
/// This event is helpful for creating custom log viewing systems such as consoles and terminals.
#[derive(Event, Debug, Clone)]
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
    mut log_events: EventWriter<LogMessage>,
) {
    log_events.send_batch(receiver.0.try_iter());
}

/// This struct temporarily stores [`LogMessage`]s before they are
/// written to [`EventWriter<LogMessage>`] by [`transfer_log_events`].
struct CapturedLogEvents(mpsc::Receiver<LogMessage>);

/// A [`Layer`] that captures log events and saves them to [`CapturedLogEvents`].
struct LogCaptureLayer {
    sender: mpsc::Sender<LogMessage>,
}
impl<S: Subscriber> Layer<S> for LogCaptureLayer {
    fn on_event(
        &self,
        event: &bevy::utils::tracing::Event<'_>,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
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
        field: &bevy::utils::tracing::field::Field,
        value: &dyn std::fmt::Debug,
    ) {
        // Only log out messages
        if field.name() == "message" {
            *self.0 = Some(format!("{value:?}"));
        }
    }
}
