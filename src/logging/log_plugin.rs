#![warn(missing_docs)]
//! This crate provides logging functions and configuration for [Bevy](https://bevyengine.org)
//! apps, and automatically configures platform specific log handlers (i.e. WASM or Android).
//!
//! The macros provided for logging are reexported from [`tracing`](https://docs.rs/tracing),
//! and behave identically to it.
//!
//! By default, the [`ConsoleLogPlugin`] from this crate is included in Bevy's `DefaultPlugins`
//! and the logging macros can be used out of the box, if used.
//!
//! For more fine-tuned control over logging behavior, set up the [`ConsoleLogPlugin`] or
//! `DefaultPlugins` during app initialization.

#[cfg(feature = "trace")]
use std::panic;
use std::sync::{Arc, Mutex};
use web_time::SystemTime;
#[cfg(target_os = "android")]
mod android_tracing;

#[cfg(feature = "trace_tracy_memory")]
#[global_allocator]
static GLOBAL: tracy_client::ProfiledAllocator<std::alloc::System> =
    tracy_client::ProfiledAllocator::new(std::alloc::System, 100);

use bevy::ecs::event::{Event, EventWriter};
use bevy::ecs::system::{Res, Resource};
use bevy::log::trace;
use bevy::utils::tracing::Subscriber;
pub use bevy::utils::tracing::{warn, Level};

use bevy::app::{App, Plugin, Update};
use tracing_log::LogTracer;
use tracing_subscriber::field::Visit;
#[cfg(feature = "tracing-chrome")]
use tracing_subscriber::fmt::{format::DefaultFields, FormattedFields};
use tracing_subscriber::layer::Layer;
use tracing_subscriber::prelude::*;
use tracing_subscriber::registry::Registry;
use tracing_subscriber::EnvFilter;

/// [`bevy_dev_console`](crate)'s custom [LogPlugin](bevy::log::LogPlugin).
/// This plugin allows [`bevy_dev_console`](crate) to access Bevy logs
/// and display them in the developer console.
///
/// # Panics
///
/// This plugin should not be added multiple times in the same process. This plugin
/// sets up global logging configuration for **all** Apps in a given process, and
/// rerunning the same initialization multiple times will lead to a panic.
///
/// **This means you have to remove Bevy's built-in [`LogPlugin`](bevy::log::LogPlugin) for this to work!**
pub struct ConsoleLogPlugin {
    /// Filters logs using the [`EnvFilter`] format
    pub filter: String,

    /// Filters out logs that are "less than" the given level.
    /// This can be further filtered using the `filter` setting.
    pub level: Level,
}

impl ConsoleLogPlugin {
    /// Appends a filter to the [`ConsoleLogPlugin`], allowing you to change the
    /// log level of a specific module/crate.
    ///
    /// ## Examples
    ///
    /// Changing the log level of the current module to `TRACE`.
    /// ```
    /// # use bevy_dev_console::prelude::ConsoleLogPlugin;
    /// # use bevy::log::Level;
    /// ConsoleLogPlugin::default()
    ///     .append_filter(module_path!(), Level::TRACE)
    /// # ;
    /// ```
    pub fn append_filter(mut self, target: &str, level: Level) -> Self {
        self.filter.push(',');
        self.filter += target;
        self.filter.push('=');
        self.filter += level.as_str();
        self
    }
}

impl Default for ConsoleLogPlugin {
    fn default() -> Self {
        Self {
            filter: "wgpu=error,naga=warn,bevy_dev_console=trace".to_string(),
            level: Level::INFO,
        }
    }
}

impl Plugin for ConsoleLogPlugin {
    #[cfg_attr(not(feature = "tracing-chrome"), allow(unused_variables))]
    fn build(&self, app: &mut App) {
        #[cfg(feature = "trace")]
        {
            let old_handler = panic::take_hook();
            panic::set_hook(Box::new(move |infos| {
                println!("{}", tracing_error::SpanTrace::capture());
                old_handler(infos);
            }));
        }
        trace!("a");

        let finished_subscriber;
        let default_filter = { format!("{},{}", self.level, self.filter) };
        let filter_layer = EnvFilter::try_from_default_env()
            .or_else(|_| EnvFilter::try_new(&default_filter))
            .unwrap();

        let log_events = LogEvents(Arc::new(Mutex::new(Vec::new())));

        let log_event_handler = LogEventLayer {
            events: log_events.0.clone(),
        };

        app.insert_resource(log_events);
        app.add_event::<LogMessage>();
        app.add_systems(Update, transfer_log_events);

        let subscriber = Registry::default()
            .with(filter_layer)
            .with(log_event_handler);

        #[cfg(feature = "trace")]
        let subscriber = subscriber.with(tracing_error::ErrorLayer::default());

        #[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
        {
            #[cfg(feature = "tracing-chrome")]
            let chrome_layer = {
                let mut layer = tracing_chrome::ChromeLayerBuilder::new();
                if let Ok(path) = std::env::var("TRACE_CHROME") {
                    layer = layer.file(path);
                }
                let (chrome_layer, guard) = layer
                    .name_fn(Box::new(|event_or_span| match event_or_span {
                        tracing_chrome::EventOrSpan::Event(event) => event.metadata().name().into(),
                        tracing_chrome::EventOrSpan::Span(span) => {
                            if let Some(fields) =
                                span.extensions().get::<FormattedFields<DefaultFields>>()
                            {
                                format!("{}: {}", span.metadata().name(), fields.fields.as_str())
                            } else {
                                span.metadata().name().into()
                            }
                        }
                    }))
                    .build();
                app.world.insert_non_send_resource(guard);
                chrome_layer
            };

            #[cfg(feature = "tracing-tracy")]
            let tracy_layer = tracing_tracy::TracyLayer::new();

            let fmt_layer = tracing_subscriber::fmt::Layer::default().with_writer(std::io::stderr);

            // bevy_render::renderer logs a `tracy.frame_mark` event every frame
            // at Level::INFO. Formatted logs should omit it.
            #[cfg(feature = "tracing-tracy")]
            let fmt_layer =
                fmt_layer.with_filter(tracing_subscriber::filter::FilterFn::new(|meta| {
                    meta.fields().field("tracy.frame_mark").is_none()
                }));

            let subscriber = subscriber.with(fmt_layer);

            #[cfg(feature = "tracing-chrome")]
            let subscriber = subscriber.with(chrome_layer);
            #[cfg(feature = "tracing-tracy")]
            let subscriber = subscriber.with(tracy_layer);

            finished_subscriber = subscriber;
        }

        #[cfg(target_arch = "wasm32")]
        {
            console_error_panic_hook::set_once();
            finished_subscriber = subscriber.with(tracing_wasm::WASMLayer::new(
                tracing_wasm::WASMLayerConfig::default(),
            ));
        }

        #[cfg(target_os = "android")]
        {
            finished_subscriber = subscriber.with(android_tracing::AndroidLayer::default());
        }

        let logger_already_set = LogTracer::init().is_err();
        let subscriber_already_set =
            bevy::utils::tracing::subscriber::set_global_default(finished_subscriber).is_err();

        match (logger_already_set, subscriber_already_set) {
            (true, true) => warn!(
                "Could not set global logger and tracing subscriber as they are already set. Consider disabling ConsoleLogPlugin."
            ),
            (true, _) => warn!("Could not set global logger as it is already set. Consider disabling ConsoleLogPlugin."),
            (_, true) => warn!("Could not set global tracing subscriber as it is already set. Consider disabling ConsoleLogPlugin."),
            _ => (),
        }
    }
}

/// A [`tracing`](bevy::utils::tracing) log message event.
///
/// This event is helpful for creating custom log viewing systems such as consoles and terminals.
#[derive(Event, Debug, Clone)]
pub struct LogMessage {
    /// The message contents.
    pub message: String,
    /// The name of the span described by this metadata.
    pub name: &'static str,
    /// The part of the system that the span that this metadata describes
    /// occurred in.
    pub target: &'static str,

    /// The level of verbosity of the described span.
    pub level: Level,

    /// The name of the Rust module where the span occurred, or `None` if this
    /// could not be determined.
    pub module_path: Option<&'static str>,

    /// The name of the source code file where the span occurred, or `None` if
    /// this could not be determined.
    pub file: Option<&'static str>,

    /// The line number in the source code file where the span occurred, or
    /// `None` if this could not be determined.
    pub line: Option<u32>,

    /// The time the log occurred.
    /// It is recommended to use a third-party crate like
    /// [chrono](https://crates.io/crates/chrono) to format the [`SystemTime`].
    pub time: SystemTime,
}

/// Transfers information from the [`LogEvents`] resource to [`Events<LogEvent>`](bevy::ecs::event::Events<LogEvent>).
fn transfer_log_events(handler: Res<LogEvents>, mut log_events: EventWriter<LogMessage>) {
    let events = &mut *handler.0.lock().unwrap();
    if !events.is_empty() {
        log_events.send_batch(std::mem::take(events));
    }
}

/// This struct temporarily stores [`LogMessage`]s before they are
/// written to [`EventWriter<LogMessage>`] by [`transfer_log_events`].
#[derive(Resource)]
struct LogEvents(Arc<Mutex<Vec<LogMessage>>>);

/// A [`Layer`] that captures log events and saves them to [`LogEvents`].
struct LogEventLayer {
    events: Arc<Mutex<Vec<LogMessage>>>,
}
impl<S: Subscriber> Layer<S> for LogEventLayer {
    fn on_event(
        &self,
        event: &bevy::utils::tracing::Event<'_>,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        let mut message = None;
        event.record(&mut LogEventVisitor(&mut message));
        if let Some(message) = message {
            let metadata = event.metadata();
            self.events.lock().unwrap().push(LogMessage {
                message,
                name: metadata.name(),
                target: metadata.target(),
                level: *metadata.level(),
                module_path: metadata.module_path(),
                file: metadata.file(),
                line: metadata.line(),
                time: SystemTime::now(),
            });
        }
    }
}

/// A [`Visit`]or that records log messages that are transfered to [`LogEventLayer`].
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
