use std::{cell::Ref, ops::Range};

use bevy::log::info;

use super::{RunError, Value, Environment, Spanned};

fn print(value: Spanned<Value>) -> Result<(), RunError> {
    match value.value {
        Value::String(string) => info!("{string}"),
        _ => {
            let string = value.value.try_format(value.span)?;
            info!("{string}");
        }
    }
    Ok(())
}

fn dbg(any: Value) {
    info!("Value::{any:?}");
}

fn ref_depth(Spanned { span, value }: Spanned<Value>) -> Result<f64, RunError> {
    fn ref_depth_reference(value: Ref<Value>, span: Range<usize>) -> Result<f64, RunError> {
        Ok(match &*value {
            Value::Reference(reference) => {
                ref_depth_reference(
                    reference
                        .upgrade()
                        .ok_or(RunError::ReferenceToMovedData(span.clone()))?
                        .borrow(),
                    span,
                )? + 1.0
            }
            _ => 0.0,
        })
    }

    Ok(match value {
        Value::Reference(reference) => {
            ref_depth_reference(
                reference
                    .upgrade()
                    .ok_or(RunError::ReferenceToMovedData(span.clone()))?
                    .borrow(),
                span,
            )? + 1.0
        }
        _ => 0.0,
    })
}

/// Macro for mass registering functions.
///
/// ```
/// fn a() {}
/// fn b() {}
/// fn c() {}
///
/// # let mut environment = bevy_dev_console::builtin_parser::Environment::default();
/// # use bevy_dev_console::register;
/// register!(environment => {
///     fn a;
///     fn b;
///     fn c;
/// });
/// ```
#[macro_export]
macro_rules! register {
    {
        $environment:expr => fn $fn_name:ident;
    } => {
        $environment
            .register_fn(stringify!($fn_name), $fn_name)
    };
    {
        $environment:expr => {
            $(
                fn $fn_name:ident;
            )*
        }
    } => {
        $environment
        $(
            .register_fn(stringify!($fn_name), $fn_name)
        )*
    };
}
pub fn register(environment: &mut Environment) {
    register!(environment => {
        fn print;
        fn dbg;
        fn ref_depth;
    });
}
