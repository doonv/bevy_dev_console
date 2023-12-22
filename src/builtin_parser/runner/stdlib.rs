use crate::register;
use bevy::log::info;
use std::{cell::Ref, ops::Range};

use super::{Environment, RunError, Spanned, Value};

// This doesn't work because of a bug
// fn print(value: Spanned<Value>, world: &mut World) -> Result<(), RunError> {
//     match value.value {
//         Value::String(string) => info!("{string}"),
//         _ => {
//             let string = value.value.try_format(value.span, world)?;
//             info!("{string}");
//         }
//     }
//     Ok(())
// }

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

/// Disposes of a [`Value`].
fn drop(_v: Value) {}

pub fn register(environment: &mut Environment) {
    register!(environment => {
        // fn print;
        fn dbg;
        fn ref_depth;
        fn drop;
    });
}
