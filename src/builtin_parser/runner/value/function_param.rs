//! FunctionParam implementations for Value

use bevy::ecs::world::World;
use bevy::reflect::TypeRegistration;

use crate::builtin_parser::number::{Number, NumberKind};
use crate::builtin_parser::{Environment, Spanned, StrongRef};
use kinded::Kinded;

use super::super::error::EvalError;
use super::super::function::FunctionParam;
use super::Value;
use super::kind::ValueKind;

impl FunctionParam for Spanned<Value> {
    type Item<'world, 'env, 'reg> = Self;
    const IS_ARGUMENT: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError> {
        Ok(value.unwrap())
    }
}

impl<T: TryFrom<Spanned<Value>, Error = EvalError>> FunctionParam for Spanned<T> {
    type Item<'world, 'env, 'reg> = Self;
    const IS_ARGUMENT: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError> {
        let value = value.unwrap();
        Ok(Spanned {
            span: value.span.clone(),
            value: T::try_from(value)?,
        })
    }
}

impl FunctionParam for Value {
    type Item<'world, 'env, 'reg> = Self;
    const IS_ARGUMENT: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError> {
        Ok(value.unwrap().value)
    }
}

macro_rules! impl_function_param_for_value {
    (impl $type:ty: $value_kind:ident($value_name:ident) => $return:expr) => {
        impl_function_param_for_value!(impl $type: $value_kind($value_name) {$value_kind} => $return);
    };
    (impl $type:ty: $value_kind:ident($value_name:ident) {$kind:ident} => $return:expr) => {
        impl FunctionParam for $type {
            type Item<'world, 'env, 'reg> = Self;
            const IS_ARGUMENT: bool = true;

            fn get<'world, 'env, 'reg>(
                value: Option<Spanned<Value>>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError> {
                let Spanned { span, value } = value.unwrap();
                if let Value::$value_kind($value_name) = value {
                    Ok($return)
                } else {
                    Err(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: value.kind(),
                        span,
                    })
                }
            }
        }
        impl TryFrom<Spanned<Value>> for $type {
            type Error = EvalError;

            fn try_from(Spanned { span, value }: Spanned<Value>) -> Result<Self, Self::Error> {
                if let Value::$value_kind($value_name) = value {
                    Ok($return)
                } else {
                    Err(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: value.kind(),
                        span,
                    })
                }
            }
        }
    };
}

macro_rules! impl_function_param_for_numbers {
    ($generic:ident ($($number:ident),*$(,)?)) => {
        $(
            impl FunctionParam for $number {
                type Item<'world, 'env, 'reg> = Self;
                const IS_ARGUMENT: bool = true;

                fn get<'world, 'env, 'reg>(
                    value: Option<Spanned<Value>>,
                    _: &mut Option<&'world mut World>,
                    _: &mut Option<&'env mut Environment>,
                    _: &'reg [&'reg TypeRegistration],
                ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError> {
                    let Spanned { span, value } = value.unwrap();
                    match value {
                        Value::Number(Number::$number(value)) => Ok(value),
                        Value::Number(Number::$generic(value)) => Ok(value as $number),
                        _ => Err(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::Number(NumberKind::$number),
                            actual: value.kind(),
                            span,
                        })
                    }
                }
            }
            impl TryFrom<Spanned<Value>> for $number {
                type Error = EvalError;

                fn try_from(Spanned {span, value}: Spanned<Value>) -> Result<Self, Self::Error> {
                    match value {
                        Value::Number(Number::$number(value)) => Ok(value),
                        Value::Number(Number::$generic(value)) => Ok(value as $number),
                        _ => Err(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::Number(NumberKind::$number),
                            actual: value.kind(),
                            span
                        })
                    }
                }
            }
        )*
    };
}

impl_function_param_for_numbers!(Float(f32, f64));
impl_function_param_for_numbers!(Integer(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize));

impl_function_param_for_value!(impl bool: Boolean(boolean) => boolean);
impl_function_param_for_value!(impl Number: Number(number) {AnyNumber} => number);
impl_function_param_for_value!(impl String: String(string) => string);

impl_function_param_for_value!(impl std::collections::HashMap<String, Value>: Object(object) => {
    object.into_iter().map(|(k, v)| (k, v.into_inner())).collect()
});
impl_function_param_for_value!(impl StrongRef<Value>: Reference(reference) => reference.upgrade().unwrap());

impl FunctionParam for &mut World {
    type Item<'world, 'env, 'reg> = &'world mut World;
    const IS_ARGUMENT: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        world: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError> {
        let Some(world) = world.take() else {
            // make this unreachable by checking the function when it gets registered
            return Err(EvalError::Custom {
                text: "world borrowed twice".into(),
                span: 0..0,
            });
        };

        Ok(world)
    }
}

impl FunctionParam for &mut Environment {
    type Item<'world, 'env, 'reg> = &'env mut Environment;
    const IS_ARGUMENT: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError> {
        Ok(environment.take().unwrap())
    }
}

impl FunctionParam for &[&TypeRegistration] {
    type Item<'world, 'env, 'reg> = &'reg [&'reg TypeRegistration];
    const IS_ARGUMENT: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        registrations: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError> {
        Ok(registrations)
    }
}
