//! Implementations for [`FunctionParam`].

use std::cell::{Ref, RefMut};

use bevy::ecs::world::World;
use bevy::reflect::TypeRegistration;
use logos::Span;

use crate::builtin_parser::number::{Number, NumberKind};
use crate::builtin_parser::{Environment, Spanned, StrongRef};
use kinded::Kinded;

use super::super::error::EvalError;
use super::super::function::FunctionParam;
use super::Value;
use super::kind::ValueKind;

impl FunctionParam for Spanned<Value> {
    type State<'world, 'env, 'reg> = Option<Self>;
    type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
    type Item<'val, 'world, 'env, 'reg> = Self;
    const IS_ARGUMENT: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
        Ok(Some(value.unwrap()))
    }
    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.take()
    }
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
        Ok(guard.take().unwrap())
    }
}

impl<T: TryFrom<Spanned<Value>, Error = EvalError>> FunctionParam for Spanned<T> {
    type State<'world, 'env, 'reg> = Option<Self>;
    type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
    type Item<'val, 'world, 'env, 'reg> = Self;
    const IS_ARGUMENT: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
        let value = value.unwrap();
        Ok(Some(Spanned {
            span: value.span.clone(),
            value: T::try_from(value)?,
        }))
    }
    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.take()
    }
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
        Ok(guard.take().unwrap())
    }
}

impl FunctionParam for Value {
    type State<'world, 'env, 'reg> = Option<Self>;
    type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
    type Item<'val, 'world, 'env, 'reg> = Self;
    const IS_ARGUMENT: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
        Ok(Some(value.unwrap().value))
    }
    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.take()
    }
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
        Ok(guard.take().unwrap())
    }
}

macro_rules! impl_function_param_for_value {
    (impl $type:ty: $value_kind:ident($value_name:ident) => $return:expr) => {
        impl_function_param_for_value!(impl $type: $value_kind($value_name) {$value_kind} => $return);
    };
    (impl ref $type:ty: $value_kind:ident($value_name:ident) => $return:expr) => {
        impl_function_param_for_value!(impl ref $type: $value_kind($value_name) {$value_kind} => $return);
    };
    (impl $type:ty: $value_kind:ident($value_name:ident) {$kind:ident} => $return:expr) => {
        impl FunctionParam for $type {
            type State<'world, 'env, 'reg> = Option<Self>;
            type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
            type Item<'val, 'world, 'env, 'reg> = Self;
            const IS_ARGUMENT: bool = true;

            fn get<'world, 'env, 'reg>(
                value: Option<Spanned<Value>>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
                let Spanned { span, value } = value.unwrap();
                if let Value::$value_kind($value_name) = value {
                    Ok(Some($return))
                } else {
                    Err(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: value.kind(),
                        span,
                    })
                }
            }
            fn borrow<'val, 'world, 'env, 'reg>(state: &'val mut Self::State<'world, 'env, 'reg>) -> Self::Guard<'val, 'world, 'env, 'reg> { state.take() }
            fn as_arg<'val, 'world, 'env, 'reg>(guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> { Ok(guard.take().unwrap()) }
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
    (impl ref $type:ty: $value_kind:ident($value_name:ident) {$kind:ident} => $return:expr) => {
        impl_function_param_for_value!(impl $type: $value_kind($value_name) {$kind} => $return);

        impl FunctionParam for &mut $type {
            type State<'world, 'env, 'reg> = (StrongRef<Value>, Span);
            type Guard<'val, 'world, 'env, 'reg> = (RefMut<'val, Value>, &'val Span);
            type Item<'val, 'world, 'env, 'reg> = &'val mut $type;
            const IS_ARGUMENT: bool = true;

            fn get<'world, 'env, 'reg>(
                value: Option<Spanned<Value>>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
                let Spanned { span, value } = value.unwrap();

                if let Value::Reference(reference) = value {
                    reference
                        .upgrade()
                        .ok_or(EvalError::ReferenceToMovedData(span.clone()))
                        .map(|r| (r, span))
                } else {
                    Err(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::Reference,
                        actual: value.kind(),
                        span,
                    })
                }
            }

            fn borrow<'val, 'world, 'env, 'reg>(
                (state, span): &'val mut Self::State<'world, 'env, 'reg>,
            ) -> Self::Guard<'val, 'world, 'env, 'reg> {
                (state.borrow_mut(), span)
            }

            fn as_arg<'val, 'world, 'env, 'reg>(
                (guard, span): &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
            ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
                let reference = &mut **guard;
                if let Value::$value_kind(value_ref) = reference {
                    Ok(value_ref)
                } else {
                    Err(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: reference.kind(),
                        span: span.clone(),
                    })
                }
            }
        }
        impl FunctionParam for &$type {
            type State<'world, 'env, 'reg> = (StrongRef<Value>, Span);
            type Guard<'val, 'world, 'env, 'reg> = (Ref<'val, Value>, &'val Span);
            type Item<'val, 'world, 'env, 'reg> = &'val $type;
            const IS_ARGUMENT: bool = true;

            fn get<'world, 'env, 'reg>(
                value: Option<Spanned<Value>>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
                let Spanned { span, value } = value.unwrap();

                if let Value::Reference(reference) = value {
                    reference
                        .upgrade()
                        .ok_or(EvalError::ReferenceToMovedData(span.clone()))
                        .map(|r| (r, span))
                } else {
                    Err(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::Reference,
                        actual: value.kind(),
                        span,
                    })
                }
            }

            fn borrow<'val, 'world, 'env, 'reg>(
                (state, span): &'val mut Self::State<'world, 'env, 'reg>,
            ) -> Self::Guard<'val, 'world, 'env, 'reg> {
                (state.borrow(), span)
            }

            fn as_arg<'val, 'world, 'env, 'reg>(
                (guard, span): &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
            ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
                let reference = &**guard;
                if let Value::$value_kind(value_ref) = reference {
                    Ok(value_ref)
                } else {
                    Err(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: reference.kind(),
                        span: span.clone(),
                    })
                }
            }
        }
    }
}

impl_function_param_for_value!(impl ref bool: Boolean(boolean) => boolean);
impl_function_param_for_value!(impl ref Number: Number(number) {AnyNumber} => number);
impl_function_param_for_value!(impl ref String: String(string) => string);

impl_function_param_for_value!(impl std::collections::HashMap<String, Value>: Object(object) => {
    object.into_iter().map(|(k, v)| (k, v.into_inner())).collect()
});
// impl_function_param_for_value!(impl StrongRef<Value>: Reference(reference) => reference.upgrade().unwrap());

macro_rules! impl_function_param_for_numbers {
    ($generic:ident ($($number:ident),*$(,)?)) => {
        $(
            impl FunctionParam for $number {
                type State<'world, 'env, 'reg> = Option<Self>;
                type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
                type Item<'val, 'world, 'env, 'reg> = Self;
                const IS_ARGUMENT: bool = true;

                fn get<'world, 'env, 'reg>(
                    value: Option<Spanned<Value>>,
                    _: &mut Option<&'world mut World>,
                    _: &mut Option<&'env mut Environment>,
                    _: &'reg [&'reg TypeRegistration],
                ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
                    let Spanned { span, value } = value.unwrap();
                    match value {
                        Value::Number(Number::$number(value)) => Ok(Some(value)),
                        Value::Number(Number::$generic(value)) => Ok(Some(value as $number)),
                        _ => Err(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::Number(NumberKind::$number),
                            actual: value.kind(),
                            span,
                        })
                    }
                }
                fn borrow<'val, 'world, 'env, 'reg>(state: &'val mut Self::State<'world, 'env, 'reg>) -> Self::Guard<'val, 'world, 'env, 'reg> { state.take() }
                fn as_arg<'val, 'world, 'env, 'reg>(guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> { Ok(guard.take().unwrap()) }
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

impl FunctionParam for &Value {
    type State<'world, 'env, 'reg> = StrongRef<Value>;
    type Guard<'val, 'world, 'env, 'reg> = Ref<'val, Value>;
    type Item<'val, 'world, 'env, 'reg> = &'val Value;
    const IS_ARGUMENT: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
        let Spanned { span, value } = value.unwrap();

        if let Value::Reference(reference) = value {
            reference
                .upgrade()
                .ok_or(EvalError::ReferenceToMovedData(span))
        } else {
            Err(EvalError::IncorrectFunctionParameterType {
                expected: ValueKind::Reference,
                actual: value.kind(),
                span,
            })
        }
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.borrow()
    }

    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
        Ok(&**guard)
    }
}

impl FunctionParam for &mut Value {
    type State<'world, 'env, 'reg> = StrongRef<Value>;
    type Guard<'val, 'world, 'env, 'reg> = RefMut<'val, Value>;
    type Item<'val, 'world, 'env, 'reg> = &'val mut Value;
    const IS_ARGUMENT: bool = true;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
        let Spanned { span, value } = value.unwrap();

        if let Value::Reference(reference) = value {
            reference
                .upgrade()
                .ok_or(EvalError::ReferenceToMovedData(span))
        } else {
            Err(EvalError::IncorrectFunctionParameterType {
                expected: ValueKind::Reference,
                actual: value.kind(),
                span,
            })
        }
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.borrow_mut()
    }

    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
        Ok(&mut **guard)
    }
}

impl FunctionParam for &mut World {
    type State<'world, 'env, 'reg> = Option<&'world mut World>;
    type Guard<'val, 'world, 'env, 'reg> = Option<&'val mut World>;
    type Item<'val, 'world, 'env, 'reg> = &'val mut World;
    const IS_ARGUMENT: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        world: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
        let Some(world) = world.take() else {
            return Err(EvalError::Custom {
                text: "world borrowed twice".into(),
                span: 0..0,
            });
        };
        Ok(Some(world))
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.take()
    }

    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
        Ok(guard.as_mut().map(|w| &mut **w).unwrap())
    }
}

impl FunctionParam for &mut Environment {
    type State<'world, 'env, 'reg> = Option<&'env mut Environment>;
    type Guard<'val, 'world, 'env, 'reg> = Option<&'val mut Environment>;
    type Item<'val, 'world, 'env, 'reg> = &'val mut Environment;
    const IS_ARGUMENT: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
        Ok(Some(environment.take().unwrap()))
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.take()
    }
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
        Ok(guard.as_mut().map(|e| &mut **e).unwrap())
    }
}

impl FunctionParam for &[&TypeRegistration] {
    type State<'world, 'env, 'reg> = Option<&'reg [&'reg TypeRegistration]>;
    type Guard<'val, 'world, 'env, 'reg> = Option<&'reg [&'reg TypeRegistration]>;
    type Item<'val, 'world, 'env, 'reg> = &'reg [&'reg TypeRegistration];
    const IS_ARGUMENT: bool = false;

    fn get<'world, 'env, 'reg>(
        _: Option<Spanned<Value>>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        registrations: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError> {
        Ok(Some(registrations))
    }
    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        *state
    }
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError> {
        Ok(guard.unwrap())
    }
}
