//! Implementations for [`FunctionParam`].

use std::cell::{Ref, RefMut};

use bevy::ecs::world::World;
use bevy::reflect::TypeRegistration;
use logos::Span;
use smallvec::SmallVec;

use kinded::Kinded;

use crate::builtin_parser::number::{
    Float, FloatKind, Integer, IntegerKind, Number, NumberKind, SignedInteger, SignedIntegerKind,
    UnsignedInteger, UnsignedIntegerKind,
};
use crate::builtin_parser::runner::function::ParamType;
use crate::builtin_parser::{Diagnostic, Environment, SpanExtension, Spanned, StrongRef};

use super::super::error::EvalError;
use super::super::function::FunctionParam;
use super::Value;
use super::kind::ValueKind;

macro_rules! arg {
    (impl$({ $($generics:tt)* })? $ty:ty: $value:ident => $expr:expr ) => {
        impl$(<$($generics)*>)? FunctionParam for $ty {
            type State<'world, 'env, 'reg> = Option<Self>;
            type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
            type Item<'val, 'world, 'env, 'reg> = Self;
            const PARAMETER_TYPE: ParamType = ParamType::Argument;

            fn get<'world, 'env, 'reg>(
                mut value: SmallVec<[Spanned<Value>; 1]>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
                let $value = value.pop().unwrap();
                Ok(Some($expr))
            }
            fn borrow<'val, 'world, 'env, 'reg>(
                state: &'val mut Self::State<'world, 'env, 'reg>,
            ) -> Self::Guard<'val, 'world, 'env, 'reg> {
                state.take()
            }
            fn as_arg<'val, 'world, 'env, 'reg>(
                guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
            ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
                Ok(guard.take().unwrap())
            }
        }
    };
}
arg!(impl Spanned<Value>: value => value);
arg!(impl{T: TryFrom<Spanned<Value>, Error = Diagnostic<EvalError>>} Spanned<T>: value => Spanned {
    span: value.span.clone(),
    value: T::try_from(value)?,
});
arg!(impl Value: value => value.value);

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
            const PARAMETER_TYPE: ParamType = ParamType::Argument;

            fn get<'world, 'env, 'reg>(
                mut value: SmallVec<[Spanned<Value>; 1]>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
                let Spanned { span, value } = value.pop().unwrap();
                if let Value::$value_kind($value_name) = value {
                    Ok(Some($return))
                } else {
                    Err(span.wrap(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: value.kind(),
                    }).into())
                }
            }
            fn borrow<'val, 'world, 'env, 'reg>(state: &'val mut Self::State<'world, 'env, 'reg>) -> Self::Guard<'val, 'world, 'env, 'reg> { state.take() }
            fn as_arg<'val, 'world, 'env, 'reg>(guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> { Ok(guard.take().unwrap()) }
        }
        impl TryFrom<Spanned<Value>> for $type {
            type Error = Diagnostic<EvalError>;
            fn try_from(Spanned { span, value }: Spanned<Value>) -> Result<Self, Self::Error> {
                if let Value::$value_kind($value_name) = value {
                    Ok($return)
                } else {
                    Err(span.wrap(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: value.kind(),
                    }).into())
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
            const PARAMETER_TYPE: ParamType = ParamType::Argument;

            fn get<'world, 'env, 'reg>(
                mut value: SmallVec<[Spanned<Value>; 1]>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
                let Spanned { span, value } = value.pop().unwrap();

                if let Value::Reference(reference) = value {
                    reference
                        .upgrade()
                        .ok_or_else(|| span.clone().diagnose(EvalError::ReferenceToMovedData))
                        .map(|r| (r, span))
                } else {
                    Err(span.wrap(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::Reference,
                        actual: value.kind(),
                    }).into())
                }
            }

            fn borrow<'val, 'world, 'env, 'reg>(
                (state, span): &'val mut Self::State<'world, 'env, 'reg>,
            ) -> Self::Guard<'val, 'world, 'env, 'reg> {
                (state.borrow_mut(), span)
            }

            fn as_arg<'val, 'world, 'env, 'reg>(
                (guard, span): &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
            ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
                let reference = &mut **guard;
                if let Value::$value_kind(value_ref) = reference {
                    Ok(value_ref)
                } else {
                    Err(span.clone().wrap(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: reference.kind(),
                    }).into())
                }
            }
        }
        impl FunctionParam for &$type {
            type State<'world, 'env, 'reg> = (StrongRef<Value>, Span);
            type Guard<'val, 'world, 'env, 'reg> = (Ref<'val, Value>, &'val Span);
            type Item<'val, 'world, 'env, 'reg> = &'val $type;
            const PARAMETER_TYPE: ParamType = ParamType::Argument;

            fn get<'world, 'env, 'reg>(
                mut value: SmallVec<[Spanned<Value>; 1]>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
                let Spanned { span, value } = value.pop().unwrap();

                if let Value::Reference(reference) = value {
                    reference
                        .upgrade()
                        .ok_or_else(|| span.clone().diagnose(EvalError::ReferenceToMovedData))
                        .map(|r| (r, span))
                } else {
                    Err(span.wrap(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::Reference,
                        actual: value.kind(),
                    }).into())
                }
            }

            fn borrow<'val, 'world, 'env, 'reg>(
                (state, span): &'val mut Self::State<'world, 'env, 'reg>,
            ) -> Self::Guard<'val, 'world, 'env, 'reg> {
                (state.borrow(), span)
            }

            fn as_arg<'val, 'world, 'env, 'reg>(
                (guard, span): &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
            ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
                let reference = &**guard;
                if let Value::$value_kind(value_ref) = reference {
                    Ok(value_ref)
                } else {
                    Err(span.clone().wrap(EvalError::IncorrectFunctionParameterType {
                        expected: ValueKind::$kind,
                        actual: reference.kind(),
                    }).into())
                }
            }
        }
    };
}

impl_function_param_for_value!(impl ref bool: Boolean(boolean) => boolean);
impl_function_param_for_value!(impl ref Number: Number(number) {AnyNumber} => number);
impl_function_param_for_value!(impl ref String: String(string) => string);

impl_function_param_for_value!(impl std::collections::HashMap<String, Value>: Object(object) => {
    object.into_iter().map(|(k, v)| (k, v.into_inner())).collect()
});
// impl_function_param_for_value!(impl StrongRef<Value>: Reference(reference) => reference.upgrade().unwrap());

macro_rules! impl_function_param_for_group {
    ($type:ty, $pattern:pat, $var:ident, $kind:ident) => {
        impl FunctionParam for $type {
            type State<'world, 'env, 'reg> = Option<Self>;
            type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
            type Item<'val, 'world, 'env, 'reg> = Self;
            const PARAMETER_TYPE: ParamType = ParamType::Argument;

            fn get<'world, 'env, 'reg>(
                mut value: SmallVec<[Spanned<Value>; 1]>,
                _: &mut Option<&'world mut World>,
                _: &mut Option<&'env mut Environment>,
                _: &'reg [&'reg TypeRegistration],
            ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
                let Spanned { span, value } = value.pop().unwrap();
                if let Value::Number($pattern) = value {
                    Ok(Some($var))
                } else {
                    Err(span
                        .wrap(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::$kind,
                            actual: value.kind(),
                        })
                        .into())
                }
            }
            fn borrow<'val, 'world, 'env, 'reg>(
                state: &'val mut Self::State<'world, 'env, 'reg>,
            ) -> Self::Guard<'val, 'world, 'env, 'reg> {
                state.take()
            }
            fn as_arg<'val, 'world, 'env, 'reg>(
                guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
            ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
                Ok(guard.take().unwrap())
            }
        }
        impl TryFrom<Spanned<Value>> for $type {
            type Error = Diagnostic<EvalError>;
            fn try_from(Spanned { span, value }: Spanned<Value>) -> Result<Self, Self::Error> {
                if let Value::Number($pattern) = value {
                    Ok($var)
                } else {
                    Err(span
                        .wrap(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::$kind,
                            actual: value.kind(),
                        })
                        .into())
                }
            }
        }
    };
}

impl_function_param_for_group!(Integer, Number::Integer(v), v, AnyInteger);
impl_function_param_for_group!(
    UnsignedInteger,
    Number::Integer(Integer::Unsigned(v)),
    v,
    AnyUnsignedInteger
);
impl_function_param_for_group!(
    SignedInteger,
    Number::Integer(Integer::Signed(v)),
    v,
    AnySignedInteger
);
impl_function_param_for_group!(Float, Number::Float(v), v, AnyFloat);

macro_rules! impl_function_param_for_numbers {
    ($group_variant:ident, $variant:ident, $group:ident, $group_kind:ident, $generic:ident ($($number:ident),*$(,)?)) => {
        $(
            impl FunctionParam for $number {
                type State<'world, 'env, 'reg> = Option<Self>;
                type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
                type Item<'val, 'world, 'env, 'reg> = Self;
                const PARAMETER_TYPE: ParamType = ParamType::Argument;

                fn get<'world, 'env, 'reg>(
                    mut value: SmallVec<[Spanned<Value>; 1]>,
                    _: &mut Option<&'world mut World>,
                    _: &mut Option<&'env mut Environment>,
                    _: &'reg [&'reg TypeRegistration],
                ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
                    let Spanned { span, value } = value.pop().unwrap();
                    #[allow(unreachable_patterns)]
                    match value {
                        Value::Number(Number::Integer(Integer::$group_variant($group::$number(value)))) => Ok(Some(value)),
                        Value::Number(Number::Integer(Integer::Signed(SignedInteger::Unspecified(value)))) if stringify!($group_variant) != "Signed" || stringify!($generic) != "Integer" => Ok(Some(value as $number)),
                        Value::Number(Number::Integer(Integer::$group_variant($group::$generic(value)))) => Ok(Some(value as $number)),
                        _ => Err(span.wrap(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::Number(NumberKind::Integer(IntegerKind::$group_variant($group_kind::$number))),
                            actual: value.kind(),
                        }).into())
                    }
                }
                fn borrow<'val, 'world, 'env, 'reg>(state: &'val mut Self::State<'world, 'env, 'reg>) -> Self::Guard<'val, 'world, 'env, 'reg> { state.take() }
                fn as_arg<'val, 'world, 'env, 'reg>(guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> { Ok(guard.take().unwrap()) }
            }
            impl TryFrom<Spanned<Value>> for $number {
                type Error = Diagnostic<EvalError>;
                fn try_from(Spanned {span, value}: Spanned<Value>) -> Result<Self, Self::Error> {
                    #[allow(unreachable_patterns)]
                    match value {
                        Value::Number(Number::Integer(Integer::$group_variant($group::$number(value)))) => Ok(value),
                        Value::Number(Number::Integer(Integer::Signed(SignedInteger::Unspecified(value)))) if stringify!($group_variant) != "Signed" || stringify!($generic) != "Integer" => Ok(value as $number),
                        Value::Number(Number::Integer(Integer::$group_variant($group::$generic(value)))) => Ok(value as $number),
                        _ => Err(span.wrap(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::Number(NumberKind::Integer(IntegerKind::$group_variant($group_kind::$number))),
                            actual: value.kind(),
                        }).into())
                    }
                }
            }
        )*
    };
}

impl_function_param_for_numbers!(
    Unsigned,
    Unsigned,
    UnsignedInteger,
    UnsignedIntegerKind,
    u64(u8, u16, u32, u64, usize)
);
impl_function_param_for_numbers!(
    Signed,
    Signed,
    SignedInteger,
    SignedIntegerKind,
    Unspecified(i8, i16, i32, i64, isize)
);

macro_rules! impl_function_param_for_floats {
    ($generic:ident ($($number:ident),*$(,)?)) => {
        $(
            impl FunctionParam for $number {
                type State<'world, 'env, 'reg> = Option<Self>;
                type Guard<'val, 'world, 'env, 'reg> = Option<Self>;
                type Item<'val, 'world, 'env, 'reg> = Self;
                const PARAMETER_TYPE: ParamType = ParamType::Argument;

                fn get<'world, 'env, 'reg>(
                    mut value: SmallVec<[Spanned<Value>; 1]>,
                    _: &mut Option<&'world mut World>,
                    _: &mut Option<&'env mut Environment>,
                    _: &'reg [&'reg TypeRegistration],
                ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
                    let Spanned { span, value } = value.pop().unwrap();
                    #[allow(unreachable_patterns)]
                    match value {
                        Value::Number(Number::Float(Float::$number(value))) => Ok(Some(value)),
                        Value::Number(Number::Float(Float::$generic(value))) => Ok(Some(value as $number)),
                        _ => Err(span.wrap(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::Number(NumberKind::Float(FloatKind::$number)),
                            actual: value.kind(),
                        }).into())
                    }
                }
                fn borrow<'val, 'world, 'env, 'reg>(state: &'val mut Self::State<'world, 'env, 'reg>) -> Self::Guard<'val, 'world, 'env, 'reg> { state.take() }
                fn as_arg<'val, 'world, 'env, 'reg>(guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> { Ok(guard.take().unwrap()) }
            }
            impl TryFrom<Spanned<Value>> for $number {
                type Error = Diagnostic<EvalError>;
                fn try_from(Spanned {span, value}: Spanned<Value>) -> Result<Self, Self::Error> {
                    #[allow(unreachable_patterns)]
                    match value {
                        Value::Number(Number::Float(Float::$number(value))) => Ok(value),
                        Value::Number(Number::Float(Float::$generic(value))) => Ok(value as $number),
                        _ => Err(span.wrap(EvalError::IncorrectFunctionParameterType {
                            expected: ValueKind::Number(NumberKind::Float(FloatKind::$number)),
                            actual: value.kind(),
                        }).into())
                    }
                }
            }
        )*
    };
}
impl_function_param_for_floats!(Unspecified(f32, f64));

impl FunctionParam for &Value {
    type State<'world, 'env, 'reg> = StrongRef<Value>;
    type Guard<'val, 'world, 'env, 'reg> = Ref<'val, Value>;
    type Item<'val, 'world, 'env, 'reg> = &'val Value;
    const PARAMETER_TYPE: ParamType = ParamType::Argument;

    fn get<'world, 'env, 'reg>(
        mut value: SmallVec<[Spanned<Value>; 1]>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
        let Spanned { span, value } = value.pop().unwrap();

        if let Value::Reference(reference) = value {
            reference
                .upgrade()
                .ok_or_else(|| span.diagnose(EvalError::ReferenceToMovedData))
        } else {
            Err(span
                .wrap(EvalError::IncorrectFunctionParameterType {
                    expected: ValueKind::Reference,
                    actual: value.kind(),
                })
                .into())
        }
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.borrow()
    }

    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(&**guard)
    }
}

impl FunctionParam for &mut Value {
    type State<'world, 'env, 'reg> = StrongRef<Value>;
    type Guard<'val, 'world, 'env, 'reg> = RefMut<'val, Value>;
    type Item<'val, 'world, 'env, 'reg> = &'val mut Value;
    const PARAMETER_TYPE: ParamType = ParamType::Argument;

    fn get<'world, 'env, 'reg>(
        mut value: SmallVec<[Spanned<Value>; 1]>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
        let Spanned { span, value } = value.pop().unwrap();

        if let Value::Reference(reference) = value {
            reference
                .upgrade()
                .ok_or_else(|| span.diagnose(EvalError::ReferenceToMovedData))
        } else {
            Err(span
                .wrap(EvalError::IncorrectFunctionParameterType {
                    expected: ValueKind::Reference,
                    actual: value.kind(),
                })
                .into())
        }
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.borrow_mut()
    }

    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(&mut **guard)
    }
}

impl FunctionParam for &mut World {
    type State<'world, 'env, 'reg> = Option<&'world mut World>;
    type Guard<'val, 'world, 'env, 'reg> = Option<&'val mut World>;
    type Item<'val, 'world, 'env, 'reg> = &'val mut World;
    const PARAMETER_TYPE: ParamType = ParamType::Parameter;

    fn get<'world, 'env, 'reg>(
        _: SmallVec<[Spanned<Value>; 1]>,
        world: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
        let Some(world) = world.take() else {
            return Err(Diagnostic::empty(EvalError::Custom(
                "world borrowed twice".into(),
            )));
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
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(guard.as_mut().map(|w| &mut **w).unwrap())
    }
}
impl FunctionParam for &World {
    type State<'world, 'env, 'reg> = Option<&'world World>;
    type Guard<'val, 'world, 'env, 'reg> = Option<&'val World>;
    type Item<'val, 'world, 'env, 'reg> = &'val World;
    const PARAMETER_TYPE: ParamType = ParamType::Parameter;

    fn get<'world, 'env, 'reg>(
        _: SmallVec<[Spanned<Value>; 1]>,
        world: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
        let Some(world) = world.take() else {
            return Err(Diagnostic::empty(EvalError::Custom(
                "world borrowed twice".into(),
            )));
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
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(guard.as_mut().map(|w| &**w).unwrap())
    }
}

impl FunctionParam for &mut Environment {
    type State<'world, 'env, 'reg> = Option<&'env mut Environment>;
    type Guard<'val, 'world, 'env, 'reg> = Option<&'val mut Environment>;
    type Item<'val, 'world, 'env, 'reg> = &'val mut Environment;
    const PARAMETER_TYPE: ParamType = ParamType::Parameter;

    fn get<'world, 'env, 'reg>(
        _: SmallVec<[Spanned<Value>; 1]>,
        _: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(Some(environment.take().unwrap()))
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.take()
    }
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(guard.as_mut().map(|e| &mut **e).unwrap())
    }
}
impl FunctionParam for &Environment {
    type State<'world, 'env, 'reg> = Option<&'env Environment>;
    type Guard<'val, 'world, 'env, 'reg> = Option<&'val Environment>;
    type Item<'val, 'world, 'env, 'reg> = &'val Environment;
    const PARAMETER_TYPE: ParamType = ParamType::Parameter;

    fn get<'world, 'env, 'reg>(
        _: SmallVec<[Spanned<Value>; 1]>,
        _: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        _: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(Some(environment.take().unwrap()))
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.take()
    }
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(guard.as_mut().map(|e| &**e).unwrap())
    }
}

impl FunctionParam for &[&TypeRegistration] {
    type State<'world, 'env, 'reg> = Option<&'reg [&'reg TypeRegistration]>;
    type Guard<'val, 'world, 'env, 'reg> = Option<&'reg [&'reg TypeRegistration]>;
    type Item<'val, 'world, 'env, 'reg> = &'reg [&'reg TypeRegistration];
    const PARAMETER_TYPE: ParamType = ParamType::Parameter;

    fn get<'world, 'env, 'reg>(
        _: SmallVec<[Spanned<Value>; 1]>,
        _: &mut Option<&'world mut World>,
        _: &mut Option<&'env mut Environment>,
        registrations: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(Some(registrations))
    }
    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        *state
    }
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
        Ok(guard.unwrap())
    }
}

impl<T: FunctionParam> FunctionParam for Vec<T> {
    type State<'world, 'env, 'reg> = Vec<T::State<'world, 'env, 'reg>>;
    type Guard<'val, 'world, 'env, 'reg> = Vec<T::Guard<'val, 'world, 'env, 'reg>>;
    type Item<'val, 'world, 'env, 'reg> = Vec<T::Item<'val, 'world, 'env, 'reg>>;
    const PARAMETER_TYPE: ParamType = ParamType::VarArg;

    fn get<'world, 'env, 'reg>(
        values: SmallVec<[Spanned<Value>; 1]>,
        world: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        registrations: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, Diagnostic<EvalError>> {
        values
            .into_iter()
            .map(|value| {
                T::get(
                    SmallVec::from_buf([value]),
                    world,
                    environment,
                    registrations,
                )
            })
            .collect()
    }

    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg> {
        state.iter_mut().map(T::borrow).collect()
    }

    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, Diagnostic<EvalError>> {
        guard.iter_mut().map(T::as_arg).collect()
    }
}
