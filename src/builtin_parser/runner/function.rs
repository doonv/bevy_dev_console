//! [`Function`] registration and handling

use std::fmt::Debug;

use bevy::ecs::world::World;
use bevy::reflect::TypeRegistration;
use variadics_please::all_tuples;

use super::super::Spanned;
use super::super::parser::Expression;
use super::environment::Environment;
use super::error::EvalError;
use super::{EvalParams, Value, eval_expression};

/// Get around implementation of [`Result`] causing stupid errors
pub(super) struct ResultContainer<T, E>(pub Result<T, E>);

/// Trait for types that can be returned from a registered [`Function`].
pub(super) trait FunctionReturn {
    fn into_result_container(self) -> ResultContainer<Value, EvalError>;
}

impl<T: Into<Value>> FunctionReturn for T {
    fn into_result_container(self) -> ResultContainer<Value, EvalError> {
        ResultContainer(Ok(self.into()))
    }
}

impl<T: Into<Value>, E: Into<EvalError>> FunctionReturn for Result<T, E> {
    fn into_result_container(self) -> ResultContainer<Value, EvalError> {
        ResultContainer(self.map(Into::into).map_err(Into::into))
    }
}

impl<T, E> From<ResultContainer<T, E>> for Result<T, E> {
    fn from(ResultContainer(result): ResultContainer<T, E>) -> Self {
        result
    }
}

/// A parameter in a [`Function`].
///
/// This trait uses a three-step process to allow for parameters like [`&Value`] to be possible.
///
/// 1. **[`get`](Self::get)**: Gets the parameter data.
/// 2. **[`borrow`](Self::borrow)**: Creates a [`Ref`]/[`RefMut`] of that data, or just transfers the data over.
/// 3. **[`as_arg`](Self::as_arg)**: [`Ref<T>`] is converted into `&T`, or just transfers the data over again.
///
/// [`&Value`]: Value
/// [`Ref`]: std::cell::Ref
/// [`RefMut`]: std::cell::RefMut
/// [`Ref<T>`]: std::cell::Ref
/// [`Item`]: Self::Item
pub trait FunctionParam: Sized {
    /// The data of the parameter.
    type State<'world, 'env, 'reg>;

    /// A temporary guard (like a [`Ref`](std::cell::Ref) that must live on the stack
    /// while the function is being executed. Or just the same as [`State`](Self::State).
    type Guard<'val, 'world, 'env, 'reg>;

    /// The final type passed as an argument to the function.
    ///
    /// Should always be `Self`, but [associated type defaults are unstable](https://github.com/rust-lang/rust/issues/29661).
    type Item<'val, 'world, 'env, 'reg>; // = Self

    /// Determines if this parameter corresponds to a positional argument.
    ///
    /// - If `true`, an argument is consumed from the function call.
    /// - If `false`, this parameter is "injected" from the runner context
    ///   (e.g., `&mut World`).
    const IS_ARGUMENT: bool;

    /// Step 1: Initialize the parameter state.
    ///
    /// If [`IS_ARGUMENT`](Self::IS_ARGUMENT) is true, `value` will be `Some`.
    /// Otherwise, it will be `None`.
    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        world: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        registrations: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::State<'world, 'env, 'reg>, EvalError>;

    /// Step 2: Create a [`Guard`](Self::Guard) from the [`State`] if needed, otherwise just transfer over the [`State`].
    ///
    /// [`State`]: Self::State
    fn borrow<'val, 'world, 'env, 'reg>(
        state: &'val mut Self::State<'world, 'env, 'reg>,
    ) -> Self::Guard<'val, 'world, 'env, 'reg>;

    /// Step 3: Produce the final argument.
    fn as_arg<'val, 'world, 'env, 'reg>(
        guard: &'val mut Self::Guard<'_, 'world, 'env, 'reg>,
    ) -> Result<Self::Item<'val, 'world, 'env, 'reg>, EvalError>;
}
pub type FunctionType = dyn FnMut(Vec<Spanned<Expression>>, EvalParams) -> Result<Value, EvalError>;
pub struct Function {
    pub argument_count: usize,
    pub body: Box<FunctionType>,
}
impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("argument_count", &self.argument_count)
            .finish_non_exhaustive()
    }
}

/// Trait that represents a [`Fn`] that can be turned into a parser [`Function`].
pub trait IntoFunction<T> {
    fn into_function(self) -> Function;
}

macro_rules! impl_into_function {
    (
        $(#[$meta:meta])* $($(
                $params:ident
        ),+)?
    ) => {
        $(#[$meta])*
        #[allow(non_snake_case, reason = "param types (like T0, T1, T2...) are used as variable names")]
        impl<F: 'static $(, $($params: FunctionParam),+ )?, R> IntoFunction<( $($($params,)+)? )> for F
        where
            for<'val, 'world, 'env, 'reg> &'val mut F:
                FnMut( $($($params),*)? ) -> R +
                FnMut( $($(<$params as FunctionParam>::Item<'val, 'world, 'env, 'reg>),*)? ) -> R,
            R: FunctionReturn,
        {
            fn into_function(mut self) -> Function {
                #[allow(unused_variables, unused_mut)]
                let body = Box::new(move |args: Vec<Spanned<Expression>>, params: EvalParams| {
                    let EvalParams {
                        world,
                        environment,
                        registrations,
                    } = params;
                    let mut args = args.into_iter().map(|expr| {
                        Ok(Spanned {
                            span: expr.span.clone(),
                            value: eval_expression(
                                expr,
                                EvalParams {
                                    world,
                                    environment,
                                    registrations,
                                }
                            )?
                        })
                    }).collect::<Result<Vec<_>, EvalError>>()?.into_iter();
                    let world = &mut Some(world);
                    let environment = &mut Some(environment);

                    $(
                        $(
                            let arg = $params::IS_ARGUMENT.then(|| args.next().unwrap());

                            let mut $params = $params::get(
                                arg,
                                world,
                                environment,
                                registrations
                            )?;
                        )+
                    )?

                    $(
                        $(
                            let mut $params = $params::borrow(&mut $params);
                        )+
                    )?

                    #[allow(clippy::too_many_arguments)]
                    fn call_inner<R: FunctionReturn, $($($params),*)?>(
                        mut f: impl FnMut($($($params),*)?) -> R,
                        $($($params: $params),*)?
                    ) -> R {
                        f($($($params),*)?)
                    }

                    call_inner(
                        &mut self,
                        $($(
                            $params::as_arg(&mut $params)?
                        ),+)?
                    )
                    .into_result_container().into()
                });

                let argument_count = $($(
                    $params::IS_ARGUMENT as usize +
                )+)? 0;

                Function { body, argument_count }
            }
        }
    }
}

all_tuples!(
    #[doc(fake_variadic)]
    impl_into_function,
    0,
    15,
    T
);
