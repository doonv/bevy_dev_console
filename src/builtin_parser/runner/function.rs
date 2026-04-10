//! Function registration and handling

use std::fmt::Debug;

use bevy::ecs::world::World;
use bevy::reflect::TypeRegistration;
use variadics_please::all_tuples;

use super::super::Spanned;
use super::super::parser::Expression;
use super::environment::Environment;
use super::error::EvalError;
use super::{EvalParams, Value, eval_expression};

/// Get around implementation of Result causing stupid errors
pub(super) struct ResultContainer<T, E>(pub Result<T, E>);

/// Trait for types that can be returned from a registered function.
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
pub trait FunctionParam: Sized {
    /// TODO: Add `Self` as default when <https://github.com/rust-lang/rust/issues/29661> gets merged
    type Item<'world, 'env, 'reg>;

    /// Whether this parameter is a function argument.
    /// - If [`true`], an argument is consumed and [`get`](Self::get)'s `value` is populated with some [`Value`].
    /// - Otherwise no argument is consumed and `value` is [`None`].
    const IS_ARGUMENT: bool;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        world: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        registrations: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, EvalError>;
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

/// Trait that represents a [`Fn`] that can be turned into a [`Function`].
pub trait IntoFunction<T> {
    fn into_function(self) -> Function;
}

macro_rules! impl_into_function {
    (
        $($(
                $params:ident
        ),+)?
    ) => {
        #[allow(non_snake_case)]
        impl<F: 'static $(, $($params: FunctionParam),+ )?, R> IntoFunction<( $($($params,)+)? )> for F
        where
            for<'a, 'world, 'env, 'reg> &'a mut F:
                FnMut( $($($params),*)? ) -> R +
                FnMut( $($(<$params as FunctionParam>::Item<'world, 'env, 'reg>),*)? ) -> R,
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

                    #[allow(clippy::too_many_arguments)]
                    fn call_inner<R: FunctionReturn, $($($params),*)?>(
                        mut f: impl FnMut($($($params),*)?) -> R,
                        $($($params: $params),*)?
                    ) -> R {
                        f($($($params),*)?)
                    }
                    call_inner(
                        &mut self,
                        $($({
                            let arg = if $params::IS_ARGUMENT {
                                Some(args.next().unwrap())
                            } else {
                                None
                            };

                            let res = $params::get(
                                arg,
                                world,
                                environment,
                                registrations
                            )?;

                            res
                        }),+)?
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

all_tuples!(impl_into_function, 0, 15, T);
