//! Environment and function registeration

use std::collections::HashMap;

use bevy::{ecs::world::World, log::warn, reflect::TypeRegistration};
use logos::Span;

use super::{
    super::{parser::Expression, Spanned},
    error::RunError,
    eval_expression, stdlib,
    unique_rc::UniqueRc,
    EvalParams, Value,
};

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

/// Get around implementation of Result causing stupid errors
pub(super) struct ResultContainer<T, E>(pub Result<T, E>);

impl<T: Into<Value>> From<T> for ResultContainer<Value, RunError> {
    fn from(value: T) -> Self {
        ResultContainer(Ok(value.into()))
    }
}
impl<T, E> From<ResultContainer<T, E>> for Result<T, E> {
    fn from(ResultContainer(result): ResultContainer<T, E>) -> Self {
        result
    }
}
impl<T: Into<Value>, E> From<Result<T, E>> for ResultContainer<Value, E> {
    fn from(result: Result<T, E>) -> Self {
        ResultContainer(result.map(|v| v.into()))
    }
}
/// A parameter in a [`Function`].
pub trait FunctionParam: Sized {
    /// TODO: Add `Self` as default when <https://github.com/rust-lang/rust/issues/29661> gets merged
    type Item<'world, 'env, 'reg>;
    /// Whether this parameter requires a [`Spanned<Value>`].
    /// If `false` then `FunctionParam::get`'s `value` will be [`None`], and vice versa.
    const USES_VALUE: bool;

    fn get<'world, 'env, 'reg>(
        value: Option<Spanned<Value>>,
        world: &mut Option<&'world mut World>,
        environment: &mut Option<&'env mut Environment>,
        registrations: &'reg [&'reg TypeRegistration],
    ) -> Result<Self::Item<'world, 'env, 'reg>, RunError>;
}

pub type FunctionType = dyn FnMut(Vec<Spanned<Expression>>, EvalParams) -> Result<Value, RunError>;
pub struct Function {
    pub argument_count: usize,
    pub body: Box<FunctionType>,
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
            R: Into<ResultContainer<Value, RunError>>,
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
                    }).collect::<Result<Vec<_>, RunError>>()?.into_iter();
                    let world = &mut Some(world);
                    let environment = &mut Some(environment);

                    #[allow(clippy::too_many_arguments)]
                    fn call_inner<R: Into<ResultContainer<Value, RunError>>, $($($params),*)?>(
                        mut f: impl FnMut($($($params),*)?) -> R,
                        $($($params: $params),*)?
                    ) -> R {
                        f($($($params),*)?)
                    }
                    call_inner(
                        &mut self,
                        $($({
                            let arg = if $params::USES_VALUE {
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
                    .into().into()
                });

                let argument_count = $($(
                    $params::USES_VALUE as usize +
                )+)? 0;

                Function { body, argument_count }
            }
        }
    }
}
impl_into_function!();
impl_into_function!(T1);
impl_into_function!(T1, T2);
impl_into_function!(T1, T2, T3);
impl_into_function!(T1, T2, T3, T4);
impl_into_function!(T1, T2, T3, T4, T5);
impl_into_function!(T1, T2, T3, T4, T5, T6);
impl_into_function!(T1, T2, T3, T4, T5, T6, T7);
impl_into_function!(T1, T2, T3, T4, T5, T6, T7, T8);

/// A variable inside the [`Environment`].
pub enum Variable {
    Unmoved(UniqueRc<Value>),
    Moved,
    Function(Function),
}

/// The environment stores all variables and functions.
pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, Variable>,
}

impl Default for Environment {
    fn default() -> Self {
        let mut env = Self {
            parent: None,
            variables: HashMap::new(),
        };

        stdlib::register(&mut env);

        env
    }
}

impl Environment {
    /// Set a variable.
    pub fn set(&mut self, name: impl Into<String>, value: UniqueRc<Value>) {
        self.variables.insert(name.into(), Variable::Unmoved(value));
    }

    /// Returns a reference to a function if it exists.
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        let (env, _) = self.resolve(name, 0..0).ok()?;

        match env.variables.get(name) {
            Some(Variable::Function(function)) => Some(function),
            _ => None,
        }
    }

    pub(crate) fn function_scope<T>(
        &mut self,
        name: &str,
        function: impl FnOnce(&mut Self, &mut Function) -> T,
    ) -> T {
        let (env, _) = self.resolve_mut(name, 0..0).unwrap();

        let return_result;
        let var = env.variables.get_mut(name);
        let fn_obj = match var {
            Some(Variable::Function(_)) => {
                let Variable::Function(mut fn_obj) =
                    std::mem::replace(var.unwrap(), Variable::Moved)
                else {
                    unreachable!()
                };

                return_result = function(env, &mut fn_obj);

                fn_obj
            }
            _ => unreachable!(),
        };

        let var = env.variables.get_mut(name);
        let _ = std::mem::replace(var.unwrap(), Variable::Function(fn_obj));

        return_result
    }
    /// Returns a reference to a variable.
    pub fn get(&self, name: &str, span: Span) -> Result<&UniqueRc<Value>, RunError> {
        let (env, span) = self.resolve(name, span)?;

        match env.variables.get(name) {
            Some(Variable::Unmoved(value)) => Ok(value),
            Some(Variable::Moved) => Err(RunError::VariableMoved(span)),
            Some(Variable::Function(_)) => todo!(),
            None => Err(RunError::VariableNotFound(span)),
        }
    }

    /// "Moves" a variable, giving you ownership over it.
    ///
    /// However it will no longer be able to be used unless it's a [`Value::None`],
    /// [`Value::Boolean`], or [`Value::Number`] in which case it will be copied.  
    pub fn move_var(&mut self, name: &str, span: Span) -> Result<Value, RunError> {
        let (env, span) = self.resolve_mut(name, span)?;

        match env.variables.get_mut(name) {
            Some(Variable::Moved) => Err(RunError::VariableMoved(span)),
            Some(Variable::Function(_)) => todo!(),
            Some(variable_reference) => {
                let Variable::Unmoved(reference) = variable_reference else {
                    unreachable!()
                };
                // This is a pretty bad way of handling something similar to rust's [`Copy`] trait but whatever.
                match &*reference.borrow_inner().borrow() {
                    Value::None => return Ok(Value::None),
                    Value::Boolean(bool) => return Ok(Value::Boolean(*bool)),
                    Value::Number(number) => return Ok(Value::Number(*number)),
                    _ => {}
                };
                let Variable::Unmoved(value) =
                    std::mem::replace(variable_reference, Variable::Moved)
                else {
                    unreachable!()
                };
                Ok(value.into_inner())
            }
            None => Err(RunError::VariableNotFound(span)),
        }
    }

    fn resolve(&self, name: &str, span: Span) -> Result<(&Self, Span), RunError> {
        if self.variables.contains_key(name) {
            return Ok((self, span));
        }

        match &self.parent {
            Some(parent) => parent.resolve(name, span),
            None => Err(RunError::VariableNotFound(span)),
        }
    }
    fn resolve_mut(&mut self, name: &str, span: Span) -> Result<(&mut Self, Span), RunError> {
        if self.variables.contains_key(name) {
            return Ok((self, span));
        }

        match &mut self.parent {
            Some(parent) => parent.resolve_mut(name, span),
            None => Err(RunError::VariableNotFound(span)),
        }
    }

    /// Registers a function for use inside the language.
    ///
    /// All parameters must implement [`FunctionParam`].
    /// There is a limit of 8 parameters.
    ///
    /// The return value of the function must implement [`Into<Value>`]
    ///
    /// You should take a look at the [Standard Library](super::stdlib) for examples.
    pub fn register_fn<T>(
        &mut self,
        name: impl Into<String>,
        function: impl IntoFunction<T>,
    ) -> &mut Self {
        let name = name.into();
        if self.variables.contains_key(&name) {
            warn!("Function {name} declared twice.")
        }
        self.variables
            .insert(name, Variable::Function(function.into_function()));

        self
    }
}
