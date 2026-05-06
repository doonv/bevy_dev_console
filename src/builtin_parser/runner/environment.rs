//! Environment and variable storage

use std::collections::HashMap;

use crate::builtin_parser::runner::EvalParams;
use crate::builtin_parser::{Diagnostic, Spanned};
use bevy::ecs::world::World;
use bevy::log::warn;
use bevy::reflect::TypeRegistration;

use super::error::EvalError;
pub use super::function::{Function, IntoFunction};
use super::unique_rc::UniqueRc;
use super::{Value, stdlib};

/// Macro for mass registering functions to an [`Environment`].
///
/// ## Usage
/// ```
/// fn my_func() {}
///
/// # use bevy_dev_console::register;
/// # let mut environment = bevy_dev_console::builtin_parser::Environment::default();
/// register!(environment => fn my_func);
/// ```
///
/// ```
/// # use bevy::prelude::World;
/// fn pow2(n: i32) -> i32 { n * n }
/// fn add(a: f32, b: f32) -> f32 { a + b }
/// fn toggle_debug(world: &mut World) { /* ... */ }
///
/// # use bevy_dev_console::register;
/// # let mut environment = bevy_dev_console::builtin_parser::Environment::default();
/// register!(environment => {
///     fn pow2;
///     fn add;
///     fn toggle_debug as "tdbg";
/// });
/// ```
#[macro_export]
macro_rules! register {
    {
        $environment:expr => fn $fn_name:ident
    } => {
        $environment
            .register_fn(stringify!($fn_name), $fn_name)
    };
    {
        $environment:expr => {
            $(
                fn $fn_name:ident $(as $renamed:expr)?;
            )*
        }
    } => {
        $(
            #[allow(unused_mut, unused_assignments)]
            let mut name = stringify!($fn_name);
            $(name = $renamed;)?

            $environment.register_fn(name, $fn_name);
        )*
    };
}

/// A variable inside the [`Environment`].
#[derive(Debug)]
pub enum Variable {
    Unmoved(UniqueRc<Value>),
    Moved,
    Function(Function),
}

/// The environment stores all variables and functions for the builtin parser.
pub struct Environment {
    pub(crate) parent: Option<Box<Environment>>,
    pub(crate) variables: HashMap<String, Variable>,
}
impl Default for Environment {
    fn default() -> Self {
        let mut env = Self::empty();

        stdlib::register(&mut env);

        env
    }
}

/// An error that can occur when interacting with a variable in the [`Environment`].
#[derive(Debug, thiserror::Error)]
pub enum VariableError {
    /// The variable was not found.
    #[error("Variable `{0}` not found.")]
    NotFound(String),
    /// The variable was moved and is no longer available.
    #[error("variable `{0}` was moved")]
    Moved(String),
    /// Expected a variable, but found a function.
    #[error("expected `{0}` to be a variable, but got a function instead")]
    ExpectedVariableGotFunction(String),
}

/// An error that can occur when running a function in the [`Environment`].
#[derive(Debug, thiserror::Error)]
pub enum RunFunctionError {
    /// The function was not found.
    #[error("Function `{0}` not found.")]
    NotFound(String),
    /// An error occurred while evaluating the function.
    #[error(transparent)]
    Eval(#[from] Diagnostic<EvalError>),
}

impl From<RunFunctionError> for Diagnostic<EvalError> {
    fn from(value: RunFunctionError) -> Self {
        match value {
            RunFunctionError::NotFound(name) => {
                Diagnostic::empty(VariableError::NotFound(name).into())
            }
            RunFunctionError::Eval(diag) => diag,
        }
    }
}

impl Environment {
    /// A completely empty [`Environment`] without any standard library.
    #[must_use]
    pub fn empty() -> Self {
        Self {
            parent: None,
            variables: HashMap::new(),
        }
    }

    /// Set a variable.
    pub fn set(&mut self, name: impl Into<String>, value: UniqueRc<Value>) {
        self.variables.insert(name.into(), Variable::Unmoved(value));
    }

    /// Returns a reference to a function if it exists.
    #[must_use]
    pub fn get_function(&self, name: &str) -> Option<&Function> {
        let env = self.resolve(name)?;

        match env.variables.get(name) {
            Some(Variable::Function(function)) => Some(function),
            _ => None,
        }
    }

    pub(crate) fn function_scope<T>(
        &mut self,
        name: &str,
        function: impl FnOnce(&mut Self, &mut Function) -> T,
    ) -> Option<T> {
        let env = self.resolve_mut(name)?;
        let var = env.variables.get_mut(name);
        let return_result;
        let fn_obj = match var {
            Some(var @ Variable::Function(_)) => {
                let Variable::Function(mut fn_obj) = std::mem::replace(var, Variable::Moved) else {
                    unreachable!()
                };

                return_result = function(env, &mut fn_obj);

                fn_obj
            }
            _ => return None,
        };

        let var = env.variables.get_mut(name);
        let _ = std::mem::replace(var.unwrap(), Variable::Function(fn_obj));

        Some(return_result)
    }

    /// Returns a reference to a variable.
    pub fn get_variable(&self, name: &str) -> Result<&UniqueRc<Value>, VariableError> {
        let Some(var) = self.get(name) else {
            return Err(VariableError::NotFound(name.to_owned()));
        };

        match var {
            Variable::Unmoved(value) => Ok(value),
            Variable::Moved => Err(VariableError::Moved(name.to_owned())),
            Variable::Function(_) => Err(VariableError::ExpectedVariableGotFunction(name.to_owned())),
        }
    }

    /// "Moves" a variable, giving you ownership over it.
    ///
    /// However it will no longer be able to be used unless it's a [`Value::None`],
    /// [`Value::Boolean`], or [`Value::Number`] in which case it will be copied.  
    pub fn move_var(&mut self, name: &str) -> Result<Value, VariableError> {
        let Some(var) = self.get_mut(name) else {
            return Err(VariableError::NotFound(name.to_owned()));
        };

        match var {
            Variable::Moved => Err(VariableError::Moved(name.to_owned())),
            Variable::Function(_) => Err(VariableError::ExpectedVariableGotFunction(name.to_owned())),
            variable_reference @ Variable::Unmoved(_) => {
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
        }
    }

    fn get(&self, name: &str) -> Option<&Variable> {
        if let Some(var) = self.variables.get(name) {
            return Some(var);
        }

        self.parent.as_ref()?.get(name)
    }
    fn get_mut(&mut self, name: &str) -> Option<&mut Variable> {
        if let Some(var) = self.variables.get_mut(name) {
            return Some(var);
        }

        self.parent.as_mut()?.get_mut(name)
    }
    fn resolve(&self, name: &str) -> Option<&Self> {
        if self.variables.contains_key(name) {
            return Some(self);
        }

        self.parent.as_ref()?.resolve(name)
    }
    fn resolve_mut(&mut self, name: &str) -> Option<&mut Self> {
        if self.variables.contains_key(name) {
            return Some(self);
        }

        self.parent.as_mut()?.resolve_mut(name)
    }

    /// Registers a function for use inside the language.
    ///
    /// All parameters must implement [`FunctionParam`](super::function::FunctionParam).
    /// There is a limit of 15 parameters.
    ///
    /// The return value of the function must implement [`Into<Value>`]
    ///
    /// You should take a look at the [Standard Library] for examples.
    ///
    /// [Standard Library](https://github.com/doonv/bevy_dev_console/blob/master/src/builtin_parser/runner/stdlib.rs)
    pub fn register_fn<T>(
        &mut self,
        name: impl Into<String>,
        function: impl IntoFunction<T>,
    ) -> &mut Self {
        let name = name.into();
        if self.variables.contains_key(&name) {
            warn!("Function {name} declared twice.");
        }
        self.variables
            .insert(name, Variable::Function(function.into_function()));

        self
    }

    pub fn run_function(
        &mut self,
        name: &str,
        arguments: Vec<Spanned<Value>>,
        world: &mut World,
        registrations: &[&TypeRegistration],
    ) -> Result<Value, RunFunctionError> {
        self.function_scope(name, move |environment, function| {
            (function.body)(
                arguments,
                EvalParams {
                    world,
                    environment,
                    registrations,
                },
            )
        })
        .ok_or_else(|| RunFunctionError::NotFound(name.to_owned()))?
        .map_err(RunFunctionError::Eval)
    }

    /// Iterate over all the variables and functions in the current scope of the environment.
    ///
    /// Does not include variables and functions from higher scopes.
    #[must_use]
    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, String, Variable> {
        self.variables.iter()
    }
}

impl<'a> IntoIterator for &'a Environment {
    type Item = (&'a String, &'a Variable);
    type IntoIter = std::collections::hash_map::Iter<'a, String, Variable>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
