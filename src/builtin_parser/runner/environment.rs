//! Environment and variable storage

use std::collections::HashMap;

use crate::builtin_parser::SpanExtension;
use bevy::log::warn;
use logos::Span;

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

/// The environment stores all variables and functions.
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

impl Environment {
    /// A completely empty [`Environment`] without any functions.
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
    pub fn get(&self, name: &str, span: Span) -> Result<&UniqueRc<Value>, EvalError> {
        let (env, span) = self.resolve(name, span)?;

        match env.variables.get(name) {
            Some(Variable::Unmoved(value)) => Ok(value),
            Some(Variable::Moved) => Err(EvalError::VariableMoved(span.wrap(name.to_string()))),
            Some(Variable::Function(_)) => Err(EvalError::ExpectedVariableGotFunction(
                span.wrap(name.to_owned()),
            )),
            None => Err(EvalError::VariableNotFound(span.wrap(name.to_string()))),
        }
    }

    /// "Moves" a variable, giving you ownership over it.
    ///
    /// However it will no longer be able to be used unless it's a [`Value::None`],
    /// [`Value::Boolean`], or [`Value::Number`] in which case it will be copied.  
    pub fn move_var(&mut self, name: &str, span: Span) -> Result<Value, EvalError> {
        let (env, span) = self.resolve_mut(name, span)?;

        match env.variables.get_mut(name) {
            Some(Variable::Moved) => Err(EvalError::VariableMoved(span.wrap(name.to_string()))),
            Some(Variable::Function(_)) => Err(EvalError::ExpectedVariableGotFunction(
                span.wrap(name.to_owned()),
            )),
            Some(variable_reference @ Variable::Unmoved(_)) => {
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
            None => Err(EvalError::VariableNotFound(span.wrap(name.to_string()))),
        }
    }

    fn resolve(&self, name: &str, span: Span) -> Result<(&Self, Span), EvalError> {
        if self.variables.contains_key(name) {
            return Ok((self, span));
        }

        match &self.parent {
            Some(parent) => parent.resolve(name, span),
            None => Err(EvalError::VariableNotFound(span.wrap(name.to_string()))),
        }
    }
    fn resolve_mut(&mut self, name: &str, span: Span) -> Result<(&mut Self, Span), EvalError> {
        if self.variables.contains_key(name) {
            return Ok((self, span));
        }

        match &mut self.parent {
            Some(parent) => parent.resolve_mut(name, span),
            None => Err(EvalError::VariableNotFound(span.wrap(name.to_string()))),
        }
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
