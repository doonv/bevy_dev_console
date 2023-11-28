use std::{cell::RefCell, marker::PhantomData, rc::Rc};

use ahash::AHashMap;
use bevy::reflect::TypeRegistration;
use logos::Span;

use super::Expression;
use super::{RunError, Value};
use bevy::prelude::World;

struct Function {
    body: Box<dyn Fn(Vec<Value>) -> Value>,
}

trait IntoFunction<T> {
    fn into_function(self) -> Function;
}

// impl<F, T1, T2, R> IntoFunction<(T1, T2)> for F
// where
//     F: Fn(T1, T2) -> R + 'static,
//     T1: TryFrom<Value>,
//     T2: TryFrom<Value>,
//     R: Into<Value>,
// {
//     fn into_function(self) -> Function {
//         let body = Box::new(move |args: Vec<Value>| {
//             let mut args = args.into_iter();
//             self(
//                 args.next()
//                     .unwrap()
//                     .try_into()
//                     .unwrap_or_else(|_| unreachable!()),
//                 args.next()
//                     .unwrap()
//                     .try_into()
//                     .unwrap_or_else(|_| unreachable!()),
//             )
//             .into()
//         });

//         Function { body }
//     }
// }
// Example output:
// impl<F: FnMut(T1), T1> IntoSystem<(T1,)> for F {
//     type System = FunctionSystem<(T1,), Self>;

//     fn into_system(self) -> Self::System {
//         FunctionSystem {
//             f: self,
//             marker: Default::default(),
//         }
//     }
// }
macro_rules! impl_into_function {
    (
        $($(
                $params:ident
        ),+)?
    ) => {
        // impl<F: FnMut($($($params),+)?) $(, $($params: 'static),+ )?> IntoSystem<( $($($params,)+)? )> for F {
        //     type System = FunctionSystem<( $($($params,)+)? ), Self>;

        //     fn into_system(self) -> Self::System {
        //         FunctionSystem {
        //             f: self,
        //             marker: Default::default(),
        //         }
        //     }
        // }
        impl<F $(, $($params: TryFrom<Value>),+ )?, R> IntoFunction<( $($($params,)+)? )> for F
        where
            F: Fn($($($params),+)?) -> R + 'static,
            R: Into<Value>,
        {
            fn into_function(self) -> Function {
                let body = Box::new(move |args: Vec<Value>| {
                    #[allow(unused_variables, unused_mut)]
                    let mut args = args.into_iter();
                    self(
                        $($({
                            let _: $params; // Tell rust im talking abouts the $params
                            args.next()
                            .unwrap()
                            .try_into()
                            .unwrap_or_else(|_| unreachable!())
                        }),+)?
                    )
                    .into()
                });

                Function { body }
            }
        }
    }
}

impl_into_function!();
impl_into_function!(T1);
impl_into_function!(T1, T2);
impl_into_function!(T1, T2, T3);

pub enum Variable {
    Unmoved(Rc<RefCell<Value>>),
    Moved,
    Function(Function),
}

pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: AHashMap<String, Variable>,
}

impl Default for Environment {
    fn default() -> Self {
        let mut env = Self {
            parent: None,
            variables: AHashMap::new(),
        };

        super::stdlib::register(&mut env);

        env
    }
}

impl Environment {
    pub fn set(&mut self, name: String, value: Rc<RefCell<Value>>) -> Result<(), RunError> {
        self.variables.insert(name, Variable::Unmoved(value));

        Ok(())
    }
    pub fn get(&self, name: &str, span: Span) -> Result<&Rc<RefCell<Value>>, RunError> {
        let (env, span) = self.resolve(name, span)?;

        match env.variables.get(name) {
            Some(Variable::Unmoved(value)) => Ok(value),
            Some(Variable::Moved) => Err(RunError::VariableMoved(span)),
            Some(Variable::Function(function)) => todo!(),
            None => Err(RunError::VariableNotFound(span)),
        }
    }
    pub fn move_var(&mut self, name: &str, span: Span) -> Result<Rc<RefCell<Value>>, RunError> {
        let (env, span) = self.resolve_mut(name, span)?;

        match env.variables.get_mut(name) {
            Some(Variable::Moved) => Err(RunError::VariableMoved(span)),
            Some(Variable::Function(function)) => todo!(),
            Some(reference) => {
                let Variable::Unmoved(value) = std::mem::replace(reference, Variable::Moved) else {
                    unreachable!()
                };

                Ok(value)
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

    pub fn register_fn<T>(
        &mut self,
        name: impl Into<String>,
        function: impl IntoFunction<T> + 'static,
    ) {
        self.variables
            .insert(name.into(), Variable::Function(function.into_function()));
    }
}
