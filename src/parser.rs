//! Command parser

use bevy::{ecs::system::Command, prelude::*};
use logos::Span;

use self::{lexer::TokenStream, parser::parse};

mod lexer;
mod parser;
mod runner;

pub use runner::environment::Environment;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

pub struct ExecuteConsoleCommand(pub String);
impl Command for ExecuteConsoleCommand {
    fn apply(self, world: &mut World) {
        let mut tokens = TokenStream::new(&self.0);

        let environment = world.remove_non_send_resource::<Environment>().unwrap();
        let ast = parse(&mut tokens, &environment);
        world.insert_non_send_resource(environment);

        match ast {
            Ok(ast) => {
                runner::run(ast, world);
            }
            Err(err) => error!("{err:#?}"),
        }

        // let mut engine = rhai::Engine::new();

        // engine.on_print(|str| info!("{str}"));

        // let res = engine.eval_with_scope::<rhai::Dynamic>(&mut scope.0, command);

        // match res {
        //     Ok(val) => match val.0 {
        //         Dynamci
        //         _ => info!("{val}")
        //     },
        //     Err(err) => error!("{err}"),
        // }
    }
}
