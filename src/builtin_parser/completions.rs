use bevy::prelude::*;

use super::runner::environment::Variable;
use super::Environment;

/// Stores the names of variables and functions for fast async access.
#[derive(Resource)]
pub struct EnvironmentCache {
    pub function_names: Vec<String>,
    pub variable_names: Vec<String>,
}
impl FromWorld for EnvironmentCache {
    fn from_world(world: &mut World) -> Self {
        if let Some(environment) = world.get_non_send_resource::<Environment>() {
            store_in_cache(environment)
        } else {
            Self::empty()
        }
    }
}
impl EnvironmentCache {
    pub const fn empty() -> Self {
        EnvironmentCache {
            function_names: Vec::new(),
            variable_names: Vec::new(),
        }
    }
}

pub fn store_in_cache(environment: &Environment) -> EnvironmentCache {
    let mut function_names = Vec::new();
    let mut variable_names = Vec::new();
    store_in_cache_vec(environment, &mut function_names, &mut variable_names);

    EnvironmentCache {
        function_names,
        variable_names,
    }
}
fn store_in_cache_vec(
    environment: &Environment,
    function_names: &mut Vec<String>,
    variable_names: &mut Vec<String>,
) {
    for (name, variable) in &environment.variables {
        match variable {
            Variable::Function(_) => function_names.push(name.clone()),
            Variable::Unmoved(_) => variable_names.push(name.clone()),
            Variable::Moved => {}
        }
    }
    if let Some(environment) = &environment.parent {
        store_in_cache_vec(environment, function_names, variable_names);
    }
}
