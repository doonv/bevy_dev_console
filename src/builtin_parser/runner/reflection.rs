use std::{any::TypeId, collections::HashMap};

use bevy::{
    prelude::*,
    reflect::{DynamicStruct, ReflectFromPtr, TypeRegistration},
};

use super::Value;

#[derive(Debug)]
pub struct IntoResource {
    pub id: TypeId,
    pub path: String,
}
impl IntoResource {
    pub fn new(id: TypeId) -> Self {
        Self {
            id,
            path: String::new(),
        }
    }
    pub fn ref_dyn_reflect<'a>(
        &self,
        world: &'a World,
        registration: impl CreateRegistration,
    ) -> &'a dyn Reflect {
        let registration = registration.create_registration(self.id);
        let ref_dyn_reflect = ref_dyn_reflect(world, registration).unwrap();

        ref_dyn_reflect
    }
    pub fn mut_dyn_reflect<'a>(
        &self,
        world: &'a mut World,
        registration: impl CreateRegistration,
    ) -> Mut<'a, dyn Reflect> {
        let registration = registration.create_registration(self.id);
        let ref_dyn_reflect = mut_dyn_reflect(world, registration).unwrap();

        ref_dyn_reflect
    }
}

pub fn object_to_dynamic_struct(hashmap: HashMap<String, Value>) -> DynamicStruct {
    let mut dynamic_struct = DynamicStruct::default();
    for (key, value) in hashmap {
        dynamic_struct.insert_boxed(&key, value.reflect());
    }
    dynamic_struct
}

pub fn mut_dyn_reflect<'a>(
    world: &'a mut World,
    registration: &TypeRegistration,
) -> Option<Mut<'a, dyn Reflect>> {
    let Some(component_id) = world.components().get_resource_id(registration.type_id()) else {
        error!(
            "Couldn't get the component id of the {} resource.",
            registration.type_info().type_path()
        );
        return None;
    };
    let resource = world.get_resource_mut_by_id(component_id).unwrap();
    let reflect_from_ptr = registration.data::<ReflectFromPtr>().unwrap();
    // SAFETY: from the context it is known that `ReflectFromPtr` was made for the type of the `MutUntyped`
    let val: Mut<dyn Reflect> =
        resource.map_unchanged(|ptr| unsafe { reflect_from_ptr.as_reflect_mut(ptr) });
    Some(val)
}

pub fn ref_dyn_reflect<'a>(
    world: &'a World,
    registration: &TypeRegistration,
) -> Option<&'a dyn Reflect> {
    let Some(component_id) = world.components().get_resource_id(registration.type_id()) else {
        error!(
            "Couldn't get the component id of the {} resource.",
            registration.type_info().type_path()
        );
        return None;
    };
    let resource = world.get_resource_by_id(component_id).unwrap();
    let reflect_from_ptr = registration.data::<ReflectFromPtr>().unwrap();
    // SAFETY: from the context it is known that `ReflectFromPtr` was made for the type of the `MutUntyped`
    let val: &dyn Reflect = unsafe { reflect_from_ptr.as_reflect(resource) };
    Some(val)
}

pub trait CreateRegistration {
    fn create_registration(&self, type_id: TypeId) -> &TypeRegistration;
}
impl CreateRegistration for &TypeRegistration {
    fn create_registration(&self, type_id: TypeId) -> &TypeRegistration {
        assert!(self.type_id() == type_id);

        self
    }
}
impl CreateRegistration for &[&TypeRegistration] {
    fn create_registration(&self, type_id: TypeId) -> &TypeRegistration {
        self.iter()
            .find(|reg| reg.type_id() == type_id)
            .expect("registration no longer exists")
    }
}
