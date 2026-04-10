//! Formatting logic for values

use std::fmt::Write;

use bevy::ecs::world::World;
use bevy::reflect::{
    GetPath, PartialReflect, ReflectRef, TypeInfo, TypeRegistration, VariantInfo, VariantType,
};

use crate::builtin_parser::{BRIGHT_YELLOW, EvalError, GREEN, RED, YELLOW};

use super::super::reflection::CreateRegistration;
use super::Value;

impl Value {
    /// Attempts to format this [`Value`].
    ///
    /// # Errors
    ///
    /// Returns an error if the [`Value`] is a reference to moved data.
    pub fn try_format(
        &self,
        span: logos::Span,
        world: &World,
        registrations: &[&TypeRegistration],
    ) -> Result<String, EvalError> {
        const TAB: &str = "    ";
        match self {
            Value::None => Ok(format!("()")),
            Value::Number(number) => Ok(format!("{number}")),
            Value::Boolean(bool) => Ok(format!("{YELLOW}{bool}{YELLOW:#}")),
            Value::String(string) => Ok(format!("{GREEN}\"{string}\"{GREEN:#}")),
            Value::Reference(reference) => match reference.upgrade() {
                Some(rc) => Ok(rc.borrow().try_format(span, world, registrations)?),
                _ => Err(EvalError::ReferenceToMovedData(span)),
            },
            Value::Object(map) => {
                let mut string = String::new();
                string.push('{');
                for (key, value) in map {
                    let _ = write!(
                        string,
                        "\n{TAB}{key}: {},",
                        value.borrow_inner().borrow().try_format(
                            span.clone(),
                            world,
                            registrations
                        )?
                    );
                }
                if !map.is_empty() {
                    string.push('\n');
                }
                string.push('}');
                Ok(string)
            }
            Value::StructObject { name, map } => {
                let mut string = String::new();
                let _ = write!(string, "{name} {{");
                for (key, value) in map {
                    let _ = write!(
                        string,
                        "\n{TAB}{key}: {},",
                        value.borrow_inner().borrow().try_format(
                            span.clone(),
                            world,
                            registrations
                        )?
                    );
                }
                if !map.is_empty() {
                    string.push('\n');
                }
                string.push('}');
                Ok(string)
            }
            Value::Tuple(tuple) => {
                let mut string = String::new();
                string.push('(');
                for element in tuple {
                    let _ = write!(
                        string,
                        "\n{TAB}{},",
                        element.value.borrow_inner().borrow().try_format(
                            span.clone(),
                            world,
                            registrations
                        )?
                    );
                }
                if !tuple.is_empty() {
                    string.push('\n');
                }
                string.push(')');
                Ok(string)
            }
            Value::StructTuple { name, tuple } => {
                let mut string = String::new();
                string.push_str(name);
                string.push('(');
                for element in tuple {
                    let _ = write!(
                        string,
                        "\n{TAB}{},",
                        element.value.borrow_inner().borrow().try_format(
                            span.clone(),
                            world,
                            registrations
                        )?
                    );
                }
                if !tuple.is_empty() {
                    string.push('\n');
                }
                string.push(')');
                Ok(string)
            }
            Value::Resource(resource) => Ok(fancy_debug_print(resource, world, registrations)),
        }
    }
}

/// A massive function that takes in a type registration and the world and then
/// does all the hard work of printing out the type nicely.
fn fancy_debug_print(
    resource: &crate::builtin_parser::runner::reflection::IntoResource,
    world: &World,
    registrations: &[&TypeRegistration],
) -> String {
    const TAB: &str = "    ";
    let registration = registrations.create_registration(resource.id);
    let dyn_reflect = resource.ref_dyn_reflect(world, registration);

    let reflect = dyn_reflect.reflect_path(resource.path.as_str()).unwrap();

    fn debug_subprint(reflect: &dyn PartialReflect, indentation: usize) -> String {
        let mut f = String::new();
        let reflect_ref = reflect.reflect_ref();
        let indentation_string = TAB.repeat(indentation);
        match reflect_ref {
            ReflectRef::Struct(struct_info) => {
                f += "{\n";
                for i in 0..struct_info.field_len() {
                    let field = struct_info.field_at(i).unwrap();
                    let field_name = struct_info.name_at(i).unwrap();

                    let field_value = debug_subprint(field, indentation + 1);
                    let _ = writeln!(
                        f,
                        "{indentation_string}{TAB}{field_name}: {} = {field_value},",
                        field.reflect_short_type_path()
                    );
                }
                f += &indentation_string;
                f += "}";
            }
            ReflectRef::TupleStruct(tuple_struct) => {
                f += "(";
                f += &tuple_struct
                    .iter_fields()
                    .map(|field| debug_subprint(field, indentation + 1))
                    .collect::<Vec<_>>()
                    .join(", ");
                f += ")";
            }
            ReflectRef::Tuple(tuple_info) => {
                f += "(";
                f += &tuple_info
                    .iter_fields()
                    .map(|field| debug_subprint(field, indentation + 1))
                    .collect::<Vec<_>>()
                    .join(", ");
                f += ")";
            }
            ReflectRef::List(_) => todo!(),
            ReflectRef::Array(_) => todo!(),
            ReflectRef::Map(_) => todo!(),
            ReflectRef::Enum(variant) => {
                // Print out the enum types
                f += variant.variant_name();

                match variant.variant_type() {
                    VariantType::Struct => {
                        f += " {\n";
                        for field in variant.iter_fields() {
                            let _ = writeln!(
                                f,
                                "{indentation_string}{TAB}{}: {} = {},",
                                field.name().unwrap(),
                                field.value().reflect_short_type_path(),
                                debug_subprint(field.value(), indentation + 1)
                            );
                        }
                        f += &indentation_string;
                        f += "}";
                    }
                    VariantType::Tuple => {
                        f += "(\n";
                        for field in variant.iter_fields() {
                            let _ = writeln!(
                                f,
                                "{indentation_string}{TAB}{} = {},",
                                field.value().reflect_short_type_path(),
                                debug_subprint(field.value(), indentation + 1)
                            );
                        }
                        f += &indentation_string;
                        f += ")";
                    }
                    VariantType::Unit => {}
                }
            }
            ReflectRef::Opaque(reflect) => {
                let Some(TypeInfo::Opaque(opaque)) = reflect.get_represented_type_info() else {
                    return format!("{reflect:?}");
                };
                if opaque.is::<String>() {
                    return format!("{GREEN}{reflect:?}{GREEN:#}");
                }
                return format!("{YELLOW}{reflect:?}{YELLOW:#}");
            }
            ReflectRef::Set(_) => todo!(),
        }

        f
    }

    let mut f = String::new();
    let reflect_ref = reflect.reflect_ref();
    match reflect_ref {
        ReflectRef::Struct(struct_info) => {
            let _ = writeln!(f, "struct {} {{", struct_info.reflect_short_type_path());
            for i in 0..struct_info.field_len() {
                let field = struct_info.field_at(i).unwrap();
                let field_name = struct_info.name_at(i).unwrap();

                let field_value = debug_subprint(field, 1);
                let _ = writeln!(
                    f,
                    "{TAB}{RED}{}{RED:#}: {BRIGHT_YELLOW}{}{BRIGHT_YELLOW:#} = {},",
                    field_name,
                    field.reflect_short_type_path(),
                    field_value
                );
            }
            f += "}";
        }
        ReflectRef::Enum(set_variant_info) => {
            // Print out the enum types
            let _ = writeln!(f, "enum {} {{", set_variant_info.reflect_short_type_path());
            let TypeInfo::Enum(enum_info) = registration.type_info() else {
                unreachable!("{:?}", registration.type_info())
            };
            for variant in enum_info.iter() {
                f += "\t";
                f += variant.name();
                match variant {
                    VariantInfo::Struct(variant) => {
                        f += " {\n";
                        for field in variant.iter() {
                            let _ = writeln!(
                                f,
                                "{TAB}{TAB}{}: {},",
                                field.name(),
                                field.type_path_table().short_path()
                            );
                        }
                        f += TAB;
                        f += "}";
                    }
                    VariantInfo::Tuple(variant) => {
                        f += "(";
                        let mut iter = variant.iter();
                        if let Some(first) = iter.next() {
                            let _ = write!(f, "{}", first.type_path_table().short_path());
                            for field in iter {
                                let _ = write!(f, ", {}", field.type_path_table().short_path());
                            }
                        }
                        f += ")";
                    }
                    VariantInfo::Unit(_) => {}
                }
                f += ",\n";
            }
            // Print out the current value
            f += "} = ";
            f += set_variant_info.variant_name();
            match set_variant_info.variant_type() {
                VariantType::Struct => {
                    f += " {\n";
                    for field in set_variant_info.iter_fields() {
                        let _ =
                            writeln!(f, "{TAB}{}: {:?},\n", field.name().unwrap(), field.value());
                    }
                    f += "}";
                }
                VariantType::Tuple => {
                    f += "(\n";
                    for field in set_variant_info.iter_fields() {
                        let _ = writeln!(f, "{TAB}{:?},", field.value());
                    }
                    f += ")";
                }
                VariantType::Unit => {}
            }
        }
        ReflectRef::Opaque(value) => {
            let _ = write!(f, "{value:?}");
        }
        _ => f += &debug_subprint(reflect, 1),
    }
    f
}
