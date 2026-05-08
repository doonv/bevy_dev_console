//! Formatting logic for values

use std::any::TypeId;
use std::fmt::Write;

use bevy::ecs::world::World;
use bevy::reflect::{
    Enum, EnumInfo, GetPath, PartialReflect, ReflectRef, Struct, TypeInfo, TypeRegistration,
    VariantInfo, VariantType,
};

use crate::builtin_parser::{
    Diagnostic, EvalError, KEYWORD, MEMBER, STRING, SpanExtension, TYPE, VALUE, VARIANT,
};

use super::super::reflection::CreateRegistration;
use super::Value;

macro_rules! w {
    ($f:expr, $($arg:tt)*) => {
        { let _ = write!($f.buffer, $($arg)*); }
    };
}

impl Value {
    pub fn try_format(
        &self,
        span: logos::Span,
        world: &World,
        registrations: &[&TypeRegistration],
    ) -> Result<String, Diagnostic<EvalError>> {
        let mut formatter = Formatter {
            buffer: String::new(),
            indentation: 0,
            world,
            registrations,
            span,
        };
        formatter.format_value(self)?;
        Ok(formatter.buffer)
    }
}

struct Formatter<'a> {
    buffer: String,
    indentation: usize,
    world: &'a World,
    registrations: &'a [&'a TypeRegistration],
    span: logos::Span,
}

impl Formatter<'_> {
    const TAB: &'static str = "    ";

    fn indent(&mut self) {
        for _ in 0..self.indentation {
            self.buffer.push_str(Self::TAB);
        }
    }

    fn format_container<I, F>(
        &mut self,
        delims: (&str, &str),
        items: I,
        mut f: F,
    ) -> Result<(), Diagnostic<EvalError>>
    where
        I: IntoIterator,
        F: FnMut(&mut Self, I::Item) -> Result<(), Diagnostic<EvalError>>,
    {
        let mut iter = items.into_iter().peekable();
        let (open, close) = delims;

        if iter.peek().is_none() {
            w!(self, "{open}{close}");
            return Ok(());
        }

        w!(self, "{open}");
        self.indentation += 1;
        for item in iter {
            w!(self, "\n");
            self.indent();
            f(self, item)?;
            w!(self, ",");
        }
        self.indentation -= 1;
        w!(self, "\n");
        self.indent();
        w!(self, "{close}");
        Ok(())
    }

    fn format_value(&mut self, value: &Value) -> Result<(), Diagnostic<EvalError>> {
        match value {
            Value::None => w!(self, "{KEYWORD}(){KEYWORD:#}"),
            Value::Number(number) => w!(self, "{number}"),
            Value::Boolean(bool) => w!(self, "{VALUE}{bool}{VALUE:#}"),
            Value::String(string) => w!(self, "{STRING}\"{string}\"{STRING:#}"),
            Value::Reference(reference) => match reference.upgrade() {
                Some(rc) => self.format_value(&rc.borrow())?,
                _ => return Err(self.span.clone().diagnose(EvalError::ReferenceToMovedData)),
            },
            Value::Object(map) => {
                self.format_container(("{", "}"), map.iter(), |f, (key, value)| {
                    w!(f, "{MEMBER}{key}{MEMBER:#}: ");
                    f.format_value(&value.borrow_inner().borrow())
                })?
            }
            Value::StructObject { name, map } => {
                w!(self, "{TYPE}{name}{TYPE:#} ");
                self.format_container(("{", "}"), map.iter(), |f, (key, value)| {
                    w!(f, "{MEMBER}{key}{MEMBER:#}: ");
                    f.format_value(&value.borrow_inner().borrow())
                })?;
            }
            Value::Tuple(tuple) => {
                self.format_container(("(", ")"), tuple.iter(), |f, element| {
                    f.format_value(&element.value.borrow_inner().borrow())
                })?;
            }
            Value::StructTuple { name, tuple } => {
                w!(self, "{TYPE}{name}{TYPE:#}");
                self.format_container(("(", ")"), tuple.iter(), |f, element| {
                    f.format_value(&element.value.borrow_inner().borrow())
                })?;
            }
            Value::Resource(resource) => self.format_resource(resource)?,
        }
        Ok(())
    }

    fn format_resource(
        &mut self,
        resource: &crate::builtin_parser::runner::reflection::IntoResource,
    ) -> Result<(), Diagnostic<EvalError>> {
        let registration = self.registrations.create_registration(resource.id);
        let dyn_reflect = resource.ref_dyn_reflect(self.world, registration);
        let reflect = dyn_reflect.reflect_path(resource.path.as_str()).unwrap();
        let reflect_ref = reflect.reflect_ref();

        match reflect_ref {
            ReflectRef::Struct(s) => self.format_resource_struct(s),
            ReflectRef::Enum(v) => {
                let TypeInfo::Enum(enum_info) = registration.type_info() else {
                    unreachable!("{:?}", registration.type_info())
                };
                self.format_resource_enum(enum_info, v)
            }
            _ => self.format_reflect(reflect),
        }
    }

    fn format_resource_struct(&mut self, s: &dyn Struct) -> Result<(), Diagnostic<EvalError>> {
        w!(
            self,
            "{KEYWORD}struct{KEYWORD:#} {TYPE}{}{TYPE:#} ",
            s.reflect_short_type_path()
        );
        self.format_struct_fields(s)
    }

    fn format_resource_enum(
        &mut self,
        enum_info: &EnumInfo,
        v: &dyn Enum,
    ) -> Result<(), Diagnostic<EvalError>> {
        w!(
            self,
            "{KEYWORD}enum{KEYWORD:#} {TYPE}{}{TYPE:#} ",
            v.reflect_short_type_path()
        );
        self.format_enum_variant_definitions(enum_info)?;

        w!(self, " = ");
        self.format_resource_enum_variant(v)
    }

    fn format_enum_variant_definitions(
        &mut self,
        enum_info: &EnumInfo,
    ) -> Result<(), Diagnostic<EvalError>> {
        self.format_container(("{", "}"), enum_info.iter(), |f, variant| {
            w!(f, "{VARIANT}{}{VARIANT:#}", variant.name());
            match variant {
                VariantInfo::Struct(v) => {
                    w!(f, " ");
                    f.format_container(("{", "}"), v.iter(), |f2, field| {
                        w!(f2, "{MEMBER}{}{MEMBER:#}: ", field.name());
                        f2.write_type_path(field.type_path_table().short_path());
                        Ok(())
                    })?;
                }
                VariantInfo::Tuple(v) => {
                    f.format_container(("(", ")"), v.iter(), |f2, field| {
                        f2.write_type_path(field.type_path_table().short_path());
                        Ok(())
                    })?;
                }
                VariantInfo::Unit(_) => {}
            }
            Ok(())
        })
    }

    fn format_resource_enum_variant(&mut self, v: &dyn Enum) -> Result<(), Diagnostic<EvalError>> {
        w!(self, "{VARIANT}{}{VARIANT:#}", v.variant_name());
        match v.variant_type() {
            VariantType::Struct => {
                w!(self, " ");
                self.format_container(("{", "}"), v.iter_fields(), |f, field| {
                    w!(f, "{MEMBER}{}{MEMBER:#}: ", field.name().unwrap());
                    f.format_reflect(field.value())
                })?;
            }
            VariantType::Tuple => {
                self.format_container(("(", ")"), v.iter_fields(), |f, field| {
                    f.format_reflect(field.value())
                })?;
            }
            VariantType::Unit => {}
        }
        Ok(())
    }

    fn format_reflect(
        &mut self,
        reflect: &dyn PartialReflect,
    ) -> Result<(), Diagnostic<EvalError>> {
        match reflect.reflect_ref() {
            ReflectRef::Struct(s) => self.format_struct_fields(s),
            ReflectRef::TupleStruct(s) => {
                self.format_container(("(", ")"), s.iter_fields(), |f, field| {
                    f.format_reflect(field)
                })
            }
            ReflectRef::Tuple(s) => {
                self.format_container(("(", ")"), s.iter_fields(), |f, field| {
                    f.format_reflect(field)
                })
            }
            ReflectRef::List(l) => {
                self.format_container(("[", "]"), l.iter(), |f, item| f.format_reflect(item))
            }
            ReflectRef::Array(a) => {
                self.format_container(("[", "]"), a.iter(), |f, item| f.format_reflect(item))
            }
            ReflectRef::Map(m) => self.format_container(("{", "}"), m.iter(), |f, (k, v)| {
                f.format_reflect(k)?;
                w!(f, ": ");
                f.format_reflect(v)
            }),
            ReflectRef::Set(s) => {
                self.format_container(("[", "]"), s.iter(), |f, item| f.format_reflect(item))
            }
            ReflectRef::Enum(v) => self.format_reflect_enum_variant(v),
            ReflectRef::Opaque(reflect) => {
                if let Some(id) = reflect
                    .get_represented_type_info()
                    .map(|info| info.type_id())
                {
                    if id == TypeId::of::<String>() {
                        w!(self, "{STRING}{reflect:?}{STRING:#}");
                    } else if id == TypeId::of::<bool>() || is_number_id(id) {
                        w!(self, "{VALUE}{reflect:?}{VALUE:#}");
                    } else {
                        w!(self, "{TYPE}{reflect:?}{TYPE:#}");
                    }
                } else {
                    w!(self, "{TYPE}{reflect:?}{TYPE:#}");
                }
                Ok(())
            }
        }
    }

    fn write_type_path(&mut self, type_path: &str) {
        let mut ident_start: Option<usize> = None;

        for (i, c) in type_path.char_indices() {
            if c.is_alphanumeric() || c == '_' {
                ident_start.get_or_insert(i);
            } else {
                if let Some(start) = ident_start.take() {
                    w!(self, "{TYPE}{}{TYPE:#}", &type_path[start..i]);
                }
                self.buffer.push(c);
            }
        }

        if let Some(start) = ident_start {
            w!(self, "{TYPE}{}{TYPE:#}", &type_path[start..]);
        }
    }

    fn format_struct_fields(&mut self, s: &dyn Struct) -> Result<(), Diagnostic<EvalError>> {
        self.format_container(("{", "}"), 0..s.field_len(), |f, i| {
            let field = s.field_at(i).unwrap();
            w!(f, "{MEMBER}{}{MEMBER:#}: ", s.name_at(i).unwrap());
            f.write_type_path(field.reflect_short_type_path());
            w!(f, " = ");
            f.format_reflect(field)
        })
    }

    fn format_reflect_enum_variant(&mut self, v: &dyn Enum) -> Result<(), Diagnostic<EvalError>> {
        w!(self, "{VARIANT}{}{VARIANT:#}", v.variant_name());
        match v.variant_type() {
            VariantType::Struct => {
                w!(self, " ");
                self.format_container(("{", "}"), v.iter_fields(), |f, field| {
                    w!(f, "{MEMBER}{}{MEMBER:#}: ", field.name().unwrap());
                    f.write_type_path(field.value().reflect_short_type_path());
                    w!(f, " = ");
                    f.format_reflect(field.value())
                })?;
            }
            VariantType::Tuple => {
                self.format_container(("(", ")"), v.iter_fields(), |f, field| {
                    f.write_type_path(field.value().reflect_short_type_path());
                    w!(f, " = ");
                    f.format_reflect(field.value())
                })?;
            }
            VariantType::Unit => {}
        }
        Ok(())
    }
}

fn is_number_id(id: TypeId) -> bool {
    id == TypeId::of::<i8>()
        || id == TypeId::of::<i16>()
        || id == TypeId::of::<i32>()
        || id == TypeId::of::<i64>()
        || id == TypeId::of::<i128>()
        || id == TypeId::of::<isize>()
        || id == TypeId::of::<u8>()
        || id == TypeId::of::<u16>()
        || id == TypeId::of::<u32>()
        || id == TypeId::of::<u64>()
        || id == TypeId::of::<u128>()
        || id == TypeId::of::<usize>()
        || id == TypeId::of::<f32>()
        || id == TypeId::of::<f64>()
}
