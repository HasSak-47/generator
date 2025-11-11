use super::{endpoint::*, types::*};
use crate::builder::Code;
use anyhow::Result;
use std::collections::{HashMap, HashSet};
use std::path::Path;

/// Helper describing the wire/domain pair for a type that requires conversions.
#[derive(Debug)]
struct SplitType {
    wire: Type,
    domain: Type,
    wire_name: String,
}

/// Metadata for a single named type in the DSL.
#[derive(Debug)]
pub struct TypeInformation {
    pub name: String,
    pub ty: Type,
    conversion: Option<SplitType>,
}

impl TypeInformation {
    pub fn has_conversion(&self) -> bool {
        return self.conversion.is_some();
    }
    pub fn get_domain_type(&self) -> &Type {
        if let Some(con) = &self.conversion {
            &con.domain
        } else {
            &self.ty
        }
    }

    pub fn get_wire_type(&self) -> &Type {
        if let Some(con) = &self.conversion {
            &con.wire
        } else {
            &self.ty
        }
    }

    pub fn get_wire_name(&self) -> &String {
        if let Some(con) = &self.conversion {
            &con.wire_name
        } else {
            &self.name
        }
    }
}

/// Aggregates all parsed type and endpoint declarations.
#[derive(Debug)]
pub struct Definitons {
    pub types: HashMap<String, TypeInformation>,
    pub end_points: HashMap<String, EndPoint>,
}

impl Definitons {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            end_points: HashMap::new(),
        }
    }

    /// Ensure untagged unions only contain primitives or other unions (structs would be ambiguous).
    pub fn validate_untagged_union(&self, u: &UnionType) {
        for ty in &u.tys {
            match ty {
                Type::Struct(_) => {
                    panic!("untagged union that contains structs is not valid yet!")
                }
                Type::Named(name) => {
                    if let Type::Struct(_) = self.types.get(name).unwrap().ty {
                        panic!("untagged union that contains named structs is not valid yet!")
                    }
                }
                _ => {}
            }
        }
    }

    /// Replace `Undetermined` leaf nodes with `Named` variants when the identifier exists.
    fn resolve_type_references(ty: &mut Type, names: &Vec<String>) {
        match ty {
            Type::Struct(struct_) => {
                for (_, ty) in &mut struct_.members {
                    Definitons::resolve_type_references(ty, names);
                }
            }
            Type::Array(arr) => {
                Definitons::resolve_type_references(&mut arr.ty, names);
            }
            Type::Optional(opt) => {
                Definitons::resolve_type_references(&mut opt.ty, names);
            }
            Type::Union(union) => {
                for ty in &mut union.tys {
                    Definitons::resolve_type_references(ty, names);
                }
            }
            Type::Undetermined(name) => {
                let found_name = names.iter().find(|n| **n == *name);
                if found_name.is_none() {
                    panic!("failed to determine {name}");
                }
                *ty = Type::Named(name.clone());
            }
            _ => {}
        }
    }

    /// Walk every type/endpoint and ensure that all referenced names exist.
    fn validate_type_references(&mut self) {
        let type_names: Vec<String> = self.types.keys().map(|k| k.clone()).collect();

        for (_, ty) in &mut self.types {
            Definitons::resolve_type_references(&mut ty.ty, &type_names);
        }

        for (_, endpoint) in &mut self.end_points {
            for (_, ty) in endpoint.params.iter_mut() {
                Definitons::resolve_type_references(ty, &type_names);
            }
            Definitons::resolve_type_references(&mut endpoint.return_type, &type_names);
        }
    }

    /// Build the "domain" version of a type by replacing every `Into` field with its target repr.
    pub fn convert_to_domain_type(&self, ty: &Type) -> Type {
        assert!(ty.contains_into(self));

        return match ty {
            Type::Into(i) => Type::Repr(i.into.clone()),
            Type::Optional(o) => Type::optional(self.convert_to_domain_type(&o.ty)),
            Type::Array(a) => Type::array(self.convert_to_domain_type(&a.ty), a.len),
            Type::Struct(st) => {
                let mut s = StructType::new();
                for (name, ty) in &st.members {
                    if !ty.contains_into(self) {
                        s.members.push((name.clone(), ty.clone()));
                    } else {
                        s.members
                            .push((name.clone(), self.convert_to_domain_type(ty)));
                    }
                }
                Type::Struct(s)
            }
            Type::Union(u) => {
                let mut s = UnionType::new(vec![]);
                for ty in &u.tys {
                    if !ty.contains_into(self) {
                        s.tys.push(ty.clone());
                    } else {
                        s.tys.push(self.convert_to_domain_type(ty));
                    }
                }
                s.kind = u.kind.clone();
                Type::Union(s)
            }

            Type::Named(name) => Type::Named(name.clone()),

            _ => {
                unreachable!()
            }
        };
    }

    /// Build the "wire" version of a type by replacing every `Into` field with the transport type.
    pub fn convert_to_wire_type(&self, ty: &Type) -> Type {
        assert!(ty.contains_into(self), "{ty:?} doesn't contains into");

        return match ty {
            Type::Into(i) => (*i.from).clone(),
            Type::Optional(o) => Type::optional(self.convert_to_wire_type(&o.ty)),
            Type::Array(a) => Type::array(self.convert_to_wire_type(&a.ty), a.len),
            Type::Struct(st) => {
                let mut s = StructType::new();
                for (name, ty) in &st.members {
                    if !ty.contains_into(self) {
                        s.members.push((name.clone(), ty.clone()));
                    } else {
                        s.members
                            .push((name.clone(), self.convert_to_wire_type(ty)));
                    }
                }
                Type::Struct(s)
            }
            Type::Union(u) => {
                let mut s = UnionType::new(vec![]);
                for ty in &u.tys {
                    if !ty.contains_into(self) {
                        s.tys.push(ty.clone());
                    } else {
                        s.tys.push(self.convert_to_wire_type(ty));
                    }
                }
                s.kind = u.kind.clone();
                Type::Union(s)
            }
            Type::Named(name) => {
                if self.types[name].ty.contains_into(self) {
                    Type::Named(format!("_{name}"))
                } else {
                    Type::Named(name.clone())
                }
            }

            _ => {
                unreachable!()
            }
        };
    }

    /// Insert a parsed `type` declaration into the definitions map.
    pub fn register_type(&mut self, name: String, ty: Type) {
        self.types.insert(
            name.clone(),
            TypeInformation {
                name,
                ty,
                conversion: None,
            },
        );
    }

    /// Load and normalize the DSL definitions file, annotating types with conversion metadata.
    pub fn load_from_path<P: AsRef<Path>>(p: P) -> Result<Self> {
        let mut defs = super::dsl::get_definitions(p)?;
        defs.validate_type_references();

        // I hate the borrow checker sometimes
        let mut new_types = HashMap::new();
        for (name, ty) in &defs.types {
            if let Type::Union(u) = &ty.ty {
                if u.kind == UnionKind::Untagged {
                    defs.validate_untagged_union(u);
                }
            }

            if !ty.ty.contains_into(&defs) {
                continue;
            }

            let wire_name = format!("_{name}");
            let wire = defs.convert_to_wire_type(&ty.ty);
            let domain = defs.convert_to_domain_type(&ty.ty);

            new_types.insert(
                name.clone(),
                SplitType {
                    wire_name,
                    wire,
                    domain,
                },
            );
        }

        for (name, s) in new_types {
            let t = defs.types.get_mut(&name).unwrap();
            t.conversion = Some(s);
        }

        return Ok(defs);
    }

    /// Emit code for every domain type using the provided generator implementation.
    pub fn render_domain_type_definitions<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (name, ty) in &self.types {
            let ty = ty.get_domain_type();
            code.add_child(generator.generate_type(name, ty, true, self));
        }
        return code;
    }

    /// Emit the wire-model definitions for types that require conversion.
    pub fn render_wire_type_definitions<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (_, ty) in &self.types {
            if let Some(c) = &ty.conversion {
                code.add_child(generator.generate_type(c.wire_name.as_str(), &c.wire, false, self));
            }
        }

        return code;
    }

    /// Emit the helper functions that translate between domain and wire representations.
    pub fn render_conversion_helpers<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (_, ty) in &self.types {
            if ty.conversion.is_none() {
                continue;
            }
            code.add_child(generator.generate_type_translation(ty, self));
        }

        return code;
    }

    /// Emit every endpoint definition (handlers or client functions) via the generator.
    pub fn render_endpoint_definitions<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();

        for (name, endpoint) in &self.end_points {
            code.add_child(generator.generate_endpoint(name, endpoint, self));
        }

        return code;
    }

    /// Build a standalone chunk of code that only contains type declarations.
    pub fn build_type_module<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_type_header(self));
        code.add_child(self.render_domain_type_definitions(generator));

        return code;
    }

    /// Build the endpoint-only output (wire structs + translations + endpoints).
    pub fn build_endpoint_module<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_endpoint_header(self));
        code.add_child(self.render_wire_type_definitions(generator));
        code.add_child(self.render_conversion_helpers(generator));
        code.add_child(self.render_endpoint_definitions(generator));

        return code;
    }

    /// Build a single combined output that contains both type and endpoint definitions.
    pub fn build_combined_module<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_endpoint_header(self));
        code.add_child(generator.generate_type_header(self));

        code.add_child(self.render_domain_type_definitions(generator));

        code.add_child(self.render_wire_type_definitions(generator));
        code.add_child(self.render_conversion_helpers(generator));
        code.add_child(self.render_endpoint_definitions(generator));

        return code;
    }
}

pub trait Generator {
    fn generate_endpoint_header(&self, _defs: &Definitons) -> Code {
        return Code::new_segment();
    }
    fn generate_type_header(&self, _defs: &Definitons) -> Code {
        return Code::new_segment();
    }

    fn generate_type(&self, name: &str, model: &Type, public: bool, defs: &Definitons) -> Code;
    fn generate_type_translation(&self, model: &TypeInformation, defs: &Definitons) -> Code;
    fn generate_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code;
}
