use super::{endpoint::*, types::*};
use crate::builder::Code;
use anyhow::Result;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug)]
struct SplitType {
    wire: Type,
    domain: Type,
    wire_name: String,
}

#[derive(Debug)]
pub struct TypeInformation {
    pub(super) name: String,
    pub(super) ty: Type,
    pub(super) conversion: Option<SplitType>,
}

impl TypeInformation {
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

    fn determine_type(ty: &mut Type, names: &Vec<String>) {
        match ty {
            Type::Struct(struct_) => {
                for (_, ty) in &mut struct_.members {
                    Definitons::determine_type(ty, names);
                }
            }
            Type::Array(arr) => {
                Definitons::determine_type(&mut arr.ty, names);
            }
            Type::Optional(opt) => {
                Definitons::determine_type(&mut opt.ty, names);
            }
            Type::Union(union) => {
                for ty in &mut union.tys {
                    Definitons::determine_type(ty, names);
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

    /// Will check if all names are known, will fail fast on unknown names.
    fn check_if_defined(&mut self) {
        let type_names: Vec<String> = self.types.keys().map(|k| k.clone()).collect();

        for (_, ty) in &mut self.types {
            Definitons::determine_type(&mut ty.ty, &type_names);
        }

        for (_, endpoint) in &mut self.end_points {
            for (_, ty) in endpoint.params.iter_mut() {
                Definitons::determine_type(ty, &type_names);
            }
            Definitons::determine_type(&mut endpoint.return_type, &type_names);
        }
    }

    pub fn generate_domain_type(&self, ty: &Type) -> Type {
        assert!(ty.contains_into(self));

        return match ty {
            Type::Into(i) => Type::Repr(i.into.clone()),
            Type::Optional(o) => Type::optional(self.generate_domain_type(&o.ty)),
            Type::Array(a) => Type::array(self.generate_domain_type(&a.ty), a.len),
            Type::Struct(st) => {
                let mut s = StructType::new();
                for (name, ty) in &st.members {
                    if !ty.contains_into(self) {
                        s.members.push((name.clone(), ty.clone()));
                    } else {
                        s.members
                            .push((name.clone(), self.generate_domain_type(ty)));
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
                        s.tys.push(self.generate_domain_type(ty));
                    }
                }
                Type::Union(s)
            }

            Type::Named(name) => Type::Named(name.clone()),

            _ => {
                unreachable!()
            }
        };
    }

    pub fn generate_wire_type(&self, ty: &Type) -> Type {
        assert!(ty.contains_into(self), "{ty:?} doesn't contains into");

        return match ty {
            Type::Into(i) => (*i.from).clone(),
            Type::Optional(o) => Type::optional(self.generate_wire_type(&o.ty)),
            Type::Array(a) => Type::array(self.generate_wire_type(&a.ty), a.len),
            Type::Struct(st) => {
                let mut s = StructType::new();
                for (name, ty) in &st.members {
                    if !ty.contains_into(self) {
                        s.members.push((name.clone(), ty.clone()));
                    } else {
                        s.members.push((name.clone(), self.generate_wire_type(ty)));
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
                        s.tys.push(self.generate_wire_type(ty));
                    }
                }
                Type::Union(s)
            }
            Type::Named(name) => Type::Named(format!("_{name}")),

            _ => {
                unreachable!()
            }
        };
    }

    pub fn add_type(&mut self, name: String, ty: Type) {
        self.types.insert(
            name.clone(),
            TypeInformation {
                name,
                ty,
                conversion: None,
            },
        );
    }

    pub fn get_definitions<P: AsRef<Path>>(p: P) -> Result<Self> {
        let mut defs = super::dsl::get_definitions(p)?;
        defs.check_if_defined();

        // I hate the borrow checker sometimes
        let mut new_types = HashMap::new();
        for (name, ty) in &defs.types {
            if !ty.ty.contains_into(&defs) {
                continue;
            }

            let wire_name = format!("_{name}");
            let wire = defs.generate_wire_type(&ty.ty);
            let domain = defs.generate_domain_type(&ty.ty);

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

    pub fn generate_type_definitons<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (name, ty) in &self.types {
            let ty = ty.get_domain_type();
            code.add_child(generator.generate_type(name, ty, true, self));
        }
        return code;
    }

    pub fn generate_wire_type_definitons<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (_, ty) in &self.types {
            if let Some(c) = &ty.conversion {
                code.add_child(generator.generate_type(c.wire_name.as_str(), &c.wire, false, self));
            }
        }

        return code;
    }

    pub fn generate_translation_code<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (_, ty) in &self.types {
            if ty.conversion.is_none() {
                continue;
            }
            code.add_child(generator.generate_type_translation(ty, self));
        }

        return code;
    }

    pub fn generate_endpoint_definitons<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();

        for (name, endpoint) in &self.end_points {
            code.add_child(generator.generate_endpoint(name, endpoint, self));
        }

        return code;
    }

    pub fn generate_type_code<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_type_header(self));
        code.add_child(self.generate_type_definitons(generator));

        return code;
    }

    pub fn generate_endpoint_code<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_endpoint_header(self));
        code.add_child(self.generate_wire_type_definitons(generator));
        code.add_child(self.generate_translation_code(generator));
        code.add_child(self.generate_endpoint_definitons(generator));

        return code;
    }

    pub fn generate_united_code<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_endpoint_header(self));
        code.add_child(generator.generate_type_header(self));

        code.add_child(self.generate_type_definitons(generator));

        code.add_child(self.generate_wire_type_definitons(generator));
        code.add_child(self.generate_translation_code(generator));
        code.add_child(self.generate_endpoint_definitons(generator));

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
