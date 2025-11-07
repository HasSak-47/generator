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

    fn check_type(ty: &Type, names: &Vec<String>) -> bool {
        match ty {
            Type::Struct(struct_) => {
                for (_, ty) in &struct_.members {
                    let found = Definitons::check_type(ty, names);
                    if !found {
                        let ty_name = ty.to_string();
                        panic!("could not expand type {ty_name}");
                    }
                }
            }
            Type::Union(union) => {
                for ty in &union.tys {
                    let found = Definitons::check_type(ty, names);
                    if !found {
                        let ty_name = ty.to_string();
                        panic!("could not expand type {ty_name}");
                    }
                }
            }
            Type::Named(name) => return names.iter().find(|n| **n == *name).is_some(),
            _ => {}
        }

        return true;
    }

    /// Will check if all names are known, will fail fast on unknown names.
    fn check_if_defined(&mut self) {
        let type_names: Vec<String> = self.types.keys().map(|k| k.clone()).collect();

        for (_, ty) in &self.types {
            let found = Definitons::check_type(&ty.ty, &type_names);
            if !found {
                let ty_name = ty.ty.to_string();
                panic!("could not expand type {ty_name}");
            }
        }

        for (_, endpoint) in self.end_points.iter_mut() {
            for (_, ty) in endpoint.params.iter_mut() {
                let param_name = ty.to_string();
                let found = Definitons::check_type(ty, &type_names);
                if !found {
                    panic!("could not expand type {param_name}: {self:?}");
                }
            }
            let found = Definitons::check_type(&endpoint.return_type, &type_names);
            if !found {
                panic!("could not expand type: {self:?}");
            }
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
                    s.members
                        .push((name.clone(), self.generate_domain_type(ty)));
                }
                Type::Struct(s)
            }
            Type::Union(u) => {
                let mut s = UnionType::new(vec![]);
                for ty in &u.tys {
                    s.tys.push(self.generate_domain_type(ty));
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
        assert!(ty.contains_into(self));

        return match ty {
            Type::Into(i) => (*i.from).clone(),
            Type::Optional(o) => Type::optional(self.generate_wire_type(&o.ty)),
            Type::Array(a) => Type::array(self.generate_wire_type(&a.ty), a.len),
            Type::Struct(st) => {
                let mut s = StructType::new();
                for (name, ty) in &st.members {
                    s.members.push((name.clone(), self.generate_wire_type(ty)));
                }
                Type::Struct(s)
            }
            Type::Union(u) => {
                let mut s = UnionType::new(vec![]);
                for ty in &u.tys {
                    s.tys.push(self.generate_wire_type(ty));
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
}

pub trait Generator {
    fn generate_endpoint_header(&self, _defs: &Definitons) -> Code {
        return Code::new_segment();
    }
    fn generate_type_header(&self, _defs: &Definitons) -> Code {
        return Code::new_segment();
    }

    fn generate_type(&self, name: &str, model: &Type, defs: &Definitons) -> Code;
    fn generate_type_translations(&self, name: &str, model: &Type, defs: &Definitons) -> Code;
    fn generate_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code;
}
