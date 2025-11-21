use super::{endpoint::*, types::*};
use crate::builder::Code;
use anyhow::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Helper describing the wire/domain pair for a type that requires conversions.
#[derive(Debug)]
struct SplitType {
    wire: Type,
    domain: Type,
    wire_name: Option<String>,
}

pub struct TypeInformationBuilder {
    name: Option<String>,
    ty: Type,
    path: Option<PathBuf>,
    line: Option<usize>,
    col: Option<usize>,
}

impl TypeInformationBuilder {
    pub fn new(ty: Type) -> Self {
        return Self {
            name: None,
            ty,
            path: None,
            line: None,
            col: None,
        };
    }
    pub fn new_named(name: String, ty: Type) -> Self {
        return Self {
            name: Some(name),
            ty,
            path: None,
            line: None,
            col: None,
        };
    }

    pub fn build_type(self) -> TypeInformation {
        return TypeInformation {
            name: self.name,
            ty: self.ty,
            path: self.path.unwrap(),
            line: self.line.unwrap(),
            col: self.col.unwrap(),
            conversion: None,
        };
    }

    pub fn set_path<P: AsRef<Path>>(&mut self, p: P) {
        self.path = Some(p.as_ref().to_path_buf());
    }

    pub fn set_col(&mut self, col: usize) {
        self.col = Some(col);
    }

    pub fn set_line(&mut self, line: usize) {
        self.line = Some(line);
    }

    pub fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }
}

/// Metadata for a single named type in the DSL.
#[derive(Debug)]
pub struct TypeInformation {
    /// Name of the type
    pub name: Option<String>,
    pub ty: Type,
    pub path: PathBuf,
    pub line: usize,
    pub col: usize,
    conversion: Option<SplitType>,
}

impl TypeInformation {
    #[allow(dead_code)]
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

    pub fn get_wire_name(&self) -> &Option<String> {
        if let Some(con) = &self.conversion {
            &con.wire_name
        } else {
            &self.name
        }
    }
}

pub struct EndPointInformationBuilder {
    name: Option<String>,
    endpoint: EndPoint,
    path: Option<PathBuf>,
    line: Option<usize>,
    col: Option<usize>,
}

impl EndPointInformationBuilder {
    pub fn new(endpoint: EndPoint) -> Self {
        return Self {
            name: None,
            endpoint,
            path: None,
            line: None,
            col: None,
        };
    }
    pub fn new_named(name: String, endpoint: EndPoint) -> Self {
        return Self {
            name: Some(name),
            endpoint,
            path: None,
            line: None,
            col: None,
        };
    }

    pub fn build_type(self) -> EndPointInformation {
        return EndPointInformation {
            name: self.name,
            endpoint: self.endpoint,
            path: self.path.unwrap(),
            line: self.line.unwrap(),
            col: self.col.unwrap(),
        };
    }

    pub fn set_path<P: AsRef<Path>>(&mut self, p: P) {
        self.path = Some(p.as_ref().to_path_buf());
    }

    pub fn set_col(&mut self, col: usize) {
        self.col = Some(col);
    }

    pub fn set_line(&mut self, line: usize) {
        self.line = Some(line);
    }

    pub fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }
}

#[derive(Debug)]
pub struct EndPointInformation {
    pub name: Option<String>,
    pub endpoint: EndPoint,
    pub path: PathBuf,
    pub line: usize,
    pub col: usize,
}

/// Aggregates all parsed type and endpoint declarations.
#[derive(Debug)]
pub struct Definitons {
    named_types: HashMap<String, TypeInformation>,
    end_points: HashMap<String, EndPointInformation>,
}

impl Definitons {
    pub fn new() -> Self {
        Self {
            named_types: HashMap::new(),
            end_points: HashMap::new(),
        }
    }

    pub fn populate_union_tags(&mut self) {
        for ty in &mut self.named_types {
            if let Type::Union(u) = &mut ty.1.ty {
                if u.kind == UnionKind::Untagged {
                    continue;
                }

                for mem in &mut u.members {
                    if mem.tag.is_none() {
                        mem.tag = Some(mem.ty.to_string())
                    }
                }
            }
        }
    }

    /// Ensure untagged unions only contain primitives or other unions (structs would be ambiguous).
    pub fn validate_untagged_union(&self, u: &UnionType) {
        for UnionMember { ty, .. } in &u.members {
            match ty {
                Type::Struct(_) => {
                    panic!("untagged union that contains structs is not valid yet!")
                }
                Type::Named(name) => {
                    if let Type::Struct(_) = self.named_types.get(name).unwrap().ty {
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
                for UnionMember { ty, .. } in &mut union.members {
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
        let type_names: Vec<String> = self.named_types.keys().map(|k| k.clone()).collect();

        for (_, ty) in &mut self.named_types {
            Definitons::resolve_type_references(&mut ty.ty, &type_names);
        }

        for (_, endpoint) in &mut self.end_points {
            for (_, ty) in endpoint.endpoint.params.iter_mut() {
                Definitons::resolve_type_references(ty, &type_names);
            }
            Definitons::resolve_type_references(&mut endpoint.endpoint.return_type, &type_names);
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
                let mut s = UnionType::new();
                for member in &u.members {
                    if !member.ty.contains_into(self) {
                        s.add_member(member.clone());
                    } else {
                        s.add_member(UnionMember {
                            tag: member.tag.clone(),
                            ty: self.convert_to_domain_type(&member.ty),
                        });
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
                let mut s = UnionType::new();
                for member in &u.members {
                    if !member.ty.contains_into(self) {
                        s.add_member(member.clone());
                    } else {
                        s.add_member(UnionMember {
                            tag: member.tag.clone(),
                            ty: self.convert_to_wire_type(&member.ty),
                        });
                    }
                }
                s.kind = u.kind.clone();
                Type::Union(s)
            }
            Type::Named(name) => {
                if self.named_types[name].ty.contains_into(self) {
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

    /// Insert a parsed `endpoint` declaration into the definitions map.
    pub fn register_endpoint(&mut self, builder: EndPointInformationBuilder) {
        self.end_points
            .insert(builder.name.as_ref().unwrap().clone(), builder.build_type());
    }

    /// Insert a parsed `type` declaration into the definitions map.
    pub fn register_type(&mut self, builder: TypeInformationBuilder) {
        self.named_types
            .insert(builder.name.as_ref().unwrap().clone(), builder.build_type());
    }

    pub fn build_definitons(&mut self) {
        self.validate_type_references();

        // I hate the borrow checker sometimes
        let mut new_types = HashMap::new();
        for (name, ty) in &self.named_types {
            if let Type::Union(u) = &ty.ty {
                if u.kind == UnionKind::Untagged {
                    self.validate_untagged_union(u);
                }
            }

            if !ty.ty.contains_into(&self) {
                continue;
            }

            let wire_name = Some(format!("_{name}"));
            let wire = self.convert_to_wire_type(&ty.ty);
            let domain = self.convert_to_domain_type(&ty.ty);

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
            let t = self.named_types.get_mut(&name).unwrap();
            t.conversion = Some(s);
        }
    }

    /// Load and normalize the DSL definitions file, annotating types with conversion metadata.
    pub fn load_from_file<P: AsRef<Path>>(&mut self, p: P) -> Result<()> {
        super::dsl::add_definitions(self, p)?;
        self.populate_union_tags();
        return Ok(());
    }

    /// Emit code for every domain type using the provided generator implementation.
    pub fn render_domain_type_definitions<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (name, ty) in &self.named_types {
            let ty = ty.get_domain_type();
            code.add_child(generator.generate_type(name, ty, true, self));
        }
        return code;
    }

    /// Emit the wire-model definitions for types that require conversion.
    pub fn render_wire_type_definitions<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (_, ty) in &self.named_types {
            if ty.has_conversion() {
                let ty = generator.generate_type(
                    ty.get_wire_name().as_ref().unwrap().as_str(),
                    ty.get_wire_type(),
                    false,
                    self,
                );
                code.add_child(ty);
            }
        }

        return code;
    }

    /// Emit the helper functions that translate between domain and wire representations.
    pub fn render_conversion_helpers<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        for (_, ty) in &self.named_types {
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
            code.add_child(generator.generate_endpoint(name, &endpoint.endpoint, self));
        }

        return code;
    }

    /// Build a standalone chunk of code that has all the type declarations.
    pub fn build_unified_type_module<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_type_header(self));
        code.add_child(self.render_domain_type_definitions(generator));

        return code;
    }

    /// Build a standalone chunk of code thaat has all the endpoint-only output (wire structs + translations + endpoints).
    pub fn build_unified_endpoint_module<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_endpoint_header(self));
        code.add_child(self.render_wire_type_definitions(generator));
        code.add_child(self.render_conversion_helpers(generator));
        code.add_child(self.render_endpoint_definitions(generator));

        return code;
    }

    /// Build a single combined output that contains all type and all endpoint definitions.
    pub fn build_unified_joint_module<G: Generator + ?Sized>(&self, generator: &G) -> Code {
        let mut code = Code::new_segment();
        code.add_child(generator.generate_endpoint_header(self));
        code.add_child(generator.generate_type_header(self));

        code.add_child(self.render_domain_type_definitions(generator));

        code.add_child(self.render_wire_type_definitions(generator));
        code.add_child(self.render_conversion_helpers(generator));
        code.add_child(self.render_endpoint_definitions(generator));

        return code;
    }

    /// Build a HashMap in which the key is the file name and the value is a chunk of code
    /// containing type definitions
    pub fn build_decoupled_type_module<G>(&self, generator: &G) -> HashMap<String, Code>
    where
        G: Generator + ?Sized,
    {
        let mut modules: HashMap<_, Code> = HashMap::new();
        for (name, info) in &self.named_types {
            let path = info.path.file_name().unwrap().to_str().unwrap().to_string();
            if let Some(code) = modules.get_mut(&path) {
                code.add_child(generator.generate_type(&name, &info.ty, true, self));
            } else {
                let mut code = Code::new_segment();
                code.add_child(generator.generate_type(&name, &info.ty, true, self));
                modules.insert(path, code);
            }
        }

        return modules;
    }

    /// Build a HashMap in which the key is the file name and the value is a chunk of code
    /// containing endpoint definitions
    pub fn build_decoupled_endpoint_module<G>(&self, generator: &G) -> HashMap<String, Code>
    where
        G: Generator + ?Sized,
    {
        todo!()
    }

    /// Build a HashMap in which the key is the file name and the value is a chunk of code
    /// containing type and endpoint definitions
    pub fn build_decoupled_joint_module<G>(&self, generator: &G) -> HashMap<String, Code>
    where
        G: Generator + ?Sized,
    {
        todo!()
    }

    pub fn get_named_type<S: AsRef<str>>(&self, name: S) -> Option<&TypeInformation> {
        let name = name.as_ref();
        return self
            .named_types
            .iter()
            .find(|n| name == *n.0)
            .and_then(|t| Some(t.1));
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
