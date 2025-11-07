use anyhow::{Result, anyhow};
use std::fmt::{Debug, Display};

use crate::parser::definitions::Definitons;

#[derive(PartialEq, Clone)]
pub enum PrimitiveType {
    Bool,                    // int_x
    Integer(Option<usize>),  // int_x
    Unsigned(Option<usize>), // uint_x
    Float(Option<usize>),    // float_x
    String(Option<usize>),   // string_x
}

#[derive(PartialEq, Clone)]
pub struct OptionType {
    pub ty: Box<Type>,
}

impl OptionType {
    pub fn new(ty: Type) -> Self {
        Self { ty: Box::new(ty) }
    }
}

#[derive(PartialEq, Clone)]
pub struct ArrayType {
    pub ty: Box<Type>,
    pub len: Option<usize>,
}

impl ArrayType {
    pub fn new(ty: Type, len: Option<usize>) -> Self {
        Self {
            ty: Box::new(ty),
            len,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct IntoType {
    pub from: Box<Type>,
    pub into: Repr,
}

impl IntoType {
    pub fn new(from: Type, into: Repr) -> Self {
        Self {
            from: Box::new(from),
            into,
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Repr {
    Datetime,
}

impl Repr {}

#[derive(PartialEq, Clone)]
pub enum LiteralType {
    String(String),
    Uint(u64),
    Int(i64),
    Bool(bool),
    Float(f64),
}

#[derive(PartialEq, Clone)]
pub struct UnionType {
    pub tys: Vec<Type>,
}

impl UnionType {
    pub fn new(tys: Vec<Type>) -> Self {
        return Self { tys };
    }
}

#[derive(PartialEq, Clone)]
pub struct StructType {
    pub members: Vec<(String, Type)>,
}

impl StructType {
    pub fn new() -> Self {
        return Self {
            members: Vec::new(),
        };
    }

    pub fn new_with(members: Vec<(String, Type)>) -> Self {
        return Self { members };
    }
}

#[derive(PartialEq, Clone)]
pub enum Type {
    Null,                     // null
    Literal(LiteralType),     // "ok", "err", etc etc
    Primitive(PrimitiveType), // PT
    Repr(Repr),               // RT
    Optional(OptionType),     // T?
    Array(ArrayType),         // T[x] singled typed arrays
    Union(UnionType),         // T1 | T2 | ...| Tn
    Struct(StructType),       // T1 | T2 | ...| Tn
    Into(IntoType),           // T as Repr
    Named(String),            // Name
    Undetermined(String),     // Name
}

impl Default for Type {
    fn default() -> Self {
        return Self::Null;
    }
}

impl Type {
    #[allow(dead_code)]
    pub fn int(prec: Option<usize>) -> Self {
        Self::Primitive(PrimitiveType::Integer(prec))
    }
    #[allow(dead_code)]
    pub fn uint(prec: Option<usize>) -> Self {
        Self::Primitive(PrimitiveType::Unsigned(prec))
    }
    #[allow(dead_code)]
    pub fn float(prec: Option<usize>) -> Self {
        Self::Primitive(PrimitiveType::Float(prec))
    }
    #[allow(dead_code)]
    pub fn string(prec: Option<usize>) -> Self {
        Self::Primitive(PrimitiveType::String(prec))
    }
    #[allow(dead_code)]
    pub fn bool() -> Self {
        Self::Primitive(PrimitiveType::Bool)
    }

    #[allow(dead_code)]
    pub fn optional(t: Type) -> Self {
        Self::Optional(OptionType::new(t))
    }
    #[allow(dead_code)]
    pub fn array(t: Type, prec: Option<usize>) -> Self {
        Self::Array(ArrayType::new(t, prec))
    }
    #[allow(dead_code)]
    pub fn into(from: Type, to: Repr) -> Self {
        Self::Into(IntoType::new(from, to))
    }

    /// Recursively collect every model referenced by this type (including nested fields).
    pub fn unfold_models(&self, defs: &Definitons) {
        todo!()
        // let mut s = HashSet::new();
        // if let Self::Struct(m) = self {
        //     s.insert(m.clone());
        //     for (_, ty) in &defs.models[m].members {
        //         for v in ty.unfold_models(defs) {
        //             s.insert(v);
        //         }
        //     }
        // }

        // return s;
    }

    /// Returns true if the type or any nested field needs an `Into` conversion before transport.
    pub fn contains_into(&self, defs: &Definitons) -> bool {
        match self {
            Self::Into(_) => true,
            Self::Optional(o) => o.ty.contains_into(defs),
            Self::Array(a) => a.ty.contains_into(defs),
            Self::Struct(m) => {
                for (_, ty) in &m.members {
                    if ty.contains_into(defs) {
                        return true;
                    }
                }
                false
            }
            Self::Named(name) => defs.types[name].ty.contains_into(defs),
            _ => false,
        }
    }

    /// Replace `Undetermined` placeholders with concrete enum/model types based on known names.
    fn determine<F: Fn(String) -> Type>(&mut self, models: &Vec<String>, builder: F) -> Result<()> {
        match self {
            Self::Array(arr) => {
                return arr.ty.determine(models, builder);
            }
            Self::Optional(opt) => {
                return opt.ty.determine(models, builder);
            }
            Self::Undetermined(name) => {
                if models.contains(&name) {
                    *self = builder(name.clone());
                } else {
                    return Err(anyhow!(
                        "unknown Struct/Enum found: {name} is not in {models:?}"
                    ));
                }
            }
            _ => {}
        }

        return Ok(());
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn prec(p: &Option<usize>) -> String {
            return p
                .and_then(|v| Some(format!("_{v}")))
                .unwrap_or(String::new());
        }

        match self {
            Self::Bool => write!(f, "bool")?,
            Self::Integer(p) => write!(f, "int{}", prec(p))?,
            Self::Unsigned(p) => write!(f, "uint{}", prec(p))?,
            Self::Float(p) => write!(f, "float{}", prec(p))?,
            Self::String(p) => write!(f, "string{}", prec(p))?,
        }

        return Ok(());
    }
}

impl Display for OptionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}?", self.ty);
    }
}

impl Display for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}[{}]", self.ty, self.len.unwrap_or(0));
    }
}

impl Display for IntoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "'{}' as '{}'", self.from, self.into);
    }
}

impl Display for Repr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Datetime => write!(f, "datetime")?,
        }

        return Ok(());
    }
}

impl Display for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}i")?,
            Self::Uint(v) => write!(f, "{v}u")?,
            Self::Float(v) => write!(f, "{v}f")?,
            Self::String(v) => write!(f, "'{v}'")?,
            Self::Bool(v) => write!(f, "{v}")?,
        }

        return Ok(());
    }
}

impl Display for UnionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ty in &self.tys {
            write!(f, "or {ty}")?;
        }

        return Ok(());
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Union(s) => write!(f, "{s}")?,
            Self::Literal(l) => write!(f, "{l}")?,
            Self::Primitive(p) => write!(f, "{p}")?,
            Self::Repr(r) => write!(f, "{r}")?,
            Self::Optional(o) => write!(f, "{o}")?,
            Self::Array(a) => write!(f, "{a}")?,
            Self::Into(i) => write!(f, "{i}")?,
            Self::Struct(m) => write!(f, "{m}")?,
            Self::Named(n) => write!(f, "{n}")?,
            Self::Undetermined(u) => write!(f, "{u}")?,
            Self::Null => write!(f, "Null")?,
        }

        return Ok(());
    }
}

impl Debug for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Primitive<{self}>")
    }
}

impl Debug for OptionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Option<{:?}>", self.ty)
    }
}

impl Debug for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Array[{}]<{:?}>", self.len.unwrap_or(0), self.ty)
    }
}

impl Debug for IntoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Into<{:?}, {:?}>", self.from, self.into)
    }
}

impl Debug for Repr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Repr<{}>", self)
    }
}

impl Debug for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}i")?,
            Self::Uint(v) => write!(f, "{v}u")?,
            Self::Float(v) => write!(f, "{v}f")?,
            Self::String(v) => write!(f, "{v}")?,
            Self::Bool(v) => write!(f, "'{v}'")?,
        }

        return Ok(());
    }
}

impl Debug for UnionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ty in &self.tys {
            write!(f, "or {ty}")?;
        }

        return Ok(());
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Union(s) => write!(f, "{s}")?,
            Self::Literal(l) => write!(f, "{l}")?,
            Self::Primitive(p) => write!(f, "{p:?}")?,
            Self::Repr(r) => write!(f, "{r:?}")?,
            Self::Optional(o) => write!(f, "{o:?}")?,
            Self::Array(a) => write!(f, "{a:?}")?,
            Self::Into(i) => write!(f, "{i:?}")?,
            Self::Struct(s) => write!(f, "{s:?}")?,
            Self::Named(n) => write!(f, "{n:?}")?,
            Self::Undetermined(u) => write!(f, "Undetermined: {u}")?,
            Self::Null => write!(f, "Null")?,
        }

        return Ok(());
    }
}

impl Debug for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.members.iter()).finish()
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.members.iter()).finish()
    }
}
