use anyhow::{Result, anyhow};
use std::{
    collections::HashSet,
    fmt::{Debug, Display},
};

use crate::dsl::Definitons;

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
pub enum Type {
    Primitive(PrimitiveType), // PT
    Repr(Repr),               // RT
    Optional(OptionType),     // T?
    Null,                     // null
    Array(ArrayType),         // T[x] singled typed arrays
    /*
    List(Vec<Type>),          // [T1, T2, ..., Tn] mixed typed arrays
    Union(Vec<Type>),         // T1 | T2 | ...| Tn Union (only used for Into)
                              */
    Into(IntoType),       // T as Repr
    Model(String),        // Name
    Enum(String),         // Name
    Undetermined(String), // Name
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

    pub fn unfold_models(&self, defs: &Definitons) -> HashSet<String> {
        let mut s = HashSet::new();
        if let Self::Model(m) = self {
            s.insert(m.clone());
            for (_, ty) in &defs.models[m].params {
                for v in ty.unfold_models(defs) {
                    s.insert(v);
                }
            }
        }

        return s;
    }

    pub fn contains_into(&self, defs: &Definitons) -> bool {
        match self {
            Self::Into(_) => true,
            Self::Optional(o) => o.ty.contains_into(defs),
            Self::Array(a) => a.ty.contains_into(defs),
            Self::Model(m) => {
                for (_, ty) in &defs.models[m].params {
                    if ty.contains_into(defs) {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

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
                        "unknown Model/Enum found: {name} is not in {models:?}"
                    ));
                }
            }
            _ => {}
        }

        return Ok(());
    }

    pub fn determine_enum(&mut self, enums: &Vec<String>) -> Result<()> {
        return self.determine(enums, |s| Self::Enum(s));
    }

    pub fn determine_model(&mut self, models: &Vec<String>) -> Result<()> {
        return self.determine(models, |s| Self::Model(s));
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(p) => write!(f, "{p}")?,
            Self::Repr(r) => write!(f, "{r}")?,
            Self::Optional(o) => write!(f, "{o}")?,
            Self::Array(a) => write!(f, "{a}")?,
            Self::Into(i) => write!(f, "{i}")?,
            Self::Model(m) => write!(f, "{m}")?,
            Self::Enum(e) => write!(f, "{e}")?,
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

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(p) => write!(f, "{p:?}")?,
            Self::Repr(r) => write!(f, "{r:?}")?,
            Self::Optional(o) => write!(f, "{o:?}")?,
            Self::Array(a) => write!(f, "{a:?}")?,
            Self::Into(i) => write!(f, "{i:?}")?,
            Self::Model(m) => write!(f, "Model: {m}")?,
            Self::Enum(e) => write!(f, "Enum: {e}")?,
            Self::Undetermined(u) => write!(f, "Undetermined: {u}")?,
            Self::Null => write!(f, "Null")?,
        }

        return Ok(());
    }
}
