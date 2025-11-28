use std::{
    collections::HashSet,
    fmt::{Debug, Display},
};

use crate::parser::definitions::Definitons;

/// Primitive scalars available in the DSL.
#[derive(PartialEq, Clone)]
pub enum PrimitiveType {
    Bool,                    // int_x
    Integer(Option<usize>),  // int_x
    Unsigned(Option<usize>), // uint_x
    Float(Option<usize>),    // float_x
    String(Option<usize>),   // string_x
}

/// `T?` wrapper.
#[derive(PartialEq, Clone)]
pub struct OptionType {
    pub ty: Box<Type>,
}

impl OptionType {
    pub fn new(ty: Type) -> Self {
        Self { ty: Box::new(ty) }
    }
}

/// `T[n]` or `T[]` wrapper.
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

/// Representation coercion, e.g. `string as datetime`.
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

/// Transport representations available to `IntoType`.
#[derive(PartialEq, Clone)]
pub enum Repr {
    Datetime,
}

impl Repr {}

/// Literal leaf values used when constructing unions or defaults.
#[derive(PartialEq, Clone)]
pub enum LiteralType {
    String(String),
    Uint(u64),
    Int(i64),
    Bool(bool),
    Float(f64),
}

/// Controls whether a union is untagged, externally tagged, or carries inline discriminators.
#[derive(Debug, PartialEq, Clone)]
pub enum UnionKind {
    Untagged,
    Interal,
    External,
}

#[derive(PartialEq, Clone)]
pub struct UnionMember {
    pub tag: Option<String>,
    pub ty: Type,
}

impl UnionMember {
    pub fn untagged(ty: Type) -> Self {
        return Self { ty, tag: None };
    }

    pub fn tagged(tag: String, ty: Type) -> Self {
        return Self { ty, tag: Some(tag) };
    }
}

/// Wrapper describing `T1 | T2 | ... | Tn` and how it is tagged.
#[derive(PartialEq, Clone)]
pub struct UnionType {
    pub members: Vec<UnionMember>,
    pub kind: UnionKind,
}

impl UnionType {
    pub fn add_member(&mut self, m: UnionMember) {
        self.members.push(m);
    }

    pub fn add_untagged_member(&mut self, ty: Type) {
        self.add_member(UnionMember::untagged(ty))
    }

    pub fn add_tagged_member(&mut self, tag: String, ty: Type) {
        self.add_member(UnionMember::tagged(tag, ty))
    }

    pub fn new() -> Self {
        return Self {
            members: Vec::new(),
            kind: UnionKind::Untagged,
        };
    }
}

/// Struct-like layout (`type Foo = { bar: Baz }`).
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
}

#[derive(PartialEq, Clone)]
pub struct MapType {
    pub key: PrimitiveType,
    pub val: Box<Type>,
}

impl MapType {
    pub fn new(key: PrimitiveType, val: Type) -> Self {
        return Self {
            key,
            val: Box::new(val),
        };
    }
}

/// Root semantic type tree used by generators.
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
    Map(MapType),
    Named(String),        // Name
    Undetermined(String), // Name
}

impl Default for Type {
    fn default() -> Self {
        return Self::Null;
    }
}

impl Type {
    pub fn get_dependencies(&self, defs: &Definitons) -> HashSet<String> {
        match self {
            Self::Named(name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                set.extend(defs.get_named_type(name).unwrap().ty.get_dependencies(defs));

                return set;
            }
            Self::Array(arr) => return arr.ty.get_dependencies(defs),
            Self::Optional(opt) => return opt.ty.get_dependencies(defs),
            Self::Union(union) => {
                let mut v = HashSet::new();
                for ty in &union.members {
                    let w = ty.ty.get_dependencies(defs);
                    v.extend(w.into_iter());
                }

                return v;
            }
            Self::Struct(s) => {
                let mut v = HashSet::new();
                for (_, ty) in &s.members {
                    let w = ty.get_dependencies(defs);
                    v.extend(w.into_iter());
                }

                return v;
            }
            Self::Map(m) => {
                let mut h = HashSet::new();
                h.extend(m.val.get_dependencies(defs));
                return h;
            }
            _ => HashSet::new(),
        }
    }

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

    #[allow(dead_code)]
    pub fn get_struct(&self) -> &StructType {
        match self {
            Self::Struct(s) => return s,
            _ => panic!("type was not a struct"),
        }
    }
    #[allow(dead_code)]
    pub fn map(key: PrimitiveType, val: Type) -> Self {
        Self::Map(MapType::new(key, val))
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
            Self::Union(u) => {
                for ty in &u.members {
                    if ty.ty.contains_into(defs) {
                        return true;
                    }
                }
                false
            }
            Self::Named(name) => defs.get_named_type(name).unwrap().ty.contains_into(defs),
            _ => false,
        }
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
        write!(f, "{:?}: ", self.kind)?;
        for ty in &self.members {
            // TODO: also print the tags :p
            write!(f, "| {} ", ty.ty)?;
        }

        return Ok(());
    }
}

impl Display for MapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}, {}>", self.key, self.val)?;
        return Ok(());
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Map(m) => write!(f, "{m}")?,
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
        write!(f, "{self}")
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
        write!(f, "{:?}: ", self.kind)?;
        for ty in &self.members {
            // TODO: also print the tags :p
            write!(f, "| {}", ty.ty)?;
        }

        return Ok(());
    }
}

impl Debug for MapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{:?}, {:?}>", self.key, self.val)?;
        return Ok(());
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Map(m) => write!(f, "{m}")?,
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
