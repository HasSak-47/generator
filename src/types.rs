use std::fmt::{Debug, Display};

use anyhow::anyhow;

#[derive(PartialEq)]
pub enum PrimitiveType {
    Integer(Option<usize>),  // int_x
    Unsigned(Option<usize>), // uint_x
    Float(Option<usize>),    // float_x
    String(Option<usize>),   // string_x
}

impl PrimitiveType {
    pub fn parse<S: AsRef<str>>(s: S) -> anyhow::Result<Self> {
        let s = s.as_ref();
        let has_prec = s.find('_').is_some();
        let (type_dec, type_prec) = if has_prec {
            let mut split = s.split('_');
            (
                split.next().unwrap(),
                split.next().and_then(|s| usize::from_str_radix(s, 10).ok()),
            )
        } else {
            (s, None)
        };

        return match type_dec {
            "int" => Ok(Self::Integer(type_prec)),   // int
            "uint" => Ok(Self::Unsigned(type_prec)), // uint
            "float" => Ok(Self::Float(type_prec)),   // float
            "string" => Ok(Self::String(type_prec)), // string
            _ => Err(anyhow!("failed to find primitive type")),
        };
    }
}

#[derive(PartialEq)]
pub struct OptionType {
    ty: Box<Type>,
}

impl OptionType {
    pub fn parse<S: AsRef<str>>(s: S) -> anyhow::Result<Self> {
        let mut chars: Vec<char> = s.as_ref().chars().collect();
        let qs = chars.pop().unwrap();
        if qs != '?' {
            return Err(anyhow!("could not find '?'"));
        }
        let type_body = String::from_iter(chars);

        return Ok(Self {
            ty: Box::new(Type::parse(type_body)?),
        });
    }

    fn new(ty: Type) -> Self {
        Self { ty: Box::new(ty) }
    }
}

#[derive(PartialEq)]
pub struct ArrayType {
    ty: Box<Type>,
    len: Option<usize>,
}

impl ArrayType {
    pub fn parse<S: AsRef<str>>(s: S) -> anyhow::Result<Self> {
        let chars: Vec<char> = s.as_ref().chars().collect();
        let ob = chars
            .iter()
            .enumerate()
            .find(|x| *x.1 == '[')
            .ok_or(anyhow!("No open bracket found"))?
            .0;
        let cb = chars
            .iter()
            .enumerate()
            .find(|x| *x.1 == ']')
            .ok_or(anyhow!("No close bracket found"))?
            .0;

        let prec_str = String::from_iter(chars[ob..cb].iter());
        let prec = usize::from_str_radix(&prec_str, 10).ok();

        let type_body = String::from_iter(chars[0..ob].iter());
        return Ok(Self {
            ty: Box::new(Type::parse(type_body)?),
            len: prec,
        });
    }

    fn new(ty: Type, len: Option<usize>) -> Self {
        Self {
            ty: Box::new(ty),
            len,
        }
    }
}

#[derive(PartialEq)]
pub struct IntoType {
    from: Box<Type>,
    into: Repr,
}

impl IntoType {
    fn new(from: Type, into: Repr) -> Self {
        Self {
            from: Box::new(from),
            into,
        }
    }

    pub fn parse<S: AsRef<str>>(s: S) -> anyhow::Result<Self> {
        let s = s.as_ref();

        let split = s.find("as").ok_or(anyhow!("as not found"))?;
        let chars: Vec<_> = s.chars().collect();
        let start_t = Type::parse(String::from_iter(chars[..(split - 1)].iter()))?;
        let end_t = Repr::parse(String::from_iter(chars[(split + 3)..].iter()))?;

        return Ok(Self {
            from: Box::new(start_t),
            into: end_t,
        });
    }
}

#[derive(PartialEq)]
pub enum Repr {
    Datetime,
}

impl Repr {
    pub fn parse<S: AsRef<str>>(s: S) -> anyhow::Result<Self> {
        return match s.as_ref() {
            "datetime" => Ok(Self::Datetime),
            _ => Err(anyhow!("failed to find Repr type")),
        };
    }
}

#[derive(PartialEq)]
pub enum Type {
    Primitive(PrimitiveType), // PT
    Optional(OptionType),     // T?
    Array(ArrayType),         // T[x] singled typed arrays
    /*
    List(Vec<Type>),          // [T1, T2, ..., Tn] mixed typed arrays
    Union(Vec<Type>),         // T1 | T2 | ...| Tn Union (only used for Into)
                              */
    Into(IntoType), // T as BuiltIn
}

impl Type {
    fn int(prec: Option<usize>) -> Self {
        Self::Primitive(PrimitiveType::Integer(prec))
    }
    fn uint(prec: Option<usize>) -> Self {
        Self::Primitive(PrimitiveType::Unsigned(prec))
    }
    fn float(prec: Option<usize>) -> Self {
        Self::Primitive(PrimitiveType::Float(prec))
    }
    fn string(prec: Option<usize>) -> Self {
        Self::Primitive(PrimitiveType::String(prec))
    }

    fn optional(t: Type) -> Self {
        Self::Optional(OptionType::new(t))
    }
    fn array(t: Type, prec: Option<usize>) -> Self {
        Self::Array(ArrayType::new(t, prec))
    }
    fn into(from: Type, to: Repr) -> Self {
        Self::Into(IntoType::new(from, to))
    }

    pub fn parse<S: AsRef<str>>(s: S) -> anyhow::Result<Self> {
        let s = s.as_ref();

        match OptionType::parse(s) {
            Ok(k) => return Ok(Self::Optional(k)),
            Err(_) => {} // Err(e) => println!("\"{s}\" is not OptionType {e}"),
        }

        match ArrayType::parse(s) {
            Ok(k) => return Ok(Self::Array(k)),
            Err(_) => {} // Err(e) => println!("\"{s}\" is not ArrayType {e}"),
        }

        match IntoType::parse(s) {
            Ok(k) => return Ok(Self::Into(k)),
            Err(_) => {} // Err(e) => println!("\"{s}\" is not IntoType {e}"),
        }

        match PrimitiveType::parse(s) {
            Ok(k) => return Ok(Self::Primitive(k)),
            Err(_) => {} // Err(e) => println!("\"{s}\" is not PrimitiveType {e}"),
        }

        panic!("could not determine type");
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
            Self::Optional(o) => write!(f, "{o}")?,
            Self::Array(a) => write!(f, "{a}")?,
            Self::Into(i) => write!(f, "{i}")?,
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
            Self::Optional(o) => write!(f, "{o:?}")?,
            Self::Array(a) => write!(f, "{a:?}")?,
            Self::Into(i) => write!(f, "{i:?}")?,
        }

        return Ok(());
    }
}

#[cfg(test)]
mod test {
    use crate::types::*;

    #[test]
    fn test_type_parser() -> anyhow::Result<()> {
        use Type as Ty;
        let trans = &[
            (Ty::int(None), "int"),
            (Ty::optional(Ty::int(None)), "int?"),
            (Ty::array(Ty::int(None), None), "int[]"),
            (Ty::optional(Ty::array(Ty::int(None), None)), "int[]?"),
            (Ty::array(Ty::optional(Ty::int(None)), None), "int?[]"),
            (Ty::into(Ty::int(None), Repr::Datetime), "int as datetime"),
            (Ty::into(Ty::int(None), Repr::Datetime), "int as datetime"),
            (
                Ty::optional(Ty::into(Ty::int(None), Repr::Datetime)),
                "int as datetime?",
            ),
            (
                Ty::array(Ty::into(Ty::int(None), Repr::Datetime), None),
                "int as datetime[]",
            ),
            (
                Ty::array(Ty::optional(Ty::into(Ty::int(None), Repr::Datetime)), None),
                "int as datetime?[]",
            ),
            (
                Ty::optional(Ty::array(Ty::into(Ty::int(None), Repr::Datetime), None)),
                "int as datetime[]?",
            ),
            (
                Ty::optional(Ty::array(
                    Ty::optional(Ty::into(Ty::int(None), Repr::Datetime)),
                    None,
                )),
                "int as datetime?[]?",
            ),
        ];

        for (ex, s) in trans {
            assert_eq!(*ex, Type::parse(s)?, "failed: {s}");
        }

        Ok(())
    }
}
