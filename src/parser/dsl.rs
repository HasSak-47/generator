use std::{collections::HashMap, fmt::Display, fs::File, io::Read, path::Path, str::FromStr};

use crate::{builder::Code, parser::types::*};
use anyhow::Result;
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

#[derive(Debug, Default)]
pub enum EndPointMethod {
    #[default]
    GET,
    POST,
    PUT,
}

impl Display for EndPointMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::GET => "get",
                Self::POST => "post",
                Self::PUT => "put",
            }
        )?;
        return Ok(());
    }
}

#[derive(Debug, Default)]
pub struct EndPoint {
    pub params: Vec<(String, Type)>,
    pub method: EndPointMethod,
    pub url: String,
    pub return_type: Type,
}

#[derive(Debug, Default)]
pub enum EndPointParamKind {
    #[default]
    Body,
    Path,
    Query,
}

impl EndPoint {
    pub fn get_param_type<S: AsRef<str>>(&self, name: S) -> Option<EndPointParamKind> {
        let name = name.as_ref();
        let (name, ty) = self.params.iter().find(|p| p.0 == name)?;
        match &ty {
            Type::Union(_) | Type::Struct(_) => return Some(EndPointParamKind::Body),
            _ => {
                if self.url.contains(format!("{{{name}}}").as_str()) {
                    return Some(EndPointParamKind::Path);
                } else {
                    return Some(EndPointParamKind::Query);
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Definitons {
    pub types: HashMap<String, Type>,
    pub end_points: HashMap<String, EndPoint>,
}

#[derive(Debug, Parser)]
#[grammar = "pest/lang.pest"]
pub struct LangParser {}

/// Convert a primitive rule from the parser into the corresponding strongly typed variant.
fn handle_primitive_type<'a>(p: Pair<'a, Rule>) -> Type {
    let mut iter = p.into_inner();
    let kind = iter.next().unwrap();
    match kind.as_rule() {
        Rule::pprec => {
            let prec = iter
                .next()
                .and_then(|p| usize::from_str_radix(p.as_str(), 10).ok());
            let primitive = kind.as_str().to_string();
            match primitive.as_str() {
                "int" => Type::int(prec),
                "uint" => Type::uint(prec),
                "float" => Type::float(prec),
                "string" => Type::string(prec),
                s => unreachable!("unreachable string reached? : {s:?}"),
            }
        }
        Rule::praw => match kind.as_str() {
            "bool" => Type::bool(),
            s => unreachable!("unreachable string reached? : {s:?}"),
        },
        _ => unreachable!(),
    }
}

fn handle_struct<'a>(p: Pair<'a, Rule>) -> Result<Type> {
    let inner = p.into_inner();
    let mut struct_ = StructType::new();
    for member_field in inner {
        assert_eq!(member_field.as_rule(), Rule::member_field);
        let mut inner = member_field.into_inner();
        let name = inner.next().unwrap().as_str().to_string();
        let ty = handle_type(inner.next().unwrap());
        struct_.members.push((name, ty?));
    }

    return Ok(Type::Struct(struct_));
}

/// Recursively walk the parsed type expression and build the semantic `Type`.
fn handle_type<'a>(p: Pair<'a, Rule>) -> Result<Type> {
    assert_eq!(p.as_rule(), Rule::ty);
    let mut iter = p.into_inner();
    let next = iter.next().unwrap();

    let mut ty = match next.as_rule() {
        Rule::primitive_type => handle_primitive_type(next),
        Rule::null_type => Type::Null,
        Rule::named_type => Type::Undetermined(next.as_str().to_string()),
        Rule::repr_type => Type::Repr(Repr::Datetime),

        Rule::into_type => {
            let mut inner = next.into_inner();
            let from = inner.next().unwrap();
            let to = inner.next().unwrap();

            Type::into(
                handle_primitive_type(from),
                match to.as_str() {
                    "datetime" => Repr::Datetime,
                    _ => unreachable!(),
                },
            )
        }
        Rule::value_type => {
            let mut inner = next.into_inner();
            let kind = inner.next().unwrap().into_inner().next().unwrap();
            let value = kind.as_str();
            return Ok(match kind.as_rule() {
                Rule::int => Type::Literal(LiteralType::Int(i64::from_str_radix(value, 10)?)),
                Rule::uint => Type::Literal(LiteralType::Uint(u64::from_str_radix(value, 10)?)),
                Rule::float => Type::Literal(LiteralType::Float(f64::from_str(value)?)),
                Rule::string => {
                    let s = kind
                        .into_inner()
                        .next()
                        .and_then(|s| Some(s.as_str().to_string()))
                        .unwrap_or(String::new());
                    Type::Literal(LiteralType::String(s))
                }
                Rule::bool => Type::Literal(LiteralType::Bool(bool::from_str(value)?)),
                _ => unreachable!(),
            });
        }
        Rule::union_type => {
            let inner = next.into_inner();
            let mut types = Vec::new();
            for ty in inner {
                types.push(handle_type(ty)?);
            }
            return Ok(Type::Union(UnionType::new(types)));
        }
        Rule::struct_type => handle_struct(next)?,
        e => unreachable!("unreachable rule reached? : {e:?}"),
    };

    // Apply trailing decorators such as option or array in the order they appear.
    while let Some(_) = iter.peek() {
        let next = iter.next().unwrap();
        assert_eq!(next.as_rule(), Rule::weird_mark);
        let mark = next.into_inner().next().unwrap();
        match mark.as_rule() {
            Rule::option_mark => ty = Type::optional(ty),
            Rule::array_mark => {
                let prec = mark
                    .into_inner()
                    .next()
                    .and_then(|p| usize::from_str_radix(p.as_str(), 10).ok());
                ty = Type::array(ty, prec);
            }
            e => unreachable!("unreachable rule reached? : {e:?}"),
        }
    }

    return Ok(ty);
}

fn handle_member_field<'a>(p: Pair<'a, Rule>) -> Result<(String, Type)> {
    assert!(p.as_rule() == Rule::member_field);

    let mut name = String::new();
    for inner in p.into_inner() {
        match inner.as_rule() {
            Rule::name => name = inner.as_str().to_string(),
            Rule::ty => return Ok((name, handle_type(inner)?)),
            _ => unreachable!("not a definition file!"),
        }
    }
    unreachable!("not a member definition!")
}
impl Definitons {
    fn new() -> Self {
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
            let found = Definitons::check_type(ty, &type_names);
            if !found {
                let ty_name = ty.to_string();
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

    /// Load the DSL file, parse it with pest, and translate the AST into `Definitons`.
    pub fn get_definitions<P: AsRef<Path>>(p: P) -> Result<Self> {
        let mut file = File::open(p)?;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        let p = LangParser::parse(Rule::definitions, buf.as_str())?
            .next()
            .unwrap();

        if let Rule::definitions = p.as_rule() {
        } else {
            unreachable!("not a definition file!")
        }

        let mut defs = Definitons::new();

        for inner in p.into_inner() {
            match inner.as_rule() {
                Rule::struct_definition => {
                    let mut iter = inner.into_inner();
                    let name = iter.next().unwrap();
                    assert!(name.as_rule() == Rule::name);
                    // TODO: handle extensions
                    let struct_ = handle_struct(iter.next().unwrap())?;
                    defs.types.insert(name.as_str().to_string(), struct_);
                }
                Rule::end_point => {
                    let mut iter = inner.into_inner().peekable();
                    let mut end_point = EndPoint::default();
                    let name = iter.next().unwrap().as_str().to_string();

                    while iter.peek().unwrap().as_rule() == Rule::member_field {
                        end_point
                            .params
                            .push(handle_member_field(iter.next().unwrap())?);
                    }

                    match iter.next().unwrap().into_inner().next().unwrap().as_str() {
                        "post" => end_point.method = EndPointMethod::POST,
                        "put" => end_point.method = EndPointMethod::PUT,
                        "get" => end_point.method = EndPointMethod::GET,
                        x => unreachable!("{x} is not a supported method"),
                    }

                    end_point.url = iter
                        .next()
                        .unwrap()
                        .into_inner()
                        .next()
                        .unwrap()
                        .as_str()
                        .to_string();
                    if let Some(o) = iter.next() {
                        end_point.return_type = handle_type(o)?;
                    }
                    defs.end_points.insert(name, end_point);
                }
                Rule::COMMENT | Rule::EOI => {}
                _ => unreachable!("how did you got here??"),
            }
        }

        defs.check_if_defined();

        return Ok(defs);
    }
}

pub trait Generator {
    fn generate_endpoint_header(&self, _defs: &Definitons) -> Code {
        return Code::new_segment();
    }
    fn generate_model_header(&self, _defs: &Definitons) -> Code {
        return Code::new_segment();
    }

    fn handle_type(&self, name: &str, model: &Type, defs: &Definitons) -> Code;
    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code;
}

#[cfg(test)]
mod test {
    use crate::parser::dsl;

    use super::*;
    #[test]
    fn type_parsing_test() -> anyhow::Result<()> {
        let tests = &[
            ("int_32", Type::int(Some(32))),
            ("string", Type::string(None)),
            ("int?", Type::optional(Type::int(None))),
            ("int_32[]", Type::array(Type::int(Some(32)), None)),
            (
                "int_32[]?",
                Type::optional(Type::array(Type::int(Some(32)), None)),
            ),
            (
                "string as datetime",
                Type::into(Type::string(None), Repr::Datetime),
            ),
            (
                "string as datetime[]",
                Type::array(Type::into(Type::string(None), Repr::Datetime), None),
            ),
            ("null", Type::Null),
            ("1", Type::Literal(LiteralType::Int(1))),
            ("10", Type::Literal(LiteralType::Int(10))),
            ("-0.1", Type::Literal(LiteralType::Float(-0.1))),
            (
                "\"string\"",
                Type::Literal(LiteralType::String("string".to_string())),
            ),
            ("false", Type::Literal(LiteralType::Bool(false))),
            ("true", Type::Literal(LiteralType::Bool(true))),
            (
                "{\"ok\" | \"err\"}",
                Type::Union(UnionType {
                    tys: vec![
                        Type::Literal(LiteralType::String("ok".to_string())),
                        Type::Literal(LiteralType::String("err".to_string())),
                    ],
                }),
            ),
            (
                "{ foo: int, bar: string as datetime? }",
                Type::Struct(StructType {
                    members: vec![
                        ("foo".to_string(), Type::int(None)),
                        (
                            "bar".to_string(),
                            Type::optional(Type::into(Type::string(None), Repr::Datetime)),
                        ),
                    ],
                }),
            ),
            (
                "{{foo: \"ok\", bar: string} | {foo:\"err\", bar: int, baz: string?}}",
                Type::Union(UnionType {
                    tys: vec![
                        Type::Struct(StructType {
                            members: vec![
                                (
                                    "foo".to_string(),
                                    Type::Literal(LiteralType::String("ok".to_string())),
                                ),
                                ("bar".to_string(), Type::string(None)),
                            ],
                        }),
                        Type::Struct(StructType {
                            members: vec![
                                (
                                    "foo".to_string(),
                                    Type::Literal(LiteralType::String("err".to_string())),
                                ),
                                ("bar".to_string(), Type::int(None)),
                                ("baz".to_string(), Type::optional(Type::string(None))),
                            ],
                        }),
                    ],
                }),
            ),
        ];
        for (text, ty) in tests {
            let p = LangParser::parse(Rule::ty, text)?.next().unwrap();
            let target_ty = dsl::handle_type(p)?;
            assert_eq!(*ty, target_ty, "failed to process {text} {ty}");
        }

        return Ok(());
    }
}
