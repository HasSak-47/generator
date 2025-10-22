use std::{collections::HashMap, fmt::Display, fs::File, io::Read, path::Path};

use crate::types::*;
use anyhow::Result;
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

// TODO: merge Model and Enum with Type
#[derive(Debug, Default)]
pub struct Model {
    pub params: Vec<(String, Type)>,
}

#[derive(Debug, Default)]
pub struct Enum {
    pub params: Vec<String>,
}

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
            Type::Enum(_) | Type::Model(_) => return Some(EndPointParamKind::Body),
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
    pub models: HashMap<String, Model>,
    pub enums: HashMap<String, Enum>,
    pub end_points: HashMap<String, EndPoint>,
}

#[derive(Debug, Parser)]
#[grammar = "pest/lang.pest"]
pub struct LangParser {}

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

fn handle_type<'a>(p: Pair<'a, Rule>) -> Type {
    assert_eq!(p.as_rule(), Rule::ty);
    let mut iter = p.into_inner();
    let next = iter.next().unwrap();

    let mut ty = match next.as_rule() {
        Rule::primitive_type => handle_primitive_type(next),
        Rule::null_type => Type::Null,
        Rule::complex_type => Type::Undetermined(next.as_str().to_string()),
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
        e => unreachable!("unreachable rule reached? : {e:?}"),
    };

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

    return ty;
}

fn handle_member_field<'a>(p: Pair<'a, Rule>) -> (String, Type) {
    assert!(p.as_rule() == Rule::member_field);

    let mut name = String::new();
    for inner in p.into_inner() {
        match inner.as_rule() {
            Rule::name => name = inner.as_str().to_string(),
            Rule::ty => return (name, handle_type(inner)),
            _ => unreachable!("not a definition file!"),
        }
    }
    unreachable!("not a member definition!")
}
impl Definitons {
    fn new() -> Self {
        Self {
            models: HashMap::new(),
            enums: HashMap::new(),
            end_points: HashMap::new(),
        }
    }

    fn expand_types(&mut self) {
        let model_names: Vec<String> = self.models.keys().map(|k| k.clone()).collect();
        let enum_names: Vec<String> = self.enums.keys().map(|k| k.clone()).collect();
        for (_, model) in self.models.iter_mut() {
            for (_, param) in model.params.iter_mut() {
                let found = param.determine_enum(&enum_names).is_ok()
                    | param.determine_model(&model_names).is_ok();
                if !found {
                    panic!("could not expand type: {self:?}");
                }
            }
        }

        for (_, endpoint) in self.end_points.iter_mut() {
            for (_, param) in endpoint.params.iter_mut() {
                let found = param.determine_enum(&enum_names).is_ok()
                    | param.determine_model(&model_names).is_ok();
                if !found {
                    panic!("could not expand type: {self:?}");
                }
            }
            let found = endpoint.return_type.determine_enum(&enum_names).is_ok()
                | endpoint.return_type.determine_model(&model_names).is_ok();
            if !found {
                panic!("could not expand type: {self:?}");
            }
        }
    }

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
                Rule::model => {
                    let mut model = Model::default();
                    let mut iter = inner.into_inner();
                    let name = iter.next().unwrap();
                    assert!(name.as_rule() == Rule::name);

                    for pair in iter {
                        match pair.as_rule() {
                            Rule::member_field => model.params.push(handle_member_field(pair)),
                            _ => unreachable!("idk how you got here??"),
                        }
                    }
                    defs.models.insert(name.as_str().to_string(), model);
                }
                Rule::enums => {
                    let mut r#enum = Enum::default();
                    let mut iter = inner.into_inner();
                    let name = iter.next().unwrap();
                    assert!(name.as_rule() == Rule::name);

                    for inner in iter {
                        assert!(inner.as_rule() == Rule::string);
                        let s = inner.as_str().to_string();
                        r#enum.params.push(s);
                    }
                    defs.enums.insert(name.as_str().to_string(), r#enum);
                }
                Rule::end_point => {
                    let mut iter = inner.into_inner().peekable();
                    let mut end_point = EndPoint::default();
                    let name = iter.next().unwrap().as_str().to_string();

                    while iter.peek().unwrap().as_rule() == Rule::member_field {
                        end_point
                            .params
                            .push(handle_member_field(iter.next().unwrap()));
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
                        end_point.return_type = handle_type(o);
                    }
                    defs.end_points.insert(name, end_point);
                }
                Rule::COMMENT | Rule::EOI => {}
                _ => unreachable!("how did you got here??"),
            }
        }

        defs.expand_types();

        return Ok(defs);
    }
}

pub trait Generator {
    fn generate_endpoint_header(&self) -> String {
        return String::new();
    }
    fn generate_model_header(&self) -> String {
        return String::new();
    }

    fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> String;
    fn handle_enum(&self, name: &str, model: &Enum) -> String;
    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> String;
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn type_test() -> anyhow::Result<()> {
        const PREC_PRIMITIVES: &[&str] = &["int", "uint", "float", "string"];
        const PRIMITIVES: &[&str] = &["bool"];

        fn make_prec_primitive(prim: &str, bits: Option<usize>) -> Type {
            match prim {
                "int" => Type::int(bits),
                "uint" => Type::uint(bits),
                "float" => Type::float(bits),
                "string" => Type::string(bits),
                _ => unreachable!(),
            }
        }

        fn make_primitive(prim: &str) -> Type {
            match prim {
                "bool" => Type::bool(),
                _ => unreachable!(),
            }
        }

        for &prim in PREC_PRIMITIVES {
            let matches = &[
                (format!("{prim}_32"), make_prec_primitive(prim, Some(32))),
                (
                    format!("{prim}?"),
                    Type::optional(make_prec_primitive(prim, None)),
                ),
                (
                    format!("{prim}[]"),
                    Type::array(make_prec_primitive(prim, None), None),
                ),
                (
                    format!("{prim}[10]"),
                    Type::array(make_prec_primitive(prim, None), Some(10)),
                ),
                (
                    format!("{prim}[10]?"),
                    Type::optional(Type::array(make_prec_primitive(prim, None), Some(10))),
                ),
                (
                    format!("{prim}?[10]"),
                    Type::array(Type::optional(make_prec_primitive(prim, None)), Some(10)),
                ),
            ];

            for (s, expected) in matches {
                let parse = LangParser::parse(Rule::ty, s)?.next().unwrap();
                let ty = handle_type(parse);
                assert_eq!(ty, *expected, "Failed for {s}");
            }
        }

        for &prim in PRIMITIVES {
            let matches = &[
                // Width-specific variant (only applies to int/uint/float)
                (format!("{prim}"), make_primitive(prim)),
                (format!("{prim}?"), Type::optional(make_primitive(prim))),
                (format!("{prim}[]"), Type::array(make_primitive(prim), None)),
                (
                    format!("{prim}[10]"),
                    Type::array(make_primitive(prim), Some(10)),
                ),
                (
                    format!("{prim}[10]?"),
                    Type::optional(Type::array(make_primitive(prim), Some(10))),
                ),
                (
                    format!("{prim}?[10]"),
                    Type::array(Type::optional(make_primitive(prim)), Some(10)),
                ),
            ];

            for (s, expected) in matches {
                let parse = LangParser::parse(Rule::ty, s)?.next().unwrap();
                let ty = handle_type(parse);
                assert_eq!(ty, *expected, "Failed for {s}");
            }
        }

        return Ok(());
    }
}
