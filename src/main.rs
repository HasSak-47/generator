mod dsl;
mod python;
mod ts;
mod types;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fs::File,
    io::Read,
    path::Path,
};

use anyhow::{Result, anyhow};
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;
use types::*;

// TODO: merge Model and Enum with Type
#[derive(Debug, Default)]
struct Model {
    params: Vec<(String, Type)>,
}

#[derive(Debug, Default)]
struct Enum {
    params: Vec<String>,
}

#[derive(Debug, Default)]
enum EndPointMethod {
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
struct EndPoint {
    params: Vec<(String, Type)>,
    method: EndPointMethod,
    url: String,
    return_type: Type,
}

#[derive(Debug, Default)]
pub enum EndPointParamKind {
    #[default]
    Body,
    Path,
    Query,
}

impl EndPoint {
    fn get_param_type<S: AsRef<str>>(&self, name: S) -> Option<EndPointParamKind> {
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
struct Definitons {
    models: HashMap<String, Model>,
    enums: HashMap<String, Enum>,
    end_points: HashMap<String, EndPoint>,
}

#[derive(Debug, Parser)]
#[grammar = "pest/lang.pest"]
struct LangParser {}

fn handle_primitive_type<'a>(p: Pair<'a, Rule>) -> Type {
    let mut iter = p.into_inner();
    let primitve_root = iter.next().unwrap();
    assert_eq!(
        primitve_root.as_rule(),
        Rule::primitive_root,
        "non primitive root found!"
    );
    let prec = iter
        .next()
        .and_then(|p| usize::from_str_radix(p.as_str(), 10).ok());
    let primitive = primitve_root.as_str().to_string();
    match primitive.as_str() {
        "int" => Type::int(prec),
        "uint" => Type::uint(prec),
        "float" => Type::float(prec),
        "string" => Type::string(prec),
        s => unreachable!("unreachable string reached? : {s:?}"),
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
                param.determine_enum(&enum_names);
                param.determine_model(&model_names);
            }
        }

        for (_, endpoint) in self.end_points.iter_mut() {
            for (_, param) in endpoint.params.iter_mut() {
                param.determine_enum(&enum_names);
                param.determine_model(&model_names);
            }
            endpoint.return_type.determine_enum(&enum_names);
            endpoint.return_type.determine_model(&model_names);
        }
    }

    fn get_definitions<P: AsRef<Path>>(p: P) -> Result<Self> {
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

trait Generator {
    fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> String;
    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> String;
}

fn main() -> Result<()> {
    let defs = Definitons::get_definitions("ex.dsl")?;
    let generator = ts::React::new();
    for (name, model) in &defs.models {
        println!("{}\n", generator.handle_model(name, model, &defs))
    }
    for (name, endpoint) in &defs.end_points {
        println!("{}\n", generator.handle_endpoint(name, endpoint, &defs))
    }
    return Ok(());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn type_test() -> anyhow::Result<()> {
        const PRIMITIVES: &[&str] = &["int", "uint", "float", "string"];
        let matches = &[
            ("int", Type::int(None)),
            ("int_32", Type::int(Some(32))),
            ("int?", Type::optional(Type::int(None))),
            ("int[]", Type::array(Type::int(None), None)),
            ("int[10]", Type::array(Type::int(None), Some(10))),
            (
                "int[10]?",
                Type::optional(Type::array(Type::int(None), Some(10))),
            ),
            (
                "int?[10]",
                Type::array(Type::optional(Type::int(None)), Some(10)),
            ),
            (
                "Product?[10]",
                Type::array(
                    Type::optional(Type::Undetermined("Product".to_string())),
                    Some(10),
                ),
            ),
            (
                "int_32 as datetime[10]",
                Type::array(Type::into(Type::int(Some(32)), Repr::Datetime), Some(10)),
            ),
            (
                "int_32 as datetime?[10]",
                Type::array(
                    Type::optional(Type::into(Type::int(Some(32)), Repr::Datetime)),
                    Some(10),
                ),
            ),
        ];
        for (s, t) in matches {
            let parse = LangParser::parse(Rule::ty, s)?.next().unwrap();
            let ty = handle_type(parse);
            assert_eq!(ty, *t);
        }
        return Ok(());
    }
}
