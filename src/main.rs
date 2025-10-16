mod dsl;
mod types;

use std::{
    collections::{HashMap, HashSet},
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

#[derive(Debug, Default)]
struct EndPoint {
    params: Vec<(String, Type)>,
    method: EndPointMethod,
    url: String,
    return_type: Type,
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

fn handle_type<'a>(p: Pair<'a, Rule>) -> Type {
    assert!(p.as_rule() == Rule::ty);
    // WARN: I already had type parsing code
    // and I didn't want to waste it.
    // it is probably hot garbage
    if let Ok(t) = Type::parse(p.as_str()) {
        return t;
    } else {
        return Type::Undetermined(p.as_str().to_string());
    }
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
                if let Type::Undetermined(u) = param {
                    if model_names.iter().find(|n| **n == *u).is_some() {
                        *param = Type::Model(u.clone());
                        continue;
                    }

                    if enum_names.iter().find(|n| **n == *u).is_some() {
                        *param = Type::Model(u.clone());
                        continue;
                    }
                }
            }
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

                    end_point.url = iter.next().unwrap().to_string();
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
    fn handle_type(&self, defs: &Definitons, ty: &Type) -> String;
    fn handle_param(&self, name: String, ty: String) -> String;
}

fn main() -> Result<()> {
    let defs = Definitons::get_definitions("ex.dsl")?;
    return Ok(());
}
