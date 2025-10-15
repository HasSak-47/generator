mod dsl;
mod types;

use std::{collections::HashMap, fs::File, io::Read, path::Path};

use anyhow::{Result, anyhow};
use pest::{Parser, iterators::Pair};
use pest_derive::Parser;
use types::*;

#[derive(Debug, Default)]
struct Model {
    name: String,
    params: Vec<(String, Type)>,
}

#[derive(Debug, Default)]
struct Enum {
    name: String,
    params: Vec<String>,
}

#[derive(Debug)]
enum EndPointMethod {
    GET,
    POST,
    PUT,
}

#[derive(Debug)]
struct EndPoint {
    name: String,
    params: Vec<(String, Type)>,
    method: EndPointMethod,
    url: String,
    return_type: Option<Type>,
}

#[derive(Debug)]
struct Definitons {}

#[derive(Debug, Parser)]
#[grammar = "pest/lang.pest"]
struct LangParser {}

fn handle_type<'a>(p: Pair<'a, Rule>) -> Type {
    assert!(p.as_rule() == Rule::ty);
    // WARN: I already had parsing code and didn't want to waste it
    // it is probably hot garbage
    if let Ok(t) = Type::parse(p.as_str()) {
        return t;
    }

    unreachable!("not a type!")
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

        let mut models = HashMap::<String, Model>::new();
        let mut enums = HashMap::<String, Enum>::new();
        let mut enums = HashMap::<String, Enum>::new();

        for inner in p.into_inner() {
            match inner.as_rule() {
                Rule::model => {
                    let mut model = Model::default();
                    for pair in inner.into_inner() {
                        match pair.as_rule() {
                            Rule::name => model.name = pair.as_str().to_string(),
                            Rule::member_field => model.params.push(handle_member_field(pair)),
                            _ => unreachable!("idk how you got here??"),
                        }
                    }
                    models.insert(model.name.clone(), model);
                }
                Rule::enums => {
                    let mut r#enum = Enum::default();
                    let mut iter = inner.into_inner();
                    let name = iter.next().unwrap();
                    assert!(name.as_rule() == Rule::name);
                    r#enum.name = name.to_string();

                    for inner in iter {
                        assert!(inner.as_rule() == Rule::string);
                        let s = inner.as_str().to_string();
                        r#enum.params.push(s);
                    }
                    enums.insert(r#enum.name.clone(), r#enum);
                }
                Rule::end_point => {
                    let inner = inner.into_inner();
                }
                Rule::COMMENT => {}
                Rule::EOI => {}
                _ => unreachable!("idk how you got here??"),
            }
        }

        todo!("finish this lmao");
    }
}

fn main() -> Result<()> {
    Definitons::get_definitions("ex.dsl")?;
    return Ok(());
}
