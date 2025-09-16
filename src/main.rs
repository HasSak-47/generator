mod builder;
mod generators;
mod parser;
#[cfg(test)]
mod tests;

use crate::{builder::Code, parser::definitions::*};

use std::{fs::File, io::Write, path::PathBuf};

use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::generators::{python::FastApi, ts::TS};

#[derive(Parser)]
struct Cli {
    #[arg(default_value_os_t = {PathBuf::from("./definitions.defs")})]
    pub definitions: PathBuf,

    #[arg(short = 'S', long, default_value_t = false)]
    pub split: bool,

    #[arg(short = 'P', long)]
    pub prefix: Option<String>,

    #[arg(short, long, default_value_os_t = {PathBuf::from("./src/generated")})]
    pub path: PathBuf,

    #[command(subcommand)]
    pub generator: Generators,
}

#[derive(Subcommand, Clone)]
enum Generators {
    PythonFastApi(FastApi),
    Typescript(TS),
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let prefix = cli.prefix.unwrap_or(String::new());
    let defs = Definitons::get_definitions(cli.definitions)?;
    let (generator, extension): (Box<dyn Generator>, &str) = match cli.generator {
        Generators::Typescript(ts) => (Box::new(ts), "ts"),
        Generators::PythonFastApi(fastapi) => (Box::new(fastapi), "py"),
    };

    // let add_enums = |code: &mut Code| {
    //     let mut names: Vec<_> = defs.enums.keys().collect();
    //     names.sort();
    //     for name in names {
    //         let model = &defs.enums[name];
    //         let g = generator.handle_enum(name, model);
    //         if g.has_code() {
    //             code.add_child(g);
    //         }
    //     }
    // };

    // let add_models = |code: &mut Code| {
    //     let mut names: Vec<_> = defs.models.keys().collect();
    //     names.sort();
    //     for name in names {
    //         let model = &defs.models[name];
    //         let g = generator.handle_model(name, model, &defs);
    //         if g.has_code() {
    //             code.add_line(String::new());
    //             code.add_child(g);
    //         }
    //     }
    // };

    // let add_endpoints = |code: &mut Code| {
    //     let mut names: Vec<_> = defs.end_points.keys().collect();
    //     names.sort();
    //     for name in names {
    //         let endpoint = &defs.end_points[name];
    //         let g = generator.handle_endpoint(name, endpoint, &defs);
    //         if g.has_code() {
    //             code.add_line(String::new());
    //             code.add_child(g);
    //         }
    //     }
    // };

    // if cli.split {
    //     let mut endpoint_code = generator.generate_endpoint_header(&defs);
    //     let mut model_code = generator.generate_model_header(&defs);

    //     // add_enums(&mut model_code);
    //     add_models(&mut model_code);
    //     add_endpoints(&mut endpoint_code);

    //     let model_code = model_code.collapse_root("\t");
    //     let endpoint_code = endpoint_code.collapse_root("\t");

    //     let mut model_path = cli.path.clone();
    //     model_path.push(format!("{prefix}models"));
    //     model_path.set_extension(extension);

    //     let mut model_file = File::create(model_path)?;
    //     model_file.write_all(model_code.as_bytes())?;

    //     let mut endpoint_path = cli.path.clone();
    //     endpoint_path.push(format!("{prefix}endpoints"));
    //     endpoint_path.set_extension(extension);

    //     let mut endpoint_file = File::create(endpoint_path)?;
    //     endpoint_file.write_all(endpoint_code.as_bytes())?;
    // } else {
    //     let mut code = Code::new_segment();

    //     code.add_child(generator.generate_model_header(&defs));
    //     code.add_child(generator.generate_endpoint_header(&defs));

    //     // add_enums(&mut code);
    //     add_models(&mut code);
    //     add_endpoints(&mut code);

    //     let code = code.collapse_root("\t");
    //     let mut path = cli.path.clone();

    //     path.push(format!("{prefix}generted"));
    //     path.set_extension(extension);

    //     let mut file = File::create(path)?;
    //     file.write_all(code.as_bytes())?;
    // }

    return Ok(());
}
