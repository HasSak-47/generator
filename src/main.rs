mod dsl;
mod python;
mod ts;
mod types;

use dsl::{Definitons, Generator};

use std::{fs::File, io::Write, path::PathBuf};

use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::{python::FastApi, ts::TS};

#[derive(Parser)]
struct Cli {
    #[arg(default_value_os_t = {PathBuf::from("./definitions.defs")})]
    pub definitions: PathBuf,

    #[arg(short, long, default_value_t = false)]
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

    if cli.split {
        let mut endpoint_code = generator.generate_endpoint_header();
        let mut model_code = generator.generate_model_header();

        for (name, e) in &defs.enums {
            let g = generator.handle_enum(name, e);
            if g.len() > 0 {
                model_code += format!("{g}\n").as_str();
            }
        }

        for (name, model) in &defs.models {
            let g = generator.handle_model(name, model, &defs);
            if g.len() > 0 {
                model_code += format!("{g}\n").as_str();
            }
        }

        for (name, end_points) in &defs.end_points {
            let g = generator.handle_endpoint(name, end_points, &defs);
            if g.len() > 0 {
                endpoint_code += format!("{g}\n").as_str();
            }
        }

        let mut model_path = cli.path.clone();
        model_path.push(format!("{prefix}models"));
        model_path.set_extension(extension);
        let mut model_file = File::create(model_path)?;
        model_file.write_all(model_code.as_bytes())?;

        let mut endpoint_path = cli.path.clone();
        endpoint_path.push(format!("{prefix}endpoints"));
        endpoint_path.set_extension(extension);
        let mut endpoint_file = File::create(endpoint_path)?;
        endpoint_file.write_all(endpoint_code.as_bytes())?;
    } else {
        let mut code = generator.generate_endpoint_header();
        code += generator.generate_model_header().as_str();

        for (name, e) in &defs.enums {
            let g = generator.handle_enum(name, e);
            if g.len() > 0 {
                code += format!("{g}\n").as_str();
            }
        }

        for (name, model) in &defs.models {
            let g = generator.handle_model(name, model, &defs);
            if g.len() > 0 {
                code += format!("{g}\n").as_str();
            }
        }

        for (name, end_points) in &defs.end_points {
            let g = generator.handle_endpoint(name, end_points, &defs);
            if g.len() > 0 {
                code += format!("{g}\n").as_str();
            }
        }

        let mut path = cli.path.clone();
        path.push(format!("{prefix}generted"));
        path.set_extension(extension);
        let mut file = File::create(path)?;
        file.write_all(code.as_bytes())?;
    }

    return Ok(());
}
