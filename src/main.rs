mod dsl;
mod python;
mod ts;
mod types;

use dsl::{Definitons, Generator};

use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::{python::FastApi, ts::TS};

#[derive(Parser)]
struct Cli {
    #[arg(default_value_os_t = {PathBuf::from("./definitions.defs")})]
    pub definitions: PathBuf,

    #[arg(short, long, default_value_t = false)]
    pub split: bool,

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
    let defs = Definitons::get_definitions(cli.definitions)?;
    let (generator, extension): (Box<dyn Generator>, &str) = match cli.generator {
        Generators::Typescript(ts) => (Box::new(ts), "ts"),
        Generators::PythonFastApi(fastapi) => (Box::new(fastapi), "py"),
    };

    println!("{}\n", generator.generate_endpoint_header());
    println!("{}\n", generator.generate_model_header());

    for (name, e) in &defs.enums {
        println!("{}\n", generator.handle_enum(name, e))
    }

    for (name, model) in &defs.models {
        println!("{}\n", generator.handle_model(name, model, &defs))
    }

    for (name, end_points) in &defs.end_points {
        println!("{}\n", generator.handle_endpoint(name, end_points, &defs))
    }

    return Ok(());
}
