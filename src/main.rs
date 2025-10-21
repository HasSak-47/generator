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
    let generator: Box<dyn Generator> = match cli.generator {
        Generators::Typescript(ts) => Box::new(ts),
        Generators::PythonFastApi(fastapi) => Box::new(fastapi),
    };

    println!("{}", generator.generate_header());

    for (name, e) in &defs.enums {
        println!("{}", generator.handle_enum(name, e))
    }

    for (name, model) in &defs.models {
        println!("{}", generator.handle_model(name, model, &defs))
    }

    for (name, end_points) in &defs.end_points {
        println!("{}", generator.handle_endpoint(name, end_points, &defs))
    }

    return Ok(());
}
