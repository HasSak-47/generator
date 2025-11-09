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
    let defs = Definitons::load_from_path(cli.definitions)?;
    let (generator, extension): (Box<dyn Generator>, &str) = match cli.generator {
        Generators::Typescript(ts) => (Box::new(ts), "ts"),
        Generators::PythonFastApi(fastapi) => (Box::new(fastapi), "py"),
    };

    if cli.split {
        let endpoint_code = defs.build_endpoint_module(&*generator).collapse_root("\t");
        let type_code = defs.build_type_module(&*generator).collapse_root("\t");

        let mut type_path = cli.path.clone();
        type_path.push(format!("{prefix}models"));
        type_path.set_extension(extension);

        let mut type_file = File::create(type_path)?;
        type_file.write_all(type_code.as_bytes())?;

        let mut endpoint_path = cli.path.clone();
        endpoint_path.push(format!("{prefix}endpoints"));
        endpoint_path.set_extension(extension);

        let mut endpoint_file = File::create(endpoint_path)?;
        endpoint_file.write_all(endpoint_code.as_bytes())?;
    } else {
        let code = defs.build_combined_module(&*generator).collapse_root("\t");
        let mut path = cli.path.clone();

        path.push(format!("{prefix}generted"));
        path.set_extension(extension);

        let mut file = File::create(path)?;
        file.write_all(code.as_bytes())?;
    }

    return Ok(());
}
