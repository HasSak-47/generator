//! Command-line entry point that wires CLI flags to the concrete code generators.


use genlib::parser::definitions::*;

use std::{fs::File, io::Write, path::PathBuf};

use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::generators::{python::FastApi, ts::TS};

/// CLI surface exposed by `cargo run -- ...`.
#[derive(Parser)]
struct Cli {
    pub definitions: Vec<PathBuf>,

    /// Split output into model and endpoint files instead of a single bundle
    #[arg(short = 'S', long, default_value_t = false)]
    pub split: bool,

    /// Unite all the models and enpoints into a file handle
    #[arg(short, long)]
    pub united: Option<String>,

    /// Prefix added to every generated filename (helps when mixing variants in the same folder).
    #[arg(long, default_value_t = String::new())]
    pub prefix: String,

    /// Postfix added to every generated filename (helps when mixing variants in the same folder).
    #[arg(long, default_value_t = String::new())]
    pub postfix: String,

    /// Destination directory for generated files.
    #[arg(short, long, default_value_os_t = {PathBuf::from("./src/generated")})]
    pub path: PathBuf,

    #[command(subcommand)]
    pub generator: Generators,
}

/// Select which target backend should render the DSL.
#[derive(Subcommand, Clone)]
enum Generators {
    PythonFastApi(FastApi),
    Typescript(TS),
}

fn main() -> Result<()> {
    // Parse CLI args, load the DSL, and hand the parsed AST to the requested generator.
    let cli = Cli::parse();
    let mut defs = Definitons::new();
    for def in cli.definitions {
        defs.load_from_file(def)?;
    }

    defs.build_definitons();

    let (generator, extension): (Box<dyn Generator>, &str) = match cli.generator {
        Generators::Typescript(ts) => (Box::new(ts), "ts"),
        Generators::PythonFastApi(fastapi) => (Box::new(fastapi), "py"),
    };

    let prefix = cli.prefix;
    let postfix = cli.postfix;
    if let Some(name) = cli.united {
        if cli.split {
            let endpoint_code = defs
                .build_unified_endpoint_module(&*generator)
                .collapse_root("\t");
            let type_code = defs
                .build_unified_type_module(&*generator)
                .collapse_root("\t");

            let mut type_path = cli.path.clone();
            type_path.push(format!("{prefix}_types_{name}_{postfix}"));
            type_path.set_extension(extension);

            let mut type_file = File::create(type_path)?;
            type_file.write_all(type_code.as_bytes())?;

            let mut endpoint_path = cli.path.clone();
            endpoint_path.push(format!("{prefix}_endpoint_{name}_{postfix}"));
            endpoint_path.set_extension(extension);

            let mut endpoint_file = File::create(endpoint_path)?;
            endpoint_file.write_all(endpoint_code.as_bytes())?;
        } else {
            let code = defs
                .build_unified_joint_module(&*generator)
                .collapse_root("\t");
            let mut path = cli.path.clone();

            path.push(format!("{prefix}_{name}_{postfix}"));
            path.set_extension(extension);

            let mut file = File::create(path)?;
            file.write_all(code.as_bytes())?;
        }
    } else {
        if cli.split {
            for (name, type_code) in defs.build_decoupled_type_module(&*generator) {
                let type_code = type_code.collapse_root("\t");
                let mut type_path = cli.path.clone();
                type_path.push(format!("{prefix}_types_{name}_{postfix}"));
                type_path.set_extension(extension);

                let mut type_file = File::create(type_path)?;
                type_file.write_all(type_code.as_bytes())?;
            }

            for (name, endpoint_code) in defs.build_decoupled_endpoint_module(&*generator) {
                let endpoint_code = endpoint_code.collapse_root("\t");
                let mut endpoint_path = cli.path.clone();
                endpoint_path.push(format!("{prefix}_endpoints_{name}_{postfix}"));
                endpoint_path.set_extension(extension);

                let mut endpoint_file = File::create(endpoint_path)?;
                endpoint_file.write_all(endpoint_code.as_bytes())?;
            }
        } else {
            for (name, code) in defs.build_decoupled_joint_module(&*generator) {
                let code = code.collapse_root("\t");
                let mut path = cli.path.clone();
                path.push(format!("{prefix}_endpoints_{name}_{postfix}"));
                path.set_extension(extension);

                let mut file = File::create(path)?;
                file.write_all(code.as_bytes())?;
            }
        }
    }

    return Ok(());
}
