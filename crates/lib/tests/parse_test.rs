#![allow(unused_imports)]

use anyhow::Result;
use genlib::{builder::*, generators::*, parser::definitions::*};
use std::{env::current_dir, fs::File, io::Write, path::PathBuf};

#[test]
fn parse_test() -> Result<()> {
    let mut defs = Definitons::new();
    let path = PathBuf::from("./tests/unit.gdsl");
    defs.load_from_file(path)?;
    defs.build_definitons();
    let generator = ts::TS::default();

    let code = defs
        .build_unified_joint_module(&generator)
        .collapse_root("\t");

    let mut path = current_dir()?;

    path.push("../../temp");
    path.push("generated");
    path.set_extension("ts");

    let mut file = File::create(path)?;
    file.write_all(code.as_bytes())?;

    return Ok(());
}
