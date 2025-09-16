#![allow(unused_imports)]

use crate::{builder::*, generators::*, parser::definitions::*};
use anyhow::Result;
use std::{env::current_dir, fs::File, io::Write};

#[test]
fn parse_test() -> Result<()> {
    let mut defs = Definitons::new();
    defs.load_from_file("./unit.gdsl")?;
    defs.build_definitons();
    let generator = ts::TS::default();

    let code = defs.build_combined_module(&generator).collapse_root("\t");

    let mut path = current_dir()?;

    path.push("temp");
    path.push("generated");
    path.set_extension("ts");

    let mut file = File::create(path)?;
    file.write_all(code.as_bytes())?;

    return Ok(());
}
