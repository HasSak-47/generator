#![allow(unused_imports)]

use crate::{builder::*, generators::*, parser::definitions::*};
use anyhow::Result;
use std::{env::current_dir, fs::File, io::Write};

#[test]
fn parse_test() -> Result<()> {
    let defs = Definitons::load_from_path("./unit.gdsl")?;
    let generator = ts::TS::default();

    let code = defs.build_combined_module(&generator).collapse_root("\t");

    let mut path = current_dir()?;

    path.push("generated");
    path.push("generated");
    path.set_extension("ts");

    let mut file = File::create(path)?;
    file.write_all(code.as_bytes())?;

    return Ok(());
}
