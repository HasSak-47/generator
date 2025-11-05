#![allow(unused_imports)]

use crate::{builder::*, generators::*, parser::dsl::*};
use anyhow::Result;
use std::{env::current_dir, fs::File, io::Write};

#[test]
fn parse_test() -> Result<()> {
    let defs = Definitons::get_definitions("./unit.gdsl")?;
    let generator = Box::new(ts::TS::default());

    let mut code = Code::new_segment();

    code.add_child(generator.generate_model_header(&defs));
    code.add_child(generator.generate_endpoint_header(&defs));

    for (name, ty) in &defs.types {
        code.add_child(generator.handle_type(name.as_str(), &ty, &defs));
    }
    for (name, endpoint) in &defs.end_points {
        code.add_child(generator.handle_endpoint(name.as_str(), &endpoint, &defs));
    }

    let code = code.collapse_root("\t");

    let mut path = current_dir()?;

    path.push("generated");
    path.push("generated");
    path.set_extension("ts");

    let mut file = File::create(path)?;
    file.write_all(code.as_bytes())?;

    return Ok(());
}
