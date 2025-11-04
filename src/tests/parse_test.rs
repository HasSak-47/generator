#![allow(unused_imports)]

use crate::{builder::*, dsl::*, ts};
use anyhow::Result;
use std::{env::current_dir, fs::File, io::Write};

#[test]
fn parse_test() -> Result<()> {
    let defs = Definitons::get_definitions("./unit.gdsl")?;
    let generator = Box::new(ts::TS::default());

    let add_enums = |code: &mut Code| {
        let mut names: Vec<_> = defs.enums.keys().collect();
        names.sort();
        for name in names {
            let model = &defs.enums[name];
            let g = generator.handle_enum(name, model);
            if g.has_code() {
                code.add_child(g);
            }
        }
    };

    let add_models = |code: &mut Code| {
        let mut names: Vec<_> = defs.models.keys().collect();
        names.sort();
        for name in names {
            let model = &defs.models[name];
            let g = generator.handle_model(name, model, &defs);
            if g.has_code() {
                code.add_line(String::new());
                code.add_child(g);
            }
        }
    };

    let add_endpoints = |code: &mut Code| {
        let mut names: Vec<_> = defs.end_points.keys().collect();
        names.sort();
        for name in names {
            let endpoint = &defs.end_points[name];
            let g = generator.handle_endpoint(name, endpoint, &defs);
            if g.has_code() {
                code.add_line(String::new());
                code.add_child(g);
            }
        }
    };

    let mut code = Code::new_segment();

    code.add_child(generator.generate_model_header(&defs));
    code.add_child(generator.generate_endpoint_header(&defs));

    add_enums(&mut code);
    add_models(&mut code);
    add_endpoints(&mut code);

    let code = code.collapse_root("\t");

    let mut path = current_dir()?;

    path.push("generated");
    path.push("generated");
    path.set_extension("ts");

    let mut file = File::create(path)?;
    file.write_all(code.as_bytes())?;

    return Ok(());
}
