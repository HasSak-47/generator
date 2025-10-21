mod dsl;
mod python;
mod ts;
mod types;

use anyhow::Result;

use dsl::{Definitons, Generator};

fn main() -> Result<()> {
    let defs = Definitons::get_definitions("ex.dsl")?;
    let generator = ts::React::new();
    for (name, model) in &defs.models {
        println!("{}\n", generator.handle_model(name, model, &defs))
    }
    for (name, endpoint) in &defs.end_points {
        println!("{}\n", generator.handle_endpoint(name, endpoint, &defs))
    }
    return Ok(());
}
