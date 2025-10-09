mod types;

use std::collections::HashMap;

use types::*;

#[derive(Debug)]
struct Model {
    name: String,
    endpoint: String,
    params: HashMap<String, Type>,
}

fn main() -> anyhow::Result<()> {
    return Ok(());
}
