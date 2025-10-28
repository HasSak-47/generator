#![allow(unused_imports)]
use crate::{dsl::Definitons, ts::*};

#[test]
fn parse_test() -> anyhow::Result<()> {
    let _ = Definitons::get_definitions("unit.gdsl")?;

    return Ok(());
}
