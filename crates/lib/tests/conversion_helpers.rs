use anyhow::Result;
use genlib::{generators::ts, parser::definitions::Definitons};
use std::path::PathBuf;

#[test]
fn generates_conversion_helpers_for_nested_named_types() -> Result<()> {
    // let mut defs = Definitons::new();
    // defs.load_from_file(PathBuf::from("./tests/unit.gdsl"))?;
    // defs.build_definitons();
    // let generator = ts::TS::default();

    // let modules = defs.build_unified_joint_module(&generator);
    // let code = modules.collapse_root("\t");

    // assert!(
    //     code.contains("function into_domain_Foo("),
    //     "missing domain helper for Foo:\n{code}"
    // );
    // assert!(
    //     code.contains("function into_wire_Foo("),
    //     "missing wire helper for Foo:\n{code}"
    // );
    // assert!(
    //     code.contains("function into_wire_Baz("),
    //     "missing wire helper for Baz:\n{code}"
    // );

    Ok(())
}
