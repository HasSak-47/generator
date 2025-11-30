# Generator

Generator is a Rust workspace that compiles a compact HTTP DSL describing structs, unions, and endpoints into ready-to-wire code. The workspace hosts the `generator` CLI crate (`crates/cli`) and the reusable `genlib` library crate (`crates/lib`), so you can drive code generation via `cargo run -p generator` or embed the parser/generators directly.

## Highlights

- **Single source of truth** – Model your API once in `*.gdsl` files; the Pest grammar lives in `crates/lib/pest/lang.pest` and the walkthrough in `lang.md`.
- **Extensible generators** – Every backend implements `genlib::generators::Generator`, making it straightforward to add more targets (including via the `ffi` shim).
- **Configurable TypeScript output** – Switch error-handling (`raise`, `result`, or tuple `pair`) and literal-union handling (`to_type`, `to_enum`, or `to_algebraic`) straight from the CLI.
- **FastAPI starter** – Provide an app/module name and get FastAPI route stubs plus pydantic models (translation helpers are tracked in `todo.md`).

## DSL at a Glance

The DSL models structs, unions, and endpoints. A small excerpt:

```dsl
type Product = {
    id: int,
    name: string,
    price: float,
}

type Status = {"pending" | "completed" | "cancelled"}

get_product(id: int) @get "/product/{id}" -> Product
create_sale(sale: Sale) @post "/sales" -> Sale
```

See `lang.md` for a longer walkthrough and `crates/lib/pest/lang.pest` for the authoritative grammar.

## Running the CLI

Provide one or more `.defs` files followed by a generator subcommand:

```bash
cargo run -p generator -- ./api.defs typescript [TS OPTIONS]
cargo run -p generator -- ./defs/users.defs ./defs/orders.defs python-fast-api <app_name> [FASTAPI OPTIONS]
```

All definition files are loaded before generation, so shared DSL modules stay visible across inputs. Run `cargo run -p generator -- --help` for the full flag list.

### Common flags

- `-d, --destructive` – Allow overwriting files in `--path` (default skips files that already exist).
- `-v, --verbose` – Log extra progress while generating.
- `-S, --split` – Emit separate `types_*` and `endpoints_*` files; otherwise each module is generated as a single bundle.
- `-u, --united <name>` – Collapse all input definitions into one shared output file handle named `<name>` (pairs nicely with split to create `types_<name>` / `endpoint_<name>`).
- `--prefix <string>` – Prefix every filename so variants can coexist.
- `--postfix <string>` – Postfix every filename to distinguish experimental runs.
- `-p, --path <dir>` – Destination directory for generated files (`./src/generated` by default). The directory must already exist for now (see `todo.md`).
- `--io` – Print generated code to stdout instead of writing files.

### TypeScript options

- `-e, --error-handling <raise|result|pair>` – Choose whether helpers throw, return `Result`, or return `(data, err)` tuples.
- `-t, --type-enum <to_type|to_enum|to_algebraic>` – Control how literal unions materialise (TypeScript aliases, enums, or tagged unions).

### FastAPI options

- `<app_name>` (positional) – Module or symbol (e.g. `app` or `server.api`) wired into decorator calls.
- `-e, --enum-handling <to_type|to_union|to_enum_class>` – Decide how literal unions appear in generated Pydantic models.

## Output Layout

- **Decoupled (default):** Each `.defs` file produces its own `{prefix}{module}{postfix}.{ext}` bundle. Add `-S/--split` to emit `types_{module}` and `endpoints_{module}` companions instead.
- **United:** Supplying `-u/--united <name>` merges all parsed modules into a single output. With split enabled this becomes `types_<name>` / `endpoint_<name>`; otherwise it is `{prefix}<name>{postfix}.{ext}`.
- **Stdout mode:** Pass `--io` to print generated files rather than writing to disk (handy for quick diffs).

The CLI only overwrites files when `--destructive` is set. Combine `--prefix`, `--postfix`, and `--path` to keep experimental output isolated from checked-in code.

## Repository Map

- `crates/cli/src/main.rs` – clap-based CLI surface that wires workspace modules into the binary.
- `crates/lib/src/parser/` – DSL AST, loader, and definition normalisation (`definitions.rs`, `types.rs`, `endpoint.rs`).
- `crates/lib/src/builder/` – Reusable code indentation/formatting helpers.
- `crates/lib/src/generators/` – TypeScript and FastAPI backends plus shared traits (`ts.rs`, `python.rs`, `ffi.rs`).
- `crates/lib/tests/` – Parser + generator regression tests with DSL fixtures (`unit.gdsl`).
- `crates/lib/pest/lang.pest` – Grammar that powers the DSL parser.
- `lang.md` – DSL walkthrough and example output.
- `AGENTS.md` – Contributor guide covering coding style, commands, and review expectations.

## Developing Locally

This repo is a Cargo workspace with `crates/lib` providing the parser/generator core and `crates/cli` exposing it as a binary. Useful commands:

```bash
cargo fmt --all
cargo clippy --all-targets --all-features -D warnings
cargo test --workspace
cargo run -p generator -- ./examples/api.defs typescript --path temp/
```

Point generator output at `temp/` or another disposable directory so local source files are not overwritten. When iterating on the grammar, focus runs with `cargo test -p lib parse_test`.

## Contributing

Read `AGENTS.md` for the contributor checklist (project layout, coding style, testing, and PR requirements). Summarise behaviour changes in PR descriptions, include any regenerated artifacts, and link the relevant entries from `todo.md` when closing outstanding work.
