# Generator

Generator is a Rust CLI that compiles a compact DSL describing HTTP APIs (struct types, tagged/untagged unions, and endpoints) into ready-to-wire code. Today it can emit a browser-friendly TypeScript fetch client and an experimental FastAPI skeleton for Python backends.

## Highlights

- **Single source of truth** – Describe your API once in `*.defs` files backed by the Pest grammar in `pest/lang.pest`.
- **Extensible generators** – Each target implements `generators::Generator`, so new outputs only need to worry about turning parsed types into strings.
- **Configurable TypeScript output** – Toggle literal-union materialisation and error-handling styles (`raise`, `result`, or tuple `pair`) straight from the CLI.
- **FastAPI starter** – Provide an app name and get per-endpoint stubs with annotated parameters (type translation work is still underway; see `todo.md`).

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

See `lang.md` for a longer walkthrough and `pest/lang.pest` for the authoritative grammar.

## Running the CLI

Provide one or more `.defs` files followed by a generator subcommand:

```bash
cargo run -p cli -- ./api.defs typescript [TS OPTIONS]
cargo run -p cli -- ./defs/users.defs ./defs/orders.defs python-fast-api <app> [FASTAPI OPTIONS]
```

### Common flags

- `-d, --destructive` – Allow overwriting files in `--path` (default skips files that already exist).
- `-v, --verbose` – Log extra progress while generating.
- `-S, --split` – Emit separate `types_*` and `endpoints_*` files; otherwise everything for a module lives in a single file.
- `-u, --united <name>` – Collapse all input definitions into one shared output file handle named `<name>` (pairs nicely with split to create `types_<name>` / `endpoint_<name>`).
- `--prefix <string>` / `--postfix <string>` – Add affixes to every filename to keep variants side-by-side.
- `-p, --path <dir>` – Destination directory for generated files (`./src/generated` by default).

### TypeScript options

- `-e, --error-handling <raise|result|pair>` – Choose whether helpers throw, return `Result`, or return `(data, err)` tuples.
- `-t, --type-enum <to_type|to_enum|to_algebraic>` – Control how literal unions materialise (TypeScript aliases, enums, or tagged unions).

### FastAPI options

- `<app_name>` (positional) – Module or symbol (e.g. `app` or `server.api`) wired into decorator calls.
- `-e, --enum-handling <to_type|to_union|to_enum_class>` – Decide how literal unions appear in generated Pydantic models.

## Output Layout

- **Decoupled (default):** Each `.defs` file produces its own `{prefix}{module}{postfix}.{ext}` bundle. Add `-S/--split` to emit `types_{module}` and `endpoints_{module}` instead.
- **United:** Supplying `-u/--united <name>` merges all parsed modules into a single output. With split enabled this becomes `types_<name>` / `endpoint_<name>`; otherwise it is `{prefix}<name>{postfix}.{ext}`.

The CLI only overwrites files when `--destructive` is set. Combine `--prefix`, `--postfix`, and `--path` to keep experimental output isolated from checked-in code.

## Repository Map

- `crates/cli/src/main.rs` – clap-based CLI surface that wires workspace modules into the binary.
- `crates/lib/src/parser/` – DSL AST, loader, and definition normalisation (`definitions.rs`, `types.rs`, `endpoint.rs`).
- `crates/lib/src/builder/` – Reusable code indentation/formatting helpers.
- `crates/lib/src/generators/` – TypeScript and FastAPI backends plus shared traits (`ts.rs`, `python.rs`, `ffi.rs`).
- `crates/lib/tests/` – Parser + generator regression tests with DSL fixtures (`unit.gdsl`).
- `crates/lib/pest/lang.pest` – Grammar that powers the DSL parser.
- `lang.md` – DSL walkthrough and example output.
- `todo.md` – Known gaps and follow-up tasks.
- `AGENTS.md` – Contributor guide covering coding style, commands, and review expectations.

## Developing Locally

This repo is a Cargo workspace with `crates/lib` providing the parser/generator core and `crates/cli` exposing it as a binary. Useful commands:

```bash
cargo fmt --all
cargo clippy --all-targets --all-features -D warnings
cargo test --workspace
cargo run --bin cli -- ./examples/api.defs typescript --path temp/
```

Point generator output at `temp/` or another disposable directory so local source files are not overwritten. When iterating on the grammar, focus runs with `cargo test -p lib parse_test`.

## Contributing

Read `AGENTS.md` for the contributor checklist (project layout, coding style, testing, and PR requirements). Summarise behaviour changes in PR descriptions, include any regenerated artifacts, and link the relevant entries from `todo.md` when closing outstanding work.
