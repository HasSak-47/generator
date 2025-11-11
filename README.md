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

The CLI always reads a definitions file (defaults to `./definitions.defs`) and requires a generator subcommand:

```bash
cargo run -- <path/to/api.defs> typescript [TS OPTIONS]
cargo run -- <path/to/api.defs> python-fast-api <app_name> [FASTAPI OPTIONS]
```

### Global flags

- `--split / -S` – Emit two files (`models` and `endpoints`). Otherwise a single `{prefix}generated.{ext}` file is written.
- `--prefix / -P <string>` – Prepends a string to every emitted filename.
- `--path / -p <dir>` – Directory that receives the generated files (`./src/generated` by default). The CLI currently always writes files; piping to stdout is on the roadmap (`todo.md`).

### TypeScript options

- `--error-handling <raise|result|pair>` – Control how network errors are represented.
- `--type-enum <to_type|to_enum|to_string|to_algebraic>` – Controls how string unions (formerly enums) are materialised. Flag name is legacy but still wired to the same behaviour.

### FastAPI options

- `<app_name>` (positional) – Module path (e.g. `app` or `server.api`) used in the `@{app}.<method>` decorators.
- `--enum-handling <to_string|to_enum|to_class>` – Legacy flag name for union handling; determines whether literal unions stay as strings or become generated helpers (support for class/enum emission is tracking in `todo.md`).

## Output Layout

- **Single file (default):** `<path>/<prefix>generated.ts|py`
- **Split mode:** `<path>/<prefix>models.ts|py` and `<path>/<prefix>endpoints.ts|py`

The CLI will create or overwrite these files. Use `--prefix` together with per-branch directories if you need to keep multiple variants around.

## Repository Map

- `src/main.rs` – CLI entry point and shared flags.
- `src/parser/` – DSL AST + loader that normalises definitions.
- `src/builder/` – Small helper for assembling indented code segments.
- `src/generators/` – TypeScript and FastAPI emitters (`ts.rs`, `python.rs`).
- `src/tests/` – Parser/unit tests (temporary-file refactor noted in `todo.md`).
- `lang.md` – DSL reference + sample output.
- `todo.md` – Known gaps and follow-up tasks.
