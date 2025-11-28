# General

- Document a quickstart in `README.md` that mirrors the commands from `AGENTS.md` so new contributors have a consistent checklist.
- Add status badges (CI, crates.io, docs) once automation exists.

# Modules

## DSL

_features_:

- Hashmaps
- Support literal defaults for struct fields (e.g., `status: Status = "pending"`).
- Allow per-endpoint tags/metadata that can flow into generators (e.g., auth scopes).

## Cli

_qol_:

- when types and endpoints are split allow for different paths
- add `--dry-run` and `--stdout` switches to preview output without writing files
- surface a friendly error when `--path` points outside the workspace (protect against accidental overwrites)

## CodeBuilder

_qol_:

- avoid generating \n\n
- add option to use tabs or spaces
- expose a streaming writer so large files do not require the whole string in memory
- provide helper for wrapping code blocks with automatically managed braces

## Generators

_features_:

- extend the TypeScript target with a `result<T, E>` error-handling flavour that never throws
- emit FastAPI dependency stubs for auth/context parameters declared on endpoints

_qol_:

- share formatting helpers between TS and Python generators to keep naming and whitespace consistent

## Testing

- convert parser tests in `crates/lib/tests` to table-driven suites so new cases are easier to add
- add end-to-end tests that run `cargo run -p cli` against sample `.defs` files and snapshot the generated artifacts
