# Repository Guidelines

## Project Structure & Module Organization

This workspace has two crates: `crates/lib` hosts the parser, builder, and generators, while `crates/cli` wires those modules into the binary. The lib crate keeps the Pest grammar under `pest/lang.pest`, parser helpers in `src/parser/`, generators in `src/generators/`, and indentation helpers in `src/builder/`. DSL examples and fixtures live under `crates/lib/tests` alongside `lang.md` and `todo.md`; add new samples there for discoverability.

## Build, Test, and Development Commands

- `cargo fmt --all` – Runs `rustfmt` across every crate; required before opening a PR.
- `cargo clippy --all-targets --all-features -D warnings` – Lints both crates at the same strictness we expect in CI.
- `cargo test --workspace` – Executes parser unit tests and CLI smoke tests; use `-p lib` when iterating on grammar work.
- `cargo run --bin cli -- <defs> typescript|python-fast-api …` – Manually exercise generators; point `--path` to `temp/` when inspecting output files.

## Coding Style & Naming Conventions

Rust defaults apply: four-space indentation, `snake_case` for modules/functions, and `UpperCamelCase` for public types and enums. Prefer the builder helpers instead of manual string concatenation when emitting code, and favor explicit `use` paths over globs. Run `cargo fmt` before committing.

## Testing Guidelines

Unit tests belong in `crates/lib/tests` or alongside modules under `src/*/`. Mirror the existing `parse_*` naming for parser tests and keep DSL fixtures as `.gdsl` files referenced via `include_str!`. When altering generators, add smoke tests that assert on emitted strings or run `cargo run` against a throwaway `.defs` file and commit representative output. Target new parser branches with tests so the grammar stays stable.

## Commit & Pull Request Guidelines

History mixes plain summaries (“added tests”) with typed prefixes (“chore:added notes”), so standardize on `<type>: imperative summary` (`feat: add FastAPI enums`). Reference `todo.md` items or issues, describe flag/output changes, and attach sample generator output when practical. PRs should list the commands executed and mention whether any generated files in `temp/` should be reviewed.

## Security & File Emission Notes

## Commit & Pull Request Guidelines

History mixes plain summaries (“added tests”) with typed prefixes (“chore:added notes”), so standardize on `<type>: imperative summary` (`feat: add FastAPI enums`). Reference `todo.md` items or issues, describe flag/output changes, and attach sample generator output when practical. PRs should list the commands executed and mention whether any generated files in `temp/` should be reviewed.

## Security & File Emission Notes

The CLI overwrites any directory passed via `--path`. Use disposable directories (such as `temp/`) when reviewing generator changes and avoid pointing the tool at real client code. Validate new CLI flags for path traversal or unbounded writes, and only touch `cbindgen.toml` when coordinating with downstream consumers.
