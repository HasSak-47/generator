# Generator

Generator is a Rust CLI that reads a small DSL describing API models, enums, and HTTP endpoints, then produces scaffolding code for different targets. The current targets are a TypeScript fetch client and a Python FastAPI handler stub.

## How It Works

- Definitions are written in the `.defs` DSL parsed by Pest (`pest/lang.pest`).
- `src/dsl.rs` loads the DSL, builds in-memory models, enums, and endpoints, and normalises types.
- Each target implements `dsl::Generator` to emit code for its platform (`src/ts.rs`, `src/python.rs`).

## Usage

```bash
cargo run -- --help
cargo run -- ex.defs typescript
cargo run -- ex.defs python-fast-api app to_string
```

- The CLI reads the definitions file (defaults to `./definitions.defs`).
- The generator outputs code to stdout; redirect to a file if needed.

## Getting Started

1. Install Rust and Cargo from <https://rustup.rs/>.
2. Run `cargo test` to verify the parser and helpers.
3. Adjust or create `.defs` files under project root to describe your API.

## Project Structure

- `src/` generator entry point and target implementations.
- `pest/` grammar and macro files for the DSL parser.
