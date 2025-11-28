# DSL Reference

This document sketches the language supported by the generator and shows the shape of the emitted code for the bundled targets.

## Example API Description

```dsl
# Data models
type Product = {
    id: int,
    name: string,
    price: float,
}

type SaleProduct = {
    product: Product,
    amount: int,
}

type Sale = {
    id: int,
    date: string as datetime,
    products: SaleProduct[],
}

type Status = {"pending" | "completed" | "cancelled"}

type SaleSummary = {
    total_sales: int,
    total_amount: float,
    status: Status,
}

# Endpoints
# Path binding: id appears in URL
get_product(id: int) @get "/product/{id}" -> Product

# Query binding: after and before not in path, not models
get_sales(after: string as datetime, before: string as datetime?) @get "/sales" -> Sale[]

# Body binding: one struct parameter, not in path
create_sale(sale: Sale) @post "/sales" -> Sale

# Mixed: path + body
update_product(id: int, product: Product) @put "/product/{id}" -> Product

# Query with union field
get_summary(status: Status?) @get "/sales/summary" -> SaleSummary
```

**Key concepts**

- Primitive fields map directly to language primitives (`int`, `string`, `float`, `bool`).
- Tagged and untagged unions use the `{ ... | ... }` syntax (string literals shown above).
- `Foo as datetime` relies on the representation helpers in the generator (`Date` in TS, `datetime` in Python).
- `Type?` results in optionals (`T | null` or `Optional[T]`).
- Arrays use `Type[]`.
- Endpoints declare positional parameters; bindings are inferred from path segments vs body payloads.
- Verbs are declared via `@get`, `@post`, `@put`, `@patch`, or `@delete` followed by a quoted route string.

### Binding rules

1. **Path parameters** – Any `{foo}` segment in the path binds to the parameter of the same name.
2. **Body payloads** – Struct parameters not used in the path become the request body and are serialized using the generator-specific builders.
3. **Query parameters** – Remaining scalar fields become query strings. Optional fields (`Type?`) translate to nullable query args.
4. **Return values** – The `-> Type` arrow drives both the client response type and FastAPI `response_model`.

Keep DSL definitions small and composable. For larger APIs, split `*.defs` files by domain (e.g., `orders.defs`, `inventory.defs`) and load each with separate CLI invocations or concatenate them before generation.

## Generated Code Sketches

Exact output varies based on CLI flags, but the snippets below show the intent.

### FastAPI target

```python
import datetime as dt
from typing import List, Optional
from fastapi import FastAPI
from pydantic import BaseModel

app = FastAPI()

class Product(BaseModel):
    id: int
    name: str
    price: float


@app.get("/product/{id}")
def get_product_endpoint(id: int) -> Product:
    return get_product(id)


@app.get("/sales")
def get_sales_endpoint(after: dt.datetime, before: Optional[dt.datetime]) -> List[Sale]:
    return get_sales(after, before)
```

### TypeScript target

```typescript
export type Product = {
  id: number;
  name: string;
  price: number;
};

export async function get_product(id: number): Promise<Product> {
  const response = await fetch(`/product/${id}`);
  if (!response.ok) {
    throw await response.json();
  }
  const payload = await response.json();
  return {
    id: payload.id,
    name: payload.name,
    price: payload.price,
  };
}
```

Use `cargo run -- <defs> typescript --help` (or the `python-fast-api` variant) to inspect the switches that tweak union handling and error-handling behaviour at generation time. When editing the grammar or DSL samples, run `cargo test -p lib parse_test` to validate parser output before sharing regenerated TypeScript or FastAPI code.
