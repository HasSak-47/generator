```dsl
# Data models
model Product {
    id: int,
    name: string,
    price: float,
}

model SaleProduct {
    product: Product,
    amount: int,
}

model Sale {
    id: int,
    date: string as datetime,
    products: SaleProduct[],
}

enum Status { "pending", "completed", "cancelled" }

model SaleSummary {
    total_sales: int,
    total_amount: float,
    status: Status,
}

# Endpoints
# Path binding: id appears in URL
get_product(id: int) @get "/product/{id}" -> Product

# Query binding: after and before not in path, not models
get_sales(after: string as datetime, before: string as datetime?) @get "/sales" -> Sale[]

# Body binding: one model parameter, not in path
create_sale(sale: Sale) @post "/sales" -> Sale

# Mixed: path + body
update_product(id: int, product: Product) @put "/product/{id}" -> Product

# Query with enum
get_summary(status: Status?) @get "/sales/summary" -> SaleSummary

```

```python
# python backend
class Product(BaseModel):
    id: int
    name: str

class ProductSale(BaseModel):
    product: string
    amount: int

class Sale(BaseModel):
    id: int
    date: datetime.datetime
    products: List[ProductSale]

@app.get("/product/{id}")
def __get_product(id: int) -> product:
    return get_product(id)


@app.get("/sales")
def __get_sales(after: dt.datetime, before: dt.datetime) -> product:
    return get_product(id)
```

```typescript
// ts react front end
type Product = {
  id: number;
  name: string;
};

type ProductSale = {
  product: string;
  amount: number;
};

type Sale = {
  id: number;
  date: Date;
  products: Array<ProductSale>;
};

export async function get_product(id: number): Product {
  let r = await fetch(`/product/${id}`);
  let j = await r.json();
  return {
    id: j.id,
    name: j.name,
  };
}

export async function get_sales(after: Date, before: Date | null): Product {
  let r = await fetch(`/product/${id}`);
  let j = await r.json();
  return {
    id: j.id,
    date: new Date(j.date),
    products: j.name,
    products: j.products,
  };
}
```
