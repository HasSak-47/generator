```
model Product {
    id: int,
    name: string,
}

model SaleProduct {
    product: string,
    amount: int,
}

model Sale {
    id: int,
    date: dt.datetime
    products: SaleProduct[],
}

product get_product(id: int) @ get "/product/{id}"
sale get_sale(after: string as datetime, before: string as datetime) @ get "/sales"

```

```python
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
def __get_product(id: int) -> product:
    return get_product(id)
    ...
```


```typescript
```
