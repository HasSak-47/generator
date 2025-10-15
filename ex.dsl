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
