use crate::{
    Definitons, Generator, handle_type,
    types::{PrimitiveType, Type},
};

struct FastApi {
    app_name: String,
    generate_enums: bool,
}

impl FastApi {
    fn new() -> Self {
        return Self {
            app_name: "app".to_string(),
            generate_enums: false,
        };
    }

    fn handle_primitive(&self, p: &PrimitiveType) -> String {
        use PrimitiveType as PT;
        return match p {
            PT::Integer(_) | PT::Unsigned(_) => "int",
            PT::Float(_) => "float",
            PT::String(_) => "str",
            PT::Null => "None",
        }
        .to_string();
    }
}

impl Generator for FastApi {
    fn handle_type(&self, defs: &Definitons, ty: &Type) -> String {
        return match ty {
            Type::Primitive(p) => self.handle_primitive(p),
            Type::Optional(o) => format!("Optional[{}]", self.handle_type(defs, &o.ty)),
            Type::Array(a) => format!("List[{}]", self.handle_type(defs, &a.ty)),
            _ => unreachable!("I just haven't implemented it :)"),
        };
    }

    fn handle_model_param(&self, name: String, ty: String) -> String {
        todo!()
    }

    fn handle_model(&self, name: String, params: Vec<String>) -> String {
        todo!()
    }
}
