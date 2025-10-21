use std::fmt::format;

use crate::{
    Definitons, EndPoint, Generator, Model, handle_type,
    types::{PrimitiveType, Repr, Type},
};

pub struct React {}

impl React {
    pub fn new() -> Self {
        return Self {};
    }

    fn handle_primitive(&self, p: &PrimitiveType) -> String {
        use PrimitiveType as PT;
        return match p {
            PT::Integer(_) | PT::Unsigned(_) | PT::Float(_) => "number",
            PT::String(_) => "str",
            PT::Null => "None",
        }
        .to_string();
    }

    fn handle_repr(&self, r: &Repr) -> String {
        return match r {
            Repr::Datetime => "Date",
        }
        .to_string();
    }

    fn handle_type_signature(&self, defs: &Definitons, ty: &Type) -> String {
        return match ty {
            Type::Primitive(p) => self.handle_primitive(p),
            Type::Optional(o) => format!("{} | null", self.handle_type_signature(defs, &o.ty)),
            Type::Array(a) => format!("{}[]", self.handle_type_signature(defs, &a.ty)),
            Type::Into(i) => format!("{}", self.handle_repr(&i.into)),
            Type::Model(m) => format!("{m}",),
            Type::Enum(e) => {
                let mut poss = defs.enums.get(e).unwrap().params.iter();
                let mut s = format!("{}", poss.next().unwrap());
                for param in poss {
                    s += format!(" | {param}").as_str();
                }

                s
            }
            Type::Undetermined(u) => panic!("Undetermined: {u:?} reached a React generator",),
            Type::Null => format!("null",),
        };
    }
}

impl Generator for React {
    fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> String {
        let mut code = format!("type {} = {{", name);
        for (name, ty) in &model.params {
            code += format!("\n\t{name}: {};", self.handle_type_signature(defs, &ty)).as_str();
        }
        code += "\n};";

        return code;
    }

    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> String {
        let mut code = format!("async function {}(", name);
        for (name, ty) in &endpoint.params {
            code += format!("{name}: {},", self.handle_type_signature(defs, &ty)).as_str();
        }
        code += "){\n";
        code += format!("\tlet url = `{}`;", endpoint.url).as_str();

        return code;
    }
}
