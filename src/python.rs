use crate::{
    dsl::{Definitons, EndPoint, Generator, Model},
    types::{PrimitiveType, Repr, Type},
};

use clap::{Parser, ValueEnum};

#[derive(ValueEnum, Debug, Default, Clone, PartialEq)]
#[value(rename_all = "snake_case")]
pub enum EnumHandling {
    ToEnum,
    #[default]
    ToString,
    ToClass,
}

// NOTE: fast api handles datetime convertion so you can just kinda ignore it
#[derive(Parser, Clone)]
pub struct FastApi {
    pub app_name: String,
    #[arg(value_enum)]
    pub enum_handling: EnumHandling,
}

impl FastApi {
    fn handle_primitive(&self, p: &PrimitiveType) -> String {
        use PrimitiveType as PT;
        return match p {
            PT::Bool => "bool",
            PT::Integer(_) | PT::Unsigned(_) => "int",
            PT::Float(_) => "float",
            PT::String(_) => "str",
        }
        .to_string();
    }

    fn handle_repr(&self, r: &Repr) -> String {
        match r {
            Repr::Datetime => "dt.datetime",
        }
        .to_string()
    }

    fn handle_type(&self, defs: &Definitons, ty: &Type) -> String {
        return match ty {
            Type::Primitive(p) => self.handle_primitive(p),
            Type::Repr(r) => self.handle_repr(r),
            Type::Optional(o) => format!("Optional[{}]", self.handle_type(defs, &o.ty)),
            Type::Array(a) => format!("List[{}]", self.handle_type(defs, &a.ty)),
            Type::Into(i) => format!("{}", self.handle_type(defs, &i.from)),
            Type::Model(m) => format!("{m}",),
            Type::Enum(e) => match self.enum_handling {
                EnumHandling::ToClass => {
                    format!("{e}",)
                }
                EnumHandling::ToString => self.handle_type(defs, &Type::string(None)),
                EnumHandling::ToEnum => {
                    let mut poss = defs.enums.get(e).unwrap().params.iter();
                    let mut s = format!("{}", poss.next().unwrap());
                    for param in poss {
                        s += format!(" | {param}").as_str();
                    }

                    s
                }
            },
            Type::Undetermined(u) => panic!("Undetermined: {u:?} reached a FastApi generator",),
            Type::Null => format!("None",),
        };
    }
}

impl Generator for FastApi {
    fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> String {
        let mut code = format!("class {}(BaseModel):", name);
        for (name, ty) in &model.params {
            code += format!("\n\t{name}: {}", self.handle_type(defs, &ty)).as_str();
        }

        return code;
    }

    #[allow(unused_variables)]
    fn handle_enum(&self, name: &str, model: &crate::dsl::Enum) -> String {
        if let EnumHandling::ToString = self.enum_handling {
            return String::new();
        }
        todo!();
    }

    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> String {
        let mut code = format!("@{}.{}({})\n", self.app_name, endpoint.method, endpoint.url);
        code += format!("def {}(", name,).as_str();
        for (name, ty) in &endpoint.params {
            code += format!("{name}: {}, ", self.handle_type(defs, &ty)).as_str();
        }
        code += "):\n\t";

        code += format!("{}(", name,).as_str();
        for (name, _) in &endpoint.params {
            code += format!("{name}, ").as_str();
        }
        code += ")";

        return code;
    }
}
