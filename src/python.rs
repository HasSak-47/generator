use crate::{
    builder::Code,
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
    #[arg(short, long, value_enum, default_value_t = EnumHandling::ToString)]
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

    /// Map DSL types to the string annotations required by FastAPI and pydantic.
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
    fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> Code {
        let mut code = Code::new();
        let class_def = code.add_child(format!("class {}(BaseModel):", name));
        for (name, ty) in &model.params {
            class_def.add_child(format!("{name}: {}", self.handle_type(defs, &ty)));
        }

        return code;
    }

    #[allow(unused_variables)]
    fn handle_enum(&self, name: &str, model: &crate::dsl::Enum) -> Code {
        if let EnumHandling::ToString = self.enum_handling {
            return Code::new();
        }
        todo!();
    }

    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code {
        let mut code = Code::new();
        code.add_child(format!(
            "@{}.{}('{}')",
            self.app_name, endpoint.method, endpoint.url
        ));
        let function = code.add_child(format!("def endpoint_{}(", name,));
        for (name, ty) in &endpoint.params {
            function.code += format!("{name}: {}, ", self.handle_type(defs, &ty)).as_str();
        }
        function.code += "):";

        let call = function.add_child(format!("{}(", name,));
        for (name, _) in &endpoint.params {
            call.code += format!("{name}, ").as_str();
        }
        call.code += ")";

        return code;
    }
}
