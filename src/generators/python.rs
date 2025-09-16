use crate::{
    builder::Code,
    parser::{definitions::*, endpoint::*, types::*},
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
            Type::Named(m) => format!("{m}",),
            Type::Union(e) => {
                todo!()
                // match self.enum_handling {
                // EnumHandling::ToClass => {
                //     format!("{e}",)
                // }
                // EnumHandling::ToString => self.handle_type(defs, &Type::string(None)),
                // EnumHandling::ToEnum => {
                //     let mut poss = defs.enums.get(e).unwrap().params.iter();
                //     let mut s = format!("{}", poss.next().unwrap());
                //     for param in poss {
                //         s += format!(" | {param}").as_str();
                //     }

                //     s
                // }
            }
            Type::Undetermined(u) => panic!("Undetermined: {u:?} reached a FastApi generator",),
            Type::Null => format!("None",),
            e => unimplemented!("{e:?}"),
        };
    }
}

impl Generator for FastApi {
    fn handle_type(&self, name: &str, model: &Type, defs: &Definitons) -> Code {
        todo!()
    }

    // fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> Code {
    //     let mut code = Code::new_segment();
    //     code.add_line(format!("class {}(BaseModel):", name));
    //     let class_body = code.create_child_block();
    //     for (name, ty) in &model.params {
    //         class_body.add_line(format!("{name}: {}", self.handle_type(defs, &ty)));
    //     }

    //     return code;
    // }

    // #[allow(unused_variables)]
    // fn handle_enum(&self, name: &str, model: &Enum) -> Code {
    //     if let EnumHandling::ToString = self.enum_handling {
    //         return Code::new_segment();
    //     }
    //     todo!();
    // }

    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code {
        let mut code = Code::new_segment();
        code.add_line(format!(
            "@{}.{}('{}')",
            self.app_name, endpoint.method, endpoint.url
        ));
        let mut func_stmt = format!("def endpoint_{}(", name,);
        for (name, ty) in &endpoint.params {
            func_stmt += format!("{name}: {}, ", self.handle_type(defs, &ty)).as_str();
        }
        func_stmt += "):";
        code.add_line(func_stmt);

        let func_body = code.create_child_block();

        let mut call = format!("{}(", name,);
        for (name, _) in &endpoint.params {
            call += format!("{name}, ").as_str();
        }
        call += ")";
        func_body.add_line(call);

        return code;
    }
}
