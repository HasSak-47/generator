// NOTE: fast api handles datetime convertion so you can just kinda ignore it

use crate::{
    builder::Code,
    parser::{definitions::*, endpoint::*, types::*},
};

use clap::{Parser, ValueEnum};

/// Determine how literal unions should be represented in Pydantic models.
#[derive(ValueEnum, Debug, Default, Clone, PartialEq)]
#[value(rename_all = "snake_case")]
pub enum EnumHandling {
    /// python unions A | B
    ToUnion,
    #[default]
    /// if A, B are of ty T, then represent the type as T
    ToType,
    /// to a class that derives Enum
    ToEnumClass,
}

/// CLI surface for the FastAPI generator.
#[derive(Parser, Clone)]
pub struct FastApi {
    /// Module path or symbol used in the generated decorators, e.g. `app`.
    pub app_name: String,
    #[arg(short, long, value_enum, default_value_t = EnumHandling::ToType)]
    pub enum_handling: EnumHandling,
}

impl Default for FastApi {
    fn default() -> Self {
        Self {
            app_name: "app".to_string(),
            enum_handling: EnumHandling::ToUnion,
        }
    }
}

impl FastApi {
    fn py_signature_for_primitive(&self, p: &PrimitiveType) -> String {
        use PrimitiveType as PT;
        return match p {
            PT::Bool => "bool",
            PT::Integer(_) | PT::Unsigned(_) => "int",
            PT::Float(_) => "float",
            PT::String(_) => "str",
        }
        .to_string();
    }

    fn py_signature_for_repr(&self, r: &Repr) -> String {
        match r {
            Repr::Datetime => "dt.datetime",
        }
        .to_string()
    }

    fn py_union_literal(&self, u: &UnionType, defs: &Definitons) -> String {
        let mut poss = u.members.iter();
        let mut s = format!("{}", self.py_type_literal(defs, &poss.next().unwrap().ty));
        for UnionMember { ty, .. } in poss {
            s += format!(" | {}", self.py_type_literal(defs, ty)).as_str();
        }

        return s;
    }

    /// Map DSL types to the string annotations required by FastAPI and pydantic.
    fn py_type_literal(&self, defs: &Definitons, ty: &Type) -> String {
        return match ty {
            Type::Primitive(p) => self.py_signature_for_primitive(p),
            Type::Repr(r) => self.py_signature_for_repr(r),
            Type::Optional(o) => format!("Optional[{}]", self.py_type_literal(defs, &o.ty)),
            Type::Array(a) => format!("List[{}]", self.py_type_literal(defs, &a.ty)),
            Type::Into(i) => format!("{}", self.py_signature_for_repr(&i.into)),
            Type::Named(m) => format!("{m}",),
            Type::Null => format!("None",),
            Type::Literal(l) => {
                format!("{l}")
            }
            Type::Union(u) => self.py_union_literal(u, defs),
            Type::Undetermined(u) => {
                panic!("Undetermined: {u:?} reached a TS generator {defs:#?}",)
            }
            #[allow(unreachable_patterns)]
            e => unimplemented!("not found signature of: {e:?}"),
        };
    }
}

impl Generator for FastApi {
    fn generate_type_header(&self, _defs: &Definitons) -> Code {
        let mut code = Code::new_segment();
        code.add_line("from __future__ import annotations".to_string());
        code.add_line("from pydantic import BaseModel".to_string());
        code.add_line("from typing import Optional, List, Dict, Tuple, Literal".to_string());
        code.add_line("import datetime as dt".to_string());

        return code;
    }

    fn generate_type(&self, name: &str, ty: &Type, public: bool, defs: &Definitons) -> Code {
        if !public {
            return Code::new_segment();
        }
        let mut code = Code::new_segment();
        match ty {
            Type::Struct(s) => {
                code.add_line(format!("class {name}(BaseModel):",));
                let member_block = code.create_child_block();
                for (name, ty) in &s.members {
                    member_block.add_line(format!("{name}: {}", self.py_type_literal(defs, &ty)));
                }
            }

            Type::Union(u) => {
                assert_ne!(u.members.len(), 0);
                let union_str = self.py_union_literal(u, defs);

                code.add_line(format!("type {name} = {union_str}",));
            }

            _ => todo!(),
        };
        return code;
    }

    fn generate_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code {
        let mut code = Code::new_segment();
        code.add_line(format!(
            "@{}.{}('{}')",
            self.app_name, endpoint.method, endpoint.url
        ));
        let mut func_stmt = format!("def endpoint_{}(", name,);
        for (name, ty) in &endpoint.params {
            func_stmt += format!("{name}: {}, ", self.py_type_literal(defs, &ty)).as_str();
        }
        func_stmt += "):";
        code.add_line(func_stmt);

        let func_body = code.create_child_block();

        let mut call = format!("return {}(", name,);
        for (name, _) in &endpoint.params {
            call += format!("{name}, ").as_str();
        }
        call += ")";
        func_body.add_line(call);

        return code;
    }

    fn generate_type_translation(&self, model: &TypeInformation, defs: &Definitons) -> Code {
        let code = Code::new_segment();
        return code;
    }
}
