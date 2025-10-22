use std::fmt::Display;

use crate::{
    builder::*,
    dsl::{Definitons, EndPoint, EndPointParamKind, Enum, Generator, Model},
    types::{PrimitiveType, Repr, Type},
};

use clap::{Parser, ValueEnum};

#[derive(ValueEnum, Debug, Default, Clone, PartialEq)]
#[value(rename_all = "snake_case")]
pub enum EnumHandling {
    ToEnum,
    ToString,
    ToAlgebraic,
    #[default]
    ToType,
}

impl Display for EnumHandling {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::ToEnum => "to_enum",
                Self::ToString => "to_string",
                Self::ToAlgebraic => "to_algebraic",
                Self::ToType => "to_type",
            }
        )
    }
}

#[derive(ValueEnum, Debug, Default, Clone, PartialEq)]
#[value(rename_all = "snake_case")]
pub enum ErrorHandling {
    Result,
    Pair,
    #[default]
    Raise,
}

impl Display for ErrorHandling {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Result => "result",
                Self::Pair => "pair",
                Self::Raise => "raise",
            }
        )
    }
}

#[derive(Parser, Clone)]
pub struct TS {
    #[arg(short, long, default_value_t = ErrorHandling::Raise)]
    error_handling: ErrorHandling,

    #[arg(short, long, default_value_t = EnumHandling::ToType)]
    type_enum: EnumHandling,
}

impl TS {
    fn handle_primitive(&self, p: &PrimitiveType) -> String {
        use PrimitiveType as PT;
        return match p {
            PT::Bool => "boolean",
            PT::Integer(_) | PT::Unsigned(_) | PT::Float(_) => "number",
            PT::String(_) => "string",
        }
        .to_string();
    }

    fn generate_enum_algebra(&self, e: &Enum) -> String {
        let mut poss = e.params.iter();
        let mut s = format!("{}", poss.next().unwrap());
        for param in poss {
            s += format!(" | {param}").as_str();
        }

        s
    }

    fn handle_repr_signature(&self, r: &Repr) -> String {
        match r {
            Repr::Datetime => "Date",
        }
        .to_string()
    }

    fn handle_singature_for_model(&self, defs: &Definitons, ty: &Type) -> String {
        return match ty {
            Type::Primitive(p) => self.handle_primitive(p),
            Type::Repr(r) => self.handle_repr_signature(r),
            Type::Optional(o) => format!("{} | null", self.handle_singature_for_model(defs, &o.ty)),
            Type::Array(a) => format!("{}[]", self.handle_singature_for_model(defs, &a.ty)),
            Type::Into(i) => format!("{}", self.handle_repr_signature(&i.into)),
            Type::Model(m) => format!("{m}",),
            Type::Enum(e) => match self.type_enum {
                EnumHandling::ToType | EnumHandling::ToEnum => format!("{e}"),
                EnumHandling::ToAlgebraic => self.generate_enum_algebra(defs.enums.get(e).unwrap()),
                EnumHandling::ToString => {
                    self.handle_singature_for_model(defs, &Type::string(None))
                }
            },
            Type::Undetermined(u) => panic!("Undetermined: {u:?} reached a TS generator",),
            Type::Null => format!("null",),
        };
    }

    fn handle_singature_for_request(&self, defs: &Definitons, ty: &Type) -> String {
        return match ty {
            Type::Primitive(p) => self.handle_primitive(p),
            Type::Repr(r) => self.handle_repr_signature(r),
            Type::Optional(o) => {
                format!("{} | null", self.handle_singature_for_request(defs, &o.ty))
            }
            Type::Array(a) => format!("{}[]", self.handle_singature_for_request(defs, &a.ty)),
            Type::Into(i) => format!("{}", self.handle_singature_for_request(defs, &i.from)),
            Type::Model(m) => format!("{m}",),
            Type::Enum(e) => match self.type_enum {
                EnumHandling::ToType | EnumHandling::ToEnum => format!("{e}"),
                EnumHandling::ToAlgebraic => self.generate_enum_algebra(defs.enums.get(e).unwrap()),
                EnumHandling::ToString => {
                    self.handle_singature_for_model(defs, &Type::string(None))
                }
            },
            Type::Undetermined(u) => panic!("Undetermined: {u:?} reached a TS generator",),
            Type::Null => format!("null",),
        };
    }

    fn get_query_string<S: AsRef<str>>(&self, name: S, ty: &Type) -> String {
        let name = name.as_ref();
        match ty {
            Type::Optional(_) => {
                format!("if({name} !== null)\n\t\tsearchParams.set('{name}', {name});",)
            }
            _ => format!("searchParams.set('{name}', {name})"),
        }
    }

    fn get_convertion_string<S: AsRef<str>>(
        &self,
        name: S,
        ty: &Type,
        defs: &Definitons,
    ) -> String {
        let name = name.as_ref();
        match ty {
            Type::Into(into) => {
                return match into.into {
                    Repr::Datetime => format!("_{name}.toISOString()"),
                };
            }
            Type::Optional(opt) => {
                format!(
                    "_{name} !== null ? {} : null",
                    self.get_convertion_string(name, &opt.ty, defs)
                )
            }
            Type::Array(arr) => {
                format!(
                    "_{name}.map(e => {{ return {}}})",
                    self.get_convertion_string("e", &arr.ty, defs)
                )
            }
            _ => format!("_{name}"),
        }
    }

    fn handle_error(&self, err: &String, ty: &String) -> String {
        return match self.error_handling {
            ErrorHandling::Result => format!("return Result.Err<{ty}, Error>({err});"),
            ErrorHandling::Pair => format!("return [{err}, null]);"),
            ErrorHandling::Raise => format!("raise {err};"),
        };
    }

    fn handle_ok(&self, ok: &String, ty: &String) -> String {
        return match self.error_handling {
            ErrorHandling::Result => format!("return Result.Ok<{ty}, Error>({ok});"),
            ErrorHandling::Pair => format!("return [{ok}, null]"),
            ErrorHandling::Raise => format!("return {ty};"),
        };
    }

    fn validate_param(&self, name: &String, expected_type: &Type, return_type: &String) -> String {
        let mut code = String::new();
        code += format!(
            "\tif(j.{name} === undefined)\n\t\t{}\n",
            self.handle_error(
                &format!("new Error('field {name} is undefined')"),
                &return_type,
            )
        )
        .as_str();

        return code;
    }
}

impl Generator for TS {
    fn generate_endpoint_header(&self) -> String {
        if let ErrorHandling::Result = self.error_handling {
            return "import Result from '@/utils/result'\n".to_string();
        } else {
            return String::new();
        }
    }

    fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> String {
        let mut code = format!("type {} = {{", name);
        for (name, ty) in &model.params {
            code += format!(
                "\n\t{name}: {};",
                self.handle_singature_for_model(defs, &ty)
            )
            .as_str();
        }
        code += "\n};";

        return code;
    }

    fn handle_enum(&self, name: &str, e: &crate::dsl::Enum) -> String {
        match self.type_enum {
            EnumHandling::ToEnum => {
                todo!();
            }
            EnumHandling::ToType => {
                return format!("type {name} = {}", self.generate_enum_algebra(e));
            }
            EnumHandling::ToString | EnumHandling::ToAlgebraic => {
                return String::new();
            }
        }
    }

    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> String {
        let mut code = format!("export async function {}(", name);
        for (name, ty) in &endpoint.params {
            if ty.root_is_into() {
                code +=
                    format!("_{name}: {}, ", self.handle_singature_for_model(defs, &ty)).as_str();
            } else {
                code +=
                    format!("{name}: {}, ", self.handle_singature_for_model(defs, &ty)).as_str();
            }
        }
        code += "){\n";
        for (name, ty) in &endpoint.params {
            if ty.root_is_into() {
                code += format!(
                    "\tconst {name} = {};\n",
                    self.get_convertion_string(name, &ty, defs)
                )
                .as_str();
            }
        }

        let mut has_query = false;
        let mut query = String::new();
        query += "\n";
        query += "\tconst searchParams = new URLSearchParams();\n";
        query += "\n";

        for (name, ty) in &endpoint.params {
            if let EndPointParamKind::Query = endpoint.get_param_type(&name).unwrap() {
                query += format!("\t{}\n", self.get_query_string(name, &ty)).as_str();
                has_query = true;
            }
        }

        let mut has_body = false;
        let mut body = String::new();
        body += "\t\tbody: JSON.stringify({\n";

        for (name, _) in &endpoint.params {
            if let EndPointParamKind::Body = endpoint.get_param_type(&name).unwrap() {
                body += format!("\t\t\t{name}: {name}\n").as_str();
                has_body = true;
            }
        }
        body += "\t\t}),\n";
        body += "\t\theaders: {\n";
        body += "\t\t\t'Content-Type': 'application/json',\n";
        body += "\t\t},\n";

        if has_query {
            code += query.as_str();
            code += "\n";
            code += format!("\tlet url = `{}`;\n", endpoint.url.replace("{", "${")).as_str();
            code += "\turl = searchParams.size > 0 ? `${url}?${searchParams}` : url;\n";
        } else {
            code += format!("\tlet url = `{}`;\n", endpoint.url.replace("{", "${")).as_str();
        }

        if has_body {
            code += format!(
                "\tlet response = await fetch(url, {{
\t\tmethod: '{}',
{}
\t}});\n",
                endpoint.method, body,
            )
            .as_str();
        } else {
            code += format!(
                "\tlet response = await fetch(url, {{ method: '{}' }});\n",
                endpoint.method,
            )
            .as_str();
        }

        let return_type = self.handle_singature_for_model(defs, &endpoint.return_type);

        // request error
        let error = "new Error(response.statusText)".to_string();

        code += format!(
            "\tif(!response.ok)\n\t\t{}\n",
            self.handle_error(&error, &return_type)
        )
        .as_str();

        if let Type::Null = endpoint.return_type {
            code += "\n}";
            return code;
        }

        code += "\tlet j = await response.json();\n";
        match &endpoint.return_type {
            Type::Model(m) => {
                for (name, ty) in &defs.models[m].params {
                    code += self.validate_param(name, ty, &return_type).as_str();
                }
            }
            Type::Primitive(p) => {
                let expected_type = self.handle_primitive(p);
                code += format!(
                    "\tif(typeof j != '{}')\n\t\t{}\n",
                    expected_type,
                    self.handle_error(
                        &format!("new Error('response was not a {expected_type}')"),
                        &return_type
                    )
                )
                .as_str();
                code += "\treturn j;\n";
            }
            _ => unreachable!(),
        }

        code += "}";

        return code;
    }
}
