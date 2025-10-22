use std::fmt::Display;

use crate::{
    builder::Code,
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

    fn get_query_code<S: AsRef<str>>(&self, name: S, ty: &Type) -> Code {
        let name = name.as_ref();
        match ty {
            Type::Optional(_) => {
                let mut c = Code::new_child(format!("if({name} !== null)"));
                c.add_child(format!("searchParams.set('{name}', {name});"));

                return c;
            }
            _ => Code::new_child(format!("searchParams.set('{name}', {name});")),
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

    fn validate_param(&self, name: &String, _expected_type: &Type, return_type: &String) -> Code {
        let mut code = Code::new_child(format!("if(j.{name} === undefined)"));
        code.add_child(self.handle_error(
            &format!("new Error('field {name} is undefined')"),
            &return_type,
        ));

        return code;
    }
}

impl Generator for TS {
    fn generate_endpoint_header(&self) -> Code {
        if let ErrorHandling::Result = self.error_handling {
            return Code::new_child("import Result from '@/utils/result'".to_string());
        } else {
            return Code::new();
        }
    }

    fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> Code {
        let mut code = Code::new();
        let type_decl = code.add_child(format!("type {} = {{", name));
        for (name, ty) in &model.params {
            type_decl.add_child(format!(
                "{name}: {};",
                self.handle_singature_for_model(defs, &ty)
            ));
        }
        type_decl.end_code = "};".to_string();

        return code;
    }

    fn handle_enum(&self, name: &str, e: &crate::dsl::Enum) -> Code {
        match self.type_enum {
            EnumHandling::ToEnum => {
                return Code::new();
            }
            EnumHandling::ToType => {
                return Code::new_child(format!("type {name} = {}", self.generate_enum_algebra(e)));
            }
            EnumHandling::ToString | EnumHandling::ToAlgebraic => {
                return Code::new();
            }
        }
    }

    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code {
        let mut code = Code::new();
        let mut function_decl = format!("export async function {}(", name);
        for (name, ty) in &endpoint.params {
            if ty.contains_into(defs) {
                function_decl +=
                    format!("_{name}: {}, ", self.handle_singature_for_model(defs, &ty)).as_str();
            } else {
                function_decl +=
                    format!("{name}: {}, ", self.handle_singature_for_model(defs, &ty)).as_str();
            }
        }
        function_decl += "){";
        let function = code.add_child(function_decl);
        function.end_code = "}".to_string();

        for (name, ty) in &endpoint.params {
            if ty.contains_into(defs) {
                function.add_child(format!(
                    "const {name} = {};",
                    self.get_convertion_string(name, &ty, defs)
                ));
            }
        }

        let mut has_query = false;
        let mut query = Code::new_child("const searchParams = new URLSearchParams();".to_string());

        for (name, ty) in &endpoint.params {
            if let EndPointParamKind::Query = endpoint.get_param_type(&name).unwrap() {
                query.add_code(self.get_query_code(name, &ty));
                has_query = true;
            }
        }

        function.add_child(format!("let url = `{}`;", endpoint.url.replace("{", "${")));

        if has_query {
            function.flat_add_code(query);
            function.add_child(
                "url = searchParams.size > 0 ? `${url}?${searchParams}` : url;".to_string(),
            );
        }
        let mut fetch_code = Code::new_child("let response = await fetch(url, {".to_string());
        fetch_code.end_code = "});".to_string();
        fetch_code.add_child(format!("method: '{}',", endpoint.method));

        let mut has_body = false;

        for (name, _) in &endpoint.params {
            if let EndPointParamKind::Body = endpoint.get_param_type(&name).unwrap() {
                has_body = true;
                break;
            }
        }
        if has_body {
            let body_code = fetch_code.add_child("body: JSON.stringify({".to_string());
            body_code.end_code = "}),".to_string();
            for (name, _) in &endpoint.params {
                if let EndPointParamKind::Body = endpoint.get_param_type(&name).unwrap() {
                    body_code.add_child(format!("{name}: {name}"));
                }
            }

            let header_code = fetch_code.add_child("headers: {".to_string());
            header_code.end_code = "}".to_string();
            header_code.add_child("'Content-Type': 'application/json'".to_string());
        }
        function.add_code(fetch_code);

        let return_type = self.handle_singature_for_model(defs, &endpoint.return_type);

        // request error
        let error = "new Error(response.statusText)".to_string();

        let if_ = function.add_child("if(!response.ok)".to_string());
        if_.add_child(self.handle_error(&error, &return_type));

        if let Type::Null = endpoint.return_type {
            function.add_child(self.handle_ok(&"null".to_string(), &return_type));
            return code;
        }

        function.add_child("let j = await response.json();".to_string());

        match &endpoint.return_type {
            Type::Model(m) => {
                for (name, ty) in &defs.models[m].params {
                    function.add_code(self.validate_param(name, ty, &return_type));
                }
            }
            Type::Primitive(p) => {
                let expected_type = self.handle_primitive(p);
                let if_ =
                    function.add_child(format!("if(typeof j != '{}')", self.handle_primitive(p)));
                if_.add_child(self.handle_error(
                    &format!("new Error('response was not a {expected_type}')"),
                    &return_type,
                ));
                function.add_child("return j;".to_string());
            }
            _ => unreachable!(),
        }

        return code;
    }
}
