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

    fn get_query_code<S: AsRef<str>>(&self, name: S, ty: &Type) -> Code {
        let mut code = Code::new_segment();
        let name = name.as_ref();

        match ty {
            Type::Optional(_) => {
                code.add_line(format!("if({name} !== null)"));
                let if_body = code.create_child_block();
                if_body.add_line(format!("searchParams.set('{name}', {name});"));
            }
            _ => code.add_line(format!("searchParams.set('{name}', {name});")),
        }

        return code;
    }

    /// Build the expression that converts an "input" value into its wire representation.
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
                    Repr::Datetime => format!("{name}.toISOString()"),
                };
            }
            Type::Optional(opt) => {
                format!(
                    "{name} !== null ? {} : null",
                    self.get_convertion_string(name, &opt.ty, defs)
                )
            }
            Type::Array(arr) => {
                format!(
                    "{name}.map(e => {{ return {}}})",
                    self.get_convertion_string("e", &arr.ty, defs)
                )
            }
            Type::Model(model) => {
                format!("transform_{model}({name})")
            }
            _ => name.to_string(),
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

    /// Strip `Into` wrappers so we can generate helper models that mirror the request payload.
    fn transform_type(&self, ty: &Type, defs: &Definitons) -> Type {
        match ty {
            Type::Array(a) => {
                return self.transform_type(&a.ty, defs);
            }
            Type::Optional(o) => {
                return self.transform_type(&o.ty, defs);
            }
            Type::Model(m) => {
                return Type::Model(format!("_{m}"));
            }
            Type::Into(i) => return *i.from.clone(),
            _ => unreachable!(),
        }
    }

    /// Create a shadow model where `Into` fields are renamed and converted to their source types.
    fn translate_model(&self, model: &Model, defs: &Definitons) -> Model {
        let mut translated = Model { params: Vec::new() };
        for (pname, pty) in &model.params {
            if pty.contains_into(defs) {
                let ty = self.transform_type(pty, defs);
                translated.params.push((pname.clone(), ty));
            } else {
                translated.params.push((pname.clone(), pty.clone()));
            }
        }

        return translated;
    }

    /// Emit internal helper types for request bodies that require pre-flight transforms.
    fn generate_request_models(&self, defs: &Definitons) -> Code {
        let mut code = Code::new_segment();
        for (model_name, model) in &defs.models {
            let ty = Type::Model(model_name.clone());
            if !ty.contains_into(defs) {
                continue;
            }

            let translated = self.translate_model(model, defs);
            let name = format!("_{model_name}");

            code.add_child(self._handle_model(name.as_str(), &translated, defs, false));
        }
        return code;
    }

    /// Emit the conversion helpers that map public models into the helper request models.
    fn generate_request_transitions(&self, defs: &Definitons) -> Code {
        let mut code = Code::new_segment();

        for (model_name, model) in &defs.models {
            let mut segment = code.create_child_segment();
            let ty = Type::Model(model_name.clone());
            if !ty.contains_into(defs) {
                continue;
            }

            code.add_line(format!(
                "function transform_{model_name}(_m: {model_name}){{"
            ));

            let func_body = code.create_child_block();
            func_body.add_line("return {".to_string());
            let obj_body = func_body.create_child_block();

            for (name, ty) in &model.params {
                obj_body.add_line(format!(
                    "{name} : {},",
                    self.get_convertion_string(format!("m.{name}"), ty, defs)
                ));
            }
            let _ = obj_body;
            func_body.add_line(format!("}} as _{model_name};"));
        }

        return code;
    }

    /// Guard against missing fields on loosely typed JSON responses.
    fn validate_param(&self, name: &String, _expected_type: &Type, return_type: &String) -> Code {
        // TODO: add type guards
        let mut code = Code::new_segment();
        code.add_line(format!("if(j.{name} === undefined)"));
        code.add_line(self.handle_error(
            &format!("new Error('field {name} is undefined')"),
            &return_type,
        ));

        return code;
    }

    fn _handle_model(&self, name: &str, model: &Model, defs: &Definitons, export: bool) -> Code {
        let mut code = Code::new_segment();
        code.add_line(format!(
            "{}type {} = {{",
            if export { "export " } else { "" },
            name
        ));
        let type_body = code.create_child_block();
        for (name, ty) in &model.params {
            type_body.add_line(format!(
                "{name}: {};",
                self.handle_singature_for_model(defs, &ty)
            ));
        }
        code.add_line("};".to_string());

        return code;
    }
}

impl Generator for TS {
    fn generate_endpoint_header(&self, defs: &Definitons) -> Code {
        let mut code = Code::new_segment();
        if let ErrorHandling::Result = self.error_handling {
            code.add_line("import Result from '@/utils/result'".to_string());
        }

        code.add_child(self.generate_request_models(defs));
        code.add_child(self.generate_request_transitions(defs));

        return code;
    }

    fn handle_model(&self, name: &str, model: &Model, defs: &Definitons) -> Code {
        return self._handle_model(name, model, defs, true);
    }

    fn handle_enum(&self, name: &str, e: &crate::dsl::Enum) -> Code {
        match self.type_enum {
            EnumHandling::ToEnum => {
                return Code::new_segment();
            }
            EnumHandling::ToType => {
                return Code::new_line(format!(
                    "export type {name} = {}",
                    self.generate_enum_algebra(e)
                ));
            }
            EnumHandling::ToString | EnumHandling::ToAlgebraic => {
                return Code::new_segment();
            }
        }
    }

    fn handle_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code {
        let mut code = Code::new_segment();
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
        code.add_line(function_decl);
        let func_body = code.create_child_block();

        for (name, ty) in &endpoint.params {
            if !ty.contains_into(defs) {
                continue;
            }

            func_body.add_line(format!(
                "const {name} = {};",
                self.get_convertion_string(name, ty, defs)
            ));
        }

        let mut has_query = false;
        let mut query = Code::new_segment();
        query.add_line("const searchParams = new URLSearchParams();".to_string());

        for (name, ty) in &endpoint.params {
            if let EndPointParamKind::Query = endpoint.get_param_type(&name).unwrap() {
                query.add_child(self.get_query_code(name, &ty));
                has_query = true;
            }
        }

        func_body.add_line(format!("let url = `{}`;", endpoint.url.replace("{", "${")));

        if has_query {
            func_body.add_child(query);
            func_body.add_line(
                "url = searchParams.size > 0 ? `${url}?${searchParams}` : url;".to_string(),
            );
        }

        let mut fetch_code = Code::new_segment();
        fetch_code.add_line("let response = await fetch(url, {".to_string());
        let fetch_body = fetch_code.create_child_block();
        fetch_body.add_line(format!("method: '{}',", endpoint.method));

        let mut has_body = false;

        for (name, _) in &endpoint.params {
            if let EndPointParamKind::Body = endpoint.get_param_type(&name).unwrap() {
                has_body = true;
                break;
            }
        }

        // request body
        if has_body {
            fetch_body.add_line("body: JSON.stringify({".to_string());
            let body_code = fetch_body.create_child_block();

            for (name, _) in &endpoint.params {
                if let EndPointParamKind::Body = endpoint.get_param_type(&name).unwrap() {
                    body_code.add_line(format!("{name}: {name}"));
                }
            }

            fetch_code.add_line("headers: {".to_string());
            let header_code = fetch_code.create_child_block();
            header_code.add_line("'Content-Type': 'application/json'".to_string());
            fetch_code.add_line("}".to_string());
        }
        fetch_code.add_line("});".to_string());
        func_body.add_child(fetch_code);

        let return_type = self.handle_singature_for_model(defs, &endpoint.return_type);

        // request error
        let error = "new Error(response.statusText)".to_string();

        let if_segment = func_body.create_child_segment();
        if_segment.add_line("if(!response.ok)".to_string());
        let if_body = if_segment.create_child_block();
        if_body.add_line(self.handle_error(&error, &return_type));

        if let Type::Null = endpoint.return_type {
            func_body.add_line(self.handle_ok(&"null".to_string(), &return_type));
            return code;
        }

        func_body.add_line("let j = await response.json();".to_string());
        let response_segment = func_body.create_child_segment();

        match &endpoint.return_type {
            Type::Model(m) => {
                for (name, ty) in &defs.models[m].params {
                    response_segment.add_child(self.validate_param(name, ty, &return_type));
                }
                func_body.add_line(format!("return j as {return_type}"));
            }
            Type::Primitive(p) => {
                let expected_type = self.handle_primitive(p);
                response_segment
                    .add_line(format!("if(typeof j != '{}')", self.handle_primitive(p)));
                let if_body = response_segment.create_child_block();
                if_body.add_line(self.handle_error(
                    &format!("new Error('response was not a {expected_type}')"),
                    &return_type,
                ));
                func_body.add_line("return j;".to_string());
            }
            // TODO: add
            Type::Array(_) => {
                func_body.add_line(format!("return j as {};", return_type));
            }
            _ => todo!(),
        }

        code.add_line("}".to_string());
        return code;
    }
}
