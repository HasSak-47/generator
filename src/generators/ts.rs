use std::fmt::Display;

use crate::{
    builder::Code,
    parser::{definitions::*, endpoint::*, types::*},
};

use clap::{Parser, ValueEnum};

/// Controls how single typed literal unions are translated in TypeScript output.
#[derive(ValueEnum, Debug, Default, Clone, PartialEq)]
#[value(rename_all = "snake_case")]
pub enum EnumHandling {
    ToEnum,
    ToType,
    #[default]
    ToAlgebraic,
}

impl Display for EnumHandling {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::ToEnum => "to_enum",
                Self::ToAlgebraic => "to_algebraic",
                Self::ToType => "to_type",
            }
        )
    }
}

/// Shapes the return type of HTTP helpers when a request fails.
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

/// CLI surface for the TypeScript generator.
#[derive(Parser, Clone, Default)]
pub struct TS {
    /// Determines whether fetch helpers throw, return `Result`, or return `(data, err)` tuples.
    #[arg(short, long, default_value_t = ErrorHandling::Raise)]
    pub error_handling: ErrorHandling,

    /// Toggles how literal unions are emitted.
    #[arg(short, long, default_value_t = EnumHandling::ToType)]
    pub type_enum: EnumHandling,
}

impl TS {
    fn ts_signature_for_primitive(&self, p: &PrimitiveType) -> String {
        use PrimitiveType as PT;
        return match p {
            PT::Bool => "boolean",
            PT::Integer(_) | PT::Unsigned(_) | PT::Float(_) => "number",
            PT::String(_) => "string",
        }
        .to_string();
    }

    fn ts_signature_for_repr(&self, r: &Repr) -> String {
        match r {
            Repr::Datetime => "Date",
        }
        .to_string()
    }

    fn ts_union_literal(&self, e: &UnionType, defs: &Definitons) -> String {
        let mut poss = e.members.iter();
        let mut s = format!("{}", self.ts_type_literal(defs, &poss.next().unwrap().ty));
        for UnionMember { ty, .. } in poss {
            s += format!(" | {}", self.ts_type_literal(defs, ty)).as_str();
        }

        return match e.kind {
            UnionKind::Untagged => s,
            UnionKind::External => {
                let mut poss = e.members.iter();
                let next = poss.next().unwrap();
                let fmtter = |u: &UnionMember| {
                    let tag = u.tag.as_ref().unwrap();
                    let ty = &u.ty;
                    return format!("{{ tag: '{tag}', data: {ty}}}");
                };
                let mut s = fmtter(next);
                for member in poss {
                    s += format!(" | {}", fmtter(member)).as_str();
                }

                return s;
            }
            UnionKind::Interal => todo!(),
        };
    }

    /// Translate a DSL `Type` into the appropriate TypeScript type literal.
    fn ts_type_literal(&self, defs: &Definitons, ty: &Type) -> String {
        return match ty {
            Type::Primitive(p) => self.ts_signature_for_primitive(p),
            Type::Repr(r) => self.ts_signature_for_repr(r),
            Type::Optional(o) => format!("{} | null", self.ts_type_literal(defs, &o.ty)),
            Type::Array(a) => format!("{}[]", self.ts_type_literal(defs, &a.ty)),
            Type::Into(i) => format!("{}", self.ts_signature_for_repr(&i.into)),
            Type::Named(m) => format!("{m}",),
            Type::Null => format!("null",),
            Type::Literal(l) => {
                format!("{l}")
            }
            Type::Union(u) => self.ts_union_literal(u, defs),
            Type::Undetermined(u) => {
                panic!("Undetermined: {u:?} reached a TS generator {defs:#?}",)
            }
            #[allow(unreachable_patterns)]
            e => unimplemented!("not found signature of: {e:?}"),
        };
    }

    /// Produce query-string serialization code for a single endpoint parameter.
    fn emit_query_param_assignment<S: AsRef<str>>(&self, name: S, ty: &Type) -> Code {
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

    /// Build the expression that converts an "input" value into its domain representation.
    fn build_into_domain_expression<S: AsRef<str>>(
        &self,
        name: S,
        ty: &Type,
        defs: &Definitons,
    ) -> String {
        assert!(ty.contains_into(defs));
        let name = name.as_ref();

        match ty {
            Type::Into(into) => {
                return match into.into {
                    Repr::Datetime => format!("new Date({name})"),
                };
            }
            Type::Optional(opt) => {
                format!(
                    "{name} !== null ? {} : null",
                    self.build_into_domain_expression(name, &opt.ty, defs)
                )
            }
            Type::Array(arr) => {
                format!(
                    "{name}.map(e => {{ return {}}})",
                    self.build_into_domain_expression("e", &arr.ty, defs)
                )
            }
            Type::Named(ty_name) => format!("into_domain_{ty_name}({name})"),
            e => unreachable!("reached: {e}"),
        }
    }

    /// Build the expression that converts an "input" value into its wire representation.
    fn build_into_wire_expression<S: AsRef<str>>(
        &self,
        name: S,
        ty: &Type,
        defs: &Definitons,
    ) -> String {
        assert!(ty.contains_into(defs));
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
                    self.build_into_wire_expression(name, &opt.ty, defs)
                )
            }
            Type::Array(arr) => {
                format!(
                    "{name}.map(e => {{ return {}}})",
                    self.build_into_wire_expression("e", &arr.ty, defs)
                )
            }
            Type::Named(ty_name) => format!("into_wire_{ty_name}({name})"),
            e => unreachable!("reached: {e}"),
        }
    }

    /// Emit the error-handling branch according to the configured strategy.
    fn emit_error_branch(&self, error_expr: &String, return_type: &String) -> String {
        return match self.error_handling {
            ErrorHandling::Result => {
                format!("return Result.Err<{return_type}, Error>({error_expr});")
            }
            ErrorHandling::Pair => format!("return [{error_expr}, null]);"),
            ErrorHandling::Raise => format!("throw {error_expr};"),
        };
    }

    /// Emit the success-handling branch according to the configured strategy.
    fn emit_success_branch(&self, value_expr: &String, return_type: &String) -> String {
        return match self.error_handling {
            ErrorHandling::Result => {
                format!("return Result.Ok<{return_type}, Error>({value_expr});")
            }
            ErrorHandling::Pair => format!("return [{value_expr}, null]"),
            ErrorHandling::Raise => format!("return {value_expr};"),
        };
    }

    /// Guard against missing fields on loosely typed JSON responses.
    fn guard_missing_response_field(
        &self,
        name: &String,
        _expected_type: &Type,
        return_type: &String,
    ) -> Code {
        // TODO: add type guards
        let mut code = Code::new_segment();
        code.add_line(format!("if(j.{name} === undefined)"));
        let body = code.create_child_block();
        body.add_line(self.emit_error_branch(
            &format!("new Error('field {name} is undefined')"),
            &return_type,
        ));

        return code;
    }

    fn generate_struct_translation<F>(
        &self,
        return_name: &String,
        s: &StructType,
        defs: &Definitons,
        translator: F,
    ) -> Code
    where
        F: Fn(String, &Type, &Definitons) -> String,
    {
        let mut code = Code::new_segment();
        code.add_line("let _m = {".to_string());
        let body = code.create_child_block();
        for (name, ty) in &s.members {
            if ty.contains_into(defs) {
                body.add_line(format!(
                    "{name}: {},",
                    translator(format!("m.{name}"), ty, defs)
                ));
            } else {
                body.add_line(format!("{name}: m.{name},"));
            }
        }

        code.add_line(format!("}} as {return_name}"));
        code.add_line("return _m".to_string());
        return code;
    }

    fn generate_union_translation<F: Fn(String, &Type, &Definitons) -> String>(
        &self,
        _return_type: &String,
        u: &UnionType,
        defs: &Definitons,
        translator: F,
    ) -> Code {
        let mut code = Code::new_segment();
        match u.kind {
            UnionKind::Interal => {
                todo!()
            }
            UnionKind::External => {
                for UnionMember { ty, tag } in &u.members {
                    let tag = tag.as_ref().unwrap();
                    code.add_line(format!("if(m.tag === '{tag}')"));
                    let ret_code = code.create_child_block();
                    if !ty.contains_into(defs) {
                        ret_code.add_line(format!("return {{tag:'{tag}',data:m.data}}"));
                    } else {
                        ret_code.add_line(format!(
                            "return {{tag:'{tag}', data:{}}}",
                            translator("m.data".to_string(), ty, defs)
                        ));
                    }
                    code.add_line("else".to_string());
                }
                code.create_child_block()
                    .add_line("throw Error('???')".to_string());
            }
            UnionKind::Untagged => {
                todo!()
            }
        }
        return code;
    }
}

impl Generator for TS {
    fn generate_endpoint_header(&self, _: &Definitons) -> Code {
        let mut code = Code::new_segment();
        if let ErrorHandling::Result = self.error_handling {
            code.add_line("import Result from '@/utils/result'".to_string());
        }

        return code;
    }

    fn generate_type_translation(&self, ty: &TypeInformation, defs: &Definitons) -> Code {
        let name = &ty.name.as_ref().unwrap();
        let mut code = Code::new_segment();

        let domain_code = code.create_child_segment();
        domain_code.add_line(format!("function into_domain_{name}(m: _{name}){{"));
        match &ty.ty {
            Type::Struct(s) => domain_code.add_child(self.generate_struct_translation(
                name,
                &s,
                defs,
                |name, ty, defs| self.build_into_domain_expression(name, ty, defs),
            )),
            Type::Union(u) => domain_code.add_child(self.generate_union_translation(
                name,
                &u,
                defs,
                |name, ty, defs| self.build_into_domain_expression(name, ty, defs),
            )),
            _ => todo!(),
        }
        domain_code.add_line("}".to_string());

        let wire_code = code.create_child_segment();
        wire_code.add_line(format!("function into_wire_{name}(m: {name}){{"));
        match &ty.ty {
            Type::Struct(s) => wire_code.add_child(self.generate_struct_translation(
                ty.get_wire_name().as_ref().unwrap(),
                &s,
                defs,
                |name, ty, defs| self.build_into_wire_expression(name, ty, defs),
            )),
            Type::Union(u) => wire_code.add_child(self.generate_union_translation(
                ty.get_wire_name().as_ref().unwrap(),
                &u,
                defs,
                |name, ty, defs| self.build_into_wire_expression(name, ty, defs),
            )),
            _ => todo!(),
        }
        wire_code.add_line("}".to_string());

        return code;
    }

    fn generate_type(&self, name: &str, ty: &Type, public: bool, defs: &Definitons) -> Code {
        let mut code = Code::new_segment();
        match ty {
            Type::Struct(s) => {
                code.add_line(format!(
                    "{}type {name} = {{",
                    if public { "export " } else { "" }
                ));
                let member_block = code.create_child_block();
                for (name, ty) in &s.members {
                    member_block.add_line(format!("{name}: {},", self.ts_type_literal(defs, &ty)));
                }
                code.add_line("}".to_string());
            }

            Type::Union(u) => {
                assert_ne!(u.members.len(), 0);
                let union_str = self.ts_union_literal(u, defs);

                code.add_line(format!(
                    "{}type {name} = {union_str}",
                    if public { "export " } else { "" }
                ));
            }

            _ => todo!(),
        };
        return code;
    }

    /// Generate a strongly typed fetch wrapper for a single endpoint definition.
    fn generate_endpoint(&self, name: &str, endpoint: &EndPoint, defs: &Definitons) -> Code {
        let mut code = Code::new_segment();
        let mut function_decl = format!("export async function {}(", name);
        for (name, ty) in &endpoint.params {
            if ty.contains_into(defs) {
                function_decl += format!("_{name}: {}, ", self.ts_type_literal(defs, &ty)).as_str();
            } else {
                function_decl += format!("{name}: {}, ", self.ts_type_literal(defs, &ty)).as_str();
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
                self.build_into_wire_expression(format!("_{name}"), ty, defs)
            ));
        }

        let mut has_query = false;
        let mut query = Code::new_segment();
        query.add_line("const searchParams = new URLSearchParams();".to_string());

        for (name, ty) in &endpoint.params {
            if let EndPointParamKind::Query = endpoint.get_param_type(&name).unwrap() {
                query.add_child(self.emit_query_param_assignment(name, &ty));
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
            fetch_body.add_line("}),".to_string());

            fetch_body.add_line("headers: {".to_string());
            let header_code = fetch_body.create_child_block();
            header_code.add_line("'Content-Type': 'application/json'".to_string());
            fetch_body.add_line("}".to_string());
        }
        fetch_code.add_line("});".to_string());
        func_body.add_child(fetch_code);

        let return_type = self.ts_type_literal(defs, &endpoint.return_type);

        // request error
        let error = "new Error(response.statusText)".to_string();

        let if_segment = func_body.create_child_segment();
        if_segment.add_line("if(!response.ok)".to_string());
        let if_body = if_segment.create_child_block();
        if_body.add_line(self.emit_error_branch(&error, &return_type));

        if let Type::Null = endpoint.return_type {
            func_body.add_line(self.emit_success_branch(&"null".to_string(), &return_type));
            code.add_line("}".to_string());
            return code;
        }

        func_body.add_line("let j = await response.json();".to_string());
        let response_segment = func_body.create_child_segment();

        match &endpoint.return_type {
            Type::Named(name) => {
                let ty = defs.get_named_type(name).unwrap();
                if let Type::Struct(s) = ty.get_wire_type() {
                    for (name, ty) in &s.members {
                        response_segment.add_child(self.guard_missing_response_field(
                            name,
                            ty,
                            &return_type,
                        ));
                    }
                }
                if endpoint.return_type.contains_into(defs) {
                    func_body.add_line(format!("return into_domain_{name}(j);"));
                } else {
                    func_body.add_line(format!("return j as {name};"));
                }
            }
            Type::Null => {
                func_body.add_line("return".to_string());
            }
            Type::Primitive(p) => {
                let expected_type = self.ts_signature_for_primitive(p);
                response_segment.add_line(format!(
                    "if(typeof j != '{}')",
                    self.ts_signature_for_primitive(p)
                ));
                let if_body = response_segment.create_child_block();
                if_body.add_line(self.emit_error_branch(
                    &format!("new Error('response was not a {expected_type}')"),
                    &return_type,
                ));
                func_body.add_line("return j;".to_string());
            }
            Type::Array(arr) => {
                if arr.ty.contains_into(defs) {
                    func_body.add_line(format!(
                        "return j.map(m => {});",
                        self.build_into_domain_expression("m".to_string(), &arr.ty, defs),
                    ));
                } else {
                    func_body.add_line(format!(
                        "return j as {}",
                        self.ts_type_literal(defs, &endpoint.return_type)
                    ));
                }
            }
            ty => func_body.add_line(format!("return j as {}", self.ts_type_literal(defs, ty))),
        }

        code.add_line("}".to_string());
        return code;
    }
}
