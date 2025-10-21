use crate::{
    dsl::{Definitons, EndPoint, EndPointParamKind, Generator, Model},
    types::{PrimitiveType, Repr, Type},
};

use clap::Parser;

#[derive(Parser, Clone)]
pub struct TS {
    #[arg(short, long, default_value_t = true)]
    result: bool,
}

impl TS {
    #[allow(lint)]
    pub fn new() -> Self {
        return Self { result: true };
    }

    fn handle_primitive(&self, p: &PrimitiveType) -> String {
        use PrimitiveType as PT;
        return match p {
            PT::Integer(_) | PT::Unsigned(_) | PT::Float(_) => "number",
            PT::String(_) => "string",
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
}

impl Generator for TS {
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
            code += format!("_{name}: {},", self.handle_type_signature(defs, &ty)).as_str();
        }
        code += "){\n";
        for (name, ty) in &endpoint.params {
            code += format!(
                "\tconst {name} = {};\n",
                self.get_convertion_string(name, &ty, defs)
            )
            .as_str();
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

        let return_type = self.handle_type_signature(defs, &endpoint.return_type);
        code += format!(
            "\tif(!response.ok)\n\t\treturn Result.Err<{}, Error>(new Error(response.statusText)) \t\t\n",
            return_type,
        )
        .as_str();

        if let Type::Null = &endpoint.return_type {
            code += "\treturn Result.Ok<null, Error>(null);\n";
        } else {
            code += format!(
                "\treturn Result.Ok<{}, Error>((await response.json()) as {});\n",
                return_type, return_type,
            )
            .as_str();
        }
        code += "}";

        return code;
    }
}
