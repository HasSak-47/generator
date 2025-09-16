use super::types::*;
use std::fmt::Display;

#[derive(Debug, Default)]
pub enum EndPointMethod {
    #[default]
    GET,
    POST,
    PUT,
}

impl Display for EndPointMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::GET => "get",
                Self::POST => "post",
                Self::PUT => "put",
            }
        )?;
        return Ok(());
    }
}

#[derive(Debug, Default)]
pub struct EndPoint {
    pub params: Vec<(String, Type)>,
    pub method: EndPointMethod,
    pub url: String,
    pub return_type: Type,
}

#[derive(Debug, Default)]
pub enum EndPointParamKind {
    #[default]
    Body,
    Path,
    Query,
}

impl EndPoint {
    pub fn get_param_type<S: AsRef<str>>(&self, name: S) -> Option<EndPointParamKind> {
        let name = name.as_ref();
        let (name, ty) = self.params.iter().find(|p| p.0 == name)?;
        match &ty {
            Type::Union(_) | Type::Struct(_) => return Some(EndPointParamKind::Body),
            _ => {
                if self.url.contains(format!("{{{name}}}").as_str()) {
                    return Some(EndPointParamKind::Path);
                } else {
                    return Some(EndPointParamKind::Query);
                }
            }
        }
    }
}
