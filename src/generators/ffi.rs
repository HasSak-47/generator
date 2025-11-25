use std::ffi::{c_char, c_str, c_void};

use crate::parser::definitions::{Definitons, Generator, TypeInformation};

#[repr(C)]
pub struct TypeWrapper {
    pub defs: *const Type,
}

#[repr(C)]
pub struct TypeInfoWrapper {
    pub defs: *const TypeInformation,
}

#[repr(C)]
pub struct EndpointWrapper {
    pub defs: *const Endpoint,
}

#[repr(C)]
pub struct DefinitionsWrapper {
    pub defs: *const Definitons,
}

pub type TypeHeader = extern "C" fn(*const c_void, DefinitionsWrapper);
pub type EndpointHeader = extern "C" fn(*const c_void, DefinitionsWrapper);
pub type Type =
    extern "C" fn(*const c_void, *const c_char, TypeWrapper, c_char, DefinitionsWrapper);
pub type TypeTranslation =
    extern "C" fn(*const c_void, c_char, TypeInfoWrapper, DefinitionsWrapper);
pub type Endpoint =
    extern "C" fn(*const c_void, *const c_char, EndpointWrapper, DefinitionsWrapper);

#[repr(C)]
#[allow(dead_code)]
struct GeneratorFFI {
    header_type: Option<TypeHeader>,
    header_endpoint: Option<EndpointHeader>,
    ty: Option<Type>,
    ty_translation: Option<TypeTranslation>,
    endpoint: Option<Endpoint>,
}

impl GeneratorFFI {
    #[allow(dead_code)]
    extern "C" fn new() -> GeneratorFFI {
        return GeneratorFFI {
            header_type: None,
            header_endpoint: None,
            ty: None,
            ty_translation: None,
            endpoint: None,
        };
    }

    #[allow(dead_code)]
    extern "C" fn set_header_type(mut self, t: TypeHeader) -> GeneratorFFI {
        self.header_type = Some(t);
        self
    }

    #[allow(dead_code)]
    extern "C" fn set_header_endpoint(mut self, t: EndpointHeader) -> GeneratorFFI {
        self.header_endpoint = Some(t);
        self
    }

    #[allow(dead_code)]
    extern "C" fn set_type(mut self, t: Type) -> GeneratorFFI {
        self.ty = Some(t);
        self
    }

    #[allow(dead_code)]
    extern "C" fn set_type_translation(mut self, t: TypeTranslation) -> GeneratorFFI {
        self.ty_translation = Some(t);
        self
    }

    #[allow(dead_code)]
    extern "C" fn set_endpoint(mut self, t: Endpoint) -> GeneratorFFI {
        self.endpoint = Some(t);
        self
    }
}
