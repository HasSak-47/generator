use std::{
    ffi::{c_char, c_void},
    ptr::null,
};

use crate::{
    builder::{Code, ffi::CodeFFI},
    parser::{
        definitions::{Definitons, Generator, TypeInformation},
        endpoint::EndPoint as EndpointDef,
        types::Type as TypeDef,
    },
};

#[repr(C)]
pub struct TypeWrapper {
    pub defs: *const TypeDef,
}

#[repr(C)]
pub struct TypeInfoWrapper {
    pub defs: *const TypeInformation,
}

#[repr(C)]
pub struct EndpointWrapper {
    pub defs: *const EndpointDef,
}

#[repr(C)]
pub struct DefinitionsWrapper {
    pub defs: *const Definitons,
}

pub type TypeHeader = extern "C" fn(*const c_void, DefinitionsWrapper) -> CodeFFI;
pub type EndpointHeader = extern "C" fn(*const c_void, DefinitionsWrapper) -> CodeFFI;
pub type Type =
    extern "C" fn(*const c_void, *const c_char, TypeWrapper, c_char, DefinitionsWrapper) -> CodeFFI;
pub type TypeTranslation =
    extern "C" fn(*const c_void, c_char, TypeInfoWrapper, DefinitionsWrapper) -> CodeFFI;
pub type Endpoint =
    extern "C" fn(*const c_void, *const c_char, EndpointWrapper, DefinitionsWrapper) -> CodeFFI;

#[repr(C)]
#[allow(dead_code)]
struct GeneratorFFI {
    this: *const c_void,
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
            this: null(),
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

    #[allow(dead_code)]
    extern "C" fn set_this(mut self, t: *const c_void) -> GeneratorFFI {
        self.this = t;
        self
    }
}

impl Generator for GeneratorFFI {
    fn generate_type(&self, name: &str, model: &TypeDef, public: bool, defs: &Definitons) -> Code {
        assert!(!self.this.is_null());
        assert!(self.ty.is_some());
        let model = TypeWrapper {
            defs: model as *const TypeDef,
        };
        let defs = DefinitionsWrapper {
            defs: defs as *const Definitons,
        };
        let codeffi = self.ty.unwrap()(
            self.this,
            name.as_ptr() as *const i8,
            model,
            public as c_char,
            defs,
        );

        match codeffi {
            CodeFFI::Code(code) => *code,
            _ => unreachable!(),
        }
    }

    fn generate_type_translation(
        &self,
        public: bool,
        tyinfo: &TypeInformation,
        defs: &Definitons,
    ) -> Code {
        assert!(!self.this.is_null());
        assert!(self.ty_translation.is_some());
        let model = TypeInfoWrapper {
            defs: tyinfo as *const TypeInformation,
        };
        let defs = DefinitionsWrapper {
            defs: defs as *const Definitons,
        };
        let codeffi = self.ty_translation.unwrap()(self.this, public as c_char, model, defs);

        match codeffi {
            CodeFFI::Code(code) => *code,
            _ => unreachable!(),
        }
    }

    fn generate_endpoint(&self, name: &str, endpoint: &EndpointDef, defs: &Definitons) -> Code {
        assert!(!self.this.is_null());
        assert!(self.endpoint.is_some());
        let model = EndpointWrapper {
            defs: endpoint as *const EndpointDef,
        };
        let defs = DefinitionsWrapper {
            defs: defs as *const Definitons,
        };
        let codeffi =
            self.endpoint.unwrap()(self.this, name.as_ptr() as *const c_char, model, defs);

        match codeffi {
            CodeFFI::Code(code) => *code,
            _ => unreachable!(),
        }
    }

    fn generate_type_header(&self, defs: &Definitons) -> Code {
        assert!(!self.this.is_null());
        assert!(self.header_type.is_some());
        let defs = DefinitionsWrapper {
            defs: defs as *const Definitons,
        };
        let codeffi = self.header_type.unwrap()(self.this, defs);

        match codeffi {
            CodeFFI::Code(code) => *code,
            _ => unreachable!(),
        }
    }

    fn generate_endpoint_header(&self, defs: &Definitons) -> Code {
        assert!(!self.this.is_null());
        assert!(self.header_endpoint.is_some());
        let defs = DefinitionsWrapper {
            defs: defs as *const Definitons,
        };
        let codeffi = self.header_endpoint.unwrap()(self.this, defs);

        match codeffi {
            CodeFFI::Code(code) => *code,
            _ => unreachable!(),
        }
    }
}
