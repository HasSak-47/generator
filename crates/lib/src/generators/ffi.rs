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

/// Exposes `Type` to foreign code without leaking Rust's layout.
#[repr(C)]
pub struct TypeWrapper {
    pub defs: *const TypeDef,
}

/// Wrapper around `TypeInformation` for FFI boundaries.
#[repr(C)]
pub struct TypeInfoWrapper {
    pub defs: *const TypeInformation,
}

/// Wrapper around `EndPoint` for FFI boundaries.
#[repr(C)]
pub struct EndpointWrapper {
    pub defs: *const EndpointDef,
}

/// Wrapper around the full `Definitons` map for FFI consumers.
#[repr(C)]
pub struct DefinitionsWrapper {
    pub defs: *const Definitons,
}

/// Signature for callbacks that emit type headers (imports, etc).
pub type TypeHeader = extern "C" fn(*const c_void, DefinitionsWrapper) -> CodeFFI;
/// Signature for callbacks that emit endpoint headers.
pub type EndpointHeader = extern "C" fn(*const c_void, DefinitionsWrapper) -> CodeFFI;
pub type Type =
    extern "C" fn(*const c_void, *const c_char, TypeWrapper, c_char, DefinitionsWrapper) -> CodeFFI;
/// Signature for callbacks that emit domain↔wire translation helpers.
pub type TypeTranslation =
    extern "C" fn(*const c_void, c_char, TypeInfoWrapper, DefinitionsWrapper) -> CodeFFI;
/// Signature for callbacks that emit endpoint functions.
pub type Endpoint =
    extern "C" fn(*const c_void, *const c_char, EndpointWrapper, DefinitionsWrapper) -> CodeFFI;

/// Trait object adapter that lets native generators be exposed via FFI.
#[repr(C)]
#[allow(dead_code)]
struct GeneratorFFI {
    this: *const c_void,
    header_type: Option<TypeHeader>,
    header_endpoint: Option<EndpointHeader>,
    ty: Option<Type>,
    wire_translation: Option<TypeTranslation>,
    domain_translation: Option<TypeTranslation>,
    endpoint: Option<Endpoint>,
}

impl GeneratorFFI {
    #[allow(dead_code)]
    #[unsafe(no_mangle)]
    extern "C" fn new() -> GeneratorFFI {
        return GeneratorFFI {
            this: null(),
            header_type: None,
            header_endpoint: None,
            ty: None,
            wire_translation: None,
            domain_translation: None,
            endpoint: None,
        };
    }

    #[allow(dead_code)]
    #[unsafe(no_mangle)]
    extern "C" fn set_header_type(mut self, t: TypeHeader) -> GeneratorFFI {
        self.header_type = Some(t);
        self
    }

    #[allow(dead_code)]
    #[unsafe(no_mangle)]
    extern "C" fn set_header_endpoint(mut self, t: EndpointHeader) -> GeneratorFFI {
        self.header_endpoint = Some(t);
        self
    }

    #[allow(dead_code)]
    #[unsafe(no_mangle)]
    extern "C" fn set_type(mut self, t: Type) -> GeneratorFFI {
        self.ty = Some(t);
        self
    }

    #[allow(dead_code)]
<<<<<<< HEAD
    extern "C" fn set_wire_translation(mut self, t: TypeTranslation) -> GeneratorFFI {
        self.wire_translation = Some(t);
        self
    }

    #[allow(dead_code)]
    extern "C" fn set_domain_translation(mut self, t: TypeTranslation) -> GeneratorFFI {
        self.domain_translation = Some(t);
=======
    #[unsafe(no_mangle)]
    extern "C" fn set_type_translation(mut self, t: TypeTranslation) -> GeneratorFFI {
        self.ty_translation = Some(t);
>>>>>>> 00ad704 (idk)
        self
    }

    #[allow(dead_code)]
    #[unsafe(no_mangle)]
    extern "C" fn set_endpoint(mut self, t: Endpoint) -> GeneratorFFI {
        self.endpoint = Some(t);
        self
    }

    #[allow(dead_code)]
    #[unsafe(no_mangle)]
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
    fn generate_to_wire_translation(
        &self,
        public: bool,
        tyinfo: &TypeInformation,
        defs: &Definitons,
    ) -> Code {
        assert!(!self.this.is_null());
        assert!(self.wire_translation.is_some());
        let model = TypeInfoWrapper {
            defs: tyinfo as *const TypeInformation,
        };
        let defs = DefinitionsWrapper {
            defs: defs as *const Definitons,
        };
        let codeffi = self.wire_translation.unwrap()(self.this, public as c_char, model, defs);

        match codeffi {
            CodeFFI::Code(code) => *code,
            _ => unreachable!(),
        }
    }

    fn generate_to_domain_translation(
        &self,
        public: bool,
        tyinfo: &TypeInformation,
        defs: &Definitons,
    ) -> Code {
        assert!(!self.this.is_null());
        assert!(self.domain_translation.is_some());
        let model = TypeInfoWrapper {
            defs: tyinfo as *const TypeInformation,
        };
        let defs = DefinitionsWrapper {
            defs: defs as *const Definitons,
        };
        let codeffi = self.domain_translation.unwrap()(self.this, public as c_char, model, defs);

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
