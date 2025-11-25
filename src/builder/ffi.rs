use std::ffi::c_char;

use crate::builder::Code;

#[repr(C)]
enum EnumCodeFFI {
    Code(Box<Code>),
    Ref(*mut Code),
}

#[repr(transparent)]
pub struct CodeFFI {
    code: EnumCodeFFI,
}

impl CodeFFI {
    fn from_enum(code: EnumCodeFFI) -> Self {
        Self { code }
    }

    fn from_ref(code: *mut Code) -> Self {
        Self::from_enum(EnumCodeFFI::Ref(code))
    }

    fn from_code(code: Code) -> Self {
        Self::from_enum(EnumCodeFFI::Code(Box::new(code)))
    }

    #[allow(dead_code)]
    extern "C" fn new_line(line: *const c_char) -> CodeFFI {
        let line = unsafe { std::ffi::CStr::from_ptr(line).to_str().unwrap().to_string() };
        CodeFFI::from_code(Code::Line(line))
    }

    #[allow(dead_code)]
    extern "C" fn new_segment() -> CodeFFI {
        CodeFFI::from_code(Code::new_segment())
    }

    #[allow(dead_code)]
    extern "C" fn new_block() -> CodeFFI {
        CodeFFI::from_code(Code::new_block())
    }

    #[allow(dead_code)]
    fn get_code(&mut self) -> &mut Code {
        match &mut self.code {
            EnumCodeFFI::Code(c) => c,
            EnumCodeFFI::Ref(c) => unsafe { &mut **c },
        }
    }

    #[allow(dead_code)]
    fn take_code(self) -> Code {
        match self.code {
            EnumCodeFFI::Code(c) => *c,
            EnumCodeFFI::Ref(_) => unreachable!(),
        }
    }

    #[allow(dead_code)]
    extern "C" fn add_child(&mut self, code: CodeFFI) {
        let code = code.take_code();
        match &mut self.get_code() {
            Code::Segment { childs } => childs.push(code),
            Code::Block { childs } => childs.push(code),
            Code::Line(_) => panic!("cannot add childs to a line!"),
        }
    }

    #[allow(dead_code)]
    extern "C" fn create_child_segment(&mut self) -> CodeFFI {
        match &mut self.get_code() {
            Code::Segment { childs } => {
                childs.push(Code::new_segment());
                return CodeFFI::from_ref(childs.last_mut().unwrap());
            }
            Code::Block { childs } => {
                childs.push(Code::new_segment());
                return CodeFFI::from_ref(childs.last_mut().unwrap());
            }
            Code::Line(_) => panic!("cannot add line to a line!"),
        }
    }

    #[allow(dead_code)]
    extern "C" fn create_child_block(&mut self) -> CodeFFI {
        match self.get_code() {
            Code::Segment { childs } => {
                childs.push(Code::new_block());
                return CodeFFI::from_ref(childs.last_mut().unwrap());
            }
            Code::Block { childs } => {
                childs.push(Code::new_block());
                return CodeFFI::from_ref(childs.last_mut().unwrap());
            }
            Code::Line(_) => panic!("cannot add line to a line!"),
        }
    }

    #[allow(dead_code)]
    extern "C" fn add_line(&mut self, line: *const c_char) {
        let line = unsafe { std::ffi::CStr::from_ptr(line).to_str().unwrap().to_string() };

        match self.get_code() {
            Code::Segment { childs } => childs.push(Code::new_line(line)),
            Code::Block { childs } => childs.push(Code::new_line(line)),
            Code::Line(_) => panic!("cannot add line to a line!"),
        }
    }
}
