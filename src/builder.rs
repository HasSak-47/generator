use std::fmt::Display;

use anyhow::{Result, anyhow};

#[derive(Debug)]
pub struct Code {
    pub code: String,
    pub childs: Vec<Code>,
    pub end_code: String,
}

impl Code {
    pub fn has_code(&self) -> bool {
        return self.code.len() > 0 || self.childs.len() > 0;
    }

    pub fn new() -> Self {
        Self {
            code: String::new(),
            childs: Vec::new(),
            end_code: String::new(),
        }
    }

    pub fn new_child(code: String) -> Self {
        Self {
            code,
            childs: Vec::new(),
            end_code: String::new(),
        }
    }

    pub fn add_code(&mut self, child: Code) -> &mut Code {
        self.childs.push(child);
        return self.childs.last_mut().unwrap();
    }

    pub fn add_child(&mut self, code: String) -> &mut Code {
        let child = Self::new_child(code);
        self.childs.push(child);
        return self.childs.last_mut().unwrap();
    }

    // NOTE: this doesn't add 1 to a childs level
    // because the level of the childs must be 0
    pub fn collapse_root<S: AsRef<str>>(&self, pad: S) -> String {
        let pad = pad.as_ref();
        let mut code = String::new();

        for child in &self.childs {
            code += child._collapse(0, pad).as_str();
        }

        return code;
    }

    fn _collapse<S: AsRef<str>>(&self, level: usize, pad: S) -> String {
        let pad = pad.as_ref();
        let mut code = String::new();

        if self.code.len() > 0 {
            code += format!("{}{}\n", pad.repeat(level), self.code).as_str();
        }
        for child in &self.childs {
            code += child._collapse(level + 1, pad).as_str();
        }
        if self.end_code.len() > 0 {
            code += format!("{}{}\n", pad.repeat(level), self.end_code).as_str();
        }

        return code;
    }
}
