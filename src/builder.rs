use anyhow::{Result, anyhow};

#[derive(Debug)]
pub struct Code {
    code: String,
    childs: Vec<Code>,
}

impl Code {
    pub fn new_parent() -> Self {
        Self {
            code: String::new(),
            childs: Vec::new(),
        }
    }

    pub fn add_child(&mut self, code: String) -> &mut Code {
        let child = Self {
            code,
            childs: Vec::new(),
        };
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

        code += format!("{}{}\n", pad.repeat(level), self.code).as_str();
        for child in &self.childs {
            code += child._collapse(level + 1, pad).as_str();
        }

        return code;
    }
}
