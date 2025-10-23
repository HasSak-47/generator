/// Tree-structured code fragment that can be rendered with indentation.
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

    /// Inline the child's immediate statements, but keep its descendants nested.
    pub fn flat_add_code(&mut self, child: Code) -> &mut Code {
        self.add_child(child.code);
        for child in child.childs {
            self.childs.push(child);
        }
        return self.childs.last_mut().unwrap();
    }

    pub fn add_child(&mut self, code: String) -> &mut Code {
        let child = Self::new_child(code);
        self.childs.push(child);
        return self.childs.last_mut().unwrap();
    }

    /// Collapse the tree into a single string while treating the current node as indentation level 0.
    /// NOTE: this doesn't add 1 to a childs level because the level of the childs must be 0
    pub fn collapse_root<S: AsRef<str>>(&self, pad: S, skip_empty: bool) -> String {
        let pad = pad.as_ref();
        let mut code = String::new();

        for child in &self.childs {
            code += child._collapse(0, pad, skip_empty).as_str();
        }

        return code;
    }

    fn _collapse<S: AsRef<str>>(&self, level: usize, pad: S, skip_empty: bool) -> String {
        let pad = pad.as_ref();
        let mut code = String::new();

        if !skip_empty || self.code.len() > 0 {
            code += format!("{}{}\n", pad.repeat(level), self.code).as_str();
        }
        for child in &self.childs {
            code += child._collapse(level + 1, pad, skip_empty).as_str();
        }
        if self.end_code.len() > 0 {
            code += format!("{}{}\n", pad.repeat(level), self.end_code).as_str();
        }

        return code;
    }
}
