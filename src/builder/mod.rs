/// Tiny tree-based code builder that preserves indentation.
pub enum Code {
    Segment { childs: Vec<Code> },
    Block { childs: Vec<Code> },
    Line(String),
}

impl Code {
    pub fn new_line(line: String) -> Self {
        Self::Line(line)
    }

    pub fn new_segment() -> Self {
        Self::Segment { childs: Vec::new() }
    }

    pub fn new_block() -> Self {
        Self::Block { childs: Vec::new() }
    }

    pub fn add_child(&mut self, code: Code) {
        match self {
            Self::Segment { childs } => childs.push(code),
            Self::Block { childs } => childs.push(code),
            Self::Line(_) => panic!("cannot add childs to a line!"),
        }
    }

    pub fn create_child_segment(&mut self) -> &mut Self {
        match self {
            Self::Segment { childs } => {
                childs.push(Self::new_segment());
                return childs.last_mut().unwrap();
            }
            Self::Block { childs } => {
                childs.push(Self::new_segment());
                return childs.last_mut().unwrap();
            }
            Self::Line(_) => panic!("cannot add line to a line!"),
        }
    }

    pub fn create_child_block(&mut self) -> &mut Self {
        match self {
            Self::Segment { childs } => {
                childs.push(Self::new_block());
                return childs.last_mut().unwrap();
            }
            Self::Block { childs } => {
                childs.push(Self::new_block());
                return childs.last_mut().unwrap();
            }
            Self::Line(_) => panic!("cannot add line to a line!"),
        }
    }

    pub fn add_line(&mut self, line: String) {
        match self {
            Self::Segment { childs } => childs.push(Self::new_line(line)),
            Self::Block { childs } => childs.push(Self::new_line(line)),
            Self::Line(_) => panic!("cannot add line to a line!"),
        }
    }

    #[allow(dead_code)]
    pub fn get_childs(&mut self) -> &mut Vec<Code> {
        match self {
            Self::Segment { childs } => childs,
            Self::Block { childs } => childs,
            Self::Line(_) => panic!("line does not have childs!!"),
        }
    }

    /// Collapse the `Code` tree into a formatted string using the provided indentation unit.
    pub fn collapse_root<S: AsRef<str>>(&mut self, _ind: S) -> String {
        return self._collapse_root(_ind, 0, 0);
    }

    /// Depth-first formatter that renders the nested `Code` structure with indentation.
    fn _collapse_root<S: AsRef<str>>(
        &mut self,
        ind: S,
        depth: usize,
        child_depth: usize,
    ) -> String {
        let ind = ind.as_ref();
        match self {
            Code::Line(s) => return format!("{}{s}\n", ind.repeat(depth)),
            Code::Segment { childs } => {
                let mut buf = String::new();
                // buf += format!("//{}SEGMENT {child_depth}\n", ">".repeat(child_depth)).as_str();
                for child in childs {
                    if child.has_code() {
                        buf += child._collapse_root(ind, depth, child_depth + 1).as_str();
                    }
                }
                buf += "\n";
                // buf += format!("//{}SEGMENT {child_depth}\n", "<".repeat(child_depth)).as_str();

                return buf;
            }
            Code::Block { childs } => {
                let mut buf = String::new();
                // buf += format!("//{}body {child_depth}\n", ">".repeat(child_depth)).as_str();
                for child in childs {
                    if child.has_code() {
                        buf += child
                            ._collapse_root(ind, depth + 1, child_depth + 1)
                            .as_str();
                    }
                }
                // buf += format!("//{}BODY {child_depth}\n", "<".repeat(child_depth)).as_str();
                return buf;
            }
        };
    }

    /// Returns true when any subtree contains at least one non-empty line.
    pub fn has_code(&self) -> bool {
        match self {
            Code::Line(s) => return !s.is_empty(),
            Self::Segment { childs } => {
                for child in childs {
                    if child.has_code() {
                        return true;
                    }
                }
            }
            Self::Block { childs } => {
                for child in childs {
                    if child.has_code() {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}
