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

    pub fn get_childs(&mut self) -> &mut Vec<Code> {
        match self {
            Self::Segment { childs } => childs,
            Self::Block { childs } => childs,
            Self::Line(_) => panic!("line does not have childs!!"),
        }
    }

    pub fn collapse_root<S: AsRef<str>>(&mut self, _ind: S) -> String {
        todo!()
    }

    pub fn has_code(&self) -> bool {
        todo!()
    }
}
