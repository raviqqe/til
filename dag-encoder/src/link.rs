use crate::Node;

#[derive(Debug, PartialEq)]
pub struct Link {
    r#type: usize,
    left: Node,
    right: Node,
    unique: bool,
}

impl Link {
    pub const fn new(r#type: usize, left: Node, right: Node, unique: bool) -> Self {
        Self {
            r#type,
            left,
            right,
            unique,
        }
    }

    pub const fn r#type(&self) -> usize {
        self.r#type
    }

    pub const fn left(&self) -> &Node {
        &self.left
    }

    pub const fn right(&self) -> &Node {
        &self.right
    }

    pub const fn is_unique(&self) -> bool {
        self.unique
    }

    pub fn set_unique(&mut self, unique: bool) {
        self.unique = unique;
    }
}
