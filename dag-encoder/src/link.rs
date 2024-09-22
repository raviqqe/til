use crate::{Node, Payload};

#[derive(Debug, PartialEq)]
pub struct Link {
    r#type: usize,
    left: Node,
    right: Node,
}

impl Link {
    pub const fn new(r#type: usize, left: Node, right: Node) -> Self {
        Self {
            r#type,
            left,
            right,
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
}
