use crate::Node;

#[derive(Debug, Default, PartialEq)]
pub struct Graph {
    root: Node,
}

impl Graph {
    pub const fn new(root: Node) -> Self {
        Self { root }
    }

    pub const fn root(&self) -> &Node {
        &self.root
    }
}
