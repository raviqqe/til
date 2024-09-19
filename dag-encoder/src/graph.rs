use crate::Node;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Graph {
    root: Rc<Node>,
}

impl Graph {
    pub fn new(root: Rc<Node>) -> Self {
        Self { root }
    }

    pub fn root(&self) -> &Rc<Node> {
        &self.root
    }
}
