use crate::Node;
use std::rc::Rc;

#[derive(Debug, Default, PartialEq)]
pub struct Graph {
    root: Option<Rc<Node>>,
}

impl Graph {
    pub fn new(root: Option<Rc<Node>>) -> Self {
        Self { root }
    }

    pub fn root(&self) -> Option<&Rc<Node>> {
        self.root.as_ref()
    }
}
