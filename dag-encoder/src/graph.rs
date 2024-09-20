use crate::Node;
use alloc::rc::Rc;

#[derive(Debug, Default, PartialEq)]
pub struct Graph {
    root: Option<Rc<Node>>,
}

impl Graph {
    pub const fn new(root: Option<Rc<Node>>) -> Self {
        Self { root }
    }

    pub const fn root(&self) -> Option<&Rc<Node>> {
        self.root.as_ref()
    }
}
