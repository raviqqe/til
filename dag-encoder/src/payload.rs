use crate::Node;
use alloc::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Payload {
    Node(Option<Rc<Node>>),
    Number(f64),
}

impl From<Node> for Payload {
    fn from(node: Node) -> Self {
        Self::Node(Some(node.into()))
    }
}

impl From<Option<Rc<Node>>> for Payload {
    fn from(node: Option<Rc<Node>>) -> Self {
        Self::Node(node)
    }
}

impl From<f64> for Payload {
    fn from(number: f64) -> Self {
        Self::Number(number)
    }
}
