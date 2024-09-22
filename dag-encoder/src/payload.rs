use crate::Node;
use alloc::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Payload {
    Node(Option<Rc<Node>>),
    Number(f64),
}
