use crate::Payload;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Node {
    Link(Payload, Option<Rc<Node>>),
    Merge(Rc<Node>, Rc<Node>),
}
