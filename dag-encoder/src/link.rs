use crate::{Node, Payload};
use alloc::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Link {
    r#type: usize,
    payload: Payload,
    next: Option<Rc<Node>>,
}

impl Link {
    pub fn new(r#type: usize, payload: Payload, next: Option<Rc<Node>>) -> Self {
        Self {
            r#type,
            payload,
            next,
        }
    }

    pub fn r#type(&self) -> usize {
        self.r#type
    }

    pub fn payload(&self) -> &Payload {
        &self.payload
    }

    pub fn next(&self) -> Option<Rc<Node>> {
        self.next.clone()
    }
}
