use crate::{Node, Payload};
use alloc::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Link {
    r#type: usize,
    payload: Payload,
    next: Option<Rc<Node>>,
}

impl Link {
    pub const fn new(r#type: usize, payload: Payload, next: Option<Rc<Node>>) -> Self {
        Self {
            r#type,
            payload,
            next,
        }
    }

    pub const fn r#type(&self) -> usize {
        self.r#type
    }

    pub const fn payload(&self) -> &Payload {
        &self.payload
    }

    pub const fn next(&self) -> Option<&Rc<Node>> {
        self.next.as_ref()
    }
}
