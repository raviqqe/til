use crate::{Node, Payload};
use alloc::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Link {
    r#type: usize,
    left: Payload,
    right: Rc<Node>,
}

impl Link {
    pub const fn new(r#type: usize, payload: Payload, next: Option<Rc<Node>>) -> Self {
        Self {
            r#type,
            left: payload,
            right: next,
        }
    }

    pub const fn r#type(&self) -> usize {
        self.r#type
    }

    pub const fn left(&self) -> &Payload {
        &self.left
    }

    pub const fn right(&self) -> Option<&Rc<Node>> {
        self.right.as_ref()
    }
}
