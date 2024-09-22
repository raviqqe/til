use crate::{Node, Payload};
use alloc::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Link {
    r#type: usize,
    left: Payload,
    right: Payload,
}

impl Link {
    pub const fn new(r#type: usize, left: Payload, right: Payload) -> Self {
        Self {
            r#type,
            left,
            right,
        }
    }

    pub const fn r#type(&self) -> usize {
        self.r#type
    }

    pub const fn left(&self) -> &Payload {
        &self.left
    }

    pub const fn right(&self) -> &Payload {
        &self.right
    }
}
