use crate::Payload;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Node {
    Link {
        r#type: usize,
        payload: Payload,
        next: Option<Rc<Node>>,
    },
    Merge {
        left: Rc<Node>,
        right: Rc<Node>,
    },
}
