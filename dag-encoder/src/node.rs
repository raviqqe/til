use crate::link::Link;
use alloc::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Node {
    Link(Link),
    Merge {
        r#type: usize,
        left: Rc<Node>,
        right: Rc<Node>,
    },
}

impl From<Link> for Node {
    fn from(link: Link) -> Self {
        Self::Link(link)
    }
}
