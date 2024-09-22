use crate::link::Link;
use alloc::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Node {
    Link(Link),
    Number(f64),
}

impl From<Link> for Node {
    fn from(link: Link) -> Self {
        Self::Link(link)
    }
}
