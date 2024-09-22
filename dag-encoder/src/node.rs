use crate::link::Link;
use alloc::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Node {
    Link(Rc<Link>),
    Number(f64),
}

impl Default for Node {
    fn default() -> Self {
        Self::Number(0.0)
    }
}

impl From<Link> for Node {
    fn from(link: Link) -> Self {
        Self::Link(Rc::new(link))
    }
}

impl From<f64> for Node {
    fn from(number: f64) -> Self {
        Self::Number(number)
    }
}
