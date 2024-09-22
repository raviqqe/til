use crate::link::Link;
use alloc::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Node {
    Link(Rc<Link>),
    Value(f64),
}

impl Node {
    pub const fn is_link(&self) -> bool {
        matches!(self, Self::Link(_))
    }

    pub const fn is_value(&self) -> bool {
        matches!(self, Self::Value(_))
    }
}

impl Default for Node {
    fn default() -> Self {
        Self::Value(0.0)
    }
}

impl From<Link> for Node {
    fn from(link: Link) -> Self {
        Self::Link(Rc::new(link))
    }
}

impl From<f64> for Node {
    fn from(number: f64) -> Self {
        Self::Value(number)
    }
}
