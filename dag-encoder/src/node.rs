use crate::link::Link;

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

impl From<f64> for Node {
    fn from(number: f64) -> Self {
        Self::Number(number)
    }
}
