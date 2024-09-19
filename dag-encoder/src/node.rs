use crate::Payload;

#[derive(Debug, PartialEq)]
pub enum Node {
    Link(Payload),
    Merge,
    Split(usize),
    Share,
}
