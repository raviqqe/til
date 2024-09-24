use crate::{Node, Share};
use derivative::Derivative;

#[derive(Derivative)]
#[derivative(Debug, PartialEq)]
pub struct Link {
    r#type: usize,
    left: Node,
    right: Node,
    #[derivative(PartialEq = "ignore")]
    share: Option<Share>,
}

impl Link {
    pub const fn new(r#type: usize, left: Node, right: Node, share: Option<Share>) -> Self {
        Self {
            r#type,
            left,
            right,
            share,
        }
    }

    pub const fn r#type(&self) -> usize {
        self.r#type
    }

    pub const fn left(&self) -> &Node {
        &self.left
    }

    pub const fn right(&self) -> &Node {
        &self.right
    }

    pub const fn share(&self) -> Option<Share> {
        self.share
    }
}
