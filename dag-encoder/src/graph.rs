use crate::Node;

#[derive(Debug, PartialEq)]
pub struct Graph {
    values: Vec<f64>,
    nodes: Vec<Node>,
}

impl Graph {
    pub fn new(values: Vec<f64>, nodes: Vec<Node>) -> Self {
        Self { values, nodes }
    }

    pub fn values(&self) -> impl Iterator<Item = f64> + '_ {
        self.values.iter().copied()
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Node> {
        self.nodes.iter()
    }
}
