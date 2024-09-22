#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Payload {
    Number(f64),
    Node(Option<Rc<Node>>),
}
