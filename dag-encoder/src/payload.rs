#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Payload {
    Immediate(f64),
    Shared(usize),
}
