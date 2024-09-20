extern crate alloc;

mod decode;
mod encode;
mod error;
mod graph;
mod node;
mod payload;

pub use decode::decode;
pub use encode::encode;
pub use error::Error;
pub use graph::Graph;
pub use node::Node;
pub use payload::Payload;

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_encode_decode {
        ($graph:expr) => {
            let graph = $graph;
            let mut buffer = vec![];

            encode(&graph, &mut buffer).unwrap();

            assert_eq!(&decode(&buffer).unwrap(), &graph);
        };
    }

    #[test]
    fn encode_empty() {
        assert_encode_decode!(Graph::default());
    }
}
