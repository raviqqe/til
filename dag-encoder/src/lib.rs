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

const INTEGER_BASE: u128 = 1 << 7;
const FIXED_LINK_PAYLOAD_BASE: u128 = 1 << 2;
const VARIADIC_LINK_TYPE: usize = 5;

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    macro_rules! assert_encode_decode {
        ($graph:expr) => {
            let graph = $graph;
            let mut buffer = vec![];

            encode(&graph, &mut buffer).unwrap();
            buffer.reverse();

            assert_eq!(&decode(&*buffer).unwrap(), &graph);
        };
    }

    #[test]
    fn encode_empty() {
        assert_encode_decode!(Graph::default());
    }

    #[test]
    fn encode_node() {
        assert_encode_decode!(Graph::new(Some(
            Node::Link {
                r#type: 0,
                payload: Payload::Number(0.0),
                next: None
            }
            .into()
        )));
    }

    #[test]
    fn encode_two_nodes() {
        assert_encode_decode!(Graph::new(Some(
            Node::Link {
                r#type: 0,
                payload: Payload::Number(0.0),
                next: Some(
                    Node::Link {
                        r#type: 0,
                        payload: Payload::Number(0.0),
                        next: None
                    }
                    .into()
                )
            }
            .into()
        )));
    }

    #[test]
    fn encode_three_nodes() {
        assert_encode_decode!(Graph::new(Some(
            Node::Link {
                r#type: 0,
                payload: Payload::Number(0.0),
                next: Some(
                    Node::Link {
                        r#type: 0,
                        payload: Payload::Number(0.0),
                        next: Some(
                            Node::Link {
                                r#type: 0,
                                payload: Payload::Number(0.0),
                                next: None
                            }
                            .into()
                        )
                    }
                    .into()
                )
            }
            .into()
        )));
    }

    macro_rules! test_payload {
        ($name:ident, $payload:literal) => {
            #[test]
            fn $name() {
                assert_encode_decode!(Graph::new(Some(
                    Node::Link {
                        r#type: 0,
                        payload: Payload::Number($payload),
                        next: None
                    }
                    .into()
                )));
            }
        };
    }

    test_payload!(zero_payload, 0.0);
    test_payload!(one_payload, 1.0);
    test_payload!(two_payload, 2.0);
    test_payload!(positive_integer_payload, 42.0);
}
