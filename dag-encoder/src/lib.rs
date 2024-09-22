extern crate alloc;

mod decode;
mod encode;
mod error;
mod graph;
mod link;
mod node;

pub use decode::decode;
pub use encode::encode;
pub use error::Error;
pub use graph::Graph;
pub use link::Link;
pub use node::Node;

const INTEGER_BASE: u128 = 1 << 7;
const VALUE_BASE: u128 = 1 << 7;
const FIXED_LINK_PAYLOAD_BASE: u128 = 1 << 2;
const VARIADIC_LINK_PAYLOAD_BASE: u128 = 1 << 5;
const VARIADIC_LINK_TYPE: usize = 4;

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
        assert_encode_decode!(Graph::new(Link::new(0, 0.0.into(), 0.0.into()).into()));
    }

    #[test]
    fn encode_two_nodes() {
        assert_encode_decode!(Graph::new(
            Link::new(0, 0.0.into(), Link::new(0, 0.0.into(), 0.0.into()).into()).into()
        ));
    }

    #[test]
    fn encode_three_nodes() {
        assert_encode_decode!(Graph::new(
            Link::new(
                0,
                0.0.into(),
                Link::new(0, 0.0.into(), Link::new(0, 0.0.into(), 0.0.into()).into()).into()
            )
            .into()
        ));
    }

    macro_rules! test_value {
        ($name:ident, $value:literal) => {
            #[test]
            fn $name() {
                assert_encode_decode!(Graph::new(Node::Value($value)));
            }
        };
    }

    test_value!(zero_value, 0.0);
    test_value!(one_value, 1.0);
    test_value!(two_value, 2.0);
    test_value!(positive_integer_value, 42.0);

    macro_rules! test_link_payload {
        ($name:ident, $payload:literal) => {
            #[test]
            fn $name() {
                assert_encode_decode!(Graph::new(Link::new(0, $payload.into(), 0.0.into()).into()));
            }
        };
    }

    test_link_payload!(zero_link_payload, 0.0);
    test_link_payload!(one_link_payload, 1.0);
    test_link_payload!(two_link_payload, 2.0);
    test_link_payload!(positive_integer_link_payload, 42.0);

    macro_rules! test_link_type {
        ($name:ident, $type:literal) => {
            #[test]
            fn $name() {
                assert_encode_decode!(Graph::new(Link::new($type, 0.0.into(), 0.0.into()).into()));
            }
        };
    }

    test_link_type!(zero_link_type, 0);
    test_link_type!(one_link_type, 1);
    test_link_type!(two_link_type, 2);
    test_link_type!(three_link_type, 3);
    test_link_type!(four_link_type, 4);
    test_link_type!(five_link_type, 5);
    test_link_type!(six_link_type, 6);
    test_link_type!(positive_integer_link_type, 42);
}
