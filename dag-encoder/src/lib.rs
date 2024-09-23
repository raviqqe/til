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
const VALUE_BASE: u128 = 1 << 6;
const TYPE_BASE: u128 = 1 << 6;

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

    mod right_value {
        use super::*;
        use pretty_assertions::assert_eq;

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
    }

    mod left_value {
        use super::*;
        use pretty_assertions::assert_eq;

        macro_rules! test_value {
            ($name:ident, $value:literal) => {
                #[test]
                fn $name() {
                    assert_encode_decode!(Graph::new(
                        Link::new(0, $value.into(), 0.0.into()).into()
                    ));
                }
            };
        }

        test_value!(zero, 0.0);
        test_value!(one, 1.0);
        test_value!(two, 2.0);
        test_value!(positive_integer, 42.0);
    }

    mod link_type {
        use super::*;
        use pretty_assertions::assert_eq;

        macro_rules! test_type {
            ($name:ident, $type:expr) => {
                #[test]
                fn $name() {
                    assert_encode_decode!(Graph::new(
                        Link::new($type, 0.0.into(), 0.0.into()).into()
                    ));
                }
            };
        }

        test_type!(zero, 0);
        test_type!(one, 1);
        test_type!(two, 2);
        test_type!(three, 3);
        test_type!(four, 4);
        test_type!(five, 5);
        test_type!(six, 6);
        test_type!(positive_integer, 42);
        test_type!(big_positive_integer, u64::MAX as _);
    }
}
