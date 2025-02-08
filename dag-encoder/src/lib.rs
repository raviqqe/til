extern crate alloc;

mod decode;
mod encode;
mod error;
mod graph;
mod link;
mod node;
mod share;

pub use decode::decode;
pub use encode::encode;
pub use error::Error;
pub use graph::Graph;
pub use link::Link;
pub use node::Node;
pub use share::Share;

const INTEGER_BASE: u128 = 1 << 7;
const VALUE_BASE: u128 = 1 << 6;
const TYPE_BASE: u128 = 1 << 5;
const SHARE_BASE: u128 = (1 << 5) - 1;

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    macro_rules! assert_encode_decode {
        ($graph:expr) => {
            let graph = $graph;
            let mut buffer = vec![];

            encode(&graph, &mut buffer).unwrap();

            assert_eq!(&decode(&*buffer).unwrap(), &graph);
        };
    }

    #[test]
    fn encode_empty() {
        assert_encode_decode!(Graph::default());
    }

    #[test]
    fn encode_node() {
        assert_encode_decode!(Graph::new(
            Link::new(0, 0.0.into(), 0.0.into(), None).into()
        ));
    }

    #[test]
    fn encode_two_nodes() {
        assert_encode_decode!(Graph::new(
            Link::new(
                0,
                0.0.into(),
                Link::new(0, 0.0.into(), 0.0.into(), None).into(),
                None,
            )
            .into()
        ));
    }

    #[test]
    fn encode_three_nodes() {
        assert_encode_decode!(Graph::new(
            Link::new(
                0,
                0.0.into(),
                Link::new(
                    0,
                    0.0.into(),
                    Link::new(0, 0.0.into(), 0.0.into(), None).into(),
                    None,
                )
                .into(),
                None,
            )
            .into()
        ));
    }

    #[test]
    fn decode_singly_shared_nodes_between_multiply_shared_nodes() {
        let nodes = [
            Node::Link(Link::new(0, 0.0.into(), 0.0.into(), Share::Single.into()).into()),
            Link::new(1, 0.0.into(), 0.0.into(), Share::Multiple.into()).into(),
        ];

        assert_encode_decode!(Graph::new(
            Link::new(
                0,
                Link::new(0, nodes[1].clone(), nodes[0].clone(), None).into(),
                Link::new(0, nodes[0].clone(), nodes[1].clone(), None).into(),
                None
            )
            .into()
        ));
    }

    #[test]
    fn decode_multiple_node_multiple_times() {
        let node = Node::Link(Link::new(0, 0.0.into(), 0.0.into(), Share::Multiple.into()).into());

        assert_encode_decode!(Graph::new(
            Link::new(
                0,
                Link::new(0, node.clone(), node.clone(), None).into(),
                node,
                None
            )
            .into()
        ));
    }

    #[test]
    fn decode_complex_graph() {
        let node0 = Node::Link(Link::new(1, 2.0.into(), 3.0.into(), Share::Multiple.into()).into());
        let node1 =
            Node::Link(Link::new(2, node0.clone(), node0.clone(), Share::Single.into()).into());
        let node2 = Node::Link(Link::new(3, node1.clone(), node0, Share::Multiple.into()).into());
        let node3 = Node::Link(Link::new(4, Node::Value(42.0), node1, None).into());

        assert_encode_decode!(Graph::new(
            Link::new(
                0,
                Link::new(0, node2, Node::Value(2045.0), None).into(),
                node3,
                None
            )
            .into()
        ));
    }

    mod left_value {
        use super::*;
        use pretty_assertions::assert_eq;

        macro_rules! test_value {
            ($name:ident, $value:expr) => {
                #[test]
                fn $name() {
                    assert_encode_decode!(Graph::new(
                        Link::new(0, $value.into(), 0.0.into(), None).into()
                    ));
                }
            };
        }

        test_value!(zero, 0.0);
        test_value!(one, 1.0);
        test_value!(two, 2.0);
        test_value!(positive_integer, 42.0);
        test_value!(big_positive_integer, u32::MAX as f64);
    }

    mod right_value {
        use super::*;
        use pretty_assertions::assert_eq;

        macro_rules! test_value {
            ($name:ident, $value:expr) => {
                #[test]
                fn $name() {
                    assert_encode_decode!(Graph::new(Node::Value($value)));
                }
            };
        }

        test_value!(zero, 0.0);
        test_value!(one, 1.0);
        test_value!(two, 2.0);
        test_value!(positive_integer, 42.0);
        test_value!(big_positive_integer, u32::MAX as f64);
    }

    mod link_type {
        use super::*;
        use pretty_assertions::assert_eq;

        macro_rules! test_type {
            ($name:ident, $type:expr) => {
                #[test]
                fn $name() {
                    assert_encode_decode!(Graph::new(
                        Link::new($type, 0.0.into(), 0.0.into(), None).into()
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
        test_type!(big_positive_integer, u32::MAX as _);
    }

    mod unique {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn encode_one_node() {
            let node =
                Node::Link(Link::new(1, 0.0.into(), 0.0.into(), Share::Single.into()).into());

            assert_encode_decode!(Graph::new(Link::new(0, node.clone(), node, None).into()));
        }

        #[test]
        fn encode_two_nodes() {
            let node =
                Node::Link(Link::new(1, 0.0.into(), 0.0.into(), Share::Single.into()).into());
            let node = Node::Link(Link::new(2, node.clone(), node, Share::Single.into()).into());

            assert_encode_decode!(Graph::new(Link::new(0, node.clone(), node, None).into()));
        }

        #[test]
        fn encode_three_nodes() {
            let node =
                Node::Link(Link::new(1, 0.0.into(), 0.0.into(), Share::Single.into()).into());
            let node = Node::Link(Link::new(2, node.clone(), node, Share::Single.into()).into());
            let node = Node::Link(Link::new(3, node.clone(), node, Share::Single.into()).into());

            assert_encode_decode!(Graph::new(Link::new(0, node.clone(), node, None).into()));
        }
    }
}
