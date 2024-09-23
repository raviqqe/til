use crate::{Error, Graph, Node, INTEGER_BASE, TYPE_BASE, VALUE_BASE};
use std::io::Write;

pub fn encode(graph: &Graph, mut writer: impl Write) -> Result<(), Error> {
    encode_node(graph.root(), &mut writer)
}

fn encode_node(node: &Node, writer: &mut impl Write) -> Result<(), Error> {
    let mut node = node;

    loop {
        match node {
            Node::Link(link) => {
                let integer = encode_integer_with_base(link.r#type() as _, TYPE_BASE, writer)?;
                writer.write_all(&[integer << 1 | 1])?;
                encode_node(link.left(), writer)?;

                node = link.right();
            }
            Node::Value(value) => {
                let integer = encode_integer_with_base(encode_value(*value), VALUE_BASE, writer)?;
                writer.write_all(&[(integer << 1)])?;
                return Ok(());
            }
        };
    }
}

fn find_common_branch(left: &Node, right: Node) -> Option<&Node> {
    match (left, right) {
        (Node::Link(left), Node::Link(right)) => {
            if left.r#type() == right.r#type() {
                find_common_branch(left.right(), right.right())
            } else {
                Foo::Link(link)
            }
        }
        (left, right) => (left == right).then_some(left),
    }
}

fn encode_value(value: f64) -> u128 {
    if value.fract() != 0.0 {
        panic!("floating point value not supported")
    } else if value.is_sign_negative() {
        panic!("negative integer not supported")
    } else {
        (value as u128) << 1
    }
}

fn encode_integer_with_base(
    integer: u128,
    base: u128,
    writer: &mut impl Write,
) -> Result<u8, Error> {
    let mut rest = integer / base;
    let mut bit = 0;

    while rest != 0 {
        writer.write_all(&[encode_integer_part(rest, INTEGER_BASE, bit)])?;
        bit = 1;
        rest /= INTEGER_BASE;
    }

    Ok(encode_integer_part(integer, base, bit))
}

const fn encode_integer_part(integer: u128, base: u128, bit: u128) -> u8 {
    (((integer % base) << 1) | bit) as u8
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Link;
    use insta::assert_debug_snapshot;

    fn encode_to_vec(graph: &Graph) -> Vec<u8> {
        let mut buffer = vec![];

        encode(graph, &mut buffer).unwrap();

        buffer
    }

    #[test]
    fn encode_empty() {
        assert_debug_snapshot!(encode_to_vec(&Graph::default()));
    }

    #[test]
    fn encode_node() {
        assert_debug_snapshot!(encode_to_vec(&Graph::new(
            Link::new(0, 0.0.into(), 0.0.into()).into()
        )));
    }

    mod left_value {
        use super::*;

        macro_rules! encode_left_value {
            ($name:ident, $value:expr) => {
                #[test]
                fn $name() {
                    assert_debug_snapshot!(encode_to_vec(&Graph::new(
                        Link::new(0, $value.into(), 0.0.into()).into(),
                    )));
                }
            };
        }

        encode_left_value!(zero, 0.0);
        encode_left_value!(one, 1.0);
        encode_left_value!(two, 2.0);
        encode_left_value!(positive, 42.0);
        encode_left_value!(big_positive, u32::MAX as f64);
    }

    mod right_value {
        use super::*;

        macro_rules! encode_right_value {
            ($name:ident, $value:expr) => {
                #[test]
                fn $name() {
                    assert_debug_snapshot!(encode_to_vec(&Graph::new(
                        Link::new(0, 0.0.into(), $value.into()).into(),
                    )));
                }
            };
        }

        encode_right_value!(zero, 0.0);
        encode_right_value!(one, 1.0);
        encode_right_value!(two, 2.0);
        encode_right_value!(positive, 42.0);
        encode_right_value!(big_positive, u32::MAX as f64);
    }
}
