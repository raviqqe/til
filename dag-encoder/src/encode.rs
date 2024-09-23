use crate::{Error, Graph, Node, INTEGER_BASE, SHARE_BASE, TYPE_BASE, VALUE_BASE};
use std::io::Write;

pub fn encode(graph: &Graph, mut writer: impl Write) -> Result<(), Error> {
    encode_node(graph.root(), &mut vec![], &mut writer)
}

fn encode_node(
    node: &Node,
    dictionary: &mut Vec<Node>,
    writer: &mut impl Write,
) -> Result<(), Error> {
    match node {
        Node::Link(link) => {
            if link.unique() {
                if let Some(index) = dictionary.iter().position(|other| node == other) {
                    let node = dictionary.remove(index);
                    dictionary.push(node);

                    let index = index as u128;
                    let rest = index / SHARE_BASE;

                    writer.write_all(&[encode_integer_part(
                        index,
                        SHARE_BASE,
                        if rest == 0 { 0 } else { 1 },
                    ) << 2
                        | 0b11])?;
                    encode_integer(rest, writer)?;
                    return Ok(());
                } else {
                    dictionary.push(node.clone());
                    writer.write_all(&[0b11])?;
                }
            }

            encode_node(link.right(), dictionary, writer)?;
            encode_node(link.left(), dictionary, writer)?;

            let value = link.r#type() as u128;
            let rest = value / TYPE_BASE;

            writer.write_all(&[encode_integer_part(
                value,
                TYPE_BASE,
                if rest == 0 { 0 } else { 1 },
            ) << 1])?;
            encode_integer(rest, writer)?;
        }
        Node::Value(value) => {
            let value = encode_value(*value);
            let rest = value / VALUE_BASE;

            writer.write_all(&[encode_integer_part(
                value,
                VALUE_BASE,
                if rest == 0 { 0 } else { 1 },
            ) << 1])?;
            encode_integer(rest, writer)?;

            return Ok(());
        }
    }

    Ok(())
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

fn encode_integer(mut integer: u128, writer: &mut impl Write) -> Result<(), Error> {
    while integer != 0 {
        writer.write_all(&[encode_integer_part(integer, INTEGER_BASE, 1)])?;
        integer /= INTEGER_BASE;
    }

    Ok(())
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
            Link::new(0, 0.0.into(), 0.0.into(), false).into()
        )));
    }

    #[test]
    fn encode_unique_node() {
        let node = Node::Link(Link::new(0, 0.0.into(), 0.0.into(), true).into());

        assert_debug_snapshot!(encode_to_vec(&Graph::new(
            Link::new(0, node.clone(), node, false).into()
        )));
    }

    mod left_value {
        use super::*;

        macro_rules! encode_left_value {
            ($name:ident, $value:expr) => {
                #[test]
                fn $name() {
                    assert_debug_snapshot!(encode_to_vec(&Graph::new(
                        Link::new(0, $value.into(), 0.0.into(), false).into(),
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
                        Link::new(0, 0.0.into(), $value.into(), false).into(),
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
