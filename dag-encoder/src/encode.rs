use crate::{Error, Graph, Node, Share, INTEGER_BASE, SHARE_BASE, TYPE_BASE, VALUE_BASE};
use alloc::collections::VecDeque;
use std::io::Write;

pub fn encode(graph: &Graph, mut writer: impl Write) -> Result<(), Error> {
    encode_node(graph.root(), &mut Default::default(), &mut writer)
}

fn encode_node(
    node: &Node,
    dictionary: &mut VecDeque<Node>,
    writer: &mut impl Write,
) -> Result<(), Error> {
    match node {
        Node::Link(link) => {
            if let Some(share) = link.share() {
                if let Some(index) = dictionary.iter().position(|other| node == other) {
                    let node = dictionary.remove(index).ok_or(Error::MissingNode)?;
                    dictionary.push_front(node);

                    let (head, rest) = encode_integer_parts(
                        ((index << 1) + if share == Share::Single { 0 } else { 1 }) as _,
                        SHARE_BASE,
                    );

                    writer.write_all(&[(head + 1) << 2 | 0b11])?;
                    encode_integer_rest(rest, writer)?;
                    return Ok(());
                }
            }

            encode_node(link.right(), dictionary, writer)?;
            encode_node(link.left(), dictionary, writer)?;

            let (head, rest) = encode_integer_parts(link.r#type() as _, TYPE_BASE);

            writer.write_all(&[head << 2 | 1])?;
            encode_integer_rest(rest, writer)?;

            if link.share().is_some() {
                dictionary.push_front(node.clone());
                writer.write_all(&[0b11])?;
            }
        }
        Node::Value(value) => {
            let (head, rest) = encode_integer_parts(encode_value(*value), VALUE_BASE);

            writer.write_all(&[head << 1])?;
            encode_integer_rest(rest, writer)?;

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

const fn encode_integer_parts(integer: u128, base: u128) -> (u8, u128) {
    let rest = integer / base;

    (
        encode_integer_part(integer, base, if rest == 0 { 0 } else { 1 }),
        rest,
    )
}

fn encode_integer_rest(mut integer: u128, writer: &mut impl Write) -> Result<(), Error> {
    while integer != 0 {
        let rest = integer / INTEGER_BASE;
        writer.write_all(&[encode_integer_part(
            integer,
            INTEGER_BASE,
            if rest == 0 { 0 } else { 1 },
        )])?;
        integer = rest;
    }

    Ok(())
}

const fn encode_integer_part(integer: u128, base: u128, bit: u128) -> u8 {
    (((integer % base) << 1) | bit) as u8
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Link, Share};
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
            Link::new(0, 0.0.into(), 0.0.into(), None).into()
        )));
    }

    #[test]
    fn encode_unique_node() {
        let node = Node::Link(Link::new(0, 0.0.into(), 0.0.into(), Share::Multiple.into()).into());

        assert_debug_snapshot!(encode_to_vec(&Graph::new(
            Link::new(0, node.clone(), node, None).into()
        )));
    }

    mod left_value {
        use super::*;

        macro_rules! encode_left_value {
            ($name:ident, $value:expr) => {
                #[test]
                fn $name() {
                    assert_debug_snapshot!(encode_to_vec(&Graph::new(
                        Link::new(0, $value.into(), 0.0.into(), None).into(),
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
                        Link::new(0, 0.0.into(), $value.into(), None).into(),
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
