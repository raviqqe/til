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
                writer.write_all(&[integer << 1])?;
                encode_node(link.left(), writer)?;

                node = link.right();
            }
            Node::Value(value) => {
                let integer = encode_integer_with_base(encode_value(*value), VALUE_BASE, writer)?;
                writer.write_all(&[(integer << 1) | 1])?;
                return Ok(());
            }
        };
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
}
