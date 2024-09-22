use crate::{
    Error, Graph, Node, Payload, FIXED_LINK_PAYLOAD_BASE, INTEGER_BASE, VARIADIC_LINK_PAYLOAD_BASE,
    VARIADIC_LINK_TYPE,
};
use std::io::Write;

pub fn encode(graph: &Graph, mut writer: impl Write) -> Result<(), Error> {
    let writer = &mut writer;
    let mut node = graph.root();

    while let Some(current) = node {
        match &**current {
            Node::Link(link) => {
                let r#type = link.r#type();
                let r#return = link.next().is_none() as u8;

                if r#type < VARIADIC_LINK_TYPE {
                    let integer = encode_integer_with_base(
                        encode_payload(link.payload()),
                        FIXED_LINK_PAYLOAD_BASE,
                        writer,
                    )?;

                    writer.write_all(&[(integer << 5) | ((r#type as u8) << 3) | r#return])?;
                } else {
                    let r#type = r#type - VARIADIC_LINK_TYPE;

                    encode_integer(encode_payload(link.payload()), writer)?;
                    let integer =
                        encode_integer_with_base(r#type as _, VARIADIC_LINK_PAYLOAD_BASE, writer)?;

                    writer.write_all(&[(integer << 3) | (1 << 2) | r#return])?;
                }

                node = link.next();
            }
            Node::Merge { .. } => {
                panic!("merge not supported")
            }
        }
    }

    Ok(())
}

fn encode_payload(payload: &Payload) -> u128 {
    match payload {
        Payload::Node(_node) => {
            panic!("node payload not supported")
        }
        &Payload::Number(number) => {
            if number.fract() != 0.0 {
                panic!("floating point number not supported")
            } else if number.is_sign_negative() {
                panic!("negative integer not supported")
            } else {
                (number as u128) << 1
            }
        }
    }
}

fn encode_integer(integer: u128, writer: &mut impl Write) -> Result<(), Error> {
    let byte = encode_integer_with_base(integer, INTEGER_BASE, writer)?;
    writer.write_all(&[byte])?;

    Ok(())
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
        assert_debug_snapshot!(encode_to_vec(&Graph::new(Some(
            Node::Link(Link::new(0, Payload::Number(0.0), None)).into()
        ))));
    }
}
