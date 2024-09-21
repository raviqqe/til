use crate::{Error, Graph, Node, Payload, INTEGER_BASE, VARIADIC_LINK_TYPE};
use std::io::Write;

pub fn encode(graph: &Graph, mut writer: impl Write) -> Result<(), Error> {
    let writer = &mut writer;
    let mut node = graph.root();

    while let Some(current) = node {
        match &**current {
            Node::Link {
                r#type,
                payload,
                next,
            } => {
                let r#type = *r#type;
                let r#return = (next.is_none() as u8) << 1;

                if r#type < VARIADIC_LINK_TYPE {
                    let integer =
                        encode_integer_with_base(encode_payload(payload), 1 << 3, writer)?;

                    writer.write_all(&[(integer << 5) | ((r#type as u8) << 3) | r#return])?;
                } else {
                    let r#type = r#type - VARIADIC_LINK_TYPE;

                    encode_integer(encode_payload(payload), writer)?;
                    let integer = encode_integer_with_base(r#type as _, 1 << 5, writer)?;

                    writer.write_all(&[(integer << 3) | (1 << 2) | r#return])?;
                }

                node = next.as_ref();
            }
            Node::Merge { .. } => {
                unimplemented!()
            }
        }
    }

    Ok(())
}

fn encode_payload(payload: &Payload) -> u128 {
    match payload {
        Payload::Number(number) => {
            if number.fract() != 0.0 {
                unimplemented!()
            } else if number.is_sign_negative() {
                unimplemented!()
            } else {
                *number as _
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
            Node::Link {
                r#type: 0,
                payload: Payload::Number(0.0),
                next: None
            }
            .into()
        ))));
    }
}
