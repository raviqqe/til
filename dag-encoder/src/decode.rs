use crate::{
    Error, Graph, Node, Payload, FIXED_LINK_PAYLOAD_BASE, INTEGER_BASE, VARIADIC_LINK_PAYLOAD_BASE,
    VARIADIC_LINK_TYPE,
};
use alloc::rc::Rc;
use std::io::Read;

pub fn decode(mut reader: impl Read) -> Result<Graph, Error> {
    Ok(Graph::new(decode_nodes(&mut reader)?))
}

fn decode_nodes(reader: &mut impl Read) -> Result<Option<Rc<Node>>, Error> {
    let mut node = None;

    while let Some(byte) = decode_byte(reader)? {
        match (byte & 0b10 != 0, byte & 0b100 != 0) {
            (false, false) => {
                let payload = decode_integer_rest(byte >> 5, FIXED_LINK_PAYLOAD_BASE, reader)?;

                node = Some(
                    Node::Link {
                        r#type: ((byte >> 3) & 0b11) as usize,
                        payload: decode_payload(payload),
                        next: node,
                    }
                    .into(),
                );
            }
            (false, true) => {
                let r#type = decode_integer_rest(byte >> 3, VARIADIC_LINK_PAYLOAD_BASE, reader)?;
                let payload = decode_integer(reader)?;

                node = Some(
                    Node::Link {
                        r#type: r#type as usize + VARIADIC_LINK_TYPE,
                        payload: decode_payload(payload),
                        next: node,
                    }
                    .into(),
                );
            }
            (true, _) => {
                panic!("merge not supported")
            }
        }
    }

    Ok(node)
}

fn decode_payload(integer: u128) -> Payload {
    let number = integer >> 1;

    Payload::Number(if integer & 1 == 0 {
        number as _
    } else {
        panic!("non-positive integer not supported")
    })
}

fn decode_integer(reader: &mut impl Read) -> Result<u128, Error> {
    let byte = decode_byte(reader)?.ok_or_else(|| Error::EndOfStream)?;
    decode_integer_rest(byte, INTEGER_BASE, reader)
}

fn decode_integer_rest(rest: u8, base: u128, reader: &mut impl Read) -> Result<u128, Error> {
    let mut x = rest;
    let mut y = 0u128;

    while x & 1 != 0 {
        y *= INTEGER_BASE;
        x = decode_byte(reader)?.ok_or_else(|| Error::EndOfStream)?;
        y += x as u128 >> 1;
    }

    Ok(y * base + (rest as u128 >> 1))
}

fn decode_byte(reader: &mut impl Read) -> Result<Option<u8>, Error> {
    let mut buffer = [0u8];
    let count = reader.read(&mut buffer)?;

    Ok(if count == 0 { None } else { Some(buffer[0]) })
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn encode_empty() {
        assert_debug_snapshot!(decode([].as_slice()));
    }
}
