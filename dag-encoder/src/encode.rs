use crate::{Error, Graph, Node, Payload, INTEGER_BASE, SHORT_INTEGER_BASE};
use std::io::Write;

pub fn encode(graph: &Graph, writer: &mut impl Write) -> Result<(), Error> {
    let node = graph.root();

    while let Some(node) = node {
        match &**node {
            Node::Link {
                r#type,
                payload,
                next,
            } => {
                let integer = encode_payload(codes, writer);

                codes.push(
                    (integer << INSTRUCTION_BITS)
                        | (instruction << 1)
                        | if next.is_some() { 0 } else { 1 },
                );
            }
            Node::Merge { left, right } => {}
        }
    }

    Ok(())
}

fn encode_payload(payload: &Payload, writer: &mut impl Write) -> Result<(), Error> {
    match payload {
        Payload::Number(&number) => {
            if number.fract() == 0.0 {
                let byte = encode_integer_with_base(codes, integer, INTEGER_BASE);
                codes.push(byte);
            } else {
                todo!()
            }
        }
    }

    Ok(())
}

fn encode_short_integer(codes: &mut Vec<u8>, integer: u64) -> u8 {
    encode_integer_with_base(codes, integer, SHORT_INTEGER_BASE)
}

fn encode_integer_with_base(integer: u64, base: u64, writer: &mut impl Write) -> u8 {
    let mut x = integer / base;
    let mut bit = 0;

    while x != 0 {
        writer.write(&[encode_integer_part(x, INTEGER_BASE, bit)]);
        bit = 1;
        x /= INTEGER_BASE;
    }

    encode_integer_part(integer, base, bit)
}

const fn encode_integer_part(integer: u64, base: u64, bit: u64) -> u8 {
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
}
