use crate::{Error, Graph, Link, Node, INTEGER_BASE, TYPE_BASE, VALUE_BASE};
use std::io::Read;

pub fn decode(mut reader: impl Read) -> Result<Graph, Error> {
    Ok(Graph::new(decode_nodes(&mut reader)?))
}

fn decode_nodes(reader: &mut impl Read) -> Result<Node, Error> {
    let mut nodes = vec![];

    while let Some(byte) = decode_byte(reader)? {
        if byte & 1 == 0 {
            let left = nodes.pop().ok_or(Error::MissingNode)?;
            let right = nodes.pop().ok_or(Error::MissingNode)?;
            let r#type = decode_integer_rest(byte >> 3, TYPE_BASE, reader)?;
            nodes.push(Link::new(r#type as usize, left, right).into());
        } else {
            nodes.push(Node::Value(decode_value(decode_integer_rest(
                byte >> 1,
                VALUE_BASE,
                reader,
            )?)));
        }
    }

    nodes.pop().ok_or(Error::MissingNode)
}

fn decode_value(integer: u128) -> f64 {
    let number = integer >> 1;

    if integer & 1 == 0 {
        number as _
    } else {
        panic!("non-positive integer not supported")
    }
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
    fn decode_empty() {
        assert_debug_snapshot!(decode([1].as_slice()));
    }
}
