use crate::{Error, Graph, Link, Node, INTEGER_BASE, SHARE_BASE, TYPE_BASE, VALUE_BASE};
use alloc::collections::VecDeque;
use std::io::Read;

pub fn decode(mut reader: impl Read) -> Result<Graph, Error> {
    Ok(Graph::new(decode_nodes(&mut reader)?))
}

fn decode_nodes(reader: &mut impl Read) -> Result<Node, Error> {
    let mut dictionary = VecDeque::new();
    let mut nodes = vec![];

    while let Some(head) = decode_byte(reader)? {
        if head & 1 == 0 {
            nodes.push(Node::Value(decode_value(decode_integer_tail(
                head >> 1,
                VALUE_BASE,
                reader,
            )?)));
        } else if head & 0b10 == 0 {
            let left = nodes.pop().ok_or(Error::MissingNode)?;
            let right = nodes.pop().ok_or(Error::MissingNode)?;
            let r#type = decode_integer_tail(head >> 2, TYPE_BASE, reader)?;
            nodes.push(Link::new(r#type as usize, left, right, None).into());
        } else {
            let head = head >> 2;

            if head == 0 {
                dictionary.push_front(nodes.last().ok_or(Error::MissingNode)?.clone());
            } else {
                let integer = decode_integer_tail(head - 1, SHARE_BASE, reader)?;
                let node = dictionary
                    .remove((integer >> 1) as _)
                    .ok_or(Error::MissingNode)?;
                if integer & 1 != 0 {
                    dictionary.push_front(node.clone());
                }
                nodes.push(node);
            }
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

fn decode_integer_tail(mut x: u8, mut base: u128, reader: &mut impl Read) -> Result<u128, Error> {
    let mut y = (x >> 1) as u128;

    while x & 1 != 0 {
        x = decode_byte(reader)?.ok_or(Error::EndOfStream)?;
        y += (x as u128 >> 1) * base;
        base *= INTEGER_BASE;
    }

    Ok(y)
}

fn decode_byte(reader: &mut impl Read) -> Result<Option<u8>, Error> {
    let mut buffer = [0u8];
    let count = reader.read(&mut buffer)?;

    Ok(if count == 0 { None } else { Some(buffer[0]) })
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn decode_empty() {
        assert_eq!(decode([0].as_slice()).unwrap(), Graph::new(0.0.into()));
    }
}
