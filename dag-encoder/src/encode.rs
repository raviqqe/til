use crate::{Error, Graph};
use std::io::Write;

pub fn encode(_graph: &Graph, _writer: &mut impl Write) -> Result<(), Error> {
    Ok(())
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
