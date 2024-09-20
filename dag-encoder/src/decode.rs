use crate::{Error, Graph};
use std::io::Read;

pub fn decode(input: impl Read) -> Result<Graph, Error> {
    Ok(Graph::default())
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
