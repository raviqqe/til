use crate::{Error, Graph};

pub fn decode(_codes: &[u8]) -> Result<Graph, Error> {
    Ok(Graph::default())
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn encode_empty() {
        assert_debug_snapshot!(decode(&[]));
    }
}
