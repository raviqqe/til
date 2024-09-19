mod decoder;

use crate::{decode::decoder::Decoder, Error, Graph};

/// Decodes a program.
pub fn decode(codes: &[u8]) -> Result<Graph, Error> {
    Decoder::new(codes).decode()
}
