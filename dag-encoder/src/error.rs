use core::fmt::{self, Display, Formatter};
use std::{error, io};

#[derive(Debug)]
pub enum Error {
    EndOfStream,
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self::Io(error)
    }
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::EndOfStream => write!(formatter, "end of stream"),
            Self::Io(error) => write!(formatter, "{error}"),
        }
    }
}
