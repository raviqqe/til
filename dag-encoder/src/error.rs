use std::error;
use core::fmt::Display;
use core::fmt::{self, Formatter};
use std::io;

#[derive(Debug)]
pub enum Error {
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
            Self::Io(error) => write!(formatter, "{error}"),
        }
    }
}
