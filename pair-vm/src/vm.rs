use core::convert::{AsMut, AsRef};

pub struct Vm<T: AsRef<[u64]> + AsMut<[u64]>> {
    register: u64,
    heap: T,
}

impl<T: AsRef<[u64]> + AsMut<[u64]>> Vm<T> {
    pub fn run(&mut self) {
        foo;
    }
}
