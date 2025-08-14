use core::convert::{AsMut, AsRef};

pub struct Vm<T: AsRef<[u64]> + AsMut<[u64]>> {
    #[expect(dead_code)]
    register: u64,
    #[expect(dead_code)]
    heap: T,
}

impl<T: AsRef<[u64]> + AsMut<[u64]>> Vm<T> {
    pub fn new(heap: T) -> Self {
        Vm { register: 0, heap }
    }

    pub fn run(&mut self) -> Result<(), &'static str> {
        Ok(())
    }
}
