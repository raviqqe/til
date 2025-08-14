use core::convert::{AsMut, AsRef};

#[expect(dead_code)]
pub struct Vm<T: AsRef<[u64]> + AsMut<[u64]>> {
    register: u64,
    heap: T,
}

impl<T: AsRef<[u64]> + AsMut<[u64]>> Vm<T> {
    pub fn new(heap: T) -> Self {
        Vm { register: 0, heap }
    }

    pub fn run(&mut self) -> Result<(), &'static str> {
        while let Some(cons) = self.get().to_cons() {}

        Ok(())
    }
}
