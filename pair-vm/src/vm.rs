use core::ops::{Deref, DerefMut};

type Heap = [u64];

pub struct Vm<T: Deref<Target = Heap> + DerefMut<Target = Heap>> {
    register: u64,
    heap: T,
}

impl<T: Deref<Target = Heap> + DerefMut<Target = Heap>> Vm<T> {
    pub const fn new(heap: T) -> Self {
        Self { register: 0, heap }
    }

    pub fn run(&mut self) -> Result<(), &'static str> {
        while let Some(value) = self.heap.get(self.register as usize) {
            println!("{value}");
        }

        Ok(())
    }
}
