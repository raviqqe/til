use core::convert::{AsMut, AsRef};

pub struct Vm<M: AsRef<[u64]> + AsMut<[u64]>> {
    memory: M,
}

impl<M: AsRef<[u64]> + AsMut<[u64]> Vm<M> {
    fn foo() {}
}
