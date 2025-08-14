mod vm;

use crate::vm::Vm;

fn main() {
    let mut vm = Vm::new();

    vm.run();
}
