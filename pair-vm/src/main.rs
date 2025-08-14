mod vm;

use crate::vm::Vm;

fn main() {
    let mut vm = Vm::new(vec![0; 1024]);

    vm.run();
}
