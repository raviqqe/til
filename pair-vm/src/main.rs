mod vm;

use crate::vm::Vm;
use core::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut vm = Vm::new(vec![0; 1024]);

    vm.run()?;

    Ok(())
}
