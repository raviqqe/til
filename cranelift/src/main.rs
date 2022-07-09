use cranelift::codegen::isa;
use cranelift::codegen::settings::{self, Flags};
use cranelift::prelude::Configurable;
use cranelift_object::ObjectBuilder;
use std::error::Error;

fn main() {
    run().unwrap();
}

fn run() -> Result<(), Box<dyn Error>> {
    let mut builder = settings::builder();

    builder.set("opt_level", "speed_and_size")?;

    let flags = Flags::new(builder);
    let builder = isa::lookup_by_name("x86_64-unknown-linux-gnu")?;

    builder.finish(flags)?;
    // ObjectBuilder::new("foo");

    Ok(())
}
