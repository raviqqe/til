use cranelift::codegen::isa;
use cranelift::codegen::settings::{self, Flags};
use cranelift::prelude::Configurable;
use cranelift_module::default_libcall_names;
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::error::Error;
use std::fs::File;
use std::io::Write;

fn main() {
    run().unwrap();
}

fn run() -> Result<(), Box<dyn Error>> {
    let module = ObjectModule::new(ObjectBuilder::new(
        isa::lookup_by_name("x86_64-unknown-linux-gnu")?.finish({
            let mut builder = settings::builder();

            builder.set("enable_atomics", "true")?;
            builder.set("enable_jump_tables", "true")?;
            builder.set("enable_nan_canonicalization", "true")?;
            builder.set("enable_simd", "true")?;
            builder.set("enable_verifier", "true")?;
            builder.set("is_pic", "true")?;
            builder.set("machine_code_cfg_info", "false")?;
            builder.set("opt_level", "speed_and_size")?;
            builder.set("unwind_info", "false")?;

            Flags::new(builder)
        })?,
        "main",
        default_libcall_names(),
    )?);

    File::options()
        .create(true)
        .truncate(true)
        .write(true)
        .open("main.o")?
        .write_all(&module.finish().emit()?)?;

    Ok(())
}
