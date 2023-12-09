use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::{types, Function};
use cranelift::codegen::ir::{AbiParam, InstBuilder, Signature};
use cranelift::codegen::isa;
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::settings::{self, Flags};
use cranelift::codegen::verifier::verify_function;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift::prelude::Configurable;
use cranelift_module::{default_libcall_names, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::error::Error;
use std::fs::File;
use std::io::Write;

const MODULE_NAME: &str = "foo";

fn main() {
    run().unwrap();
}

fn run() -> Result<(), Box<dyn Error>> {
    let flags = {
        let mut builder = settings::builder();

        builder.set("enable_atomics", "true")?;
        builder.set("enable_jump_tables", "true")?;
        builder.set("enable_nan_canonicalization", "true")?;
        builder.set("enable_verifier", "true")?;
        builder.set("is_pic", "true")?;
        builder.set("machine_code_cfg_info", "false")?;
        builder.set("opt_level", "speed_and_size")?;
        builder.set("unwind_info", "false")?;

        Flags::new(builder)
    };
    let mut module = ObjectModule::new(ObjectBuilder::new(
        isa::lookup_by_name("x86_64-unknown-linux-gnu")?.finish(flags)?,
        MODULE_NAME,
        default_libcall_names(),
    )?);

    define_function(&mut module)?;

    File::options()
        .create(true)
        .truncate(true)
        .write(true)
        .open(MODULE_NAME.to_owned() + ".o")?
        .write_all(&module.finish().emit()?)?;

    Ok(())
}

fn define_function(module: &mut ObjectModule) -> Result<(), Box<dyn Error>> {
    let mut context = module.make_context();

    compile_function(&mut context.func)?;

    let function_id = module.declare_function("foo", Linkage::Local, &context.func.signature)?;
    module.define_function(function_id, &mut context)?;

    Ok(())
}

fn compile_function(function: &mut Function) -> Result<(), Box<dyn Error>> {
    function.signature = {
        let mut signature = Signature::new(CallConv::SystemV);
        signature.returns.push(AbiParam::new(types::I32));
        signature.params.push(AbiParam::new(types::I32));
        signature
    };

    let mut context = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(function, &mut context);

    let block0 = builder.create_block();
    let block1 = builder.create_block();
    let block2 = builder.create_block();
    let block3 = builder.create_block();

    let x = Variable::new(0);
    let y = Variable::new(1);
    let z = Variable::new(2);

    builder.declare_var(x, types::I32);
    builder.declare_var(y, types::I32);
    builder.declare_var(z, types::I32);
    builder.append_block_params_for_function_params(block0);

    builder.switch_to_block(block0);
    builder.seal_block(block0);

    builder.def_var(x, builder.block_params(block0)[0]);
    {
        let tmp = builder.ins().iconst(types::I32, 2);
        builder.def_var(y, tmp);
    }
    {
        let arg1 = builder.use_var(x);
        let arg2 = builder.use_var(y);
        let tmp = builder.ins().iadd(arg1, arg2);
        builder.def_var(z, tmp);
    }
    builder.ins().jump(block1, &[]);

    builder.switch_to_block(block1);
    {
        let arg1 = builder.use_var(y);
        let arg2 = builder.use_var(z);
        let tmp = builder.ins().iadd(arg1, arg2);
        builder.def_var(z, tmp);
    }
    {
        let arg = builder.use_var(y);
        builder.ins().brif(arg, block3, &[], block2, &[]);
    }

    builder.switch_to_block(block2);
    builder.seal_block(block2);

    let arg1 = builder.use_var(z);
    let arg2 = builder.use_var(x);
    let tmp = builder.ins().isub(arg1, arg2);
    builder.def_var(z, tmp);

    let arg = builder.use_var(y);
    builder.ins().return_(&[arg]);

    builder.switch_to_block(block3);
    builder.seal_block(block3);

    {
        let arg1 = builder.use_var(y);
        let arg2 = builder.use_var(x);
        let tmp = builder.ins().isub(arg1, arg2);
        builder.def_var(y, tmp);
    }
    builder.ins().jump(block1, &[]);
    builder.seal_block(block1);

    builder.finalize();

    verify_function(function, &Flags::new(settings::builder()))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_() {
        run().unwrap();
    }
}
