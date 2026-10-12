#![no_std]
#![no_main]

use core::{
    arch::{asm, naked_asm},
    panic::PanicInfo,
};
use rustix::{io::write, stdio::stdout};

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    exit(1)
}

#[unsafe(naked)]
#[unsafe(no_mangle)]
extern "C" fn _start() -> ! {
    naked_asm!("and rsp, -16", "call {}", "ud2", sym main)
}

extern "C" fn main() -> ! {
    let _ = write(unsafe { stdout() }, b"Hello, world!\n");

    exit(0)
}

fn exit(code: i32) -> ! {
    unsafe { asm!("syscall", in("rax") 231, in("rdi") code, options(noreturn)) }
}

// Referenced by the precompiled `core` in debug builds. Never called with `panic = "abort"`.
#[unsafe(no_mangle)]
extern "C" fn rust_eh_personality() {}
