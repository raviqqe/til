#![no_std]
#![no_main]

use rustix::{io::write, stdio::stdout};

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    exit(1)
}

#[unsafe(naked)]
#[unsafe(no_mangle)]
extern "C" fn _start() -> ! {
    core::arch::naked_asm!("and rsp, -16", "call {}", "ud2", sym main)
}

extern "C" fn main() -> ! {
    let _ = write(unsafe { stdout() }, b"Hello, world!\n");
    exit(0)
}

// rustix 1.x doesn't expose exit, so make the exit_group syscall directly.
fn exit(code: i32) -> ! {
    unsafe { core::arch::asm!("syscall", in("rax") 231, in("rdi") code, options(noreturn)) }
}

// Referenced by the precompiled `core` in debug builds; never called with panic = "abort".
#[unsafe(no_mangle)]
extern "C" fn rust_eh_personality() {}
