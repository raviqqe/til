#![no_std]
#![no_main]

use core::arch::{asm, naked_asm};
use rustix::{io::write, stdio::stdout};

#[cfg(not(test))]
#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    exit(1)
}

#[unsafe(naked)]
#[unsafe(no_mangle)]
extern "C" fn _start() -> ! {
    cfg_select! {
        target_arch = "x86_64" => {
            naked_asm!("and rsp, -16", "call {}", "ud2", sym main)
        }
        target_arch = "aarch64" => {
            naked_asm!("mov x9, sp", "and sp, x9, -16", "bl {}", "udf 0", sym main)
        }
    }
}

extern "C" fn main() -> ! {
    let _ = write(unsafe { stdout() }, b"Hello, world!\n");

    exit(0)
}

fn exit(code: i32) -> ! {
    cfg_select! {
        target_arch = "x86_64" => {
            // cspell: disable-next-line
            unsafe { asm!("syscall", in("rax") 231, in("rdi") code, options(noreturn)) }
        }
        target_arch = "aarch64" => {
            unsafe { asm!("svc 0", in("x8") 94, in("x0") code, options(noreturn)) }
        }
    }
}

// Referenced by the precompiled `core` in debug builds. Never called with `panic = "abort"`.
#[unsafe(no_mangle)]
extern "C" fn rust_eh_personality() {}
