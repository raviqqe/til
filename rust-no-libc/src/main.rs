#![no_std]
#![no_main]

use core::arch::asm;
use core::panic::PanicInfo;

// cspell: ignore syscall lateout nostack

#[panic_handler]
fn panic(_: &PanicInfo) -> ! {
    exit(101)
}

fn write(fd: usize, buf: &[u8]) -> isize {
    let ret: isize;
    unsafe {
        asm!(
            "syscall",
            inlateout("rax") 1isize => ret, // SYS_write
            in("rdi") fd,
            in("rsi") buf.as_ptr(),
            in("rdx") buf.len(),
            lateout("rcx") _,
            lateout("r11") _,
            options(nostack),
        );
    }
    ret
}

fn exit(code: i32) -> ! {
    unsafe {
        asm!(
            "syscall",
            in("rax") 60, // SYS_exit
            in("rdi") code,
            options(noreturn, nostack),
        );
    }
}

// The kernel jumps here with the stack 16-byte aligned and argc/argv/envp on it.
// Naked so we can pass the stack pointer along and fix up alignment ourselves.
#[unsafe(naked)]
#[unsafe(no_mangle)]
pub extern "C" fn _start() -> ! {
    core::arch::naked_asm!(
        "xor ebp, ebp",   // mark the outermost frame
        "mov rdi, rsp",   // first argument: pointer to argc
        "and rsp, -16",   // align the stack as the ABI expects before a call
        "call {main}",
        "ud2",
        main = sym main,
    )
}

extern "C" fn main(stack: *const usize) -> ! {
    let argc = unsafe { *stack };
    let mut buf = [0u8; 4096];
    let msg = b"Hello without std or libc! argc = ";
    buf[..msg.len()].copy_from_slice(msg);
    let mut n = msg.len();
    buf[n] = b'0' + (argc % 10) as u8;
    buf[n + 1] = b'\n';
    n += 2;

    write(1, &buf[..n]);
    exit(0)
}

// Only referenced by the precompiled `core` in unoptimized builds; never called with panic = "abort".
#[unsafe(no_mangle)]
pub extern "C" fn rust_eh_personality() {}

// The compiler may emit calls to these; without libc we must provide them.
// Written with `rep` string instructions so LLVM can't turn them back into
// calls to themselves.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn memset(dst: *mut u8, c: i32, n: usize) -> *mut u8 {
    unsafe {
        asm!("rep stosb", inout("rcx") n => _, inout("rdi") dst => _, in("al") c as u8,
             options(nostack, preserves_flags));
    }
    dst
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn memcpy(dst: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    unsafe {
        asm!("rep movsb", inout("rcx") n => _, inout("rdi") dst => _, inout("rsi") src => _,
             options(nostack, preserves_flags));
    }
    dst
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn memmove(dst: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    unsafe {
        if (dst as usize) <= (src as usize) || (dst as usize) >= (src as usize) + n {
            memcpy(dst, src, n)
        } else {
            // Overlapping, copy backwards.
            asm!("std", "rep movsb", "cld",
                 inout("rcx") n => _, inout("rdi") dst.add(n - 1) => _, inout("rsi") src.add(n - 1) => _,
                 options(nostack));
            dst
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn memcmp(a: *const u8, b: *const u8, n: usize) -> i32 {
    let mut i = 0;
    while i < n {
        let (x, y) = unsafe {
            (
                core::ptr::read_volatile(a.add(i)),
                core::ptr::read_volatile(b.add(i)),
            )
        };
        if x != y {
            return x as i32 - y as i32;
        }
        i += 1;
    }
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn bcmp(a: *const u8, b: *const u8, n: usize) -> i32 {
    unsafe { memcmp(a, b, n) }
}
