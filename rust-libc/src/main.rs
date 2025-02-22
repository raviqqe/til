#![no_std]
#![cfg_attr(not(test), no_main)]

#[cfg_attr(not(test), unsafe(no_mangle))]
unsafe extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {
    unsafe { libc::printf(c"Hello, World!".as_ptr() as _) }

    0
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { libc::exit(1) }
}
