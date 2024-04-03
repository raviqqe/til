#![no_std]
#![cfg_attr(not(test), no_main)]

extern crate libc;

#[cfg_attr(not(test), no_mangle)]
unsafe extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {
    libc::printf(b"Hello, World!\n".as_ptr() as _);

    0
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    unsafe { libc::exit(1) }
}
