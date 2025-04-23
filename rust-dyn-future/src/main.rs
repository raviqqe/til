#![feature(allocator_api)]

// Swapping `std` with `allocator_api2` fails to compile.

use core::pin::Pin;
use std::{
    alloc::{Allocator, Global},
    boxed::Box,
};

fn foo<A: Allocator + 'static>(
    allocator: A,
    future: impl Future<Output = isize> + 'static,
) -> Pin<Box<dyn Future<Output = isize> + 'static, A>> {
    Box::pin_in(future, allocator)
}

#[tokio::main]
async fn main() {
    let future = foo(Global, async { 42 });
    println!("value: {}", future.await);
}
