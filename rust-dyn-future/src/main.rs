use allocator_api2::{
    alloc::{Allocator, Global},
    boxed::Box,
};
use core::pin::Pin;

fn foo<A: Allocator>(
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
