#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::{ops::Coroutine, pin::Pin};

fn main() {
    let x = Box::new(1);
    let y = Box::new(2);

    let mut generate = #[coroutine]
    || {
        yield x;
        yield y;
    };

    dbg!(Pin::new(&mut generate).resume(()));
    dbg!(Pin::new(&mut generate).resume(()));
}
