#![feature(generators, generator_trait)]

use std::{ops::Generator, pin::Pin};

fn main() {
    let x = Box::new(1);
    let y = Box::new(2);

    let mut generate = || {
        yield x;
        yield y;
    };

    dbg!(Pin::new(&mut generate).resume(()));
    dbg!(Pin::new(&mut generate).resume(()));
}
