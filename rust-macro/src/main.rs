macro_rules! foo {
    ($x:expr) => {
        $x
    };
}

macro_rules! bar {
    ($x:expr) => {
        foo!(foo!($x))
    };
}

macro_rules! baz {
    ($($x:expr),*) => {
        (bar!(($(bar!($x)),*)))
    };
}

fn main() {
    let x = 42;

    println!("{}", foo!(x));
    println!("{}", bar!(x));
    println!("{}", baz!(x));
    println!("{:?}", baz!(x, x));
}
