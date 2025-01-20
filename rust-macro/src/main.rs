macro_rules! foo {
    ($x:ident) => {
        bar!(bar!($x))
    };
}

macro_rules! bar {
    ($x:expr) => {
        $x
    };
}

fn main() {
    let x = 42;

    println!("{}", foo!(x));
}
