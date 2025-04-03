trait Bar: Sized {
    fn bar(self) {
        println!("Hello");
    }
}

trait Baz {
    fn baz(&self) {
        println!("Hello");
    }
}

struct Foo {}

impl Bar for Foo {}

impl Baz for Foo {}

impl Baz for &Foo {}

fn main() {
    Foo {}.bar();
    Foo {}.baz();

    let x = Foo {};
    let y = &x;

    x.baz();
    // y.bar(); // moves `*y`
    y.baz();
    x.bar();
}
