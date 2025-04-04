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

trait Qux: Copy {
    fn qux(self) {
        println!("Hello");
    }
}

struct Foo {}

impl Bar for Foo {}

impl Baz for Foo {}

impl Baz for &Foo {}

impl Qux for &Foo {}

fn main() {
    Foo {}.bar();
    Foo {}.baz();
    Foo {}.qux();

    let x = Foo {};
    let y = &x;

    x.qux();
    y.qux();
    x.baz();
    // y.bar(); // moves `*y`
    y.baz();
    x.bar();
}
