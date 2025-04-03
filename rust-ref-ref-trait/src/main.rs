trait Bar {
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
    Foo
}
