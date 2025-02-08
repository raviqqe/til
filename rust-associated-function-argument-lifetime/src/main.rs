struct Foo {
    foo: usize,
}

impl Foo {
    pub fn foo(&mut self, x: &usize) {}
}

fn foo(x: &usize) {
    let mut foo = Foo { foo: 0 };

    foo.foo(x);
}

fn main() {
    let x = 42;

    foo(&x);
}
