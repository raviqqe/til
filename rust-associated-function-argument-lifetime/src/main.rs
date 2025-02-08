struct Foo<'a> {
    _foo: &'a (),
}

impl<'a> Foo<'a> {
    pub fn foo(&mut self, _x: impl IntoIterator<Item = u8>) {}
}

fn foo(x: &[u8]) {
    let a = ();
    let mut foo = Foo { _foo: &a };

    foo.foo(x.iter().copied());
}

fn main() {
    let x = [42];

    foo(&x);
}
