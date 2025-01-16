use core::any::Any;

const NUMBER: usize = 42;

fn r#box<'a, T: Any>(value: T) -> Box<dyn Any> {
    Box::new(value)
}

fn main() {
    r#box(());
    r#box("");
    r#box(42);
    r#box(&NUMBER);
}
