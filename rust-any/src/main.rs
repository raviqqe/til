use core::any::Any;

fn r#box<'a, T: Any + 'a>(value: T) -> Box<dyn Any + 'a> {
    Box::new(value)
}

fn main() {
    r#box(());
    r#box("");

    let x: i64 = 42;
    r#box(x);
    r#box(&x);
}
