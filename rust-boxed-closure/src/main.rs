fn main() {
    let foo: Box<dyn Fn(u64) -> u64> = Box::new(|x| x);

    println!("{}", foo(42));

    // This results in a compile error!
    // let bar = Box::<dyn Fn(u64) -> u64>::new(|x| x);
}
