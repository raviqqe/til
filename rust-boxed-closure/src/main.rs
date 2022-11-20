fn main() {
    let foo: Box<dyn Fn(u64) -> u64> = Box::new(|x| x);
    let bar = Box::<dyn Fn(u64) -> u64>::new(|x| x);
}
