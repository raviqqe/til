use std::env::args;
use core::str::FromStr;

fn main() {
    let mut index = f64::from_str(&args().nth(1).unwrap()).unwrap();
    let mut sum = 0.0;

    while index != 0.0 {
        sum += index;
        index -= 1.0;
    }

    println!("{sum}");
}
