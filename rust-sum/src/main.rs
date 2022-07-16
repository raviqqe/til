use std::env::args;
use std::str::FromStr;

fn main() {
    let mut index = f64::from_str(&args().next().unwrap()).unwrap();
    let mut sum = 0.0;

    while index != 0.0 {
        sum += index;
        index -= 1.0;
    }

    println!("{}", sum);
}
