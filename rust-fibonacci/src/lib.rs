pub fn fibonacci(number: usize) -> usize {
    if number == 0 || number == 1 {
        number
    } else {
        fibonacci(number - 1) + fibonacci(number - 2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calculate() {
        assert_eq!(fibonacci(0), 0);
        assert_eq!(fibonacci(1), 1);
    }
}
