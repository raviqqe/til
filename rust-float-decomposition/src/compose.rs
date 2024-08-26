pub fn compose(x: i64, y: i64) -> f64 {
    (x as f64) * (y as f64).exp2()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        assert_eq!(compose(1, 0), 1.0);
        assert_eq!(compose(2, 0), 2.0);
        assert_eq!(compose(123, 0), 123.0);
        assert_eq!(compose(256, 0), 256.0);
        assert_eq!(compose(4503599627370496, 0), 52.0f64.exp2());
        assert_eq!(compose(4503599627370496, 1), 53.0f64.exp2());
        assert_eq!(compose(4503599627370496, 2), 54.0f64.exp2());
        assert_eq!(compose(4503599627370496, 3), 55.0f64.exp2());
        assert_eq!(compose(4503599627370496, 4), 56.0f64.exp2());
        assert_eq!(
            compose(4503599627370497, 4),
            (52.0f64.exp2() + 1.0) * 4.0f64.exp2()
        );
    }
}
