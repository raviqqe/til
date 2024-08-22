pub fn decompose(x: f64) -> (i64, i64) {
    let y = (x.log2() - 52.0).max(0.0);

    ((x / y.exp2()) as _, y as _)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        assert_eq!(decompose(1.0), (1, 0));
        assert_eq!(decompose(2.0), (2, 0));
        assert_eq!(decompose(52.0f64.exp2()), (4503599627370496, 0));
        assert_eq!(decompose(53.0f64.exp2()), (4503599627370496, 1));
        assert_eq!(decompose(54.0f64.exp2()), (4503599627370496, 2));
        assert_eq!(decompose(55.0f64.exp2()), (4503599627370496, 3));
        assert_eq!(decompose(56.0f64.exp2()), (4503599627370496, 4));
    }
}
