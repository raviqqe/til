pub fn decompose_simple(x: f64) -> (i64, i64) {
    let y = (x.log2() - 52.0).max(0.0);

    ((x / y.exp2()) as _, y as _)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        assert_eq!(decompose_simple(1.0), (1, 0));
        assert_eq!(decompose_simple(2.0), (2, 0));
        assert_eq!(decompose_simple(52.0f64.exp2()), (4503599627370496, 0));
        assert_eq!(decompose_simple(53.0f64.exp2()), (4503599627370496, 1));
        assert_eq!(decompose_simple(54.0f64.exp2()), (4503599627370496, 2));
        assert_eq!(decompose_simple(55.0f64.exp2()), (4503599627370496, 3));
        assert_eq!(decompose_simple(56.0f64.exp2()), (4503599627370496, 4));
        assert_eq!(
            decompose_simple((52.0f64.exp2() + 1.0) * 4.0f64.exp2()),
            (4503599627370497, 4)
        );
    }
}
