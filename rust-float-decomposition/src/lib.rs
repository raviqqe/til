pub fn decompose_simple(x: f64) -> (i64, i64) {
    calculate_parts(x, (x.log2() - 52.0).max(0.0).floor())
}

pub fn decompose_normal(x: f64) -> (i64, i64) {
    let mut y = x.log2();

    while (x / y.floor().exp2()).fract() > f64::EPSILON {
        y -= 1.0;
    }

    calculate_parts(x, y.floor())
}

fn calculate_parts(x: f64, y: f64) -> (i64, i64) {
    ((x / y.exp2()) as _, y as _)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        assert_eq!(decompose_simple(1.0), (1, 0));
        assert_eq!(decompose_simple(2.0), (2, 0));
        assert_eq!(decompose_simple(123.0), (123, 0));
        assert_eq!(decompose_simple(256.0), (256, 0));
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

    #[test]
    fn normal() {
        assert_eq!(decompose_normal(1.0), (1, 0));
        assert_eq!(decompose_normal(2.0), (1, 1));
        assert_eq!(decompose_normal(123.0), (123, 0));
        assert_eq!(decompose_normal(256.0), (1, 8));
        assert_eq!(decompose_normal(52.0f64.exp2()), (1, 52));
        assert_eq!(decompose_normal(53.0f64.exp2()), (1, 53));
        assert_eq!(decompose_normal(54.0f64.exp2()), (1, 54));
        assert_eq!(decompose_normal(55.0f64.exp2()), (1, 55));
        assert_eq!(decompose_normal(56.0f64.exp2()), (1, 56));
        assert_eq!(
            decompose_normal((52.0f64.exp2() + 1.0) * 4.0f64.exp2()),
            (4503599627370497, 4)
        );
    }
}
