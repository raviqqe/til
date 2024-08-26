mod compose;
mod decompose;

pub use compose::*;
pub use decompose::*;

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_float {
        ($x:literal) => {
            let (x, y) = decompose_normal($x);

            assert_eq!(compose(x, y), $x);
        };
    }

    #[test]
    fn simple() {
        assert_float!(0.0);
    }
}
