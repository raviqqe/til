const SIZE: usize = 8;

pub fn encode(xs: &[u8]) -> Vec<u8> {
    let mut ys = vec![];
    let mut y = 0usize;
    let mut offset = 0;

    for &x in xs {
        match x {
            0 => {
                offset += 1;
                y <<= 1;
            }
            1 => {
                offset += 2;
                y <<= 2;
                y += 1;
            }
            _ => todo!(),
        }

        if offset >= SIZE {
            ys.push(y as u8);
            y <<= SIZE;
            offset -= SIZE;
        }
    }

    if offset > 0 {
        ys.push(y as u8);
    }

    ys
}

#[cfg(test)]
mod tests {
    use super::*;

    mod encode {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn encode_nothing() {
            assert_eq!(encode(&[]), &[]);
        }

        #[test]
        fn encode_zero() {
            assert_eq!(encode(&[0]), &[0]);
        }

        #[test]
        fn encode_two_zeros() {
            assert_eq!(encode(&[0, 0]), &[0]);
        }

        #[test]
        fn encode_one() {
            let mut output = Default::default();

            encode(&[1], &mut output);

            assert_eq!(output, &[1]);
        }
    }
}
