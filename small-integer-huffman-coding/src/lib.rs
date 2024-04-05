const SIZE: usize = 8;

pub fn encode(input: &[u8], output: &mut Vec<u8>) {
    let mut y = 0usize;
    let mut offset = 0;

    for &x in input {
        match x {
            0 => {
                y <<= 1;
                offset += 1;
            }
            _ => todo!(),
        }

        if offset >= SIZE {
            output.push((y % (1 << SIZE)) as u8);
            offset -= SIZE;
        }
    }
}
