use super::instruction::Instruction;
use alloc::vec::Vec;
use core::fmt::{self, Display, Formatter};

const ESCAPED_SIGNS: &[&str] = &["\\", "+", "*", "_"];

#[derive(Debug, PartialEq)]
pub struct Graph {
    values: Vec<f64>,
    root: Vec<Instruction>,
}

impl Graph {
    pub const fn new(values: Vec<f64>, instructions: Vec<Instruction>) -> Self {
        Self {
            values,
            root: instructions,
        }
    }

    pub fn values(&self) -> &[f64] {
        &self.values
    }

    /// Returns instructions in a program.
    pub fn instructions(&self) -> &[Instruction] {
        &self.root
    }
}

impl Display for Graph {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        writeln!(formatter, "# symbols")?;

        for value in &self.values {
            writeln!(formatter, "{value}")?;
        }

        write!(formatter, "# instructions")?;
        write!(formatter, "{}", Instruction::display_slice(&self.root))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Operand;
    use alloc::vec;
    use insta::assert_snapshot;

    #[test]
    fn display_symbols() {
        assert_snapshot!(Graph::new(vec![0.0, -1.0, 42.0], vec![],));
    }

    #[test]
    fn display_if() {
        assert_snapshot!(Graph::new(
            vec![],
            vec![Instruction::If(vec![Instruction::Constant(
                Operand::Integer(42)
            )])],
        ));
    }

    #[test]
    fn display_closure() {
        assert_snapshot!(Graph::new(
            vec![],
            vec![Instruction::Close(
                42,
                vec![Instruction::Constant(Operand::Integer(2045))],
            )],
        ));
    }

    #[test]
    fn display_closure_with_if() {
        assert_snapshot!(Graph::new(
            vec![],
            vec![
                Instruction::Constant(Operand::Integer(0)),
                Instruction::Constant(Operand::Integer(1)),
                Instruction::Close(
                    42,
                    vec![
                        Instruction::Constant(Operand::Integer(2)),
                        Instruction::If(vec![
                            Instruction::Constant(Operand::Integer(3)),
                            Instruction::Constant(Operand::Integer(4)),
                        ]),
                        Instruction::Constant(Operand::Integer(5)),
                        Instruction::Constant(Operand::Integer(6)),
                    ],
                ),
                Instruction::Constant(Operand::Integer(7)),
                Instruction::Constant(Operand::Integer(8)),
            ],
        ));
    }
}
