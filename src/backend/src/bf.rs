use {
    middle::ir::{Execute, MemoryState},
    std::io::{Read, Write},
};

pub type CellInt = usize;

#[derive(Debug, Clone)]
pub enum Instruction {
    Right(usize),
    Left(usize),
    Add(CellInt),
    Sub(CellInt),
    Input,
    Output,
    Loop(Vec<Instruction>),
}

impl Instruction {
    fn execute(
        &self,
        tape: &mut Vec<CellInt>,
        ptr: &mut usize,
        stdin: &mut impl Read,
        stdout: &mut impl Write,
    ) {
        match *self {
            Instruction::Right(amount) => {
                let new_ptr = *ptr + amount;
                if new_ptr >= tape.len() {
                    tape.resize(new_ptr + 1, 0);
                }
                *ptr = new_ptr;
            }
            Instruction::Left(amount) => *ptr -= amount,
            Instruction::Add(amount) => tape[*ptr] = tape[*ptr].wrapping_add(amount),
            Instruction::Sub(amount) => tape[*ptr] = tape[*ptr].wrapping_sub(amount),
            Instruction::Input => {
                let mut buf = [0u8];
                if let Err(e) = stdin.read_exact(&mut buf) {
                    let std::io::ErrorKind::UnexpectedEof = e.kind() else {
                        panic!("error reading from stdin: {e}");
                    };
                }
                tape[*ptr] = CellInt::from(buf[0]);
            }
            Instruction::Output => {
                stdout.write_all(&[tape[*ptr] as _]).unwrap();
                let _ = stdout.flush();
            }
            Instruction::Loop(ref body) => {
                while tape[*ptr] != 0 {
                    for instruction in body {
                        instruction.execute(tape, ptr, stdin, stdout);
                    }
                }
            }
        }
    }
}

pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn helper(f: &mut std::fmt::Formatter, instructions: &[Instruction]) -> std::fmt::Result {
            use std::fmt::Write;
            for instruction in instructions {
                match *instruction {
                    Instruction::Right(amount) => {
                        for _ in 0..amount {
                            f.write_char('>')?;
                        }
                    }
                    Instruction::Left(amount) => {
                        for _ in 0..amount {
                            f.write_char('<')?;
                        }
                    }
                    Instruction::Add(amount) => {
                        for _ in 0..amount {
                            f.write_char('+')?;
                        }
                    }
                    Instruction::Sub(amount) => {
                        for _ in 0..amount {
                            f.write_char('-')?;
                        }
                    }
                    Instruction::Input => f.write_char(',')?,
                    Instruction::Output => f.write_char('.')?,
                    Instruction::Loop(ref body) => {
                        f.write_char('[')?;
                        helper(f, body)?;
                        f.write_char(']')?;
                    }
                }
            }
            Ok(())
        }
        helper(f, &self.instructions)
    }
}

impl Execute for Program {
    fn execute(&self, stdin: &mut impl Read, stdout: &mut impl Write) -> MemoryState {
        let mut tape = vec![0];
        let mut ptr = 0;
        for instruction in &self.instructions {
            instruction.execute(&mut tape, &mut ptr, stdin, stdout);
        }
        while tape.len() % 8 != 4 {
            tape.push(0);
        }
        let mut stack = vec![];
        let mut heap = vec![];
        assert_eq!(tape[0], 0); // reg1
        assert_eq!(tape[2], 0); // mem_base
        assert_eq!(tape[3], 0); // stack_base
        let mut frame_base = 0;
        for chunk in tape[4..].chunks(8) {
            let [temp_i, mem_i, mem_i_marker, unused_i, temp_j, mem_j, mem_j_marker, stack_j_marker] =
                chunk.try_into().unwrap();
            assert_eq!(temp_i, 0);
            heap.push(mem_i as _);
            assert_eq!(mem_i_marker, 0);
            assert_eq!(unused_i, 0);
            assert_eq!(temp_j, 0);
            stack.push(mem_j as _);
            assert_eq!(mem_j_marker, 0);
            assert!(stack_j_marker <= 1);
            frame_base += stack_j_marker;
        }
        MemoryState {
            frame_base: frame_base as _,
            reg: tape[1] as _,
            stack,
            heap,
        }
    }
}
