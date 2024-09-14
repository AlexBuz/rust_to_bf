use {
    crate::middle::ir::MemoryState,
    std::io::{Read, Write},
    Instruction::*,
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
        stdin: &mut std::io::StdinLock,
        stdout: &mut std::io::StdoutLock,
    ) {
        match *self {
            Right(amount) => {
                let new_ptr = *ptr + amount;
                if new_ptr >= tape.len() {
                    tape.resize(new_ptr + 1, 0);
                }
                *ptr = new_ptr;
            }
            Left(amount) => *ptr -= amount,
            Add(amount) => tape[*ptr] = tape[*ptr].wrapping_add(amount),
            Sub(amount) => tape[*ptr] = tape[*ptr].wrapping_sub(amount),
            Output => {
                stdout.write_all(&[tape[*ptr] as _]).unwrap();
                stdout.flush().unwrap();
            }
            Input => {
                let mut buf = [0u8];
                if let Err(e) = stdin.read_exact(&mut buf) {
                    let std::io::ErrorKind::UnexpectedEof = e.kind() else {
                        panic!("error reading from stdin: {e}");
                    };
                }
                tape[*ptr] = CellInt::from(buf[0]);
            }
            Loop(ref body) => {
                while tape[*ptr] != 0 {
                    for instr in body {
                        instr.execute(tape, ptr, stdin, stdout);
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
            for instr in instructions {
                match *instr {
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

impl Program {
    pub fn execute(&self) -> MemoryState {
        let mut tape = vec![0];
        let mut ptr = 0;
        let mut i = 0;
        let mut stdin = std::io::stdin().lock();
        let mut stdout = std::io::stdout().lock();
        while i < self.instructions.len() {
            self.instructions[i].execute(&mut tape, &mut ptr, &mut stdin, &mut stdout);
            i += 1;
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
