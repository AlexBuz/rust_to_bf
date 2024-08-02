use std::io::{Read, Write};

#[derive(Debug, Clone, Copy)]
pub enum DirectPlace {
    // TODO: split this into StackTop { offset_down: usize } and Stack { offset: usize }
    StackTop { offset: usize },
    // TODO: Heap { address: usize },
}

impl DirectPlace {
    fn resolve<'a>(&self, state: &'a mut MemoryState) -> &'a mut usize {
        match self {
            DirectPlace::StackTop { offset } => {
                let index = state.stack.len() - offset - 1;
                &mut state.stack[index]
            }
        }
    }
}

impl std::fmt::Display for DirectPlace {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DirectPlace::StackTop { offset } => write!(f, "stack[{offset}]"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IndirectPlace {
    // TODO: Stack { address: DirectPlace },
    Heap { address: DirectPlace },
}

impl IndirectPlace {
    fn resolve<'a>(&self, state: &'a mut MemoryState) -> &'a mut usize {
        match self {
            IndirectPlace::Heap { address } => {
                let index = *address.resolve(state);
                state.heap.resize(index + 1, 0);
                &mut state.heap[index]
            }
        }
    }
}

impl std::fmt::Display for IndirectPlace {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            IndirectPlace::Heap { address } => write!(f, "heap[{address}]"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Place {
    Direct(DirectPlace),
    Indirect(IndirectPlace),
}

impl Place {
    fn resolve<'a>(&self, state: &'a mut MemoryState) -> &'a mut usize {
        match self {
            Place::Direct(place) => place.resolve(state),
            Place::Indirect(place) => place.resolve(state),
        }
    }
}

impl std::fmt::Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Place::Direct(place) => write!(f, "{}", place),
            Place::Indirect(place) => write!(f, "{}", place),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Immediate(usize),
    Deref(Place),
}

impl Value {
    fn resolve(&self, state: &mut MemoryState) -> usize {
        match self {
            Value::Immediate(value) => *value,
            Value::Deref(place) => *place.resolve(state),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Immediate(value) => write!(f, "{}", value),
            Value::Deref(place) => write!(f, "{}", place),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum StoreMode {
    Replace,
    Add,
    Subtract,
}

impl std::fmt::Display for StoreMode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            StoreMode::Replace => write!(f, "="),
            StoreMode::Add => write!(f, "+="),
            StoreMode::Subtract => write!(f, "-="),
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Instruction {
    Move {
        dst: Place,
        src: Value,
        store_mode: StoreMode,
    },
    GrowStack {
        amount: usize,
    },
    ShrinkStack {
        amount: usize,
    },
    While {
        cond: Place,
        body: Vec<Instruction>,
    },
    // IfElse {
    //     cond: Place,
    //     main_body: Vec<Instruction>,
    //     else_body: Vec<Instruction>,
    // },
    Switch {
        cond: Place,
        cases: Vec<Vec<Instruction>>, // item at index n is the body for case n
        default: Vec<Instruction>,
    },
    Input {
        dst: Place,
    },
    Output {
        src: Value,
    },
}

macro_rules! indent_println {
    ($depth:expr, $($arg:tt)*) => {
        print!("{:width$}", "", width = $depth * 4);
        println!($($arg)*);
    };
}

impl Instruction {
    pub fn execute(&self, state: &mut MemoryState, depth: usize) {
        match self {
            Instruction::Move {
                dst,
                src,
                store_mode,
            } => {
                indent_println!(depth, "{dst} {store_mode} {src};");
                let src_value = src.resolve(state);
                let dst_place = dst.resolve(state);
                match store_mode {
                    StoreMode::Replace => *dst_place = src_value,
                    StoreMode::Add => *dst_place += src_value,
                    StoreMode::Subtract => *dst_place -= src_value,
                }
            }
            Instruction::GrowStack { amount } => {
                indent_println!(depth, "stack.grow_by({amount});");
                state.stack.resize(state.stack.len() + amount, 0);
            }
            Instruction::ShrinkStack { amount } => {
                indent_println!(depth, "stack.shrink_by({amount});");
                state.stack.truncate(state.stack.len() - amount);
            }
            Instruction::While { cond, body } => {
                indent_println!(depth, "while {cond} {{");
                let mut i = 0;
                while *cond.resolve(state) != 0 {
                    indent_println!(depth + 1, "iteration {i}: {{");
                    for instruction in body {
                        instruction.execute(state, depth + 2);
                    }
                    indent_println!(depth + 1, "}}");
                    i += 1;
                }
                indent_println!(depth, "}}");
            }
            Instruction::Switch {
                cond,
                cases,
                default,
            } => {
                indent_println!(depth, "switch {cond} {{");
                // cases
                //     .get(*cond.resolve(state))
                //     .unwrap_or(default)
                //     .iter()
                //     .for_each(|instruction| instruction.execute(state, depth + 1));
                let case_index = *cond.resolve(state);
                if case_index < cases.len() {
                    indent_println!(depth + 1, "case {case_index}: {{");
                    for instruction in &cases[case_index] {
                        instruction.execute(state, depth + 2);
                    }
                    indent_println!(depth + 1, "}}");
                } else {
                    indent_println!(depth + 1, "default: {{");
                    for instruction in default {
                        instruction.execute(state, depth + 2);
                    }
                    indent_println!(depth + 1, "}}");
                }
                indent_println!(depth, "}}");
            }
            Instruction::Input { dst } => {
                let mut stdin = std::io::stdin().lock();
                let mut buf = [0u8];
                stdin.read_exact(&mut buf).unwrap();
                *dst.resolve(state) = usize::from(buf[0]);
            }
            Instruction::Output { src } => {
                let mut stdout = std::io::stdout().lock();
                stdout.write_all(&[src.resolve(state) as u8]).unwrap();
                stdout.flush().unwrap();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn execute(&self) -> MemoryState {
        // return self.execute_through_bf();
        let mut state = MemoryState {
            stack: vec![],
            heap: vec![],
        };
        for instruction in &self.instructions {
            instruction.execute(&mut state, 0);
        }
        state
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryState {
    pub stack: Vec<usize>,
    pub heap: Vec<usize>,
}

impl std::fmt::Display for MemoryState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Stack: {:?}", self.stack)?;
        write!(f, "Heap: {:?}", self.heap)?;
        Ok(())
    }
}

pub mod example_programs {
    use super::*;
    use DirectPlace::*;
    use IndirectPlace::*;
    use Instruction::*;
    use Place::*;
    use StoreMode::*;
    use Value::*;

    pub fn fibonacci() -> Program {
        Program {
            instructions: vec![
                GrowStack { amount: 5 },
                Move {
                    dst: Direct(StackTop { offset: 0 }),
                    src: Immediate(0), // a
                    store_mode: Replace,
                },
                Move {
                    dst: Direct(StackTop { offset: 1 }),
                    src: Immediate(1), // b
                    store_mode: Replace,
                },
                Move {
                    dst: Direct(StackTop { offset: 2 }),
                    src: Immediate(0), // temp
                    store_mode: Replace,
                },
                Move {
                    dst: Direct(StackTop { offset: 3 }),
                    src: Immediate(12), // max iterations
                    store_mode: Replace,
                },
                Move {
                    dst: Direct(StackTop { offset: 4 }),
                    src: Immediate(0), // heap pointer
                    store_mode: Replace,
                },
                While {
                    cond: Direct(StackTop { offset: 3 }),
                    body: vec![
                        Move {
                            dst: Direct(StackTop { offset: 2 }),
                            src: Deref(Direct(StackTop { offset: 1 })),
                            store_mode: Replace,
                        },
                        Move {
                            dst: Direct(StackTop { offset: 1 }),
                            src: Deref(Direct(StackTop { offset: 0 })),
                            store_mode: Add,
                        },
                        Move {
                            dst: Direct(StackTop { offset: 0 }),
                            src: Deref(Direct(StackTop { offset: 2 })),
                            store_mode: Replace,
                        },
                        Move {
                            dst: Direct(StackTop { offset: 3 }),
                            src: Immediate(1),
                            store_mode: Subtract,
                        },
                        Move {
                            dst: Indirect(Heap {
                                address: StackTop { offset: 4 },
                            }),
                            src: Deref(Direct(StackTop { offset: 1 })),
                            store_mode: Replace,
                        },
                        Move {
                            dst: Direct(StackTop { offset: 4 }),
                            src: Immediate(1),
                            store_mode: Add,
                        },
                    ],
                },
            ],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fibonacci_program() {
        let final_state = example_programs::fibonacci().execute();
        let expected_final_state = MemoryState {
            stack: vec![12, 0, 144, 233, 144],
            heap: vec![1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233],
        };
        assert_eq!(final_state, expected_final_state);
    }
}
