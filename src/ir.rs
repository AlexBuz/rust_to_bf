#[derive(Debug, Clone, Copy)]
pub enum DirectPlace {
    Stack { offset: isize },
    // TODO: Heap { address: usize },
}

#[derive(Debug, Clone, Copy)]
pub enum IndirectPlace {
    // TODO: Stack { address: DirectPlace },
    Heap { address: DirectPlace },
}

#[derive(Debug, Clone, Copy)]
pub enum Place {
    Direct(DirectPlace),
    Indirect(IndirectPlace),
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Immediate(usize),
    Deref(Place),
}

#[derive(Debug, Clone, Copy)]
pub enum StoreMode {
    Replace,
    Add,
    Subtract,
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

#[allow(unused)]
impl Instruction {
    const fn zero_out(place: Place) -> Instruction {
        Instruction::Move {
            dst: place,
            src: Value::Immediate(0),
            store_mode: StoreMode::Replace,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryState {
    pub stack: Vec<u8>,
    pub heap: Vec<u8>,
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
                    dst: Direct(Stack { offset: -1 }),
                    src: Immediate(0), // a
                    store_mode: Replace,
                },
                Move {
                    dst: Direct(Stack { offset: -2 }),
                    src: Immediate(1), // b
                    store_mode: Replace,
                },
                Move {
                    dst: Direct(Stack { offset: -3 }),
                    src: Immediate(0), // temp
                    store_mode: Replace,
                },
                Move {
                    dst: Direct(Stack { offset: -4 }),
                    src: Immediate(12), // max iterations
                    store_mode: Replace,
                },
                Move {
                    dst: Direct(Stack { offset: -5 }),
                    src: Immediate(0), // heap pointer
                    store_mode: Replace,
                },
                While {
                    cond: Direct(Stack { offset: -4 }),
                    body: vec![
                        Move {
                            dst: Direct(Stack { offset: -3 }),
                            src: Deref(Direct(Stack { offset: -2 })),
                            store_mode: Replace,
                        },
                        Move {
                            dst: Direct(Stack { offset: -2 }),
                            src: Deref(Direct(Stack { offset: -1 })),
                            store_mode: Add,
                        },
                        Move {
                            dst: Direct(Stack { offset: -1 }),
                            src: Deref(Direct(Stack { offset: -3 })),
                            store_mode: Replace,
                        },
                        Move {
                            dst: Direct(Stack { offset: -4 }),
                            src: Immediate(1),
                            store_mode: Subtract,
                        },
                        Move {
                            dst: Indirect(Heap {
                                address: Stack { offset: -5 },
                            }),
                            src: Deref(Direct(Stack { offset: -2 })),
                            store_mode: Replace,
                        },
                        Move {
                            dst: Direct(Stack { offset: -5 }),
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
