use {
    crate::common::{indented_print, indented_println},
    derive_more::Display,
    std::io::{Read, Write},
};

#[derive(Debug, Clone, Copy, Display)]
pub enum DirectPlace {
    #[display("stack[-{}]", offset + 1)]
    StackTop { offset: usize },
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

#[derive(Debug, Clone, Copy, Display)]
pub enum IndirectPlace {
    #[display("heap[{address}]")]
    Heap { address: DirectPlace },
}

impl IndirectPlace {
    fn resolve<'a>(&self, state: &'a mut MemoryState) -> &'a mut usize {
        match self {
            IndirectPlace::Heap { address } => {
                let index = *address.resolve(state);
                if index >= state.heap.len() {
                    state.heap.resize(index + 1, 0);
                }
                &mut state.heap[index]
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Display)]
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

#[derive(Debug, Clone, Copy, Display)]
pub enum Value {
    Immediate(usize),
    At(Place),
}

impl Value {
    fn resolve(&self, state: &mut MemoryState) -> usize {
        match self {
            Value::Immediate(value) => *value,
            Value::At(place) => *place.resolve(state),
        }
    }
}

#[derive(Debug, Clone, Copy, Display)]
pub enum StoreMode {
    #[display("+=")]
    Add,
    #[display("-=")]
    Subtract,
    #[display("=")]
    Replace,
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

impl Instruction {
    pub fn execute(&self, state: &mut MemoryState, depth: usize) {
        match self {
            Instruction::Move {
                dst,
                src,
                store_mode,
            } => {
                indented_println!(depth, "{dst} {store_mode} {src};");
                let src_value = src.resolve(state);
                let dst_place = dst.resolve(state);
                match store_mode {
                    StoreMode::Replace => *dst_place = src_value,
                    StoreMode::Add => *dst_place += src_value,
                    StoreMode::Subtract => *dst_place -= src_value,
                }
            }
            Instruction::GrowStack { amount } => {
                indented_println!(depth, "stack.grow_by({amount});");
                state.stack.resize(state.stack.len() + amount, 0);
            }
            Instruction::ShrinkStack { amount } => {
                indented_println!(depth, "stack.shrink_by({amount});");
                state.stack.truncate(state.stack.len() - amount);
            }
            Instruction::While { cond, body } => {
                indented_println!(depth, "while {cond} {{");
                let mut i = 0;
                while *cond.resolve(state) != 0 {
                    indented_println!(depth + 1, "iteration {i}: {{");
                    for instruction in body {
                        instruction.execute(state, depth + 2);
                    }
                    indented_println!(depth + 1, "}}");
                    i += 1;
                }
                indented_println!(depth, "}}");
            }
            Instruction::Switch {
                cond,
                cases,
                default,
            } => {
                indented_println!(depth, "switch {cond} {{");
                let case_index = *cond.resolve(state);
                if case_index < cases.len() {
                    indented_println!(depth + 1, "case {case_index}: {{");
                    for instruction in &cases[case_index] {
                        instruction.execute(state, depth + 2);
                    }
                    indented_println!(depth + 1, "}}");
                } else {
                    indented_println!(depth + 1, "default: {{");
                    for instruction in default {
                        instruction.execute(state, depth + 2);
                    }
                    indented_println!(depth + 1, "}}");
                }
                indented_println!(depth, "}}");
            }
            Instruction::Input { dst } => {
                indented_print!(depth, "{dst} = getchar!(); // ");
                std::io::stdout().flush().unwrap();
                let mut stdin = std::io::stdin().lock();
                let mut buf = [0u8];
                stdin.read_exact(&mut buf).unwrap();
                *dst.resolve(state) = usize::from(buf[0]);
            }
            Instruction::Output { src } => {
                indented_print!(depth, "putchar!({src}); // ");
                let mut stdout = std::io::stdout().lock();
                stdout.flush().unwrap();
                stdout.write_all(&[src.resolve(state) as u8]).unwrap();
                stdout.flush().unwrap();
                indented_println!();
            }
        }
        indented_println!(depth, "stack: {:?}", state.stack);
        indented_println!(depth, "heap: {:?}", state.heap);
        indented_println!();
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn execute(&self) -> MemoryState {
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
        writeln!(f, "stack: {:?}", self.stack)?;
        write!(f, "heap: {:?}", self.heap)?;
        Ok(())
    }
}
