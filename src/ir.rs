use {
    crate::common::{indented_print, indented_println},
    derive_more::Display,
    std::io::{Read, Write},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum IndirectPlace {
    #[display("*{address}")]
    Deref { address: DirectPlace },
}

impl IndirectPlace {
    fn resolve<'a>(&self, state: &'a mut MemoryState) -> &'a mut usize {
        match self {
            IndirectPlace::Deref { address } => {
                let value = *address.resolve(state);
                let index = value / 2;
                if value % 2 == 0 {
                    &mut state.stack[index]
                } else {
                    if index >= state.heap.len() {
                        state.heap.resize(index + 1, 0);
                    }
                    &mut state.heap[index]
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
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

    fn resolve_ref(&self, state: &mut MemoryState) -> usize {
        match self {
            Place::Direct(DirectPlace::StackTop { offset }) => 2 * (state.stack.len() - offset - 1),
            Place::Indirect(IndirectPlace::Deref { address }) => *address.resolve(state),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
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
    Load {
        src: Place,
    },
    LoadRef {
        src: Place,
    },
    Store {
        dst: Place,
        store_mode: StoreMode,
    },
    StoreImm {
        dst: Place,
        value: usize,
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
        match *self {
            Instruction::Load { ref src } => {
                indented_println!(depth, "reg += {src};");
                state.reg += *src.resolve(state);
            }
            Instruction::LoadRef { ref src } => {
                indented_println!(depth, "reg += &{src};");
                state.reg += src.resolve_ref(state)
            }
            Instruction::Store { dst, store_mode } => {
                indented_println!(depth, "{dst} {store_mode} reg; reg = 0;");
                let reg = std::mem::take(&mut state.reg);
                let place = dst.resolve(state);
                match store_mode {
                    StoreMode::Replace => *place = reg,
                    StoreMode::Add => *place += reg,
                    StoreMode::Subtract => *place -= reg,
                }
            }
            Instruction::StoreImm {
                dst,
                value,
                store_mode,
            } => {
                indented_println!(depth, "{dst} {store_mode} {value};");
                let place = dst.resolve(state);
                match store_mode {
                    StoreMode::Replace => *place = value,
                    StoreMode::Add => *place += value,
                    StoreMode::Subtract => *place -= value,
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
            Instruction::While { ref cond, ref body } => {
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
                ref cond,
                ref cases,
                ref default,
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
                indented_print!(depth, "{dst} = read_char!(); // ");
                std::io::stdout().flush().unwrap();
                let mut stdin = std::io::stdin().lock();
                let mut buf = [0u8];
                stdin.read_exact(&mut buf).unwrap();
                *dst.resolve(state) = usize::from(buf[0]);
            }
            Instruction::Output { src } => {
                indented_print!(depth, "print_char!({src}); // ");
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
            reg: 0,
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
    pub reg: usize,
}

impl std::fmt::Display for MemoryState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "stack: {:?}", self.stack)?;
        write!(f, "heap: {:?}", self.heap)?;
        Ok(())
    }
}
