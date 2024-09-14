use {
    crate::common::{indented_print, indented_println},
    derive_more::Display,
    std::io::{Read, Write},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum DirectPlace {
    #[display("stack_frame[{}]", offset)]
    StackFrame { offset: usize },
    #[display("*{_0}")]
    Address(usize),
}

impl DirectPlace {
    fn resolve<'a>(&self, state: &'a mut MemoryState) -> &'a mut usize {
        match self {
            DirectPlace::StackFrame { offset } => {
                let index = state.frame_base + offset;
                if index >= state.stack.len() {
                    state.stack.resize(index + 1, 0);
                }
                &mut state.stack[index]
            }
            DirectPlace::Address(address) => {
                let vector = if address % 2 == 0 {
                    &mut state.heap
                } else {
                    &mut state.stack
                };
                let index = address / 2;
                if index >= vector.len() {
                    vector.resize(index + 1, 0);
                }
                &mut vector[index]
            }
        }
    }

    fn resolve_ref(&self, state: &mut MemoryState) -> usize {
        match *self {
            DirectPlace::StackFrame { offset } => 2 * (state.frame_base + offset) + 1,
            DirectPlace::Address(address) => address,
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
        DirectPlace::Address(self.resolve_ref(state)).resolve(state)
    }

    fn resolve_ref(&self, state: &mut MemoryState) -> usize {
        match self {
            IndirectPlace::Deref { address } => *address.resolve(state),
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
            Place::Direct(place) => place.resolve_ref(state),
            Place::Indirect(place) => place.resolve_ref(state),
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
        multiplier: usize,
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
    SaveFrame {
        size: usize,
    },
    RestoreFrame {
        size: usize,
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
            Instruction::Load {
                ref src,
                multiplier,
            } => {
                indented_println!(depth, "reg += {multiplier} * {src};");
                state.reg += multiplier * *src.resolve(state);
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
                    StoreMode::Add => *place = place.wrapping_add(reg),
                    StoreMode::Subtract => *place = place.wrapping_sub(reg),
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
                    StoreMode::Add => *place = place.wrapping_add(value),
                    StoreMode::Subtract => *place = place.wrapping_sub(value),
                }
            }
            Instruction::SaveFrame { size } => {
                indented_println!(depth, "stack.grow_by({size});");
                state.frame_base += size;
                if state.stack.len() < state.frame_base {
                    state.stack.resize(state.frame_base, 0);
                }
            }
            Instruction::RestoreFrame { size } => {
                indented_println!(depth, "stack.shrink_by({size});");
                state.frame_base -= size;
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
                if let Err(e) = stdin.read_exact(&mut buf) {
                    let std::io::ErrorKind::UnexpectedEof = e.kind() else {
                        panic!("error reading from stdin: {e}");
                    };
                }
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
        indented_println!(depth, "frame: {:?}", &state.stack[state.frame_base..]);
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
            frame_base: 0,
            reg: 0,
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
    pub frame_base: usize,
    pub reg: usize,
    pub stack: Vec<usize>,
    pub heap: Vec<usize>,
}

impl std::fmt::Display for MemoryState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "stack: {:?}", self.stack)?;
        writeln!(f, "frame: {:?}", &self.stack[self.frame_base..])?;
        write!(f, "heap: {:?}", self.heap)?;
        Ok(())
    }
}
