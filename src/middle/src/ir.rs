use std::io::{Read, Write};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectPlace {
    StackFrame { offset: usize },
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndirectPlace {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy)]
pub enum StoreMode {
    Add,
    Subtract,
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
    pub fn execute(&self, state: &mut MemoryState, stdin: &mut impl Read, stdout: &mut impl Write) {
        match *self {
            Instruction::Load {
                ref src,
                multiplier,
            } => state.reg += multiplier * *src.resolve(state),
            Instruction::LoadRef { ref src } => state.reg += src.resolve_ref(state),
            Instruction::Store { dst, store_mode } => {
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
                let place = dst.resolve(state);
                match store_mode {
                    StoreMode::Replace => *place = value,
                    StoreMode::Add => *place = place.wrapping_add(value),
                    StoreMode::Subtract => *place = place.wrapping_sub(value),
                }
            }
            Instruction::SaveFrame { size } => {
                state.frame_base += size;
                if state.stack.len() < state.frame_base {
                    state.stack.resize(state.frame_base, 0);
                }
            }
            Instruction::RestoreFrame { size } => state.frame_base -= size,
            Instruction::While { ref cond, ref body } => {
                while *cond.resolve(state) != 0 {
                    for instruction in body {
                        instruction.execute(state, stdin, stdout);
                    }
                }
            }
            Instruction::Switch {
                ref cond,
                ref cases,
                ref default,
            } => cases
                .get(*cond.resolve(state))
                .unwrap_or(default)
                .iter()
                .for_each(|instruction| instruction.execute(state, stdin, stdout)),
            Instruction::Input { dst } => {
                let mut buf = [0u8];
                if let Err(e) = stdin.read_exact(&mut buf) {
                    let std::io::ErrorKind::UnexpectedEof = e.kind() else {
                        panic!("error reading from stdin: {e}");
                    };
                }
                *dst.resolve(state) = usize::from(buf[0]);
            }
            Instruction::Output { src } => {
                stdout.write_all(&[src.resolve(state) as u8]).unwrap();
                let _ = stdout.flush();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub instructions: Vec<Instruction>,
}

pub trait Execute {
    fn execute(&self, stdin: &mut impl Read, stdout: &mut impl Write) -> MemoryState;
}

impl Execute for Program {
    fn execute(&self, stdin: &mut impl Read, stdout: &mut impl Write) -> MemoryState {
        let mut state = MemoryState {
            frame_base: 0,
            reg: 0,
            stack: vec![],
            heap: vec![],
        };
        for instruction in &self.instructions {
            instruction.execute(&mut state, stdin, stdout);
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
