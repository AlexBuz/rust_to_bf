use {
    crate::{
        common::debug_println,
        ir::{
            DirectPlace::{self, *},
            IndirectPlace::{self, *},
            Instruction::{self, *},
            MemoryState,
            Place::{self, *},
            Program, StoreMode,
            Value::*,
        },
    },
    std::{
        borrow::Cow,
        io::{Read, Write},
    },
};

/*
Memory layout: [
    stack_base, heap_base,
    reg0, temp0, stack0, heap0, stack0_marker, heap0_marker,
    reg1, temp1, stack1, heap1, stack1_marker, heap1_marker,
    ...
]
*/

// TODO: Don't hardcode the movements to and from registers. For the functions that read/write registers, take an argument that specifies the register to use and apply the movements accordingly.

impl DirectPlace {
    pub fn path_to_and_from(&self) -> (String, Cow<'static, str>) {
        match self {
            StackTop { offset } => {
                let sb_to_place = format!("6>[6>]<<{}", "6<".repeat(offset + 1));
                let place_to_sb = "4<[6<]";
                (sb_to_place, Cow::Borrowed(place_to_sb))
            }
        }
    }

    pub fn convert_load_to_bf(&self, output: &mut String) {
        let (sb_to_src, src_to_sb) = self.path_to_and_from();

        // go to the source cell
        output.push_str(&sb_to_src);

        // use temp cell to preserve the value
        output.push_str("[-<+>]");

        // copy the value to reg0
        output.push_str(&format!("<[->+{src_to_sb}>>+<<{sb_to_src}<]>"));
        output.push_str(&src_to_sb);
    }

    pub fn convert_store_to_bf(&self, output: &mut String, mode: StoreMode) {
        let (sb_to_dst, dst_to_sb) = self.path_to_and_from();

        let operator = match mode {
            StoreMode::Add => "+",
            StoreMode::Subtract => "-",
            StoreMode::Replace => {
                output.push_str(&sb_to_dst);
                output.push_str("[-]");
                output.push_str(&dst_to_sb);
                "+"
            }
        };

        // move the value to the destination register
        output.push_str(">>");
        output.push_str(&format!("[-<<{sb_to_dst}{operator}{dst_to_sb}>>]"));
        output.push_str("<<");
    }

    pub fn write_bf_symbols(&self, output: &mut String, symbols: &str) {
        let (sb_to_dst, dst_to_sb) = self.path_to_and_from();

        output.push_str(&sb_to_dst);
        output.push_str(symbols);
        output.push_str(&dst_to_sb);
    }
}

static HEAP_CELL_TO_SB: &str = "4<[6<]<";
static SB_TO_HEAP_CELL: &str = "7>[6>]<<";

// TODO: Get rid of these:
static SB_TO_REG1: &str = ">>6>";
static REG1_TO_SB: &str = "<<6<";

impl IndirectPlace {
    pub fn path_to_and_from(&self) -> (String, Cow<'static, str>) {
        match self {
            Heap { address } => {
                let mut sb_to_place = String::new();

                // put the address in reg0
                address.convert_load_to_bf(&mut sb_to_place);

                // leave a trail of heap markers up to the desired heap element
                sb_to_place.push_str(">>");
                sb_to_place.push_str("[-5>+5<]");
                sb_to_place.push_str("5>");
                sb_to_place.push_str("[-[-6>+6<]+6>]<<");

                // delete the trail of heap markers and go back to the stack base
                let place_to_sb = "4<[-6<]<";

                (sb_to_place, Cow::Borrowed(place_to_sb))
            }
        }
    }

    pub fn convert_load_to_bf(&self, output: &mut String) {
        match self {
            Heap { .. } => {
                let (sb_to_place, place_to_sb) = self.path_to_and_from();
                output.push_str(&sb_to_place);

                // use temp cell to preserve the value
                output.push_str("[-<<+>>]");

                // copy the value to reg0
                output.push_str(&format!(
                    "<<[->>+{HEAP_CELL_TO_SB}>>+<<{SB_TO_HEAP_CELL}<<]>>"
                ));

                output.push_str(&place_to_sb);
            }
        }
    }

    pub fn convert_store_to_bf(&self, output: &mut String, mode: StoreMode) {
        match self {
            Heap { .. } => {
                // move the value from reg0 to reg1 so that we can use reg0 to store the address
                output.push_str(">>[-6>+6<]<<");

                let (sb_to_place, place_to_sb) = self.path_to_and_from();
                output.push_str(&sb_to_place);

                let operator = match mode {
                    StoreMode::Add => "+",
                    StoreMode::Subtract => "-",
                    StoreMode::Replace => {
                        output.push_str("[-]");
                        "+"
                    }
                };

                // move the value from reg0 to the heap cell
                output.push_str(HEAP_CELL_TO_SB);
                output.push_str(SB_TO_REG1);
                output.push_str(&format!(
                    "[-{REG1_TO_SB}{SB_TO_HEAP_CELL}{operator}{HEAP_CELL_TO_SB}{SB_TO_REG1}]"
                ));
                output.push_str(REG1_TO_SB);
                output.push_str(SB_TO_HEAP_CELL);

                output.push_str(&place_to_sb);
            }
        }
    }

    pub fn write_bf_symbols(&self, output: &mut String, symbols: &str) {
        match self {
            Heap { .. } => {
                let (sb_to_place, place_to_sb) = self.path_to_and_from();

                output.push_str(&sb_to_place);
                output.push_str(symbols);
                output.push_str(&place_to_sb);
            }
        }
    }
}

impl Place {
    pub fn convert_load_to_bf(&self, output: &mut String) {
        match self {
            Direct(direct) => direct.convert_load_to_bf(output),
            Indirect(indirect) => indirect.convert_load_to_bf(output),
        }
    }

    pub fn convert_store_to_bf(&self, output: &mut String, mode: StoreMode) {
        match self {
            Direct(direct) => direct.convert_store_to_bf(output, mode),
            Indirect(indirect) => indirect.convert_store_to_bf(output, mode),
        }
    }

    pub fn write_bf_symbols(&self, output: &mut String, symbols: &str) {
        match self {
            Direct(direct) => direct.write_bf_symbols(output, symbols),
            Indirect(indirect) => indirect.write_bf_symbols(output, symbols),
        }
    }

    pub fn path_to_and_from(&self) -> (String, Cow<'static, str>) {
        match self {
            Direct(direct) => direct.path_to_and_from(),
            Indirect(indirect) => indirect.path_to_and_from(),
        }
    }
}

impl Instruction {
    fn convert_to_bf(&self, output: &mut String) {
        // pointer is assumed to be at the stack base before and after each instruction
        match *self {
            Move {
                src,
                dst,
                store_mode,
            } => {
                match src {
                    Immediate(value) => {
                        dst.write_bf_symbols(
                            output,
                            &match store_mode {
                                StoreMode::Add => "+".repeat(value),
                                StoreMode::Subtract => "-".repeat(value),
                                StoreMode::Replace => format!("[-]{}", "+".repeat(value)),
                            },
                        );
                    }
                    At(src) => {
                        // TODO: perhaps specialize for the cases where src and dst are both direct stack locations or both direct heap locations
                        src.convert_load_to_bf(output);
                        dst.convert_store_to_bf(output, store_mode);
                    }
                }
            }
            GrowStack { amount } => {
                if amount > 0 {
                    output.push_str("6>[6>]+");
                    output.push_str(&"6>+".repeat(amount - 1));
                    output.push_str("[6<]");
                }
            }
            ShrinkStack { amount } => {
                if amount > 0 {
                    output.push_str("6>[6>]6<");
                    output.push_str(&"-<<[-]4<".repeat(amount));
                    output.push_str("[6<]");
                }
            }
            While { cond, ref body } => {
                let (sb_to_cond, cond_to_sb) = cond.path_to_and_from();
                output.push_str(&sb_to_cond);
                output.push('[');
                output.push_str(&cond_to_sb);
                for instr in body {
                    instr.convert_to_bf(output);
                }
                output.push_str(&sb_to_cond);
                output.push(']');
                output.push_str(&cond_to_sb);
            }
            Switch {
                cond,
                ref cases,
                ref default,
            } => {
                if cases.is_empty() {
                    for instr in default {
                        instr.convert_to_bf(output);
                    }
                } else {
                    cond.convert_load_to_bf(output);
                    if cases.iter().all(Vec::is_empty) {
                        // optimization for switches that express something like `if x > some_literal { ... do stuff ...  } else { do nothing }`
                        // note that it's safe to subtract 1 from cases.len() because we know cases is nonempty at this point
                        output.push_str(">>[");
                        output.push_str(&"-[".repeat(cases.len() - 1));
                        output.push_str("[-]<<");
                        for instr in default {
                            instr.convert_to_bf(output);
                        }
                        output.push_str(">>");
                        output.push_str(&"]".repeat(cases.len() - 1));
                        output.push_str("]<<");
                    } else {
                        output.push_str(">>6>+6<[");
                        output.push_str(&"-[".repeat(cases.len() - 1));
                        output.push_str("[-]6>-<<6<");
                        for instr in default {
                            instr.convert_to_bf(output);
                        }
                        output.push_str(">>");
                        for case in cases.iter().rev() {
                            output.push_str("]6>[-<<6<");
                            for instr in case {
                                instr.convert_to_bf(output);
                            }
                            output.push_str(">>6>]6<");
                        }
                        output.push_str("<<");
                    }
                }
            }
            Input { dst } => {
                dst.write_bf_symbols(output, ",");
            }
            Output { src } => match src {
                Immediate(value) => {
                    // put the value in reg0, print it, clear reg0, and go back to the stack base
                    output.push_str(&format!(">>{}.[-]<<", "+".repeat(value)));
                }
                At(src) => {
                    src.write_bf_symbols(output, ".");
                }
            },
        }
    }
}

type Cell = usize;

fn execute_bf(bf_code: &str) -> Vec<Cell> {
    let mut tape = vec![0];
    let mut ptr = 0;
    let mut i = 0;
    let mut stdout = std::io::stdout().lock();
    let mut stdin = std::io::stdin().lock();
    let mut executed_count = 0;
    while i < bf_code.len() {
        executed_count += 1;
        match bf_code.chars().nth(i).unwrap() {
            '>' => {
                ptr += 1;
                if ptr == tape.len() {
                    tape.push(0);
                }
            }
            '<' => ptr -= 1,
            '+' => tape[ptr] += 1,
            '-' => tape[ptr] -= 1,
            '.' => {
                stdout.write_all(&[tape[ptr] as _]).unwrap();
                stdout.flush().unwrap();
            }
            ',' => {
                let mut buf = [0u8];
                stdin.read_exact(&mut buf).unwrap();
                tape[ptr] = Cell::from(buf[0]);
            }
            '[' => {
                if tape[ptr] == 0 {
                    let mut depth = 1;
                    while depth > 0 {
                        i += 1;
                        match bf_code.chars().nth(i).unwrap() {
                            '[' => depth += 1,
                            ']' => depth -= 1,
                            _ => {}
                        }
                    }
                }
            }
            ']' => {
                if tape[ptr] != 0 {
                    let mut depth = 1;
                    while depth > 0 {
                        i -= 1;
                        match bf_code.chars().nth(i).unwrap() {
                            '[' => depth -= 1,
                            ']' => depth += 1,
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }
        i += 1;
    }
    debug_println!("# of instructions executed: {}", executed_count);
    tape
}

impl Program {
    pub fn convert_to_bf(&self) -> String {
        let mut output = String::new();
        for instr in &self.instructions {
            instr.convert_to_bf(&mut output);
        }
        // TODO: Use a proper BFInstruction enum instead of doing crazy stuff like this:
        for i in (3..10).rev() {
            output = output.replace(&format!("{i}<"), &"<".repeat(i));
            output = output.replace(&format!("{i}>"), &">".repeat(i));
        }
        output
    }

    pub fn execute_bf(bf_code: &str) -> MemoryState {
        let mut tape = execute_bf(bf_code);
        while tape.len() % 6 != 2 {
            tape.push(0);
        }
        let mut stack = Vec::<usize>::new();
        let mut heap = Vec::<usize>::new();
        for chunk in tape[2..].chunks(6) {
            let (reg_i, temp_i, stack_i, heap_i, stack_marker_i, heap_marker_i) =
                (chunk[0], chunk[1], chunk[2], chunk[3], chunk[4], chunk[5]);
            if stack_marker_i != 0 {
                stack.push(stack_i as _);
            }
            heap.push(heap_i as _);
            assert_eq!(heap_marker_i, 0);
            assert_eq!(reg_i, 0);
            assert_eq!(temp_i, 0);
        }
        MemoryState { stack, heap }
    }
}
