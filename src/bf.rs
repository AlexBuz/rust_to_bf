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
memory layout: [
    reg1, reg0,
    mem_base (0), stack_base (0),
    temp0, mem0, mem0_marker, unused (0),
    temp1, mem1, mem1_marker, stack0_marker,
    temp2, mem2, mem2_marker, unused (0),
    temp3, mem3, mem3_marker, stack1_marker,
    temp4, mem4, mem4_marker, unused (0),
    temp5, mem5, mem5_marker, stack2_marker,
    ...
]
*/

impl DirectPlace {
    pub fn path_to_and_from(&self) -> (String, Cow<'static, str>) {
        match *self {
            StackFrame { offset } => {
                let sb_to_place = format!("8>[8>]<<{}", "8>".repeat(offset));
                let place_to_sb = format!("{}6<[8<]", "8<".repeat(offset));
                (sb_to_place, Cow::Owned(place_to_sb))
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
        output.push_str(&format!("<[->+{src_to_sb}<<+>>{sb_to_src}<]>"));

        // go back to the stack base
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

        // go to reg0
        output.push_str("<<");

        // move the value to the destination cell
        output.push_str(&format!("[->>{sb_to_dst}{operator}{dst_to_sb}<<]"));

        // go back to the stack base
        output.push_str(">>");
    }

    pub fn write_bf_symbols(&self, output: &mut String, symbols: &str) {
        let (sb_to_dst, dst_to_sb) = self.path_to_and_from();

        output.push_str(&sb_to_dst);
        output.push_str(symbols);
        output.push_str(&dst_to_sb);
    }
}

static MEM_CELL_TO_SB: &str = "3<[4<]>";
static SB_TO_MEM_CELL: &str = "3>[4>]<";

impl IndirectPlace {
    pub fn path_to_and_from(&self) -> (String, Cow<'static, str>) {
        match self {
            Deref { address } => {
                let mut sb_to_place = String::new();

                // put the address in reg0
                address.convert_load_to_bf(&mut sb_to_place);

                // leave a trail of mem markers up to the desired location
                sb_to_place.push_str("<<");
                sb_to_place.push_str("[-5>+5<]");
                sb_to_place.push_str("5>");
                sb_to_place.push_str("[-[-4>+4<]+4>]<");

                // clear the trail of mem markers and go back to the stack base
                let place_to_sb = "3<[-4<]>";

                (sb_to_place, Cow::Borrowed(place_to_sb))
            }
        }
    }

    pub fn convert_load_to_bf(&self, output: &mut String) {
        match self {
            Deref { .. } => {
                let (sb_to_place, place_to_sb) = self.path_to_and_from();
                output.push_str(&sb_to_place);

                // use temp cell to preserve the value
                output.push_str("[-<+>]");

                // copy the value to reg0
                output.push_str(&format!("<[->+{MEM_CELL_TO_SB}<<+>>{SB_TO_MEM_CELL}<]>"));

                // go back to the stack base
                output.push_str(&place_to_sb);
            }
        }
    }

    pub fn convert_store_to_bf(&self, output: &mut String, mode: StoreMode) {
        match self {
            Deref { .. } => {
                // move the value from reg0 to reg1 so that reg0 can be used to store the address
                output.push_str("<<[-<+>]>>");

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

                // go to reg1
                output.push_str(MEM_CELL_TO_SB);
                output.push_str("<<<");

                // move the value to the destination cell
                output.push_str(&format!(
                    "[->>>{SB_TO_MEM_CELL}{operator}{MEM_CELL_TO_SB}<<<]"
                ));

                // clear the trail of mem markers and go back to the stack base
                output.push_str(">>>");
                output.push_str(SB_TO_MEM_CELL);
                output.push_str(&place_to_sb);
            }
        }
    }

    pub fn write_bf_symbols(&self, output: &mut String, symbols: &str) {
        match self {
            Deref { .. } => {
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

    pub fn convert_load_ref_to_bf(&self, output: &mut String) {
        match *self {
            Direct(direct) => match direct {
                StackFrame { offset } => {
                    // leave a trail of mem markers up to the frame base
                    output.push_str("8>[<+9>]");

                    // count the mem markers (while clearing them) and put the result in reg1
                    output.push_str("8<[<[-8<+8>]7<]");

                    // put the frame base address (2 * reg1 + 1) in reg0, and clear reg1
                    output.push_str("<[-<++>]<+");

                    // add the offset to the frame base address
                    output.push_str(&"++".repeat(offset));

                    // go back to the stack base
                    output.push_str(">>");
                }
            },
            Indirect(indirect) => match indirect {
                Deref { address } => address.convert_load_to_bf(output),
            },
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
        // pointer should be at the stack base before and after each instruction
        match *self {
            Load { src } => {
                src.convert_load_to_bf(output);
            }
            LoadRef { src } => {
                src.convert_load_ref_to_bf(output);
            }
            Store { dst, store_mode } => {
                dst.convert_store_to_bf(output, store_mode);
            }
            StoreImm {
                dst,
                value,
                store_mode,
            } => {
                dst.write_bf_symbols(
                    output,
                    &match store_mode {
                        StoreMode::Add => "+".repeat(value),
                        StoreMode::Subtract => "-".repeat(value),
                        StoreMode::Replace => format!("[-]{}", "+".repeat(value)),
                    },
                );
            }
            SaveFrame { size } => {
                if size > 0 {
                    output.push_str("8>[8>]+");
                    output.push_str(&"8>+".repeat(size - 1));
                    output.push_str("[8<]");
                }
            }
            RestoreFrame { size } => {
                if size > 0 {
                    output.push_str("8>[8>]8<");
                    output.push_str(&"-8<".repeat(size));
                    output.push_str("[8<]");
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
                        // optimization for switches that express `if x > some_literal { ... do stuff ...  } else { do nothing }`
                        // note that it's safe to subtract 1 from cases.len() because we know cases is nonempty at this point
                        output.push_str("<<[");
                        output.push_str(&"-[".repeat(cases.len() - 1));
                        output.push_str("[-]>>");
                        for instr in default {
                            instr.convert_to_bf(output);
                        }
                        output.push_str("<<");
                        output.push_str(&"]".repeat(cases.len() - 1));
                        output.push_str("]>>");
                    } else {
                        output.push_str("<<<+>[");
                        output.push_str(&"-[".repeat(cases.len() - 1));
                        output.push_str("[-]<->>>");
                        for instr in default {
                            instr.convert_to_bf(output);
                        }
                        output.push_str("<<");
                        for case in cases.iter().rev() {
                            output.push_str("]<[->>>");
                            for instr in case {
                                instr.convert_to_bf(output);
                            }
                            output.push_str("<<<]>");
                        }
                        output.push_str(">>");
                    }
                }
            }
            Input { dst } => {
                dst.write_bf_symbols(output, ",");
            }
            Output { src } => match src {
                Immediate(value) => {
                    // put the value in reg0, print it, clear reg0, and go back to the stack base
                    output.push_str(&format!("<<{}.[-]>>", "+".repeat(value)));
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
        // go to the stack base (i.e., past reg1, reg0, and mem_base)
        let mut output = String::from(">>>");
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
        while tape.len() % 8 != 4 {
            tape.push(0);
        }
        let mut stack = Vec::<usize>::new();
        let mut heap = Vec::<usize>::new();
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
            frame_base,
            reg: tape[1],
            stack,
            heap,
        }
    }
}
