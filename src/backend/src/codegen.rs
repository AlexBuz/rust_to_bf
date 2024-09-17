use {
    super::bf::{self, Instruction::*},
    middle::ir,
    std::iter::repeat,
    velcro::{iter, vec},
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

struct PlacePath {
    to: Vec<bf::Instruction>,
    from: Vec<bf::Instruction>,
}

trait BfPlace {
    fn path(&self) -> PlacePath;

    fn emit_load(&self, output: &mut Vec<bf::Instruction>, multiplier: usize);

    fn emit_store(&self, output: &mut Vec<bf::Instruction>, mode: ir::StoreMode);

    fn emit_load_ref(&self, output: &mut Vec<bf::Instruction>);

    fn emit_bf(
        &self,
        output: &mut Vec<bf::Instruction>,
        instructions: impl IntoIterator<Item = bf::Instruction>,
    ) {
        let path = self.path();
        output.extend(path.to);
        output.extend(instructions);
        output.extend(path.from);
    }
}

impl BfPlace for ir::DirectPlace {
    fn path(&self) -> PlacePath {
        match *self {
            ir::DirectPlace::StackFrame { offset } => PlacePath {
                to: vec![Right(8), Loop(vec![Right(8)]), Left(2), Right(8 * offset)],
                from: vec![Left(8 * offset + 6), Loop(vec![Left(8)])],
            },
            ir::DirectPlace::Address(address) => PlacePath {
                to: vec![Right(4 * address + 2)],
                from: vec![Left(4 * address + 2)],
            },
        }
    }

    fn emit_load(&self, output: &mut Vec<bf::Instruction>, multiplier: usize) {
        let path = self.path();

        // go to the source cell
        output.extend(path.to.iter().cloned());

        // use temp cell to preserve the value
        output.push(Loop(vec![Sub(1), Left(1), Add(1), Right(1)]));

        // copy the value to reg0
        output.extend([
            Left(1),
            Loop(vec![
                Sub(1),
                Right(1),
                Add(1),
                ..path.from.iter().cloned(),
                Left(2),
                Add(multiplier as _),
                Right(2),
                ..path.to.iter().cloned(),
                Left(1),
            ]),
            Right(1),
        ]);

        // go back to the stack base
        output.extend(path.from);
    }

    fn emit_store(&self, output: &mut Vec<bf::Instruction>, mode: ir::StoreMode) {
        let path = self.path();

        let operator = match mode {
            ir::StoreMode::Add => Add(1),
            ir::StoreMode::Subtract => Sub(1),
            ir::StoreMode::Replace => {
                output.extend(iter![
                    ..path.to.iter().cloned(),
                    Loop(vec![Sub(1)]),
                    ..path.from.iter().cloned(),
                ]);
                Add(1)
            }
        };

        // go to reg0
        output.push(Left(2));

        // move the value to the destination cell
        output.push(Loop(vec![
            Sub(1),
            Right(2),
            ..path.to,
            operator,
            ..path.from,
            Left(2),
        ]));

        // go back to the stack base
        output.push(Right(2));
    }

    fn emit_load_ref(&self, output: &mut Vec<bf::Instruction>) {
        match *self {
            ir::DirectPlace::StackFrame { offset } => {
                // leave a trail of mem markers up to the frame base
                output.extend([Right(8), Loop(vec![Left(1), Add(1), Right(9)])]);

                // count the mem markers (while clearing them) and put the result in reg1
                output.extend([
                    Left(8),
                    Loop(vec![
                        Left(1),
                        Loop(vec![Sub(1), Left(8), Add(1), Right(8)]),
                        Left(7),
                    ]),
                ]);

                // put the frame base address (2 * reg1 + 1) in reg0, and clear reg1
                output.extend([
                    Left(1),
                    Loop(vec![Sub(1), Left(1), Add(2), Right(1)]),
                    Left(1),
                    Add(1),
                ]);

                // add the offset to the frame base address
                output.push(Add((2 * offset) as _));

                // go back to the stack base
                output.push(Right(2));
            }
            ir::DirectPlace::Address(address) => {
                output.extend([Left(2), Add(address as _), Right(2)])
            }
        }
    }
}

impl BfPlace for ir::IndirectPlace {
    fn path(&self) -> PlacePath {
        match self {
            ir::IndirectPlace::Deref { address } => PlacePath {
                to: {
                    let mut path_to_place = vec![];

                    // put the address in reg0
                    address.emit_load(&mut path_to_place, 1);

                    // leave a trail of mem markers up to the desired location
                    path_to_place.extend([
                        Left(2),
                        Loop(vec![Sub(1), Right(5), Add(1), Left(5)]),
                        Right(5),
                        Loop(vec![
                            Sub(1),
                            Loop(vec![Sub(1), Right(4), Add(1), Left(4)]),
                            Add(1),
                            Right(4),
                        ]),
                        Left(1),
                    ]);

                    path_to_place
                },
                from: {
                    // clear the trail of mem markers and go back to the stack base
                    vec![Left(3), Loop(vec![Sub(1), Left(4)]), Right(1)]
                },
            },
        }
    }

    fn emit_load(&self, output: &mut Vec<bf::Instruction>, multiplier: usize) {
        match self {
            ir::IndirectPlace::Deref { .. } => {
                let path = self.path();

                // go to the source cell
                output.extend(path.to.iter().cloned());

                // use temp cell to preserve the value
                output.push(Loop(vec![Sub(1), Left(1), Add(1), Right(1)]));

                // copy the value to reg0
                output.extend([
                    Left(1),
                    Loop(vec![
                        Sub(1),
                        Right(1),
                        Add(1),
                        Left(3),
                        Loop(vec![Left(4)]),
                        Left(1),
                        Add(multiplier as _),
                        Right(5),
                        Loop(vec![Right(4)]),
                        Left(2),
                    ]),
                    Right(1),
                ]);

                // go back to the stack base
                output.extend(path.from);
            }
        }
    }

    fn emit_store(&self, output: &mut Vec<bf::Instruction>, mode: ir::StoreMode) {
        match self {
            ir::IndirectPlace::Deref { .. } => {
                // move the value from reg0 to reg1 so that reg0 can be used to store the address
                output.extend([
                    Left(2),
                    Loop(vec![Sub(1), Left(1), Add(1), Right(1)]),
                    Right(2),
                ]);

                let path = self.path();
                output.extend(path.to);

                let operator = match mode {
                    ir::StoreMode::Add => Add(1),
                    ir::StoreMode::Subtract => Sub(1),
                    ir::StoreMode::Replace => {
                        output.push(Loop(vec![Sub(1)]));
                        Add(1)
                    }
                };

                // go to reg1
                output.extend([Left(3), Loop(vec![Left(4)]), Left(2)]);

                // move the value to the destination cell
                output.extend([Loop(vec![
                    Sub(1),
                    Right(6),
                    Loop(vec![Right(4)]),
                    Left(1),
                    operator,
                    Left(3),
                    Loop(vec![Left(4)]),
                    Left(2),
                ])]);

                // clear the trail of mem markers and go back to the stack base
                output.extend([Right(6), Loop(vec![Right(4)]), Left(1)]);
                output.extend(path.from);
            }
        }
    }

    fn emit_load_ref(&self, output: &mut Vec<bf::Instruction>) {
        match *self {
            ir::IndirectPlace::Deref { address } => address.emit_load(output, 1),
        }
    }
}

impl BfPlace for ir::Place {
    fn path(&self) -> PlacePath {
        match self {
            ir::Place::Direct(place) => place.path(),
            ir::Place::Indirect(place) => place.path(),
        }
    }

    fn emit_load(&self, output: &mut Vec<bf::Instruction>, multiplier: usize) {
        match self {
            ir::Place::Direct(place) => place.emit_load(output, multiplier),
            ir::Place::Indirect(place) => place.emit_load(output, multiplier),
        }
    }

    fn emit_store(&self, output: &mut Vec<bf::Instruction>, mode: ir::StoreMode) {
        match self {
            ir::Place::Direct(place) => place.emit_store(output, mode),
            ir::Place::Indirect(place) => place.emit_store(output, mode),
        }
    }

    fn emit_load_ref(&self, output: &mut Vec<bf::Instruction>) {
        match *self {
            ir::Place::Direct(place) => place.emit_load_ref(output),
            ir::Place::Indirect(place) => place.emit_load_ref(output),
        }
    }
}

fn compile_instructions(
    mut before: Vec<bf::Instruction>,
    instructions: &[ir::Instruction],
    after: impl IntoIterator<Item = bf::Instruction>,
) -> Vec<bf::Instruction> {
    for instruction in instructions {
        compile_instruction(instruction, &mut before);
    }
    before.extend(after);
    before
}

fn compile_instruction(instruction: &ir::Instruction, output: &mut Vec<bf::Instruction>) {
    // pointer should be at the stack base before and after each instruction
    match *instruction {
        ir::Instruction::Load { src, multiplier } => src.emit_load(output, multiplier),
        ir::Instruction::LoadRef { src } => src.emit_load_ref(output),
        ir::Instruction::Store { dst, store_mode } => dst.emit_store(output, store_mode),
        ir::Instruction::StoreImm {
            dst,
            value,
            store_mode,
        } => match store_mode {
            ir::StoreMode::Add => dst.emit_bf(output, iter![Add(value as _)]),
            ir::StoreMode::Subtract => dst.emit_bf(output, iter![Sub(value as _)]),
            ir::StoreMode::Replace => {
                dst.emit_bf(output, iter![Loop(vec![Sub(1)]), Add(value as _)])
            }
        },
        ir::Instruction::SaveFrame { size } => {
            if size > 0 {
                output.extend(iter![
                    Right(8),
                    Loop(vec![Right(8)]),
                    Add(1),
                    ..repeat([Right(8), Add(1)]).take(size - 1).flatten(),
                    Loop(vec![Left(8)]),
                ]);
            }
        }
        ir::Instruction::RestoreFrame { size } => {
            if size > 0 {
                output.extend(iter![
                    Right(8),
                    Loop(vec![Right(8)]),
                    Left(8),
                    ..repeat([Sub(1), Left(8)]).take(size).flatten(),
                    Loop(vec![Left(8)]),
                ]);
            }
        }
        ir::Instruction::While { cond, ref body } => {
            let path = cond.path();
            output.extend(iter![
                ..path.to.iter().cloned(),
                Loop(compile_instructions(
                    path.from.clone(),
                    body,
                    path.to.iter().cloned(),
                )),
                ..path.from,
            ]);
        }
        ir::Instruction::Switch {
            cond,
            ref cases,
            ref default,
        } => {
            if cases.is_empty() {
                for instruction in default {
                    compile_instruction(instruction, output);
                }
            } else {
                cond.emit_load(output, 1);
                if cases.iter().all(Vec::is_empty) {
                    // optimization for switches that express `if x > some_literal { ... do stuff ...  } else { do nothing }`
                    // note that it's safe to subtract 1 from cases.len() because we know cases is nonempty at this point
                    /*
                    <<
                    [
                        -[
                            -[
                                [-]>> default <<
                            ]
                        ]
                    ]>>
                    */
                    let mut body = compile_instructions(
                        vec![Loop(vec![Sub(1)]), Right(2)],
                        default,
                        iter![Left(2)],
                    );
                    for _ in 1..cases.len() {
                        body = vec![Sub(1), Loop(body)];
                    }
                    output.extend(iter![Left(2), Loop(body), Right(2)]);
                } else {
                    /*
                    <<<+>
                    [
                        -[
                            -[
                                [-]<->>> default <<
                            ]<[->>> case 2 <<<]>
                        ]<[->>> case 1 <<<]>
                    ]<[->>> case 0 <<<]>
                    >>
                    */
                    let mut body = compile_instructions(
                        vec![Loop(vec![Sub(1)]), Left(1), Sub(1), Right(3)],
                        default,
                        iter![Left(2)],
                    );
                    for case in cases.iter().rev() {
                        body = vec![
                            Sub(1),
                            Loop(body),
                            Left(1),
                            Loop(compile_instructions(
                                vec![Sub(1), Right(3)],
                                case,
                                iter![Left(3)],
                            )),
                            Right(1),
                        ];
                    }
                    output.extend(iter![
                        Left(3),
                        Add(1),
                        Right(1),
                        ..body.into_iter().skip(1),
                        Right(2),
                    ]);
                }
            }
        }
        ir::Instruction::Input { dst } => dst.emit_bf(output, iter![Input]),
        ir::Instruction::Output { src } => match src {
            ir::Value::Immediate(value) => {
                // put the value in reg0, print it, clear reg0, and go back to the stack base
                output.extend(iter![
                    Left(2),
                    Add(value as _),
                    Output,
                    Loop(vec![Sub(1)]),
                    Right(2)
                ]);
            }
            ir::Value::At(src) => src.emit_bf(output, iter![Output]),
        },
    }
}

pub(super) fn compile(program: &ir::Program) -> bf::Program {
    bf::Program {
        instructions: compile_instructions(
            vec![Right(3)], // start at the stack base
            &program.instructions,
            [],
        ),
    }
}
