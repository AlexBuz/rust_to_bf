use {
    super::bf::{
        self,
        Instruction::{self, *},
    },
    crate::middle::ir::{
        self,
        DirectPlace::{self, *},
        IndirectPlace::{self, *},
        Place::{self, *},
        StoreMode,
        Value::*,
    },
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

impl DirectPlace {
    fn path_to_and_from(&self) -> (Vec<Instruction>, Vec<Instruction>) {
        match *self {
            StackFrame { offset } => {
                let sb_to_place = vec![Right(8), Loop(vec![Right(8)]), Left(2), Right(8 * offset)];
                let place_to_sb = vec![Left(8 * offset + 6), Loop(vec![Left(8)])];
                (sb_to_place, place_to_sb)
            }
            Address(address) => {
                let sb_to_place = vec![Right(4 * address + 2)];
                let place_to_sb = vec![Left(4 * address + 2)];
                (sb_to_place, place_to_sb)
            }
        }
    }

    fn emit_load(&self, output: &mut Vec<Instruction>, multiplier: usize) {
        let (sb_to_src, src_to_sb) = self.path_to_and_from();

        // go to the source cell
        output.extend(sb_to_src.iter().cloned());

        // use temp cell to preserve the value
        output.push(Loop(vec![Sub(1), Left(1), Add(1), Right(1)]));

        // copy the value to reg0
        output.extend([
            Left(1),
            Loop(vec![
                Sub(1),
                Right(1),
                Add(1),
                ..src_to_sb.iter().cloned(),
                Left(2),
                Add(multiplier as _),
                Right(2),
                ..sb_to_src.iter().cloned(),
                Left(1),
            ]),
            Right(1),
        ]);

        // go back to the stack base
        output.extend(src_to_sb);
    }

    fn emit_store(&self, output: &mut Vec<Instruction>, mode: StoreMode) {
        let (sb_to_dst, dst_to_sb) = self.path_to_and_from();

        let operator = match mode {
            StoreMode::Add => Add(1),
            StoreMode::Subtract => Sub(1),
            StoreMode::Replace => {
                output.extend(iter![
                    ..sb_to_dst.iter().cloned(),
                    Loop(vec![Sub(1)]),
                    ..dst_to_sb.iter().cloned(),
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
            ..sb_to_dst,
            operator,
            ..dst_to_sb,
            Left(2),
        ]));

        // go back to the stack base
        output.push(Right(2));
    }
}

impl IndirectPlace {
    fn path_to_and_from(&self) -> (Vec<Instruction>, Vec<Instruction>) {
        match self {
            Deref { address } => {
                let mut sb_to_place = vec![];

                // put the address in reg0
                address.emit_load(&mut sb_to_place, 1);

                // leave a trail of mem markers up to the desired location
                sb_to_place.extend([
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

                // clear the trail of mem markers and go back to the stack base
                let place_to_sb = vec![Left(3), Loop(vec![Sub(1), Left(4)]), Right(1)];

                (sb_to_place, place_to_sb)
            }
        }
    }

    fn emit_load(&self, output: &mut Vec<Instruction>, multiplier: usize) {
        match self {
            Deref { .. } => {
                let (sb_to_place, place_to_sb) = self.path_to_and_from();

                // go to the source cell
                output.extend(sb_to_place.iter().cloned());

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
                output.extend(place_to_sb);
            }
        }
    }

    fn emit_store(&self, output: &mut Vec<Instruction>, mode: StoreMode) {
        match self {
            Deref { .. } => {
                // move the value from reg0 to reg1 so that reg0 can be used to store the address
                output.extend([
                    Left(2),
                    Loop(vec![Sub(1), Left(1), Add(1), Right(1)]),
                    Right(2),
                ]);

                let (sb_to_place, place_to_sb) = self.path_to_and_from();
                output.extend(sb_to_place.iter().cloned());

                let operator = match mode {
                    StoreMode::Add => Add(1),
                    StoreMode::Subtract => Sub(1),
                    StoreMode::Replace => {
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
                output.extend(place_to_sb);
            }
        }
    }
}

impl Place {
    fn path_to_and_from(&self) -> (Vec<Instruction>, Vec<Instruction>) {
        match self {
            Direct(direct) => direct.path_to_and_from(),
            Indirect(indirect) => indirect.path_to_and_from(),
        }
    }

    fn emit_load(&self, output: &mut Vec<Instruction>, multiplier: usize) {
        match self {
            Direct(direct) => direct.emit_load(output, multiplier),
            Indirect(indirect) => indirect.emit_load(output, multiplier),
        }
    }

    fn emit_store(&self, output: &mut Vec<Instruction>, mode: StoreMode) {
        match self {
            Direct(direct) => direct.emit_store(output, mode),
            Indirect(indirect) => indirect.emit_store(output, mode),
        }
    }

    fn emit_load_ref(&self, output: &mut Vec<Instruction>) {
        match *self {
            Direct(direct) => match direct {
                StackFrame { offset } => {
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
                Address(address) => output.extend([Left(2), Add(address as _), Right(2)]),
            },
            Indirect(indirect) => match indirect {
                Deref { address } => address.emit_load(output, 1),
            },
        }
    }

    fn emit_bf(
        &self,
        output: &mut Vec<Instruction>,
        instructions: impl IntoIterator<Item = Instruction>,
    ) {
        let (sb_to_place, place_to_sb) = self.path_to_and_from();
        output.extend(sb_to_place);
        output.extend(instructions);
        output.extend(place_to_sb);
    }
}

fn compile_sandwich(
    mut before: Vec<Instruction>,
    instructions: &[ir::Instruction],
    after: impl IntoIterator<Item = Instruction>,
) -> Vec<Instruction> {
    for instr in instructions {
        instr.convert_to_bf(&mut before);
    }
    before.extend(after);
    before
}

impl ir::Instruction {
    fn convert_to_bf(&self, output: &mut Vec<Instruction>) {
        // pointer should be at the stack base before and after each instruction
        match *self {
            ir::Instruction::Load { src, multiplier } => src.emit_load(output, multiplier),
            ir::Instruction::LoadRef { src } => src.emit_load_ref(output),
            ir::Instruction::Store { dst, store_mode } => dst.emit_store(output, store_mode),
            ir::Instruction::StoreImm {
                dst,
                value,
                store_mode,
            } => match store_mode {
                StoreMode::Add => dst.emit_bf(output, iter![Add(value as _)]),
                StoreMode::Subtract => dst.emit_bf(output, iter![Sub(value as _)]),
                StoreMode::Replace => {
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
                let (sb_to_cond, cond_to_sb) = cond.path_to_and_from();
                output.extend(iter![
                    ..sb_to_cond.iter().cloned(),
                    Loop(compile_sandwich(
                        cond_to_sb.clone(),
                        body,
                        sb_to_cond.iter().cloned(),
                    )),
                    ..cond_to_sb,
                ]);
            }
            ir::Instruction::Switch {
                cond,
                ref cases,
                ref default,
            } => {
                if cases.is_empty() {
                    for instr in default {
                        instr.convert_to_bf(output);
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
                        let mut body = compile_sandwich(
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
                        let mut body = compile_sandwich(
                            vec![Loop(vec![Sub(1)]), Left(1), Sub(1), Right(3)],
                            default,
                            iter![Left(2)],
                        );
                        for case in cases.iter().rev() {
                            body = vec![
                                Sub(1),
                                Loop(body),
                                Left(1),
                                Loop(compile_sandwich(
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
                Immediate(value) => {
                    // put the value in reg0, print it, clear reg0, and go back to the stack base
                    output.extend(iter![
                        Left(2),
                        Add(value as _),
                        Output,
                        Loop(vec![Sub(1)]),
                        Right(2)
                    ]);
                }
                At(src) => src.emit_bf(output, iter![Output]),
            },
        }
    }
}

pub(super) fn compile(program: &ir::Program) -> bf::Program {
    bf::Program {
        instructions: compile_sandwich(
            vec![Instruction::Right(3)], // start at the stack base
            &program.instructions,
            [],
        ),
    }
}
