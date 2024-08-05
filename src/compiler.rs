use std::collections::BTreeMap;

use crate::{ast, ir};

#[derive(Debug, Clone, Copy)]
struct Var<'a> {
    decl: &'a ast::VarDecl,
    frame_offset: usize,
}

fn compile_var_access(vars: &Vars, target_name: &str, mutating: bool) -> ir::DirectPlace {
    ir::DirectPlace::StackTop {
        offset: vars
            .vars
            .iter()
            .rev()
            .find_map(|var| {
                let ast::VarDecl { name, mutable } = var.decl;
                if target_name != name {
                    return None;
                }
                if mutating && !mutable {
                    panic!("Variable `{name}` is not mutable.");
                }
                Some(vars.frame_offset - var.frame_offset - 1)
            })
            .unwrap_or_else(|| panic!("Variable `{target_name}` not found.")),
    }
}

fn compile_place(place: &ast::Place, vars: &Vars, mutating: bool) -> ir::Place {
    match place {
        ast::Place::Var(name) => ir::Place::Direct(compile_var_access(vars, name, mutating)),
        ast::Place::Deref(name) => ir::Place::Indirect(ir::IndirectPlace::Heap {
            address: compile_var_access(vars, name, false),
        }),
    }
}

fn compile_expr(expr: &ast::Expr, vars: &Vars) -> ir::Value {
    match expr {
        ast::Expr::Simple(ast::SimpleExpr::Int(i)) => ir::Value::Immediate(*i),
        ast::Expr::Simple(ast::SimpleExpr::Place(place)) => {
            ir::Value::Deref(compile_place(place, vars, false))
        }
        ast::Expr::Call { func, args } => {
            // ir::Instruction::GrowStack {
            //     // make space for parameters
            //     amount: frame_size,
            // };

            // pop the parameters
            // instructions.push(ir::Instruction::ShrinkStack { amount: frame_size });

            todo!("Call expression");
            // Stack order: [return value, return address, arguments, local variables]
            // The caller leaves space for the return value and pushes the return address and arguments
            // The callee is responsible for pushing and popping local variables when returning
            // The runtime system pops the return address and goes to the appropriate fragment
            // The callee is responsible for popping or using the return address
        }
    }
}

fn compile_store_mode(mode: ast::AssignMode) -> ir::StoreMode {
    match mode {
        ast::AssignMode::Replace => ir::StoreMode::Replace,
        ast::AssignMode::Add => ir::StoreMode::Add,
        ast::AssignMode::Subtract => ir::StoreMode::Subtract,
    }
}

struct Vars<'a> {
    vars: Vec<Var<'a>>,
    frame_offset: usize,
}

impl<'a> Vars<'a> {
    fn new() -> Self {
        Self {
            vars: vec![],
            frame_offset: 0,
        }
    }

    fn push(&mut self, decl: &'a ast::VarDecl) {
        self.vars.push(Var {
            decl,
            frame_offset: self.frame_offset,
        });
        self.frame_offset += 1;
    }

    fn truncate(&mut self, final_frame_size: usize) {
        // while let Some(var) = vars.last() {
        //     if var.frame_offset >= final_frame_size {
        //         vars.pop();
        //     } else {
        //         break;
        //     }
        // }
        // vars.retain(|var| var.frame_offset < final_frame_size);
        self.vars.truncate(
            self.vars
                .iter()
                .rev()
                .position(|var| var.frame_offset < final_frame_size)
                .map(|pos| self.vars.len() - pos)
                .unwrap_or(0),
        );
    }
}

type FragId = usize;

#[deny(unused_must_use)]
#[must_use]
fn compile_block<'a>(
    statements: &'a [ast::Statement],
    instructions: Vec<ir::Instruction>, // start with some instructions, if desired
    frags: &mut Fragments<'a>,
    final_frame_size: usize,
    vars: &mut Vars<'a>,
) -> (FragId, FragId) {
    let frag_start = frags.add(instructions);
    let mut frag_end = frag_start;
    for statement in statements {
        match statement {
            ast::Statement::Let { decl, value } => {
                frags.code[frag_end].extend([
                    ir::Instruction::GrowStack { amount: 1 },
                    ir::Instruction::Move {
                        dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                        src: compile_expr(value, vars),
                        store_mode: ir::StoreMode::Add,
                    },
                ]);
                vars.push(decl);
            }
            ast::Statement::Assign { place, value, mode } => {
                frags.code[frag_end].push(ir::Instruction::Move {
                    dst: compile_place(place, vars, true),
                    src: compile_expr(value, vars),
                    store_mode: compile_store_mode(*mode),
                });
            }
            ast::Statement::Loop { body } => {
                todo!("Loop statement");
                // let cond = ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 });
                // instructions.extend([
                //     ir::Instruction::GrowStack { amount: 1 },
                //     ir::Instruction::Move {
                //         dst: cond,
                //         src: ir::Value::Immediate(1),
                //         store_mode: ir::StoreMode::Add,
                //     },
                // ]);
                // vars.frame_offset += 1;
                // let body = compile_block(body, vec![], frags, vars.frame_offset, vars);
                // instructions.push(ir::Instruction::While { cond, body });
            }
            ast::Statement::While { cond, body } => {
                // instructions.push(ir::Instruction::While {
                //     cond: compile_place(cond, vars, false),
                //     body: compile_block(body, vec![], frags, vars.frame_offset, vars),
                // });
                vars.frame_offset += 2;
                let cond = compile_place(cond, vars, false);

                let (loop_start, loop_end) =
                    compile_block(body, vec![], frags, vars.frame_offset, vars);

                let loop_body = std::mem::take(&mut frags.code[loop_start]);
                frags.code[loop_start].push(ir::Instruction::Switch {
                    cond,
                    cases: vec![vec![ir::Instruction::ShrinkStack { amount: 1 }]],
                    default: loop_body,
                });

                let old_frag_end = frag_end;
                frag_end = frags.add(vec![ir::Instruction::ShrinkStack { amount: 1 }]);
                vars.frame_offset -= 2;

                frags.code[old_frag_end].extend([
                    ir::Instruction::GrowStack { amount: 2 },
                    ir::Instruction::Move {
                        dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                        src: ir::Value::Immediate(loop_start),
                        store_mode: ir::StoreMode::Add,
                    },
                    ir::Instruction::Move {
                        dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 1 }),
                        src: ir::Value::Immediate(frag_end),
                        store_mode: ir::StoreMode::Add,
                    },
                ]);
            }
            ast::Statement::Break => {
                vars.truncate(final_frame_size);
                frags.code[frag_end].push(ir::Instruction::ShrinkStack {
                    amount: 1 + vars.frame_offset - final_frame_size,
                });
                return (frag_start, frag_end);
            }
            &ast::Statement::Continue => {
                vars.truncate(final_frame_size);
                frags.code[frag_end].push(ir::Instruction::ShrinkStack {
                    amount: vars.frame_offset - final_frame_size,
                });
                return (frag_start, frag_end);
            }
            ast::Statement::IfElse {
                cond,
                main_body,
                else_body,
            } => {
                match compile_expr(cond, vars) {
                    ir::Value::Deref(place) => {
                        let [(main_start, main_end), (else_start, else_end)] =
                            [main_body, else_body].map(|body| {
                                compile_block(
                                    body,
                                    vec![ir::Instruction::ShrinkStack { amount: 1 }],
                                    frags,
                                    vars.frame_offset,
                                    vars,
                                )
                            });
                        frags.code[frag_end].extend([ir::Instruction::Switch {
                            cond: place,
                            cases: vec![vec![
                                ir::Instruction::GrowStack { amount: 1 },
                                ir::Instruction::Move {
                                    dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                                    src: ir::Value::Immediate(else_start),
                                    store_mode: ir::StoreMode::Add,
                                },
                            ]],
                            default: vec![
                                ir::Instruction::GrowStack { amount: 1 },
                                ir::Instruction::Move {
                                    dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                                    src: ir::Value::Immediate(main_start),
                                    store_mode: ir::StoreMode::Add,
                                },
                            ],
                        }]);
                        frag_end = frags.add(vec![ir::Instruction::ShrinkStack { amount: 1 }]);
                        for frag in [main_end, else_end] {
                            frags.code[frag].extend([
                                ir::Instruction::GrowStack { amount: 1 },
                                ir::Instruction::Move {
                                    dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                                    src: ir::Value::Immediate(frag_end),
                                    store_mode: ir::StoreMode::Add,
                                },
                            ]);
                        }
                    }
                    ir::Value::Immediate(value) => {
                        let (block_start, block_end) = compile_block(
                            if value != 0 { main_body } else { else_body },
                            vec![ir::Instruction::ShrinkStack { amount: 1 }],
                            frags,
                            vars.frame_offset,
                            vars,
                        );
                        frags.code[frag_end].extend([
                            ir::Instruction::GrowStack { amount: 1 },
                            ir::Instruction::Move {
                                dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                                src: ir::Value::Immediate(block_start),
                                store_mode: ir::StoreMode::Add,
                            },
                        ]);
                        frag_end = block_end;
                    }
                };
            }
            ast::Statement::Switch {
                cond,
                cases,
                default,
            } => {
                let case_map = cases
                    .iter()
                    .map(|&(value, ref body)| (value, body.as_slice()))
                    .collect::<BTreeMap<_, _>>();

                if case_map.len() != cases.len() {
                    panic!("Duplicate case values");
                }

                let Some(&last_case) = case_map.keys().last() else {
                    let (block_start, block_end) = compile_block(
                        default,
                        vec![ir::Instruction::ShrinkStack { amount: 1 }],
                        frags,
                        vars.frame_offset,
                        vars,
                    );
                    frags.code[frag_end].extend([
                        ir::Instruction::GrowStack { amount: 1 },
                        ir::Instruction::Move {
                            dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                            src: ir::Value::Immediate(block_start),
                            store_mode: ir::StoreMode::Add,
                        },
                    ]);
                    frag_end = block_end;
                    continue;
                };

                match compile_expr(cond, vars) {
                    ir::Value::Deref(place) => {
                        let case_frags = (0..=last_case)
                            .map(|value| {
                                compile_block(
                                    case_map.get(&value).copied().unwrap_or(default.as_slice()),
                                    vec![ir::Instruction::ShrinkStack { amount: 1 }],
                                    frags,
                                    vars.frame_offset,
                                    vars,
                                )
                            })
                            .collect::<Vec<_>>();
                        let default_frag = compile_block(
                            default,
                            vec![ir::Instruction::ShrinkStack { amount: 1 }],
                            frags,
                            vars.frame_offset,
                            vars,
                        );
                        frags.code[frag_end].extend([ir::Instruction::Switch {
                            cond: place,
                            cases: case_frags
                                .iter()
                                .map(|&(start, _)| {
                                    vec![
                                        ir::Instruction::GrowStack { amount: 1 },
                                        ir::Instruction::Move {
                                            dst: ir::Place::Direct(ir::DirectPlace::StackTop {
                                                offset: 0,
                                            }),
                                            src: ir::Value::Immediate(start),
                                            store_mode: ir::StoreMode::Add,
                                        },
                                    ]
                                })
                                .collect(),
                            default: vec![
                                ir::Instruction::GrowStack { amount: 1 },
                                ir::Instruction::Move {
                                    dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                                    src: ir::Value::Immediate(default_frag.0),
                                    store_mode: ir::StoreMode::Add,
                                },
                            ],
                        }]);
                        frag_end = frags.add(vec![ir::Instruction::ShrinkStack { amount: 1 }]);
                        for (_, end) in case_frags {
                            frags.code[end].extend([
                                ir::Instruction::GrowStack { amount: 1 },
                                ir::Instruction::Move {
                                    dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                                    src: ir::Value::Immediate(frag_end),
                                    store_mode: ir::StoreMode::Add,
                                },
                            ]);
                        }
                    }
                    ir::Value::Immediate(value) => {
                        let (block_start, block_end) = compile_block(
                            case_map.get(&value).copied().unwrap_or(default.as_slice()),
                            vec![ir::Instruction::ShrinkStack { amount: 1 }],
                            frags,
                            vars.frame_offset,
                            vars,
                        );
                        frags.code[frag_end].extend([
                            ir::Instruction::GrowStack { amount: 1 },
                            ir::Instruction::Move {
                                dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                                src: ir::Value::Immediate(block_start),
                                store_mode: ir::StoreMode::Add,
                            },
                        ]);
                        frag_end = block_end;
                    }
                };
            }
            ast::Statement::Block { body } => {
                let (block_start, block_end) = compile_block(
                    body,
                    vec![ir::Instruction::ShrinkStack { amount: 1 }],
                    frags,
                    vars.frame_offset,
                    vars,
                );
                frags.code[frag_end].extend([
                    ir::Instruction::GrowStack { amount: 1 },
                    ir::Instruction::Move {
                        dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                        src: ir::Value::Immediate(block_start),
                        store_mode: ir::StoreMode::Add,
                    },
                ]);
                frag_end = block_end;
            }
            ast::Statement::Return(value) => {
                frags.code[frag_end].extend([
                    ir::Instruction::ShrinkStack {
                        amount: vars.frame_offset,
                    },
                    ir::Instruction::Move {
                        dst: ir::Place::Direct(ir::DirectPlace::StackTop {
                            offset: vars.frame_offset + 1,
                        }),
                        src: compile_expr(value, vars),
                        store_mode: ir::StoreMode::Replace,
                    },
                ]);
                vars.truncate(final_frame_size);
                frag_end = 0;
            }
        }
    }
    vars.truncate(final_frame_size);
    frags.code[frag_end].push(ir::Instruction::ShrinkStack {
        amount: vars.frame_offset - final_frame_size,
    });
    (frag_start, frag_end)
}

struct Fragments<'a> {
    code: Vec<Vec<ir::Instruction>>,
    func_ids: BTreeMap<&'a str, usize>,
}

impl Fragments<'_> {
    fn new() -> Self {
        Self {
            code: vec![vec![]], // frag 0 is the exit psuedo-fragment (it never actually gets executed due to the while loop)
            func_ids: BTreeMap::from([("exit", 0)]),
        }
    }

    fn add(&mut self, instructions: Vec<ir::Instruction>) -> FragId {
        let frag_id = self.code.len();
        self.code.push(instructions);
        frag_id
    }
}

fn compile_func<'a>(func: &'a ast::Function, frags: &mut Fragments<'a>) -> usize {
    if let Some(&func_id) = frags.func_ids.get(func.name.as_str()) {
        // function already compiled
        return func_id;
    }

    let mut vars = Vars::new();
    for decl in &func.params {
        vars.push(decl);
    }

    let (enter_frag_id, _exit_frag_id) = compile_block(
        &func.body,
        vec![ir::Instruction::ShrinkStack {
            // remove the call address
            amount: 1,
        }],
        frags,
        0,
        &mut vars,
    );

    frags.func_ids.insert(func.name.as_str(), enter_frag_id);
    enter_frag_id
}

pub fn compile(ast: ast::Ast) -> ir::Program {
    let functions = ast
        .functions
        .iter()
        .map(|func| (func.name.as_str(), func))
        .collect::<BTreeMap<_, _>>();

    if functions.len() != ast.functions.len() {
        panic!("Duplicate function names");
    }

    let mut fragments = Fragments::new();

    let main_func_id = compile_func(
        functions.get("main").expect("Function `main` not found"),
        &mut fragments,
    );

    ir::Program {
        instructions: vec![
            ir::Instruction::GrowStack { amount: 3 },
            ir::Instruction::Move {
                dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                src: ir::Value::Immediate(main_func_id),
                store_mode: ir::StoreMode::Add,
            },
            ir::Instruction::While {
                cond: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                body: vec![ir::Instruction::Switch {
                    cond: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                    cases: fragments.code,
                    // default should never run unless we implement dynamic dispatch and an invalid function pointer is called
                    default: vec![],
                }],
            },
            ir::Instruction::ShrinkStack { amount: 2 },
        ],
    }
}
