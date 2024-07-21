use std::collections::BTreeMap;

use crate::{ast, ir};

#[derive(Debug, Clone, Copy)]
struct Var<'a> {
    decl: &'a ast::VarDecl,
    stack_offset: isize,
}

fn compile_var_access(vars: &[Var], target_name: &str, mutating: bool) -> ir::DirectPlace {
    ir::DirectPlace::Stack {
        offset: vars
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
                Some(var.stack_offset)
            })
            .unwrap_or_else(|| panic!("Variable `{target_name}` not found.")),
    }
}

fn compile_expr(expr: &ast::Expr, vars: &[Var]) -> ir::Value {
    match expr {
        ast::Expr::Simple(ast::SimpleExpr::Int(i)) => ir::Value::Immediate(*i),
        ast::Expr::Simple(ast::SimpleExpr::Place(ast::Place::Var(name))) => {
            ir::Value::Deref(ir::Place::Direct(compile_var_access(vars, name, false)))
        }
        ast::Expr::Simple(ast::SimpleExpr::Place(ast::Place::Deref(name))) => {
            ir::Value::Deref(ir::Place::Indirect(ir::IndirectPlace::Heap {
                address: compile_var_access(vars, name, false),
            }))
        }
        ast::Expr::Call { func, args } => {
            todo!("Call expression");
            // Stack order: [return value, return address, arguments, local variables]
            // The caller leaves space for the return value and pushes the return address and arguments
            // The callee is responsible for pushing and popping local variables when returning
            // The runtime system pops the return address and goes to the appropriate fragment
            // The callee is responsible for popping or using the return address
        }
    }
}

fn compile_place(place: &ast::Place, vars: &[Var], mutating: bool) -> ir::Place {
    match place {
        ast::Place::Var(name) => ir::Place::Direct(compile_var_access(vars, name, mutating)),
        ast::Place::Deref(name) => ir::Place::Indirect(ir::IndirectPlace::Heap {
            address: compile_var_access(vars, name, false),
        }),
    }
}

fn compile_store_mode(mode: ast::AssignMode) -> ir::StoreMode {
    match mode {
        ast::AssignMode::Replace => ir::StoreMode::Replace,
        ast::AssignMode::Add => ir::StoreMode::Add,
        ast::AssignMode::Subtract => ir::StoreMode::Subtract,
    }
}

#[deny(unused_must_use)]
#[must_use]
fn compile_block<'a>(
    block: &'a [ast::Statement],
    mut instructions: Vec<ir::Instruction>, // start with some instructions, if desired
    vars: &mut Vec<Var<'a>>,
    stack_offset: &mut isize,
) -> Vec<ir::Instruction> {
    let vars_len = vars.len();
    for statement in block {
        match statement {
            ast::Statement::Let { decl, value } => {
                instructions.push(ir::Instruction::Move {
                    dst: ir::Place::Direct(ir::DirectPlace::Stack {
                        offset: *stack_offset,
                    }),
                    src: compile_expr(value, vars),
                    store_mode: ir::StoreMode::Add,
                });
                vars.push(Var {
                    decl,
                    stack_offset: *stack_offset,
                });
                *stack_offset -= 1;
            }
            ast::Statement::Assign { place, value, mode } => {
                instructions.push(ir::Instruction::Move {
                    dst: compile_place(place, vars, true),
                    src: compile_expr(value, vars),
                    store_mode: compile_store_mode(*mode),
                });
            }
            ast::Statement::While { cond, body } => {
                let cond = match compile_expr(cond, vars) {
                    ir::Value::Deref(place) => place,
                    ir::Value::Immediate(value) => {
                        if value == 0 {
                            continue;
                        }
                        let place = ir::Place::Direct(ir::DirectPlace::Stack {
                            offset: *stack_offset,
                        });
                        *stack_offset -= 1;
                        instructions.push(ir::Instruction::Move {
                            dst: place,
                            src: ir::Value::Immediate(1),
                            store_mode: ir::StoreMode::Add,
                        });
                        place
                    }
                };
                let body = compile_block(body, vec![], vars, stack_offset);
                instructions.push(ir::Instruction::While { cond, body });
            }
            ast::Statement::IfElse {
                cond,
                main_body,
                else_body,
            } => match compile_expr(cond, vars) {
                ir::Value::Deref(place) => {
                    instructions.push(ir::Instruction::Switch {
                        cond: place,
                        cases: vec![compile_block(else_body, vec![], vars, stack_offset)],
                        default: compile_block(main_body, vec![], vars, stack_offset),
                    });
                }
                ir::Value::Immediate(value) => {
                    instructions = compile_block(
                        if value == 0 { else_body } else { main_body },
                        instructions,
                        vars,
                        stack_offset,
                    );
                }
            },
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

                let Some(&max_case) = case_map.keys().last() else {
                    instructions = compile_block(default, instructions, vars, stack_offset);
                    continue;
                };

                match compile_expr(cond, vars) {
                    ir::Value::Deref(place) => {
                        instructions.push(ir::Instruction::Switch {
                            cond: place,
                            cases: (0..=max_case)
                                .map(|value| {
                                    compile_block(
                                        case_map.get(&value).copied().unwrap_or(default.as_slice()),
                                        vec![],
                                        vars,
                                        stack_offset,
                                    )
                                })
                                .collect(),
                            default: compile_block(default, vec![], vars, stack_offset),
                        });
                    }
                    ir::Value::Immediate(value) => {
                        instructions = compile_block(
                            case_map.get(&value).copied().unwrap_or(default.as_slice()),
                            instructions,
                            vars,
                            stack_offset,
                        );
                    }
                };
            }
            ast::Statement::Block { body } => {
                instructions = compile_block(body, instructions, vars, stack_offset);
            }
            ast::Statement::Return(value) => {
                todo!("Return statement");
            }
        }
    }
    // TODO: perhaps shrink the stack here instead of at the end of the function
    vars.truncate(vars_len);
    instructions
}

struct Fragments<'a> {
    code: Vec<Vec<ir::Instruction>>,
    func_ids: BTreeMap<&'a str, usize>,
}

fn compile_func<'a>(func: &'a ast::Function, frags: &mut Fragments<'a>) -> usize {
    if let Some(&func_id) = frags.func_ids.get(func.name.as_str()) {
        // function already compiled
        return func_id;
    }
    let mut stack_offset = -1;
    let mut vars = vec![];
    for decl in &func.params {
        vars.push(Var { decl, stack_offset });
        stack_offset += 1;
    }

    let mut instructions = compile_block(
        &func.body,
        vec![
            ir::Instruction::ResizeStack {
                // remove the call address
                amount: -1,
            },
            ir::Instruction::ResizeStack {
                // placeholder until we know how much space we actually need
                amount: 0,
            },
        ],
        &mut vars,
        &mut stack_offset,
    );

    // TODO: do this at the block-level, not the function-level:
    {
        // make space for parameters and local variables
        instructions[1] = ir::Instruction::ResizeStack {
            amount: -stack_offset - 1,
        };

        // pop local variables
        instructions.push(ir::Instruction::ResizeStack {
            amount: stack_offset + 1,
        });
    }

    // jump to the exit fragment
    instructions.extend([
        ir::Instruction::ResizeStack { amount: 1 },
        ir::Instruction::Move {
            dst: ir::Place::Direct(ir::DirectPlace::Stack { offset: -1 }),
            src: ir::Value::Immediate(0),
            store_mode: ir::StoreMode::Replace,
        },
    ]);

    let func_id = frags.code.len();
    frags.code.push(instructions);
    frags.func_ids.insert(func.name.as_str(), func_id);
    func_id
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

    let mut fragments = Fragments {
        code: vec![vec![]], // frag 0 is the exit psuedo-fragment (it never actually gets executed due to the while loop)
        func_ids: BTreeMap::from([("exit", 0)]),
    };

    let main_func_id = compile_func(
        functions.get("main").expect("Function `main` not found"),
        &mut fragments,
    );

    ir::Program {
        instructions: vec![
            ir::Instruction::ResizeStack { amount: 1 },
            ir::Instruction::Move {
                dst: ir::Place::Direct(ir::DirectPlace::Stack { offset: 0 }),
                src: ir::Value::Immediate(main_func_id),
                store_mode: ir::StoreMode::Replace,
            },
            ir::Instruction::While {
                cond: ir::Place::Direct(ir::DirectPlace::Stack { offset: -1 }),
                body: vec![ir::Instruction::Switch {
                    cond: ir::Place::Direct(ir::DirectPlace::Stack { offset: -1 }),
                    cases: fragments.code,
                    // default should never run unless we implement dynamic dispatch and an invalid function pointer is called
                    default: vec![],
                }],
            },
            ir::Instruction::ResizeStack { amount: -1 },
        ],
    }
}
