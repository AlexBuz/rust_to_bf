use std::collections::BTreeMap;

use crate::{ast, ir};

#[derive(Debug, Clone, Copy)]
struct Var<'a> {
    decl: &'a ast::VarDecl,
    frame_offset: usize,
}

fn compile_var_access(scope: &Scope, name: &str, mutating: bool) -> ir::DirectPlace {
    ir::DirectPlace::StackTop {
        offset: scope
            .vars
            .iter()
            .rev()
            .find_map(|var| {
                if name != var.decl.name {
                    return None;
                }
                if mutating && !var.decl.mutable {
                    panic!("Variable `{name}` is not mutable.");
                }
                Some(scope.frame_offset - var.frame_offset - 1)
            })
            .unwrap_or_else(|| panic!("Variable `{name}` not found.")),
    }
}

fn compile_place(place: &ast::Place, scope: &Scope, mutating: bool) -> ir::Place {
    match place {
        ast::Place::Var(name) => ir::Place::Direct(compile_var_access(scope, name, mutating)),
        ast::Place::Deref(name) => ir::Place::Indirect(ir::IndirectPlace::Heap {
            address: compile_var_access(scope, name, false),
        }),
    }
}

fn compile_simple_expr(expr: &ast::SimpleExpr, scope: &Scope) -> ir::Value {
    match expr {
        ast::SimpleExpr::Int(i) => ir::Value::Immediate(*i),
        ast::SimpleExpr::Place(place) => ir::Value::Deref(compile_place(place, scope, false)),
    }
}

type FragId = usize;

fn compile_call(
    func: &str,
    args: &[ast::Expr],
    scope: &mut Scope,
    cur_frag: &mut FragId,
) -> ir::Value {
    // Stack order: [return value, return address, arguments, local variables]
    // The caller leaves space for the return value and pushes the return address and arguments
    // The callee is responsible for popping the arguments when returning
    // The caller then pops the return address and uses the return value (now at the top of the stack)

    scope.global.code[*cur_frag].extend([ir::Instruction::GrowStack {
        amount: 2, // space for return value and return address
    }]);
    scope.frame_offset += 2;

    for arg in args {
        compile_expr_and_push(arg, scope, cur_frag)
    }

    let call_frag = compile_func(func, scope.global);
    let return_frag = scope
        .global
        .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);

    scope.global.code[*cur_frag].extend([
        ir::Instruction::Move {
            dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: args.len() }),
            src: ir::Value::Immediate(return_frag),
            store_mode: ir::StoreMode::Add,
        },
        ir::Instruction::GrowStack { amount: 1 },
        ir::Instruction::Move {
            dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
            src: ir::Value::Immediate(call_frag),
            store_mode: ir::StoreMode::Add,
        },
    ]);

    *cur_frag = return_frag;

    // keep only the return value
    scope.frame_offset -= 1 + args.len();

    ir::Value::Deref(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }))
}

fn compile_expr(expr: &ast::Expr, scope: &mut Scope, cur_frag: &mut FragId) -> ir::Value {
    match expr {
        ast::Expr::Simple(simple_expr) => compile_simple_expr(simple_expr, scope),
        ast::Expr::Call { func, args } => compile_call(func, args, scope, cur_frag),
    }
}

fn compile_expr_and_push(expr: &ast::Expr, scope: &mut Scope, cur_frag: &mut FragId) {
    match expr {
        ast::Expr::Simple(simple_expr) => {
            scope.frame_offset += 1;
            let src = compile_simple_expr(simple_expr, scope);
            scope.global.code[*cur_frag].extend([
                ir::Instruction::GrowStack { amount: 1 },
                ir::Instruction::Move {
                    dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                    src,
                    store_mode: ir::StoreMode::Add,
                },
            ]);
        }
        ast::Expr::Call { func, args } => {
            let before_offset = scope.frame_offset;
            compile_call(func, args, scope, cur_frag);
            assert!(scope.frame_offset == before_offset + 1);
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

// TODO: rename these fields to be more descriptive, e.g., `code` -> `basic_blocks`
struct GlobalState<'a> {
    code: Vec<Vec<ir::Instruction>>,
    func_ids: BTreeMap<&'a str, FragId>,
    functions: BTreeMap<&'a str, &'a ast::Function>,
}

impl<'a> GlobalState<'a> {
    fn new(functions: BTreeMap<&'a str, &'a ast::Function>) -> Self {
        Self {
            code: vec![vec![]], // frag 0 is the exit psuedo-fragment (it never actually gets executed due to the while loop)
            func_ids: BTreeMap::from([("exit", 0)]),
            functions,
        }
    }

    fn add_frag(&mut self, instructions: Vec<ir::Instruction>) -> FragId {
        let frag_id = self.code.len();
        self.code.push(instructions);
        frag_id
    }
}

struct Scope<'a, 'b> {
    vars: Vec<Var<'a>>,
    frame_offset: usize,
    global: &'b mut GlobalState<'a>,
}

impl<'a, 'b> Scope<'a, 'b> {
    fn new(global: &'b mut GlobalState<'a>) -> Self {
        Self {
            vars: vec![],
            frame_offset: 0,
            global,
        }
    }

    fn add_decl(&mut self, decl: &'a ast::VarDecl) {
        self.vars.push(Var {
            decl,
            frame_offset: self.frame_offset,
        });
        self.frame_offset += 1;
    }

    fn truncate(&mut self, final_frame_size: usize) {
        self.vars.truncate(
            self.vars
                .iter()
                .rev()
                .position(|var| var.frame_offset < final_frame_size)
                .map(|pos| self.vars.len() - pos)
                .unwrap_or(0),
        );
        self.frame_offset = final_frame_size;
    }
}

fn enter_frag(frag_id: FragId) -> [ir::Instruction; 2] {
    [
        ir::Instruction::GrowStack { amount: 1 },
        ir::Instruction::Move {
            dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
            src: ir::Value::Immediate(frag_id),
            store_mode: ir::StoreMode::Add,
        },
    ]
}

fn execute_block<'a>(body: &'a [ast::Statement], scope: &mut Scope<'a, '_>, frag_cur: &mut FragId) {
    let block = compile_block(
        body,
        vec![ir::Instruction::ShrinkStack { amount: 1 }],
        scope,
        scope.frame_offset,
    );
    scope.global.code[*frag_cur].extend(enter_frag(block.start));
    if block.start == block.end {
        println!("Block with no branches");
    }
    *frag_cur = block.end;
}

struct CompiledBlock {
    start: FragId,
    end: FragId,
}

#[deny(unused_must_use)]
#[must_use]
fn compile_block<'a>(
    statements: &'a [ast::Statement],
    instructions: Vec<ir::Instruction>, // start with some instructions in the block, if desired
    scope: &mut Scope<'a, '_>,
    final_frame_size: usize,
) -> CompiledBlock {
    let start_frag = scope.global.add_frag(instructions);
    let mut cur_frag = start_frag;
    for statement in statements {
        match statement {
            ast::Statement::Let { decl, value } => {
                compile_expr_and_push(value, scope, &mut cur_frag);
                scope.frame_offset -= 1; // cancel out the fact that add_decl also increments frame_offset
                scope.add_decl(decl);
            }
            ast::Statement::Assign { place, value, mode } => {
                let src = compile_expr(value, scope, &mut cur_frag);
                let dst = compile_place(place, scope, true);
                scope.global.code[cur_frag].push(ir::Instruction::Move {
                    dst,
                    src,
                    store_mode: compile_store_mode(*mode),
                });
            }
            ast::Statement::Loop { body } => {
                scope.frame_offset += 2;
                let loop_start = compile_block(body, vec![], scope, scope.frame_offset).start;
                scope.frame_offset -= 2;

                let frag_old = cur_frag;
                cur_frag = scope
                    .global
                    .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);

                scope.global.code[frag_old].extend(enter_frag(cur_frag));
                scope.global.code[frag_old].extend(enter_frag(loop_start));
            }
            ast::Statement::Continue => break,
            ast::Statement::Break => {
                scope.global.code[cur_frag].push(ir::Instruction::ShrinkStack { amount: 1 });
                break;
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
                    execute_block(default, scope, &mut cur_frag);
                    continue;
                };

                match compile_expr(cond, scope, &mut cur_frag) {
                    ir::Value::Deref(place) => {
                        let case_blocks = (0..=last_case)
                            .map(|value| {
                                compile_block(
                                    case_map.get(&value).copied().unwrap_or(default.as_slice()),
                                    vec![ir::Instruction::ShrinkStack { amount: 1 }],
                                    scope,
                                    scope.frame_offset,
                                )
                            })
                            .collect::<Vec<_>>();
                        let default_block = compile_block(
                            default,
                            vec![ir::Instruction::ShrinkStack { amount: 1 }],
                            scope,
                            scope.frame_offset,
                        );
                        scope.global.code[cur_frag].extend([ir::Instruction::Switch {
                            cond: place,
                            cases: case_blocks
                                .iter()
                                .map(|block| enter_frag(block.start).to_vec())
                                .collect(),
                            default: enter_frag(default_block.start).to_vec(),
                        }]);
                        cur_frag = scope
                            .global
                            .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);
                        for block in case_blocks {
                            scope.global.code[block.end].extend(enter_frag(cur_frag));
                        }
                    }
                    ir::Value::Immediate(value) => execute_block(
                        case_map.get(&value).copied().unwrap_or(default.as_slice()),
                        scope,
                        &mut cur_frag,
                    ),
                };
            }
            ast::Statement::Block { body } => execute_block(body, scope, &mut cur_frag),
            ast::Statement::Return(value) => {
                let src = compile_expr(value, scope, &mut cur_frag);
                scope.global.code[cur_frag].extend([
                    ir::Instruction::Move {
                        dst: ir::Place::Direct(ir::DirectPlace::StackTop {
                            offset: scope.frame_offset + 1,
                        }),
                        src,
                        store_mode: ir::StoreMode::Add,
                    },
                    ir::Instruction::ShrinkStack {
                        amount: scope.frame_offset,
                    },
                ]);
                scope.truncate(final_frame_size);
                cur_frag = 0;
            }
        }
    }
    scope.global.code[cur_frag].push(ir::Instruction::ShrinkStack {
        amount: scope.frame_offset - final_frame_size,
    });
    scope.truncate(final_frame_size);
    CompiledBlock {
        start: start_frag,
        end: cur_frag,
    }
}

fn compile_func(name: &str, global_state: &mut GlobalState) -> FragId {
    let func = global_state
        .functions
        .get(name)
        .copied()
        .unwrap_or_else(|| panic!("Function `{name}` not found."));

    if let Some(&func_id) = global_state.func_ids.get(func.name.as_str()) {
        // function already compiled
        return func_id;
    }

    let block_start = global_state.code.len();
    global_state
        .func_ids
        .insert(func.name.as_str(), block_start);

    let mut scope = Scope::new(global_state);
    for decl in &func.params {
        scope.add_decl(decl);
    }

    let block = compile_block(
        &func.body,
        vec![ir::Instruction::ShrinkStack {
            // remove the call address
            amount: 1,
        }],
        &mut scope,
        0,
    );

    assert!(block_start == block.start);

    block_start
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

    let mut global_state = GlobalState::new(functions);

    let main_func_id = compile_func("main", &mut global_state);

    global_state.code[0].clear(); // remove dead code

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
                    cases: global_state.code,
                    // default should never run unless we implement dynamic dispatch and an invalid function pointer is called
                    // TODO: add some instructions to "panic" and exit in this case
                    default: vec![],
                }],
            },
            ir::Instruction::ShrinkStack { amount: 2 },
        ],
    }
}
