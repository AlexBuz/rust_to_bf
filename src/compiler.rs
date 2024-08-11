use {
    crate::{ast, ir},
    std::{cmp::Ordering::*, collections::BTreeMap, sync::LazyLock},
};

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

fn compile_simple_expr<'a>(expr: &'a ast::SimpleExpr, scope: &mut Scope<'a, '_>) -> ir::Value {
    match expr {
        ast::SimpleExpr::Int(i) => ir::Value::Immediate(*i),
        ast::SimpleExpr::String(s) => {
            ir::Value::Immediate(*scope.global.str_ids.entry(s).or_insert_with(|| {
                scope.global.frags.push(
                    [ir::Instruction::ShrinkStack { amount: 1 }]
                        .into_iter()
                        .chain(s.bytes().map(|byte| ir::Instruction::Output {
                            src: ir::Value::Immediate(byte as _),
                        }))
                        .collect(),
                );
                scope.global.frags.len() - 1
            }))
        }
        ast::SimpleExpr::Place(place) => ir::Value::Deref(compile_place(place, scope, false)),
    }
}

fn push_imm(value: usize) -> [ir::Instruction; 2] {
    [
        ir::Instruction::GrowStack { amount: 1 },
        ir::Instruction::Move {
            dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
            src: ir::Value::Immediate(value),
            store_mode: ir::StoreMode::Add,
        },
    ]
}

type FragId = usize;

static INTRINSICS: &[&str] = &[
    "read_char",
    "print_char",
    "print",
    "println",
    "printf",
    "exit",
];

fn compile_intrinsic_call<'a>(
    name: &str,
    args: &'a [ast::Expr],
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> ir::Value {
    match name {
        "read_char" => {
            if !args.is_empty() {
                panic!("`{name}` does not take any arguments");
            }
            scope.global.frags[*cur_frag].extend([
                ir::Instruction::GrowStack { amount: 1 },
                ir::Instruction::Input {
                    dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                },
            ]);
            scope.frame_offset += 1;
            ir::Value::Deref(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }))
        }
        "print_char" => {
            if args.is_empty() {
                panic!("`{name}` requires at least 1 argument");
            }
            let frame_size = scope.frame_offset;
            for arg in args {
                let src = compile_expr(arg, scope, cur_frag);
                scope.global.frags[*cur_frag].push(ir::Instruction::Output { src });
            }
            scope.shrink_frame(frame_size, *cur_frag);
            ir::Value::Immediate(0)
        }
        "print" => {
            if args.is_empty() {
                panic!("`{name}` requires at least 1 argument");
            }
            for arg in args {
                match arg {
                    ast::Expr::Simple(ast::SimpleExpr::String(s)) => {
                        scope.global.frags[*cur_frag].extend(s.bytes().map(|byte| {
                            ir::Instruction::Output {
                                src: ir::Value::Immediate(byte as _),
                            }
                        }));
                    }
                    _ => {
                        let return_frag = scope
                            .global
                            .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);

                        scope.frame_offset += 1;
                        scope.global.frags[*cur_frag].extend(push_imm(return_frag));
                        compile_expr_and_push(arg, scope, cur_frag);
                        scope.frame_offset -= 2;
                        *cur_frag = return_frag;
                    }
                }
            }
            ir::Value::Immediate(0)
        }
        "println" => {
            if !args.is_empty() {
                compile_intrinsic_call("print", args, scope, cur_frag);
            }
            scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                src: ir::Value::Immediate(b'\n' as _),
            });
            ir::Value::Immediate(0)
        }
        "printf" => {
            let Some((first_arg, mut args)) = args.split_first() else {
                panic!("`{name}` requires at least 1 argument");
            };
            let format_string = match first_arg {
                ast::Expr::Simple(ast::SimpleExpr::String(s)) => s,
                _ => panic!("First argument to `{name}` must be a string literal"),
            };
            let mut format_chars = format_string.bytes();
            let frame_size = scope.frame_offset;
            while let Some(c) = format_chars.next() {
                if c == b'%' {
                    let Some(format_specifier) = format_chars.next() else {
                        panic!("Unterminated format specifier");
                    };
                    if format_specifier == b'%' {
                        scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                            src: ir::Value::Immediate(b'%' as _),
                        });
                        continue;
                    }
                    let Some((arg, rest)) = args.split_at_checked(1) else {
                        panic!("Not enough arguments for format string `{format_string}`")
                    };
                    args = rest;
                    match format_specifier {
                        b'c' => compile_intrinsic_call("print_char", arg, scope, cur_frag),
                        b's' => compile_intrinsic_call("print", arg, scope, cur_frag),
                        b'd' => compile_func_call("print_int", arg, scope, cur_frag),
                        _ => panic!("Invalid format specifier"),
                    };
                } else {
                    scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                        src: ir::Value::Immediate(c as _),
                    });
                }
            }
            scope.shrink_frame(frame_size, *cur_frag);
            if !args.is_empty() {
                panic!("Too many arguments for format string `{format_string}`");
            }
            ir::Value::Immediate(0)
        }
        "exit" => {
            if !args.is_empty() {
                panic!("`{name}` does not take any arguments");
            }
            scope.global.frags[*cur_frag].extend(push_imm(EXIT_FRAG));
            *cur_frag = EXIT_FRAG;
            ir::Value::Immediate(0)
        }
        "&&" | "||" => {
            let Some(([lhs, rhs], [])) = args.split_first_chunk() else {
                panic!("`{name}` requires exactly 2 arguments");
            };

            let short_circuit = scope
                .global
                .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);

            let long_circuit = scope
                .global
                .add_frag(vec![ir::Instruction::ShrinkStack { amount: 2 }]);

            compile_expr_and_push(lhs, scope, cur_frag);

            let [false_circuit, true_circuit] = match name {
                "&&" => [short_circuit, long_circuit],
                "||" => [long_circuit, short_circuit],
                _ => unreachable!("The only short-circuiting operators are `&&` and `||`"),
            };

            scope.global.frags[*cur_frag].extend([ir::Instruction::Switch {
                cond: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                cases: vec![push_imm(false_circuit).to_vec()],
                default: push_imm(true_circuit).to_vec(),
            }]);

            *cur_frag = long_circuit;
            scope.frame_offset -= 1;
            compile_expr_and_push(rhs, scope, cur_frag);
            scope.global.frags[*cur_frag].extend(push_imm(short_circuit));

            *cur_frag = short_circuit;

            ir::Value::Deref(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }))
        }
        _ => {
            if scope.global.func_bodies.contains_key(name) {
                panic!("Function `{name}` must be called without `!`");
            } else {
                panic!("Intrinsic `{name}` does not exist");
            }
        }
    }
}

fn compile_func_call<'a>(
    name: &'a str,
    args: &'a [ast::Expr],
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> ir::Value {
    let Some(call_frag) = compile_func(name, scope.global) else {
        if INTRINSICS.contains(&name) {
            panic!("Intrinsic `{name}` must be called with `!`");
        } else {
            panic!("Function `{name}` not defined");
        }
    };

    // Stack order: [return value, return address, arguments, local variables]
    // The caller leaves space for the return value and pushes the return address and arguments
    // The callee is responsible for popping the arguments before returning
    // The caller then pops the return address and may use the return value

    let return_frag = scope
        .global
        .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);

    // make space for the return value
    scope.global.frags[*cur_frag].push(ir::Instruction::GrowStack { amount: 1 });

    // push the return address
    scope.global.frags[*cur_frag].extend(push_imm(return_frag));
    scope.frame_offset += 2;

    for arg in args {
        compile_expr_and_push(arg, scope, cur_frag)
    }

    scope.global.frags[*cur_frag].extend(push_imm(call_frag));

    *cur_frag = return_frag;

    // keep only the return value
    scope.frame_offset -= 1 + args.len();

    ir::Value::Deref(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }))
}

fn compile_call<'a>(
    call: &'a ast::CallExpr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> ir::Value {
    let name = call.func.as_str();
    if call.bang {
        compile_intrinsic_call(name, &call.args, scope, cur_frag)
    } else {
        compile_func_call(name, &call.args, scope, cur_frag)
    }
}

fn compile_expr<'a>(
    expr: &'a ast::Expr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> ir::Value {
    match expr {
        ast::Expr::Simple(simple_expr) => compile_simple_expr(simple_expr, scope),
        ast::Expr::Call(call_expr) => compile_call(call_expr, scope, cur_frag),
    }
}

fn compile_expr_and_push<'a>(
    expr: &'a ast::Expr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) {
    match expr {
        ast::Expr::Simple(simple_expr) => {
            scope.frame_offset += 1;
            let src = compile_simple_expr(simple_expr, scope);
            scope.global.frags[*cur_frag].extend([
                ir::Instruction::GrowStack { amount: 1 },
                ir::Instruction::Move {
                    dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                    src,
                    store_mode: ir::StoreMode::Add,
                },
            ]);
        }
        ast::Expr::Call(call_expr) => match compile_call(call_expr, scope, cur_frag) {
            ir::Value::Deref(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 })) => {}
            ir::Value::Immediate(value) => {
                scope.frame_offset += 1;
                scope.global.frags[*cur_frag].extend(push_imm(value));
            }
            _ => unreachable!("Return value should be immediate or on the top of the stack"),
        },
    }
}

fn compile_store_mode(mode: ast::AssignMode) -> ir::StoreMode {
    match mode {
        ast::AssignMode::Add => ir::StoreMode::Add,
        ast::AssignMode::Subtract => ir::StoreMode::Subtract,
        ast::AssignMode::Replace => ir::StoreMode::Replace,
    }
}

struct GlobalState<'a> {
    frags: Vec<Vec<ir::Instruction>>,
    func_ids: BTreeMap<&'a str, FragId>,
    str_ids: BTreeMap<&'a str, FragId>,
    func_bodies: BTreeMap<&'a str, &'a ast::Function>,
}

// frag 0 is the exit psuedo-fragment (it never actually gets executed due to the while loop)
const EXIT_FRAG: FragId = 0;

impl<'a> GlobalState<'a> {
    fn new(func_bodies: BTreeMap<&'a str, &'a ast::Function>) -> Self {
        Self {
            frags: vec![vec![]],
            func_ids: BTreeMap::new(),
            str_ids: BTreeMap::new(),
            func_bodies,
        }
    }

    fn add_frag(&mut self, instructions: Vec<ir::Instruction>) -> FragId {
        let frag_id = self.frags.len();
        self.frags.push(instructions);
        frag_id
    }
}

struct LoopInfo {
    start: FragId,
    after: FragId,
    frame_size: usize,
}

struct Scope<'a, 'b> {
    vars: Vec<Var<'a>>,
    frame_offset: usize,
    global: &'b mut GlobalState<'a>,
    loop_stack: Vec<LoopInfo>,
}

impl<'a, 'b> Scope<'a, 'b> {
    fn new(global: &'b mut GlobalState<'a>) -> Self {
        Self {
            vars: vec![],
            frame_offset: 0,
            global,
            loop_stack: vec![],
        }
    }

    fn add_decl(&mut self, decl: &'a ast::VarDecl) {
        self.vars.push(Var {
            decl,
            frame_offset: self.frame_offset,
        });
        self.frame_offset += 1;
    }

    fn shrink_frame_early(&mut self, target_frame_size: usize, cur_frag: FragId) {
        match target_frame_size.cmp(&self.frame_offset) {
            Less => {
                self.global.frags[cur_frag].push(ir::Instruction::ShrinkStack {
                    amount: self.frame_offset - target_frame_size,
                });
            }
            Equal => {}
            Greater => unreachable!("Cannot shrink_frame_early to a larger size"),
        }
    }

    fn shrink_frame(&mut self, target_frame_size: usize, cur_frag: FragId) {
        match target_frame_size.cmp(&self.frame_offset) {
            Less => {
                self.shrink_frame_early(target_frame_size, cur_frag);
                self.vars.truncate(
                    self.vars
                        .iter()
                        .rev()
                        .position(|var| var.frame_offset < target_frame_size)
                        .map(|pos| self.vars.len() - pos)
                        .unwrap_or(0),
                );
                self.frame_offset = target_frame_size;
            }
            Equal => {}
            Greater => unreachable!("Cannot shrink_frame to a larger size"),
        }
    }
}

fn execute_block<'a>(body: &'a [ast::Statement], scope: &mut Scope<'a, '_>, cur_frag: &mut FragId) {
    let block = compile_block(
        body,
        vec![ir::Instruction::ShrinkStack { amount: 1 }],
        scope,
        scope.frame_offset,
    );
    scope.global.frags[*cur_frag].extend(push_imm(block.start));
    // TODO: if block.start == block.end, then inline the block
    *cur_frag = block.end;
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
            ast::Statement::Assign { place, value, mode } => match (mode, value) {
                (
                    ast::AssignMode::Replace,
                    ast::Expr::Call(ast::CallExpr {
                        func,
                        bang: true,
                        args,
                    }),
                ) if func == "getchar" && args.is_empty() => {
                    // optimization for reading a character into an existing location
                    let dst = compile_place(place, scope, true);
                    scope.global.frags[cur_frag].push(ir::Instruction::Input { dst });
                }
                _ => {
                    let src = compile_expr(value, scope, &mut cur_frag);
                    let dst = compile_place(place, scope, true);
                    scope.global.frags[cur_frag].push(ir::Instruction::Move {
                        dst,
                        src,
                        store_mode: compile_store_mode(*mode),
                    });
                }
            },
            ast::Statement::Loop(body) => {
                let after_loop = scope
                    .global
                    .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);
                let loop_start = scope.global.frags.len();
                scope.loop_stack.push(LoopInfo {
                    start: loop_start,
                    after: after_loop,
                    frame_size: scope.frame_offset,
                });
                let loop_body = compile_block(
                    body,
                    vec![ir::Instruction::ShrinkStack { amount: 1 }],
                    scope,
                    scope.frame_offset,
                );
                assert!(loop_start == loop_body.start);
                scope.global.frags[cur_frag].extend(push_imm(loop_start));
                scope.global.frags[loop_body.end].extend(push_imm(loop_start));
                scope.loop_stack.pop();
                cur_frag = after_loop;
            }
            ast::Statement::Continue => {
                let &LoopInfo {
                    start, frame_size, ..
                } = scope
                    .loop_stack
                    .last()
                    .expect("`continue` may only be used inside a loop");
                scope.shrink_frame_early(frame_size, cur_frag);
                scope.global.frags[cur_frag].extend(push_imm(start));
                cur_frag = EXIT_FRAG;
                break;
            }
            ast::Statement::Break => {
                let &LoopInfo {
                    after, frame_size, ..
                } = scope
                    .loop_stack
                    .last()
                    .expect("`break` may only be used inside a loop");
                scope.shrink_frame_early(frame_size, cur_frag);
                scope.global.frags[cur_frag].extend(push_imm(after));
                cur_frag = EXIT_FRAG;
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
                        scope.global.frags[cur_frag].extend([ir::Instruction::Switch {
                            cond: place,
                            cases: case_blocks
                                .iter()
                                .map(|block| push_imm(block.start).to_vec())
                                .collect(),
                            default: push_imm(default_block.start).to_vec(),
                        }]);
                        cur_frag = scope
                            .global
                            .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);
                        for block in case_blocks.into_iter().chain([default_block]) {
                            scope.global.frags[block.end].extend(push_imm(cur_frag));
                        }
                    }
                    ir::Value::Immediate(value) => execute_block(
                        case_map.get(&value).copied().unwrap_or(default.as_slice()),
                        scope,
                        &mut cur_frag,
                    ),
                };
            }
            ast::Statement::Block(body) => execute_block(body, scope, &mut cur_frag),
            ast::Statement::Return(value) => {
                let src = compile_expr(value, scope, &mut cur_frag);
                scope.global.frags[cur_frag].push(ir::Instruction::Move {
                    dst: ir::Place::Direct(ir::DirectPlace::StackTop {
                        offset: scope.frame_offset + 1,
                    }),
                    src,
                    store_mode: ir::StoreMode::Add,
                });
                scope.shrink_frame_early(0, cur_frag);
                cur_frag = EXIT_FRAG;
                break;
            }
            ast::Statement::Eval(expr) => {
                let frame_size = scope.frame_offset;
                compile_expr(expr, scope, &mut cur_frag);
                scope.shrink_frame(frame_size, cur_frag);
            }
        }
    }
    scope.shrink_frame(final_frame_size, cur_frag);
    CompiledBlock {
        start: start_frag,
        end: cur_frag,
    }
}

fn compile_func<'a>(name: &'a str, global_state: &mut GlobalState<'a>) -> Option<FragId> {
    if let Some(&func_id) = global_state.func_ids.get(name) {
        // function already compiled
        return Some(func_id);
    }

    let func = global_state.func_bodies.get(name).copied()?;

    let block_start = global_state.frags.len();
    global_state.func_ids.insert(name, block_start);

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

    Some(block_start)
}

static STD: LazyLock<ast::Ast> = LazyLock::new(|| {
    let src = include_str!("std.bs");
    let mut ast = ast::Ast::parse(src).expect("Failed to parse standard library");
    for func in &mut ast.functions {
        let op = match func.name.as_str() {
            "add" => "+",
            "sub" => "-",
            "mul" => "*",
            "div" => "/",
            "mod" => "%",
            "eq" => "==",
            "ne" => "!=",
            "lt" => "<",
            "le" => "<=",
            "gt" => ">",
            "ge" => ">=",
            "not" => "!",
            _ => continue,
        };
        func.name.clear();
        func.name.push_str(op);
    }
    ast
});

pub fn compile(mut ast: ast::Ast) -> ir::Program {
    ast.functions.extend(STD.functions.iter().cloned());

    let functions = ast
        .functions
        .iter()
        .map(|func| (func.name.as_str(), func))
        .collect::<BTreeMap<_, _>>();

    if functions.len() != ast.functions.len() {
        panic!("Duplicate function names");
    }

    let mut global_state = GlobalState::new(functions);

    let main_func_id = compile_func("main", &mut global_state)
        .unwrap_or_else(|| panic!("Function `main` not defined."));

    global_state.frags[EXIT_FRAG].clear(); // remove dead code

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
                    cases: global_state.frags,
                    // default should never run unless we implement dynamic dispatch and an invalid function pointer is called
                    // TODO: add some instructions to "panic" and exit in this case
                    default: vec![],
                }],
            },
            ir::Instruction::ShrinkStack { amount: 2 },
        ],
    }
}
