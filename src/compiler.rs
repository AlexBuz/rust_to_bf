use {
    crate::{ast, ir},
    std::{cmp::Ordering::*, collections::BTreeMap, sync::LazyLock},
};

#[derive(Debug, Clone)]
struct Var<'a> {
    mutable: bool,
    name: &'a str,
    ty: Type<'a>,
    frame_offset: usize,
}

fn size_of(ty: &Type, scope: &Scope) -> usize {
    match ty {
        Type::Usize => 1,
        Type::Tuple(tys) => tys.iter().map(|ty| size_of(ty, scope)).sum(),
        Type::Named(name) => scope
            .global
            .struct_defs
            .get(name)
            .unwrap_or_else(|| panic!("Type `{name}` not found."))
            .fields
            .iter()
            .map(|field| size_of(&field.ty, scope))
            .sum(),
        Type::Reference { .. } => 1,
    }
}

#[derive(Debug, Clone, Default)]
#[must_use]
struct Typed<'a, T> {
    ty: Type<'a>,
    value: T,
}

impl<'a, T> Typed<'a, T> {
    fn new(ty: Type<'a>, value: T) -> Self {
        Self { ty, value }
    }

    fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Typed<'a, U> {
        Typed {
            ty: self.ty,
            value: f(self.value),
        }
    }

    fn expect(self, expected_ty: &Type<'a>) -> T {
        if &self.ty != expected_ty {
            panic!("Expected type `{expected_ty}` but got `{}`.", self.ty);
        }
        self.value
    }

    fn expect_unit(self) -> T {
        self.expect(&Type::default())
    }
}

fn compile_path_access<'a>(
    path: &ast::Path,
    mutating: bool,
    scope: &Scope<'a, '_>,
) -> Typed<'a, ir::Place> {
    let Some(var) = scope.vars.iter().rev().find(|var| var.name == path.root) else {
        panic!("Variable `{}` not found.", path.root);
    };
    if mutating && !var.mutable {
        panic!("Variable `{}` is not mutable.", path.root);
    }
    let mut ty = &var.ty;
    let mut offset = scope.frame_offset - var.frame_offset - 1;
    for segment in &path.trail {
        match *segment {
            ast::FieldIdent::Index(index) => {
                let Type::Tuple(tys) = ty else {
                    panic!("Non-tuple type `{ty}` has no field at index `{index}`.");
                };
                ty = &tys[index];
                for ty in &tys[index + 1..] {
                    offset += size_of(ty, scope);
                }
            }
            ast::FieldIdent::Named(ref field_name) => {
                let Type::Named(ty_name) = ty else {
                    panic!("Non-struct type `{ty}` has no field named `{field_name}`.");
                };
                let Some(struct_def) = scope.global.struct_defs.get(ty_name) else {
                    panic!("Type `{ty_name}` not found.");
                };
                ty = struct_def
                    .fields
                    .iter()
                    .rev()
                    .find_map(|field| {
                        if field.name == field_name {
                            Some(&field.ty)
                        } else {
                            offset += size_of(&field.ty, scope);
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        panic!("Field `{field_name}` not found in struct `{ty_name}`.")
                    });
            }
        }
    }
    Typed::new(
        ty.clone(),
        ir::Place::Direct(ir::DirectPlace::StackTop { offset }),
    )
}

fn compile_place<'a>(
    place: &ast::Place,
    mutating: bool,
    scope: &Scope<'a, '_>,
) -> Typed<'a, ir::Place> {
    match place {
        ast::Place::Path(path) => compile_path_access(path, mutating, scope),
        ast::Place::Deref(_) => {
            todo!("Dereferencing");
        }
    }
}

fn compile_simple_expr<'a>(
    simple_expr: &'a ast::SimpleExpr,
    scope: &mut Scope<'a, '_>,
) -> Typed<'a, ir::Value> {
    match simple_expr {
        ast::SimpleExpr::Int(i) => Typed::new(Type::Usize, ir::Value::Immediate(*i)),
        ast::SimpleExpr::String(s) => Typed::new(
            Type::Usize,
            ir::Value::Immediate(*scope.global.string_ids.entry(s).or_insert_with(|| {
                scope.global.frags.push(
                    [ir::Instruction::ShrinkStack { amount: 1 }]
                        .into_iter()
                        .chain(s.bytes().map(|byte| ir::Instruction::Output {
                            src: ir::Value::Immediate(byte as _),
                        }))
                        .collect(),
                );
                scope.global.frags.len() - 1
            })),
        ),
        ast::SimpleExpr::Place(place) => compile_place(place, false, scope).map(ir::Value::At),
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
) -> Typed<'a, ir::Value> {
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
            Typed::new(
                Type::Usize,
                ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 })),
            )
        }
        "print_char" => {
            if args.is_empty() {
                panic!("`{name}` requires at least 1 argument");
            }
            let frame_size = scope.frame_offset;
            for arg in args {
                let src = compile_expr(arg, scope, cur_frag).expect(&Type::Usize);
                scope.global.frags[*cur_frag].push(ir::Instruction::Output { src });
            }
            scope.shrink_frame(frame_size, *cur_frag);
            Typed::default()
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
                        compile_expr_and_push(arg, scope, cur_frag).expect(&Type::Usize);
                        scope.frame_offset -= 2;
                        *cur_frag = return_frag;
                    }
                }
            }
            Typed::default()
        }
        "println" => {
            if !args.is_empty() {
                compile_intrinsic_call("print", args, scope, cur_frag).expect_unit();
            }
            scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                src: ir::Value::Immediate(b'\n' as _),
            });
            Typed::default()
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
                    }
                    .expect_unit();
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
            Typed::default()
        }
        "exit" => {
            if !args.is_empty() {
                panic!("`{name}` does not take any arguments");
            }
            scope.global.frags[*cur_frag].extend(push_imm(EXIT_FRAG));
            *cur_frag = EXIT_FRAG;
            Typed::default()
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

            compile_expr_and_push(lhs, scope, cur_frag).expect(&Type::Usize);

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
            compile_expr_and_push(rhs, scope, cur_frag).expect(&Type::Usize);
            scope.global.frags[*cur_frag].extend(push_imm(short_circuit));

            *cur_frag = short_circuit;

            Typed::new(
                Type::Usize,
                ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 })),
            )
        }
        _ => {
            if scope.global.func_defs.contains_key(name) {
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
) -> Typed<'a, ir::Value> {
    let call_frag = compile_func(name, scope.global);

    let func_def = &scope.global.func_defs[name];
    if args.len() != func_def.params.len() {
        panic!(
            "Function `{name}` expects {} arguments, but {} were provided.",
            func_def.params.len(),
            args.len()
        );
    }

    // Stack order: [return value, return address, arguments, local variables]
    // The caller leaves space for the return value and pushes the return address and arguments
    // The callee is responsible for popping the arguments before returning
    // The caller then pops the return address and may use the return value

    let return_frag = scope
        .global
        .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);

    // make space for the return value
    scope.grow_frame(size_of(&func_def.ret_ty, scope), *cur_frag);

    // remember the frame offset before the call
    let return_frame_offset = scope.frame_offset;

    // push the return address
    scope.global.frags[*cur_frag].extend(push_imm(return_frag));
    scope.frame_offset += 1;

    for (arg, param) in args.iter().zip(&func_def.params) {
        compile_expr_and_push(arg, scope, cur_frag).expect(&param.ty);
    }

    scope.global.frags[*cur_frag].extend(push_imm(call_frag));

    *cur_frag = return_frag;

    // restore the frame offset
    scope.frame_offset = return_frame_offset;

    Typed::new(
        func_def.ret_ty.clone(),
        ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 })),
    )
}

fn compile_call<'a>(
    call_expr: &'a ast::CallExpr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    let name = call_expr.func.as_str();
    if call_expr.bang {
        compile_intrinsic_call(name, &call_expr.args, scope, cur_frag)
    } else {
        compile_func_call(name, &call_expr.args, scope, cur_frag)
    }
}

fn compile_struct_expr<'a>(
    struct_expr: &'a ast::StructExpr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    let Some(struct_def) = scope.global.struct_defs.get(struct_expr.name.as_str()) else {
        panic!("Type `{}` not found.", struct_expr.name);
    };
    if struct_expr.fields.len() != struct_def.fields.len() {
        panic!(
            "Struct `{}` has {} fields, but {} were provided.",
            struct_expr.name,
            struct_def.fields.len(),
            struct_expr.fields.len()
        );
    }
    // TODO: add support for initializing fields in arbitrary order
    for (field_def, field) in struct_def.fields.iter().zip(&struct_expr.fields) {
        if field_def.name != field.name {
            panic!(
                "Expected field `{}` but got `{}`.",
                field_def.name, field.name
            );
        }
        compile_expr_and_push(&field.value, scope, cur_frag).expect(&field_def.ty);
    }
    let struct_ty = Type::Named(&struct_expr.name);
    Typed::new(
        struct_ty,
        ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 })),
    )
}

fn compile_tuple_expr<'a>(
    tuple_expr: &'a [ast::Expr],
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    Typed::new(
        Type::Tuple(
            tuple_expr
                .iter()
                .map(|expr| compile_expr_and_push(expr, scope, cur_frag).ty)
                .collect(),
        ),
        ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 })),
    )
}

fn compile_expr<'a>(
    expr: &'a ast::Expr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    match expr {
        ast::Expr::Simple(simple_expr) => compile_simple_expr(simple_expr, scope),
        ast::Expr::Call(call_expr) => compile_call(call_expr, scope, cur_frag),
        ast::Expr::Struct(struct_expr) => compile_struct_expr(struct_expr, scope, cur_frag),
        ast::Expr::Tuple(tuple_expr) => compile_tuple_expr(tuple_expr, scope, cur_frag),
    }
}

fn type_of_simple_expr<'a>(simple_expr: &'a ast::SimpleExpr, scope: &Scope<'a, '_>) -> Type<'a> {
    match simple_expr {
        ast::SimpleExpr::Int(_) => Type::Usize,
        ast::SimpleExpr::String(_) => Type::Usize,
        ast::SimpleExpr::Place(place) => compile_place(place, false, scope).ty,
    }
}

fn increment_place(place: ir::Place) -> ir::Place {
    match place {
        ir::Place::Direct(ir::DirectPlace::StackTop { offset }) => {
            ir::Place::Direct(ir::DirectPlace::StackTop { offset: offset + 1 })
        }
        ir::Place::Indirect(ir::IndirectPlace::Heap { .. }) => {
            todo!("Incrementing heap addresses");
        }
    }
}

fn increment_value_place(value: ir::Value) -> ir::Value {
    match value {
        ir::Value::At(place) => ir::Value::At(increment_place(place)),
        ir::Value::Immediate(_) => value,
    }
}

fn compile_move<'a>(
    mut dst: ir::Place,
    mut src: ir::Value,
    store_mode: ir::StoreMode,
    ty: &Type<'a>,
    scope: &mut Scope<'a, '_>,
    cur_frag: FragId,
) {
    match size_of(ty, scope) {
        0 => {}
        1 => scope.global.frags[cur_frag].push(ir::Instruction::Move {
            dst,
            src,
            store_mode,
        }),
        size => {
            for _ in 0..size {
                scope.global.frags[cur_frag].push(ir::Instruction::Move {
                    dst,
                    src,
                    store_mode,
                });
                dst = increment_place(dst);
                src = increment_value_place(src);
            }
        }
    }
}

fn compile_expr_and_push<'a>(
    expr: &'a ast::Expr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ()> {
    let ty = match expr {
        ast::Expr::Simple(simple_expr) => {
            scope.grow_frame(
                size_of(&type_of_simple_expr(simple_expr, scope), scope),
                *cur_frag,
            );
            let src = compile_simple_expr(simple_expr, scope);
            compile_move(
                ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                src.value,
                ir::StoreMode::Replace,
                &src.ty,
                scope,
                *cur_frag,
            );
            src.ty
        }
        ast::Expr::Call(call_expr) => {
            let ret = compile_call(call_expr, scope, cur_frag);
            match ret.value {
                ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 })) => {}
                ir::Value::Immediate(value) => {
                    scope.frame_offset += 1;
                    scope.global.frags[*cur_frag].extend(push_imm(value));
                }
                _ => unreachable!(
                    "Return value should always either be immediate or on the top of the stack"
                ),
            }
            ret.ty
        }
        ast::Expr::Struct(struct_expr) => compile_struct_expr(struct_expr, scope, cur_frag).ty,
        ast::Expr::Tuple(tuple_expr) => compile_tuple_expr(tuple_expr, scope, cur_frag).ty,
    };
    Typed::new(ty, ())
}

fn compile_store_mode(mode: ast::AssignMode) -> ir::StoreMode {
    match mode {
        ast::AssignMode::Add => ir::StoreMode::Add,
        ast::AssignMode::Subtract => ir::StoreMode::Subtract,
        ast::AssignMode::Replace => ir::StoreMode::Replace,
    }
}

struct GlobalState<'a> {
    func_defs: &'a BTreeMap<&'a str, FuncDef<'a>>,
    struct_defs: &'a BTreeMap<&'a str, StructDef<'a>>,
    frags: Vec<Vec<ir::Instruction>>,
    func_ids: BTreeMap<&'a str, FragId>,
    string_ids: BTreeMap<&'a str, FragId>,
}

// frag 0 is the exit psuedo-fragment (it never actually gets executed due to the while loop)
const EXIT_FRAG: FragId = 0;

impl<'a> GlobalState<'a> {
    fn new(
        func_defs: &'a BTreeMap<&'a str, FuncDef<'a>>,
        struct_defs: &'a BTreeMap<&'a str, StructDef<'a>>,
    ) -> Self {
        Self {
            func_defs,
            struct_defs,
            frags: vec![vec![]],
            func_ids: BTreeMap::new(),
            string_ids: BTreeMap::new(),
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
    func_def: &'a FuncDef<'a>,
    vars: Vec<Var<'a>>,
    frame_offset: usize,
    global: &'b mut GlobalState<'a>,
    loop_stack: Vec<LoopInfo>,
}

impl<'a, 'b> Scope<'a, 'b> {
    fn new(func_def: &'a FuncDef<'a>, global: &'b mut GlobalState<'a>) -> Self {
        Self {
            func_def,
            vars: vec![],
            frame_offset: 0,
            global,
            loop_stack: vec![],
        }
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

    fn grow_frame(&mut self, amount: usize, cur_frag: FragId) {
        self.global.frags[cur_frag].push(ir::Instruction::GrowStack { amount });
        self.frame_offset += amount;
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

#[must_use]
struct CompiledBlock {
    start: FragId,
    end: FragId,
}

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
            ast::Statement::Let {
                mutable,
                name,
                ty,
                value,
            } => {
                let ty = match (ty.as_ref().map(Type::from), value) {
                    (None, None) => panic!("Variable `{name}` must have a type or a value."),
                    (Some(ty), Some(value)) => {
                        compile_expr_and_push(value, scope, &mut cur_frag).expect(&ty);
                        ty
                    }
                    (Some(ty), None) => {
                        if !mutable {
                            panic!("Variable `{name}` must have a value if it is not mutable.");
                        }
                        scope.grow_frame(size_of(&ty, scope), cur_frag);
                        ty
                    }
                    (None, Some(value)) => compile_expr_and_push(value, scope, &mut cur_frag).ty,
                };
                scope.vars.push(Var {
                    mutable: *mutable,
                    name,
                    ty,
                    frame_offset: scope.frame_offset - 1,
                });
            }
            ast::Statement::Assign { place, value, mode } => match (mode, value) {
                (
                    ast::AssignMode::Replace,
                    ast::Expr::Call(ast::CallExpr {
                        func,
                        bang: true,
                        args,
                    }),
                ) if func == "read_char" && args.is_empty() => {
                    // optimization for reading a character into an existing location
                    let dst = compile_place(place, true, scope).expect(&Type::Usize);
                    scope.global.frags[cur_frag].push(ir::Instruction::Input { dst });
                }
                _ => {
                    let src = compile_expr(value, scope, &mut cur_frag);
                    let dst = compile_place(place, true, scope);
                    if src.ty != dst.ty {
                        panic!(
                            "Cannot assign value of type `{}` to place of type `{}`.",
                            src.ty, dst.ty
                        );
                    }
                    compile_move(
                        dst.value,
                        src.value,
                        compile_store_mode(*mode),
                        &src.ty,
                        scope,
                        cur_frag,
                    );
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

                match compile_expr(cond, scope, &mut cur_frag).expect(&Type::Usize) {
                    ir::Value::At(place) => {
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
                let src = compile_expr(value, scope, &mut cur_frag).expect(&scope.func_def.ret_ty);
                compile_move(
                    ir::Place::Direct(ir::DirectPlace::StackTop {
                        offset: scope.frame_offset + 1,
                    }),
                    src,
                    ir::StoreMode::Add,
                    &scope.func_def.ret_ty,
                    scope,
                    cur_frag,
                );
                scope.shrink_frame_early(0, cur_frag);
                cur_frag = EXIT_FRAG;
                break;
            }
            ast::Statement::Eval(expr) => {
                let frame_size = scope.frame_offset;
                let _ = compile_expr(expr, scope, &mut cur_frag);
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

fn compile_func<'a>(name: &'a str, global_state: &mut GlobalState<'a>) -> FragId {
    if let Some(&func_id) = global_state.func_ids.get(name) {
        // function already compiled
        return func_id;
    }

    let func_def = global_state.func_defs.get(name).unwrap_or_else(|| {
        if INTRINSICS.contains(&name) {
            panic!("Intrinsic `{name}` must be called with `!`");
        } else {
            panic!("Function `{name}` not defined");
        }
    });

    let block_start = global_state.frags.len();
    global_state.func_ids.insert(name, block_start);

    let mut scope = Scope::new(func_def, global_state);
    for param in &func_def.params {
        scope.frame_offset += size_of(&param.ty, &scope);
        scope.vars.push(Var {
            mutable: param.mutable,
            name: param.name,
            ty: param.ty.clone(),
            frame_offset: scope.frame_offset - 1,
        });
    }

    let block = compile_block(
        &func_def.body,
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

static STD: LazyLock<ast::Ast> = LazyLock::new(|| {
    let src = include_str!("std.bs");
    let mut ast = ast::Ast::parse(src).expect("Failed to parse standard library");
    for item in &mut ast.items {
        let ast::Item::FuncDef { name, .. } = item else {
            continue;
        };
        let op = match name.as_str() {
            "add" => "+",
            "sub" => "-",
            "mul" => "*",
            "div" => "/",
            "rem" => "%",
            "eq" => "==",
            "ne" => "!=",
            "lt" => "<",
            "le" => "<=",
            "gt" => ">",
            "ge" => ">=",
            "not" => "!",
            _ => continue,
        };
        name.clear();
        name.push_str(op);
    }
    ast
});

#[derive(Debug, Clone, PartialEq, Eq)]
#[must_use]
enum Type<'a> {
    Usize,
    Tuple(Vec<Type<'a>>),
    Named(&'a str),
    Reference { mutable: bool, ty: Box<Type<'a>> },
}

impl Default for Type<'_> {
    fn default() -> Self {
        Type::Tuple(vec![])
    }
}

impl std::fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Usize => write!(f, "usize"),
            Type::Tuple(tys) => {
                write!(f, "(")?;
                if let Some((first, rest)) = tys.split_first() {
                    write!(f, "{}", first)?;
                    if rest.is_empty() {
                        write!(f, ",")?;
                    } else {
                        for ty in rest {
                            write!(f, ", {}", ty)?;
                        }
                    }
                }
                write!(f, ")")
            }
            Type::Named(name) => write!(f, "{}", name),
            Type::Reference { mutable, ty } => {
                write!(f, "&")?;
                if *mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{}", ty)
            }
        }
    }
}

impl<'a> From<&'a ast::Type> for Type<'a> {
    fn from(ty: &'a ast::Type) -> Self {
        match ty {
            ast::Type::Tuple(tys) => Type::Tuple(tys.iter().map(Type::from).collect()),
            ast::Type::Named(name) => match name.as_str() {
                "usize" => Type::Usize,
                _ => Type::Named(name),
            },
            ast::Type::Reference { mutable, ref ty } => Type::Reference {
                mutable: *mutable,
                ty: Box::new(Type::from(ty.as_ref())),
            },
        }
    }
}

struct Param<'a> {
    mutable: bool,
    name: &'a str,
    ty: Type<'a>,
}

struct FuncDef<'a> {
    // name: &'a str,
    params: Vec<Param<'a>>,
    ret_ty: Type<'a>,
    body: Vec<ast::Statement>,
}

struct Field<'a> {
    name: &'a str,
    ty: Type<'a>,
}

struct StructDef<'a> {
    // name: &'a str,
    fields: Vec<Field<'a>>,
}

pub fn compile(ast: &ast::Ast) -> ir::Program {
    let mut func_defs = BTreeMap::new();
    let mut struct_defs = BTreeMap::new();

    for item in STD.items.iter().chain(&ast.items) {
        match item {
            ast::Item::FuncDef {
                name,
                params,
                ret_ty,
                body,
            } => {
                let func_def = FuncDef {
                    // name: name.as_str(),
                    params: params
                        .iter()
                        .map(|param| Param {
                            mutable: param.mutable,
                            name: param.name.as_str(),
                            ty: Type::from(&param.ty),
                        })
                        .collect(),
                    ret_ty: Type::from(ret_ty),
                    body: body.clone(),
                };
                if func_defs.insert(name.as_str(), func_def).is_some() {
                    panic!("Function `{name}` already defined.");
                }
            }
            ast::Item::StructDef { name, fields } => {
                let struct_def = StructDef {
                    // name: name.as_str(),
                    fields: fields
                        .iter()
                        .map(|field| Field {
                            name: field.name.as_str(),
                            ty: Type::from(&field.ty),
                        })
                        .collect(),
                };
                if struct_defs.insert(name.as_str(), struct_def).is_some() {
                    panic!("Type `{name}` already defined.");
                }
            }
        }
    }

    let mut global_state = GlobalState::new(&func_defs, &struct_defs);

    let main_func_id = compile_func("main", &mut global_state);

    if global_state.func_defs["main"].ret_ty != Type::default() {
        panic!("`main` must return `()`");
    }

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
