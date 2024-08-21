use {
    crate::{ast, ir},
    std::{cmp::Ordering::*, collections::BTreeMap, sync::LazyLock},
};

#[derive(Debug, Clone)]
struct Var<'a> {
    frame_offset: usize,
    mutable: bool,
    name: &'a str,
    ty: Type<'a>,
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
        Type::Ref { .. } => 1,
    }
}

#[derive(Debug, Clone)]
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
        self.expect(&Type::unit())
    }
}

fn push_imm(value: usize) -> [ir::Instruction; 2] {
    [
        ir::Instruction::GrowStack { amount: 1 },
        ir::Instruction::StoreImm {
            dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
            value,
            store_mode: ir::StoreMode::Add,
        },
    ]
}

fn stack_top(offset: usize) -> ir::Value {
    ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackTop { offset }))
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
            scope.frame_size += 1;
            Typed::new(Type::Usize, stack_top(0))
        }
        "print_char" => {
            if args.is_empty() {
                panic!("`{name}` requires at least 1 argument");
            }
            let frame_size = scope.frame_size;
            for arg in args {
                let src = compile_expr(arg, scope, cur_frag).expect(&Type::Usize);
                scope.global.frags[*cur_frag].push(ir::Instruction::Output { src });
            }
            scope.shrink_frame(frame_size, *cur_frag);
            Typed::new(Type::unit(), stack_top(0))
        }
        "print" => {
            if args.is_empty() {
                panic!("`{name}` requires at least 1 argument");
            }
            for arg in args {
                match arg {
                    ast::Expr::String(s) => {
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

                        scope.frame_size += 1;
                        scope.global.frags[*cur_frag].extend(push_imm(return_frag));
                        compile_expr_and_push(arg, scope, cur_frag).expect(&Type::Usize);
                        scope.frame_size -= 2;
                        *cur_frag = return_frag;
                    }
                }
            }
            Typed::new(Type::unit(), stack_top(0))
        }
        "println" => {
            if !args.is_empty() {
                compile_intrinsic_call("print", args, scope, cur_frag).expect_unit();
            }
            scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                src: ir::Value::Immediate(b'\n' as _),
            });
            Typed::new(Type::unit(), stack_top(0))
        }
        "printf" => {
            let Some((first_arg, mut args)) = args.split_first() else {
                panic!("`{name}` requires at least 1 argument");
            };
            let format_string = match first_arg {
                ast::Expr::String(s) => s,
                _ => panic!("First argument to `{name}` must be a string literal"),
            };
            let mut format_chars = format_string.bytes();
            let frame_size = scope.frame_size;
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
            Typed::new(Type::unit(), stack_top(0))
        }
        "exit" => {
            if !args.is_empty() {
                panic!("`{name}` does not take any arguments");
            }
            scope.global.frags[*cur_frag].extend(push_imm(EXIT_FRAG));
            *cur_frag = EXIT_FRAG;
            Typed::new(Type::unit(), stack_top(0))
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
            scope.frame_size -= 1;
            compile_expr_and_push(rhs, scope, cur_frag).expect(&Type::Usize);
            scope.global.frags[*cur_frag].extend(push_imm(short_circuit));

            *cur_frag = short_circuit;

            Typed::new(Type::Usize, stack_top(0))
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
    let ret_size = size_of(&func_def.ret_ty, scope);
    scope.grow_frame(ret_size, *cur_frag);

    // remember the frame size before the call so we can restore it after the call
    let return_frame_size = scope.frame_size;

    // push the return address
    scope.global.frags[*cur_frag].extend(push_imm(return_frag));
    scope.frame_size += 1;

    for (arg, param) in args.iter().zip(&func_def.params) {
        compile_expr_and_push(arg, scope, cur_frag).expect(&param.ty);
    }

    scope.global.frags[*cur_frag].extend(push_imm(call_frag));

    *cur_frag = return_frag;

    // restore the frame size
    scope.frame_size = return_frame_size;

    Typed::new(func_def.ret_ty.clone(), stack_top(0))
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
    Typed::new(struct_ty, stack_top(0))
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
        stack_top(0),
    )
}

fn compute_trail_offset<'a>(
    mut ty: &Type<'a>,
    trail: &'a [ast::FieldIdent],
    scope: &Scope<'a, '_>,
) -> Typed<'a, usize> {
    let mut offset = 0;
    for segment in trail {
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
    Typed::new(ty.clone(), offset)
}

fn compile_var_access<'a>(
    path: &'a ast::Path,
    mutating: bool,
    scope: &Scope<'a, '_>,
) -> Typed<'a, ir::DirectPlace> {
    let Some(var) = scope.vars.iter().rev().find(|var| var.name == path.root) else {
        panic!("Variable `{}` not found.", path.root);
    };
    if mutating && !var.mutable {
        panic!("Variable `{}` is not mutable.", path.root);
    }
    compute_trail_offset(&var.ty, &path.trail, scope).map(|offset| ir::DirectPlace::StackTop {
        offset: scope.frame_size - var.frame_offset - 1 + offset,
    })
}

fn compile_place<'a>(
    place: &'a ast::Place,
    mutating: bool,
    scope: &mut Scope<'a, '_>,
) -> Typed<'a, ir::Place> {
    match place {
        ast::Place::Path(path) => compile_var_access(path, mutating, scope).map(ir::Place::Direct),
        ast::Place::Deref(path) => match compile_var_access(path, false, scope) {
            Typed {
                ty: Type::Ref { mutable, ty },
                value,
            } => {
                if mutating && !mutable {
                    panic!("Cannot mutate immutable reference");
                }
                Typed::new(
                    *ty,
                    ir::Place::Indirect(ir::IndirectPlace::Deref { address: value }),
                )
            }
            Typed { ty, .. } => panic!("Expected reference but got `{ty}`"),
        },
    }
}

fn compile_ref<'a>(
    mutable: bool,
    place: &'a ast::Place,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    let place = compile_place(place, mutable, scope);
    scope.global.frags[*cur_frag].extend([
        ir::Instruction::LoadRef { src: place.value },
        ir::Instruction::GrowStack { amount: 1 },
        ir::Instruction::Store {
            dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
            store_mode: ir::StoreMode::Add,
        },
    ]);
    scope.frame_size += 1;
    Typed::new(
        Type::Ref {
            mutable,
            ty: Box::new(place.ty),
        },
        stack_top(0),
    )
}

fn compile_int<'a>(value: usize) -> Typed<'a, ir::Value> {
    Typed::new(Type::Usize, ir::Value::Immediate(value))
}

fn compile_string<'a>(s: &'a str, scope: &mut Scope<'a, '_>) -> Typed<'a, ir::Value> {
    Typed::new(
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
    )
}

fn compile_expr<'a>(
    expr: &'a ast::Expr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    match *expr {
        ast::Expr::Int(i) => compile_int(i),
        ast::Expr::String(ref s) => compile_string(s, scope),
        ast::Expr::Place(ref place) => compile_place(place, false, scope).map(ir::Value::At),
        ast::Expr::Ref { mutable, ref place } => compile_ref(mutable, place, scope, cur_frag),
        ast::Expr::Call(ref call_expr) => compile_call(call_expr, scope, cur_frag),
        ast::Expr::Struct(ref struct_expr) => compile_struct_expr(struct_expr, scope, cur_frag),
        ast::Expr::Tuple(ref tuple_expr) => compile_tuple_expr(tuple_expr, scope, cur_frag),
    }
}

fn compile_expr_and_push<'a>(
    expr: &'a ast::Expr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ()> {
    let orig_frame_size = scope.frame_size;
    let mut result = compile_expr(expr, scope, cur_frag);
    let size = size_of(&result.ty, scope);
    if scope.frame_size < orig_frame_size + size || result.value != stack_top(0) {
        scope.grow_frame(size, *cur_frag);
        if !matches!(result.value, ir::Value::Immediate(_)) {
            // recompile to account for the new frame size
            // TODO: remove the need for this by making stack offsets relative to the frame base
            result = compile_expr(expr, scope, cur_frag);
        }
        compile_move(
            ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
            result.value,
            ir::StoreMode::Add,
            &result.ty,
            scope,
            *cur_frag,
        );
    }
    Typed::new(result.ty, ())
}

fn increment_place(place: ir::Place) -> ir::Place {
    match place {
        ir::Place::Direct(ir::DirectPlace::StackTop { offset }) => {
            ir::Place::Direct(ir::DirectPlace::StackTop { offset: offset + 1 })
        }
        ir::Place::Indirect(_) => {
            todo!("incrementing indirect places")
        }
    }
}

fn compile_move<'a>(
    mut dst: ir::Place,
    src: ir::Value,
    store_mode: ir::StoreMode,
    ty: &Type<'a>,
    scope: &mut Scope<'a, '_>,
    cur_frag: FragId,
) {
    match size_of(ty, scope) {
        0 => {}
        1 => match src {
            ir::Value::Immediate(value) => {
                scope.global.frags[cur_frag].extend([ir::Instruction::StoreImm {
                    dst,
                    value,
                    store_mode,
                }]);
            }
            ir::Value::At(src) => {
                scope.global.frags[cur_frag].extend([
                    ir::Instruction::Load { src },
                    ir::Instruction::Store { dst, store_mode },
                ]);
            }
        },
        size => {
            let ir::Value::At(mut src) = src else {
                unreachable!("Expected source value to be a place in a move of size > 1");
            };
            for _ in 0..size {
                scope.global.frags[cur_frag].extend([
                    ir::Instruction::Load { src },
                    ir::Instruction::Store { dst, store_mode },
                ]);
                dst = increment_place(dst);
                src = increment_place(src);
            }
        }
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
    frame_size: usize,
    global: &'b mut GlobalState<'a>,
    loop_stack: Vec<LoopInfo>,
}

impl<'a, 'b> Scope<'a, 'b> {
    fn new(func_def: &'a FuncDef<'a>, global: &'b mut GlobalState<'a>) -> Self {
        Self {
            func_def,
            vars: vec![],
            frame_size: 0,
            global,
            loop_stack: vec![],
        }
    }

    fn shrink_frame_early(&mut self, target_frame_size: usize, cur_frag: FragId) {
        match target_frame_size.cmp(&self.frame_size) {
            Less => {
                self.global.frags[cur_frag].push(ir::Instruction::ShrinkStack {
                    amount: self.frame_size - target_frame_size,
                });
            }
            Equal => {}
            Greater => unreachable!("Cannot shrink_frame_early to a larger size"),
        }
    }

    fn shrink_frame(&mut self, target_frame_size: usize, cur_frag: FragId) {
        match target_frame_size.cmp(&self.frame_size) {
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
                self.frame_size = target_frame_size;
            }
            Equal => {}
            Greater => unreachable!("Cannot shrink_frame to a larger size"),
        }
    }

    fn grow_frame(&mut self, amount: usize, cur_frag: FragId) {
        self.global.frags[cur_frag].push(ir::Instruction::GrowStack { amount });
        self.frame_size += amount;
    }
}

fn execute_block<'a>(body: &'a [ast::Statement], scope: &mut Scope<'a, '_>, cur_frag: &mut FragId) {
    let block = compile_block(body, scope);
    scope.global.frags[*cur_frag].extend(push_imm(block.start));
    // TODO: if block.start == block.end, then inline the block
    *cur_frag = block.end;
}

#[must_use]
struct CompiledBlock {
    start: FragId,
    end: FragId,
}

fn compile_block<'a>(statements: &'a [ast::Statement], scope: &mut Scope<'a, '_>) -> CompiledBlock {
    let orig_frame_size = scope.frame_size;
    let start_frag = scope
        .global
        .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);
    let mut cur_frag = start_frag;
    for statement in statements {
        match *statement {
            ast::Statement::Let {
                mutable,
                ref name,
                ref ty,
                ref value,
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
                    frame_offset: scope.frame_size - 1,
                    mutable,
                    name,
                    ty,
                });
            }
            ast::Statement::Assign {
                ref place,
                ref value,
                mode,
            } => match (mode, value) {
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
                    let frame_size = scope.frame_size;
                    let dst = compile_place(place, true, scope);
                    assert!(frame_size == scope.frame_size);
                    if src.ty != dst.ty {
                        panic!(
                            "Cannot assign value of type `{}` to place of type `{}`.",
                            src.ty, dst.ty
                        );
                    }
                    compile_move(
                        dst.value,
                        src.value,
                        compile_store_mode(mode),
                        &src.ty,
                        scope,
                        cur_frag,
                    );
                }
            },
            ast::Statement::Loop(ref body) => {
                let after_loop = scope
                    .global
                    .add_frag(vec![ir::Instruction::ShrinkStack { amount: 1 }]);
                let loop_start = scope.global.frags.len();
                scope.loop_stack.push(LoopInfo {
                    start: loop_start,
                    after: after_loop,
                    frame_size: scope.frame_size,
                });
                let loop_body = compile_block(body, scope);
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
                ref cond,
                ref cases,
                ref default,
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
                                    scope,
                                )
                            })
                            .collect::<Vec<_>>();
                        let default_block = compile_block(default, scope);
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
            ast::Statement::Block(ref body) => execute_block(body, scope, &mut cur_frag),
            ast::Statement::Return(ref value) => {
                let src = compile_expr(value, scope, &mut cur_frag).expect(&scope.func_def.ret_ty);
                compile_move(
                    ir::Place::Direct(ir::DirectPlace::StackTop {
                        offset: scope.frame_size + 1,
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
            ast::Statement::Eval(ref expr) => {
                let frame_size = scope.frame_size;
                let _ = compile_expr(expr, scope, &mut cur_frag);
                scope.shrink_frame(frame_size, cur_frag);
            }
        }
    }
    scope.shrink_frame(orig_frame_size, cur_frag);
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
        scope.frame_size += size_of(&param.ty, &scope);
        scope.vars.push(Var {
            frame_offset: scope.frame_size - 1,
            mutable: param.mutable,
            name: param.name,
            ty: param.ty.clone(),
        });
    }

    let block = compile_block(&func_def.body, &mut scope);
    scope.shrink_frame(0, block.end);

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
    Ref { mutable: bool, ty: Box<Type<'a>> },
}

impl Type<'_> {
    const fn unit() -> Self {
        Type::Tuple(vec![])
    }
}

impl std::fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Type::Usize => write!(f, "usize"),
            Type::Tuple(ref tys) => {
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
            Type::Ref { mutable, ref ty } => {
                write!(f, "&")?;
                if mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{}", ty)
            }
        }
    }
}

impl<'a> From<&'a ast::Type> for Type<'a> {
    fn from(ty: &'a ast::Type) -> Self {
        match *ty {
            ast::Type::Tuple(ref tys) => Type::Tuple(tys.iter().map(Type::from).collect()),
            ast::Type::Named(ref name) => match name.as_str() {
                "usize" => Type::Usize,
                _ => Type::Named(name),
            },
            ast::Type::Ref { mutable, ref ty } => Type::Ref {
                mutable,
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

    if global_state.func_defs["main"].ret_ty != Type::unit() {
        panic!("`main` must return `()`");
    }

    global_state.frags[EXIT_FRAG].clear(); // remove dead code

    ir::Program {
        instructions: vec![
            ir::Instruction::GrowStack { amount: 3 },
            ir::Instruction::StoreImm {
                dst: ir::Place::Direct(ir::DirectPlace::StackTop { offset: 0 }),
                value: main_func_id,
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
