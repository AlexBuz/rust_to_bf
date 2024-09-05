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

#[derive(Debug, Clone)]
#[must_use]
struct Typed<'a, T> {
    value: T,
    ty: Type<'a>,
}

impl<'a, T> Typed<'a, T> {
    fn new(value: T, ty: Type<'a>) -> Self {
        Self { value, ty }
    }

    fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Typed<'a, U> {
        Typed {
            value: f(self.value),
            ty: self.ty,
        }
    }

    fn expect(self, expected_ty: &Type<'a>, context: &str) -> T {
        if self.ty != *expected_ty {
            panic!("{context}: expected `{expected_ty}`, found `{}`", self.ty);
        }
        self.value
    }

    fn expect_ref(self, expected_ty: &Type<'a>, context: &str) -> T {
        match self.ty {
            Type::Ref { ty, .. } if *ty == *expected_ty => self.value,
            _ => panic!("{context}: expected `&{expected_ty}`, found `{}`", self.ty),
        }
    }
}

type FragId = usize;

fn set_imm(frame_offset: usize, value: usize) -> ir::Instruction {
    ir::Instruction::StoreImm {
        dst: ir::Place::Direct(ir::DirectPlace::StackFrame {
            offset: frame_offset,
        }),
        value,
        store_mode: ir::StoreMode::Replace,
    }
}

fn goto(frag_id: FragId, frame_base_offset: usize) -> [ir::Instruction; 2] {
    [
        ir::Instruction::SaveFrame {
            size: frame_base_offset,
        },
        set_imm(0, frag_id),
    ]
}

fn stack_value_at(offset: usize) -> ir::Value {
    ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackFrame { offset }))
}

static MACROS: &[&str] = &[
    "read_char",
    "print_char",
    "print_str",
    "print",
    "println",
    "exit",
];

fn compile_macro_call<'a>(
    name: &str,
    args: &'a [ast::Expr],
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    let orig_frame_offset = scope.frame_offset;
    match name {
        "read_char" => {
            if !args.is_empty() {
                panic!("`{name}` does not take any arguments");
            }
            scope.global.frags[*cur_frag].extend([ir::Instruction::Input {
                dst: ir::Place::Direct(ir::DirectPlace::StackFrame {
                    offset: scope.frame_offset,
                }),
            }]);
            scope.frame_offset += 1;
            Typed::new(stack_value_at(orig_frame_offset), Type::Char)
        }
        "print_char" => {
            if args.is_empty() {
                panic!("`{name}` requires at least 1 argument");
            }
            for arg in args {
                let src = compile_expr(arg, scope, cur_frag).expect(&Type::Char, name);
                scope.global.frags[*cur_frag].push(ir::Instruction::Output { src });
                scope.frame_offset = orig_frame_offset;
            }
            Typed::new(stack_value_at(orig_frame_offset), Type::unit())
        }
        "print_str" => {
            if args.is_empty() {
                panic!("`{name}` requires at least 1 argument");
            }
            for arg in args {
                match arg {
                    ast::Expr::Str(s) => {
                        scope.global.frags[*cur_frag].extend(s.bytes().map(|byte| {
                            ir::Instruction::Output {
                                src: ir::Value::Immediate(byte as usize),
                            }
                        }));
                    }
                    _ => {
                        let return_frag =
                            scope.global.add_frag(vec![ir::Instruction::RestoreFrame {
                                size: scope.frame_offset,
                            }]);

                        scope.global.frags[*cur_frag]
                            .push(set_imm(scope.frame_offset, return_frag));
                        scope.frame_offset += 1;

                        compile_expr_and_push(arg, scope, cur_frag).expect_ref(&Type::Str, name);

                        scope.global.frags[*cur_frag].extend([ir::Instruction::SaveFrame {
                            size: scope.frame_offset - 1,
                        }]);
                        scope.frame_offset = orig_frame_offset;

                        *cur_frag = return_frag;
                    }
                }
            }
            Typed::new(stack_value_at(orig_frame_offset), Type::unit())
        }
        "print" => {
            let Some((first_arg, mut args)) = args.split_first() else {
                panic!("`{name}` requires at least 1 argument");
            };
            let format_string = match first_arg {
                ast::Expr::Str(s) => s,
                _ => panic!("first argument to `{name}` must be a string literal"),
            };
            let mut format_chars = format_string.bytes();
            while let Some(c) = format_chars.next() {
                if c == b'%' {
                    let Some(format_specifier) = format_chars.next() else {
                        panic!("unterminated format specifier");
                    };
                    if format_specifier == b'%' {
                        scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                            src: ir::Value::Immediate(b'%' as _),
                        });
                        continue;
                    }
                    let Some((arg, rest)) = args.split_at_checked(1) else {
                        panic!("not enough arguments for format string `{format_string}`")
                    };
                    args = rest;
                    match format_specifier {
                        b'i' | b'd' => compile_func_call("print_int", arg, scope, cur_frag),
                        b'c' => compile_macro_call("print_char", arg, scope, cur_frag),
                        b's' => compile_macro_call("print_str", arg, scope, cur_frag),
                        _ => panic!("invalid format specifier"),
                    }
                    .expect(&Type::unit(), name);
                } else {
                    scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                        src: ir::Value::Immediate(c as _),
                    });
                }
            }
            scope.frame_offset = orig_frame_offset;
            if !args.is_empty() {
                panic!("too many arguments for format string `{format_string}`");
            }
            Typed::new(stack_value_at(orig_frame_offset), Type::unit())
        }
        "println" => {
            if !args.is_empty() {
                compile_macro_call("print", args, scope, cur_frag).expect(&Type::unit(), name);
            }
            scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                src: ir::Value::Immediate(b'\n' as usize),
            });
            Typed::new(stack_value_at(orig_frame_offset), Type::unit())
        }
        "exit" => {
            if !args.is_empty() {
                panic!("`{name}` does not take any arguments");
            }
            scope.global.frags[*cur_frag].extend(goto(EXIT_FRAG, scope.frame_call_offset));
            *cur_frag = EXIT_FRAG;
            Typed::new(stack_value_at(orig_frame_offset), Type::unit())
        }
        "==" | "!=" => {
            let [lhs, rhs] = args else {
                panic!("`{name}` requires exactly 2 arguments");
            };
            let lhs = compile_expr_and_push(lhs, scope, cur_frag);
            let rhs = compile_expr(rhs, scope, cur_frag);
            if lhs.ty != rhs.ty {
                panic!("cannot compare `{}` to `{}`", lhs.ty, rhs.ty);
            }
            let size = scope.size_of(&lhs.ty);
            if size != 1 {
                todo!("comparison of types with size != 1");
            }
            let [if_same, if_diff] = match name {
                "==" => [1, 0],
                "!=" => [0, 1],
                _ => unreachable!("the only comparison operators are `==` and `!=`"),
            };
            match rhs.value {
                ir::Value::At(src) => scope.global.frags[*cur_frag].extend([
                    ir::Instruction::Load { src, multiplier: 1 },
                    ir::Instruction::Store {
                        dst: ir::Place::Direct(lhs.value),
                        store_mode: ir::StoreMode::Subtract,
                    },
                ]),
                ir::Value::Immediate(value) => {
                    scope.global.frags[*cur_frag].push(ir::Instruction::StoreImm {
                        dst: ir::Place::Direct(lhs.value),
                        value,
                        store_mode: ir::StoreMode::Subtract,
                    })
                }
            }
            scope.global.frags[*cur_frag].extend([ir::Instruction::Switch {
                cond: ir::Place::Direct(lhs.value),
                cases: vec![vec![ir::Instruction::StoreImm {
                    dst: ir::Place::Direct(lhs.value),
                    value: if_same,
                    store_mode: ir::StoreMode::Replace,
                }]],
                default: vec![ir::Instruction::StoreImm {
                    dst: ir::Place::Direct(lhs.value),
                    value: if_diff,
                    store_mode: ir::StoreMode::Replace,
                }],
            }]);
            scope.frame_offset = orig_frame_offset + size;
            Typed::new(stack_value_at(orig_frame_offset), Type::Bool)
        }
        "&&" | "||" => {
            let [lhs, rhs] = args else {
                panic!("`{name}` requires exactly 2 arguments");
            };

            let short_circuit = scope.new_frag();
            let long_circuit = scope.new_frag();

            let lhs = compile_expr_and_push(lhs, scope, cur_frag).expect(&Type::Bool, name);

            let [false_circuit, true_circuit] = match name {
                "&&" => [short_circuit, long_circuit],
                "||" => [long_circuit, short_circuit],
                _ => unreachable!("the only short-circuiting operators are `&&` and `||`"),
            };

            scope.global.frags[*cur_frag].extend([ir::Instruction::Switch {
                cond: ir::Place::Direct(lhs),
                cases: vec![goto(false_circuit, scope.frame_call_offset).to_vec()],
                default: goto(true_circuit, scope.frame_call_offset).to_vec(),
            }]);

            *cur_frag = long_circuit;
            scope.frame_offset -= 1;
            compile_expr_and_push(rhs, scope, cur_frag).expect(&Type::Bool, name);
            scope.global.frags[*cur_frag].extend(goto(short_circuit, scope.frame_call_offset));

            *cur_frag = short_circuit;

            Typed::new(stack_value_at(orig_frame_offset), Type::Bool)
        }
        _ => {
            if scope.global.func_defs.contains_key(name) {
                panic!("function `{name}` must be called without `!`");
            } else {
                panic!("macro `{name}` does not exist");
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
            "function `{name}` expects {} arguments, but {} were provided",
            func_def.params.len(),
            args.len()
        );
    }

    // Stack order: [return value, return address, arguments, local variables]
    // The caller leaves space for the return value and pushes the return address and arguments
    // The callee is responsible for popping the arguments before returning
    // The caller then pops the return address and may use the return value

    let orig_frame_offset = scope.frame_offset;

    // leave space for the return value
    let ret_size = scope.size_of(&func_def.ret_ty);
    scope.frame_offset += ret_size;

    // the return frag will restore everything up to this point
    let return_frag = scope.global.add_frag(vec![ir::Instruction::RestoreFrame {
        size: scope.frame_offset,
    }]);

    // push the return frag id
    scope.global.frags[*cur_frag].push(set_imm(scope.frame_offset, return_frag));
    scope.frame_offset += 1;

    // push the call frag id
    scope.global.frags[*cur_frag].push(set_imm(scope.frame_offset, call_frag));
    scope.frame_offset += 1;

    // push the arguments
    for (arg, param) in args.iter().zip(&func_def.params) {
        compile_expr_and_push(arg, scope, cur_frag).expect(&param.ty, name);
    }

    // make the call
    scope.global.frags[*cur_frag].push(ir::Instruction::SaveFrame {
        size: orig_frame_offset + ret_size + 1,
    });

    // restore the frame after the call
    scope.frame_offset = orig_frame_offset + ret_size;
    *cur_frag = return_frag;

    Typed::new(stack_value_at(orig_frame_offset), func_def.ret_ty.clone())
}

fn compile_call<'a>(
    call_expr: &'a ast::CallExpr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    if call_expr.bang {
        compile_macro_call(&call_expr.func, &call_expr.args, scope, cur_frag)
    } else {
        compile_func_call(&call_expr.func, &call_expr.args, scope, cur_frag)
    }
}

fn compile_struct_expr<'a>(
    struct_expr: &'a ast::StructExpr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    let Some(struct_def) = scope.global.struct_defs.get(&*struct_expr.name) else {
        panic!("type `{}` not found", struct_expr.name);
    };

    if struct_expr.fields.len() != struct_def.fields.len() {
        panic!(
            "struct `{}` has {} fields, but {} were provided",
            struct_expr.name,
            struct_def.fields.len(),
            struct_expr.fields.len()
        );
    }

    struct FieldInfo<'a> {
        ty: &'a Type<'a>,
        frame_offset: usize,
    }

    let struct_frame_offset = scope.frame_offset;

    let mut expected_fields: BTreeMap<&str, Option<FieldInfo>> = struct_def
        .fields
        .iter()
        .scan(struct_frame_offset, |frame_offset, field| {
            let info = Some((
                field.name,
                Some(FieldInfo {
                    ty: &field.ty,
                    frame_offset: *frame_offset,
                }),
            ));
            *frame_offset += scope.size_of(&field.ty);
            info
        })
        .collect();

    for field in &struct_expr.fields {
        let Some(expected_field) = expected_fields.get_mut(&*field.name).map(Option::take) else {
            panic!(
                "field `{}` not found in struct `{}`",
                field.name, struct_expr.name
            );
        };
        let Some(field_info) = expected_field else {
            panic!("field `{}` specified more than once", field.name);
        };
        match scope.frame_offset.cmp(&field_info.frame_offset) {
            Less | Equal => {
                scope.frame_offset = field_info.frame_offset;
                compile_expr_and_push(&field.value, scope, cur_frag)
                    .expect(field_info.ty, &field.name);
            }
            Greater => {
                let prev_frame_offset = scope.frame_offset;
                compile_expr_and_push(&field.value, scope, cur_frag)
                    .expect(field_info.ty, &field.name);
                compile_move(
                    ir::Place::Direct(ir::DirectPlace::StackFrame {
                        offset: field_info.frame_offset,
                    }),
                    stack_value_at(prev_frame_offset),
                    ir::StoreMode::Replace,
                    field_info.ty,
                    scope,
                    *cur_frag,
                );
                scope.frame_offset = prev_frame_offset;
            }
        }
    }

    Typed::new(
        stack_value_at(struct_frame_offset),
        Type::Named(&struct_expr.name),
    )
}

fn compile_tuple_expr<'a>(
    elements: &'a [ast::Expr],
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    Typed::new(
        stack_value_at(scope.frame_offset),
        Type::Tuple(
            elements
                .iter()
                .map(|expr| compile_expr_and_push(expr, scope, cur_frag).ty)
                .collect(),
        ),
    )
}

fn compile_array_expr<'a>(
    array_expr: &'a ast::ArrayExpr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    Typed::new(
        stack_value_at(scope.frame_offset),
        match *array_expr {
            ast::ArrayExpr::List(ref elements) => Type::Array {
                len: elements.len(),
                ty: Box::new(match elements.split_first() {
                    Some((first, rest)) => {
                        let first_ty = compile_expr_and_push(first, scope, cur_frag).ty;
                        for element in rest {
                            if compile_expr_and_push(element, scope, cur_frag).ty != first_ty {
                                panic!("array elements must all have the same type");
                            }
                        }
                        first_ty
                    }
                    None => todo!("empty array type inference"),
                }),
            },
            ast::ArrayExpr::Repeat { ref value, len } => {
                let first_elem = compile_expr_and_push(value, scope, cur_frag);
                let size = scope.size_of(&first_elem.ty);
                match len {
                    0 => scope.frame_offset -= size,
                    _ => {
                        for _ in 1..len {
                            compile_move(
                                ir::Place::Direct(ir::DirectPlace::StackFrame {
                                    offset: scope.frame_offset,
                                }),
                                ir::Value::At(ir::Place::Direct(first_elem.value)),
                                ir::StoreMode::Replace,
                                &first_elem.ty,
                                scope,
                                *cur_frag,
                            );
                            scope.frame_offset += size;
                        }
                    }
                }
                Type::Array {
                    len,
                    ty: Box::new(first_elem.ty),
                }
            }
        },
    )
}

fn compile_var_access<'a>(
    name: &'a str,
    mutating: bool,
    scope: &Scope<'a, '_>,
) -> Typed<'a, ir::Place> {
    let Some(var) = scope.vars.iter().rev().find(|var| var.name == name) else {
        panic!("variable `{name}` not found");
    };
    if mutating && !var.mutable {
        panic!("variable `{name}` is not mutable");
    }
    Typed::new(
        ir::Place::Direct(ir::DirectPlace::StackFrame {
            offset: var.frame_offset,
        }),
        var.ty.clone(),
    )
}

fn compile_field_access<'a>(
    base: &'a ast::Place,
    field_name: &'a str,
    mutating: bool,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Place> {
    let mut base = compile_place(base, mutating, scope, cur_frag);
    while let Type::Ref { .. } = base.ty {
        base = compile_deref(base, mutating, scope, *cur_frag);
    }
    let mut found_field = None;
    match &base.ty {
        Type::Tuple(tys) => {
            if let Ok(index) = field_name.parse() {
                if let Some(ty) = tys.get(index) {
                    found_field = Some(Typed::new(
                        tys[..index].iter().map(|ty| scope.size_of(ty)).sum(),
                        ty.to_owned(),
                    ));
                }
            }
        }
        Type::Named(ty_name) => {
            let Some(struct_def) = scope.global.struct_defs.get(ty_name) else {
                panic!("type `{ty_name}` not found");
            };
            found_field = struct_def
                .fields
                .iter()
                .enumerate()
                .find_map(|(index, field)| {
                    (field.name == field_name).then(|| {
                        Typed::new(
                            struct_def.fields[..index]
                                .iter()
                                .map(|field| scope.size_of(&field.ty))
                                .sum(),
                            field.ty.to_owned(),
                        )
                    })
                });
        }
        _ => {}
    }
    let Some(Typed { value: offset, ty }) = found_field else {
        panic!("no field `{}` on type `{}`", field_name, base.ty);
    };
    if offset > 0 {
        let size = scope.size_of(&ty);
        own_place(
            &mut base.value,
            &mut scope.global.frags[*cur_frag],
            scope.frame_offset + size,
        );
        increment_place(&mut base.value, &mut scope.global.frags[*cur_frag], offset);
    }
    Typed::new(base.value, ty)
}

fn compile_deref<'a>(
    place: Typed<'a, ir::Place>,
    mutating: bool,
    scope: &mut Scope<'a, '_>,
    cur_frag: FragId,
) -> Typed<'a, ir::Place> {
    match place {
        Typed {
            ty: Type::Ref { mutable, ty },
            value,
        } => {
            if mutating && !mutable {
                panic!("cannot mutate value of type `{ty}` through an immutable reference");
            }
            Typed::new(
                match value {
                    ir::Place::Direct(address) => {
                        ir::Place::Indirect(ir::IndirectPlace::Deref { address })
                    }
                    ir::Place::Indirect(_) => {
                        let offset = scope.frame_offset + scope.size_of(&ty);
                        compile_move(
                            ir::Place::Direct(ir::DirectPlace::StackFrame { offset }),
                            ir::Value::At(value),
                            ir::StoreMode::Replace,
                            &ty,
                            scope,
                            cur_frag,
                        );
                        ir::Place::Indirect(ir::IndirectPlace::Deref {
                            address: ir::DirectPlace::StackFrame { offset },
                        })
                    }
                },
                *ty,
            )
        }
        Typed { ty, .. } => panic!("cannot dereference value of type `{ty}`"),
    }
}

fn compile_index<'a>(
    base: &'a ast::Place,
    index: &'a ast::Expr,
    mutating: bool,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Place> {
    let mut base = compile_place(base, mutating, scope, cur_frag);
    while let Type::Ref { .. } = base.ty {
        base = compile_deref(base, mutating, scope, *cur_frag);
    }
    let Type::Array { ty: elem_ty, .. } = base.ty else {
        panic!("cannot index into value of type `{}`", base.ty);
    };
    let index = compile_expr(index, scope, cur_frag).expect(&Type::Usize, "array index");
    let elem_size = scope.size_of(&elem_ty);
    let address = ir::DirectPlace::StackFrame {
        offset: scope.frame_offset + elem_size,
    };
    scope.global.frags[*cur_frag].extend([
        ir::Instruction::LoadRef { src: base.value },
        ir::Instruction::Store {
            dst: ir::Place::Direct(address),
            store_mode: ir::StoreMode::Replace,
        },
    ]);
    compile_single_move(
        ir::Place::Direct(address),
        index,
        ir::StoreMode::Add,
        elem_size * 2,
        &mut scope.global.frags[*cur_frag],
    );
    Typed::new(
        ir::Place::Indirect(ir::IndirectPlace::Deref { address }),
        *elem_ty,
    )
}

fn compile_place<'a>(
    place: &'a ast::Place,
    mutating: bool,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Place> {
    let orig_frame_offset = scope.frame_offset;
    let result = match place {
        ast::Place::Var(ident) => compile_var_access(ident, mutating, scope),
        ast::Place::FieldAccess { base, field } => {
            compile_field_access(base, field, mutating, scope, cur_frag)
        }
        ast::Place::Index { base, index } => compile_index(base, index, mutating, scope, cur_frag),
        ast::Place::Deref(expr) => compile_deref(
            compile_expr(expr, scope, cur_frag).map(|value| match value {
                ir::Value::At(place) => place,
                ir::Value::Immediate(address) => {
                    ir::Place::Direct(ir::DirectPlace::Address(address))
                }
            }),
            mutating,
            scope,
            *cur_frag,
        ),
    };
    scope.frame_offset = orig_frame_offset;
    result
}

fn compile_ref<'a>(
    mutable: bool,
    place: &'a ast::Place,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    let place = compile_place(place, mutable, scope, cur_frag);
    scope.global.frags[*cur_frag].extend([
        ir::Instruction::LoadRef { src: place.value },
        ir::Instruction::Store {
            dst: ir::Place::Direct(ir::DirectPlace::StackFrame {
                offset: scope.frame_offset,
            }),
            store_mode: ir::StoreMode::Replace,
        },
    ]);
    scope.frame_offset += 1;
    Typed::new(
        stack_value_at(scope.frame_offset - 1),
        Type::Ref {
            mutable,
            ty: Box::new(place.ty),
        },
    )
}

fn compile_str<'a>(s: &'a str, scope: &mut Scope<'a, '_>) -> Typed<'a, ir::Value> {
    Typed::new(
        ir::Value::Immediate(*scope.global.string_ids.entry(s).or_insert_with(|| {
            scope.global.frags.push(
                [ir::Instruction::RestoreFrame { size: 1 }]
                    .into_iter()
                    .chain(s.bytes().map(|byte| ir::Instruction::Output {
                        src: ir::Value::Immediate(byte as usize),
                    }))
                    .collect(),
            );
            scope.global.frags.len() - 1
        })),
        Type::Ref {
            mutable: false,
            ty: Box::new(Type::Str),
        },
    )
}

fn compile_expr<'a>(
    expr: &'a ast::Expr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::Value> {
    match *expr {
        ast::Expr::Int(i) => Typed::new(ir::Value::Immediate(i), Type::Usize),
        ast::Expr::Char(c) => Typed::new(ir::Value::Immediate(c as usize), Type::Char),
        ast::Expr::Bool(b) => Typed::new(ir::Value::Immediate(b as usize), Type::Bool),
        ast::Expr::Str(ref s) => compile_str(s, scope),
        ast::Expr::Place(ref place) => {
            compile_place(place, false, scope, cur_frag).map(ir::Value::At)
        }
        ast::Expr::Ref { mutable, ref place } => compile_ref(mutable, place, scope, cur_frag),
        ast::Expr::Call(ref call_expr) => compile_call(call_expr, scope, cur_frag),
        ast::Expr::Struct(ref struct_expr) => compile_struct_expr(struct_expr, scope, cur_frag),
        ast::Expr::Tuple(ref elements) => compile_tuple_expr(elements, scope, cur_frag),
        ast::Expr::Array(ref array_expr) => compile_array_expr(array_expr, scope, cur_frag),
        ast::Expr::Cast { ref expr, ref ty } => {
            let expr = compile_expr(expr, scope, cur_frag);
            let target_ty = Type::from(ty);
            if scope.size_of(&expr.ty) != scope.size_of(&target_ty) {
                panic!("cannot cast `{}` to `{}`", expr.ty, target_ty);
            }
            Typed::new(expr.value, target_ty)
        }
    }
}

fn compile_expr_and_push<'a>(
    expr: &'a ast::Expr,
    scope: &mut Scope<'a, '_>,
    cur_frag: &mut FragId,
) -> Typed<'a, ir::DirectPlace> {
    let orig_frame_offset = scope.frame_offset;
    let result = compile_expr(expr, scope, cur_frag);
    let size = scope.size_of(&result.ty);
    if scope.frame_offset == orig_frame_offset && size > 0 {
        compile_move(
            ir::Place::Direct(ir::DirectPlace::StackFrame {
                offset: scope.frame_offset,
            }),
            result.value,
            ir::StoreMode::Replace,
            &result.ty,
            scope,
            *cur_frag,
        );
        scope.frame_offset += size;
    }
    Typed::new(
        ir::DirectPlace::StackFrame {
            offset: orig_frame_offset,
        },
        result.ty,
    )
}

fn increment_place(place: &mut ir::Place, cur_frag: &mut Vec<ir::Instruction>, amount: usize) {
    match place {
        ir::Place::Direct(ir::DirectPlace::StackFrame { offset }) => {
            *offset += amount;
        }
        ir::Place::Direct(ir::DirectPlace::Address(address)) => *address += 2 * amount,
        ir::Place::Indirect(ir::IndirectPlace::Deref { address }) => {
            cur_frag.push(ir::Instruction::StoreImm {
                dst: ir::Place::Direct(*address),
                value: 2 * amount,
                store_mode: ir::StoreMode::Add,
            });
        }
    }
}

fn own_place(place: &mut ir::Place, cur_frag: &mut Vec<ir::Instruction>, new_frame_offset: usize) {
    let ir::Place::Indirect(ir::IndirectPlace::Deref { address }) = place else {
        return;
    };
    let owned_address = ir::DirectPlace::StackFrame {
        offset: new_frame_offset,
    };
    cur_frag.extend([
        ir::Instruction::Load {
            src: ir::Place::Direct(*address),
            multiplier: 1,
        },
        ir::Instruction::Store {
            dst: ir::Place::Direct(owned_address),
            store_mode: ir::StoreMode::Replace,
        },
    ]);
    *address = owned_address;
}

fn compile_single_move(
    dst: ir::Place,
    src: ir::Value,
    store_mode: ir::StoreMode,
    multiplier: usize,
    cur_frag: &mut Vec<ir::Instruction>,
) {
    match src {
        ir::Value::Immediate(value) => {
            cur_frag.extend([ir::Instruction::StoreImm {
                dst,
                value: value * multiplier,
                store_mode,
            }]);
        }
        ir::Value::At(src) => {
            cur_frag.extend([
                ir::Instruction::Load { src, multiplier },
                ir::Instruction::Store { dst, store_mode },
            ]);
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
    let size = scope.size_of(ty);
    let cur_frag = &mut scope.global.frags[cur_frag];
    match size {
        0 => {}
        1 => compile_single_move(dst, src, store_mode, 1, cur_frag),
        2.. => {
            let ir::Value::At(mut src) = src else {
                unreachable!("expected source value to be a place in a move of size > 1");
            };
            own_place(&mut dst, cur_frag, scope.frame_offset + size);
            own_place(&mut src, cur_frag, scope.frame_offset + size + 1);
            for _ in 0..size {
                cur_frag.extend([
                    ir::Instruction::Load { src, multiplier: 1 },
                    ir::Instruction::Store { dst, store_mode },
                ]);
                increment_place(&mut dst, cur_frag, 1);
                increment_place(&mut src, cur_frag, 1);
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

    fn size_of(&self, ty: &Type) -> usize {
        match ty {
            Type::Usize => 1,
            Type::Char => 1,
            Type::Bool => 1,
            Type::Str => panic!("cannot get size of unsized type `str`"),
            Type::Tuple(tys) => tys.iter().map(|ty| self.size_of(ty)).sum(),
            Type::Array { ty, len } => self.size_of(ty) * len,
            Type::Named(name) => self
                .struct_defs
                .get(name)
                .unwrap_or_else(|| panic!("type `{name}` not found"))
                .fields
                .iter()
                .map(|field| self.size_of(&field.ty))
                .sum(),
            Type::Ref { .. } => 1,
        }
    }
}

struct LoopInfo {
    start: FragId,
    after: FragId,
}

struct Scope<'a, 'b> {
    func_def: &'a FuncDef<'a>,
    vars: Vec<Var<'a>>,
    frame_offset: usize,
    frame_call_offset: usize,
    global: &'b mut GlobalState<'a>,
    loop_stack: Vec<LoopInfo>,
}

impl<'a, 'b> Scope<'a, 'b> {
    fn new(func_def: &'a FuncDef<'a>, global: &'b mut GlobalState<'a>) -> Self {
        let frame_call_offset = global.size_of(&func_def.ret_ty) + 1; // return value and frag id
        let mut frame_offset = frame_call_offset + 1; // call frag id
        let mut vars = vec![];
        for param in &func_def.params {
            vars.push(Var {
                frame_offset,
                mutable: param.mutable,
                name: param.name,
                ty: param.ty.clone(),
            });
            frame_offset += global.size_of(&param.ty);
        }
        Self {
            func_def,
            vars,
            frame_offset,
            frame_call_offset,
            global,
            loop_stack: vec![],
        }
    }

    fn shrink_frame(&mut self, target_frame_offset: usize) {
        match target_frame_offset.cmp(&self.frame_offset) {
            Less => {
                self.vars.truncate(
                    self.vars
                        .iter()
                        .rev()
                        .position(|var| var.frame_offset < target_frame_offset)
                        .map(|pos| self.vars.len() - pos)
                        .unwrap_or(0),
                );
                self.frame_offset = target_frame_offset;
            }
            Equal => {}
            Greater => unreachable!("cannot shrink frame to a larger size"),
        }
    }

    fn emit_return(&mut self, cur_frag: FragId) {
        self.global.frags[cur_frag].push(ir::Instruction::SaveFrame {
            size: self.frame_call_offset - 1,
        });
    }

    fn new_frag(&mut self) -> FragId {
        self.global.add_frag(vec![ir::Instruction::RestoreFrame {
            size: self.frame_call_offset,
        }])
    }

    fn size_of(&self, ty: &Type) -> usize {
        self.global.size_of(ty)
    }
}

fn execute_block<'a>(body: &'a [ast::Statement], scope: &mut Scope<'a, '_>, cur_frag: &mut FragId) {
    let block = compile_block(body, scope);
    scope.global.frags[*cur_frag].extend(goto(block.start, scope.frame_call_offset));
    // TODO: if block.start == block.end, then inline the block
    *cur_frag = block.end;
}

#[must_use]
struct CompiledBlock {
    start: FragId,
    end: FragId,
}

fn compile_block<'a>(statements: &'a [ast::Statement], scope: &mut Scope<'a, '_>) -> CompiledBlock {
    let orig_frame_offset = scope.frame_offset;
    let start_frag = scope.new_frag();
    let mut cur_frag = start_frag;
    for statement in statements {
        let prev_frame_offset = scope.frame_offset;
        match *statement {
            ast::Statement::Let {
                mutable,
                ref name,
                ref ty,
                ref value,
            } => {
                let var = Var {
                    frame_offset: scope.frame_offset,
                    mutable,
                    name,
                    ty: match (ty.as_ref().map(Type::from), value) {
                        (None, None) => {
                            panic!("variable `{name}` needs a type annotation or a value")
                        }
                        (Some(ty), Some(value)) => {
                            compile_expr_and_push(value, scope, &mut cur_frag).expect(&ty, name);
                            ty
                        }
                        (Some(ty), None) => {
                            if !mutable {
                                panic!("variable `{name}` must be mutable to be uninitialized")
                            }
                            scope.frame_offset += scope.size_of(&ty);
                            ty
                        }
                        (None, Some(value)) => {
                            compile_expr_and_push(value, scope, &mut cur_frag).ty
                        }
                    },
                };
                scope.vars.push(var);
            }
            ast::Statement::Assign {
                ref place,
                ref value,
                mode,
            } => {
                let src = compile_expr_and_push(value, scope, &mut cur_frag);
                let dst = compile_place(place, true, scope, &mut cur_frag);
                compile_move(
                    dst.value,
                    ir::Value::At(ir::Place::Direct(src.expect(&dst.ty, "assignment"))),
                    compile_store_mode(mode),
                    &dst.ty,
                    scope,
                    cur_frag,
                );
                scope.frame_offset = prev_frame_offset;
            }
            ast::Statement::Loop(ref body) => {
                let after_loop = scope.new_frag();
                let loop_start = scope.global.frags.len();
                scope.loop_stack.push(LoopInfo {
                    start: loop_start,
                    after: after_loop,
                });
                let loop_body = compile_block(body, scope);
                assert!(loop_start == loop_body.start);
                scope.global.frags[cur_frag].extend(goto(loop_start, scope.frame_call_offset));
                scope.global.frags[loop_body.end].extend(goto(loop_start, scope.frame_call_offset));
                scope.loop_stack.pop();
                cur_frag = after_loop;
            }
            ast::Statement::Continue => {
                let &LoopInfo { start, .. } = scope
                    .loop_stack
                    .last()
                    .expect("`continue` may only be used inside a loop");
                scope.global.frags[cur_frag].extend(goto(start, scope.frame_call_offset));
                cur_frag = EXIT_FRAG;
                break;
            }
            ast::Statement::Break => {
                let &LoopInfo { after, .. } = scope
                    .loop_stack
                    .last()
                    .expect("`break` may only be used inside a loop");
                scope.global.frags[cur_frag].extend(goto(after, scope.frame_call_offset));
                cur_frag = EXIT_FRAG;
                break;
            }
            ast::Statement::If {
                ref cond,
                ref true_branch,
                ref false_branch,
            } => {
                match compile_expr(cond, scope, &mut cur_frag).expect(&Type::Bool, "if condition") {
                    ir::Value::At(place) => {
                        let true_block = compile_block(true_branch, scope);
                        let false_block = compile_block(false_branch, scope);

                        scope.global.frags[cur_frag].extend([ir::Instruction::Switch {
                            cond: place,
                            cases: vec![goto(false_block.start, scope.frame_call_offset).to_vec()],
                            default: goto(true_block.start, scope.frame_call_offset).to_vec(),
                        }]);

                        cur_frag = scope.new_frag();
                        for block in [true_block, false_block] {
                            scope.global.frags[block.end]
                                .extend(goto(cur_frag, scope.frame_call_offset));
                        }
                    }
                    ir::Value::Immediate(value) => execute_block(
                        if value == 0 {
                            false_branch
                        } else {
                            true_branch
                        },
                        scope,
                        &mut cur_frag,
                    ),
                };
            }
            ast::Statement::Match {
                scrutinee: ref cond,
                ref arms,
            } => {
                let cond = compile_expr(cond, scope, &mut cur_frag);

                let mut ordered_arms = BTreeMap::new();
                let mut default_body = None;

                for (pat, body) in arms {
                    let body = body.as_slice();
                    let pat = match *pat {
                        ast::Pattern::Int(value) => Typed::new(value, Type::Usize),
                        ast::Pattern::Char(value) => Typed::new(value as usize, Type::Char),
                        ast::Pattern::Bool(value) => Typed::new(value as usize, Type::Bool),
                        ast::Pattern::Wildcard => {
                            default_body = Some(body);
                            break;
                        }
                    };
                    ordered_arms
                        .entry(pat.expect(&cond.ty, "match pattern"))
                        .or_insert(body);
                }

                let default_body = default_body.unwrap_or_default();

                let Some(&last_arm) = ordered_arms.keys().last() else {
                    execute_block(default_body, scope, &mut cur_frag);
                    continue;
                };

                match cond.value {
                    ir::Value::At(place) => {
                        let arm_blocks = (0..=last_arm)
                            .map(|value| {
                                compile_block(
                                    ordered_arms.get(&value).copied().unwrap_or(default_body),
                                    scope,
                                )
                            })
                            .collect::<Vec<_>>();
                        let default_block = compile_block(default_body, scope);
                        scope.global.frags[cur_frag].extend([ir::Instruction::Switch {
                            cond: place,
                            cases: arm_blocks
                                .iter()
                                .map(|block| goto(block.start, scope.frame_call_offset).to_vec())
                                .collect(),
                            default: goto(default_block.start, scope.frame_call_offset).to_vec(),
                        }]);
                        cur_frag = scope.new_frag();
                        for block in arm_blocks.into_iter().chain([default_block]) {
                            scope.global.frags[block.end]
                                .extend(goto(cur_frag, scope.frame_call_offset));
                        }
                    }
                    ir::Value::Immediate(value) => execute_block(
                        ordered_arms.get(&value).copied().unwrap_or(default_body),
                        scope,
                        &mut cur_frag,
                    ),
                };
            }
            ast::Statement::Block(ref body) => execute_block(body, scope, &mut cur_frag),
            ast::Statement::Return(ref value) => {
                let src = compile_expr(value, scope, &mut cur_frag)
                    .expect(&scope.func_def.ret_ty, "return value");
                compile_move(
                    ir::Place::Direct(ir::DirectPlace::StackFrame { offset: 0 }),
                    src,
                    ir::StoreMode::Replace,
                    &scope.func_def.ret_ty,
                    scope,
                    cur_frag,
                );
                scope.emit_return(cur_frag);
                cur_frag = EXIT_FRAG;
                break;
            }
            ast::Statement::Eval(ref expr) => {
                let _ = compile_expr(expr, scope, &mut cur_frag);
                scope.frame_offset = prev_frame_offset;
            }
        }
    }
    scope.shrink_frame(orig_frame_offset);
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
        if MACROS.contains(&name) {
            panic!("macro `{name}` must be called with `!`");
        } else {
            panic!("function `{name}` not defined");
        }
    });

    let block_start = global_state.frags.len();
    global_state.func_ids.insert(name, block_start);

    let mut scope = Scope::new(func_def, global_state);

    let block = compile_block(&func_def.body, &mut scope);
    scope.emit_return(block.end);

    assert!(block_start == block.start);

    block_start
}

static STD: LazyLock<ast::Ast> = LazyLock::new(|| {
    let src = include_str!("std.bs");
    let mut ast = ast::Ast::parse(src).expect("failed to parse standard library");
    for item in &mut ast.items {
        let ast::Item::FuncDef { name, .. } = item else {
            continue;
        };
        let op = match &**name {
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
    Char,
    Bool,
    Str,
    Tuple(Vec<Type<'a>>),
    Array { ty: Box<Type<'a>>, len: usize },
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
            Type::Char => write!(f, "char"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
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
            Type::Array { ref ty, len } => write!(f, "[{}; {}]", ty, len),
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

static PRIMITIVE_TYPES: LazyLock<BTreeMap<&str, Type<'static>>> = LazyLock::new(|| {
    BTreeMap::from([
        ("usize", Type::Usize),
        ("char", Type::Char),
        ("bool", Type::Bool),
        ("str", Type::Str),
    ])
});

impl<'a> From<&'a ast::Type> for Type<'a> {
    fn from(ty: &'a ast::Type) -> Self {
        match *ty {
            ast::Type::Tuple(ref tys) => Type::Tuple(tys.iter().map(Type::from).collect()),
            ast::Type::Array { ref ty, len } => Type::Array {
                ty: Box::new(Type::from(&**ty)),
                len,
            },
            ast::Type::Named(ref name) => match PRIMITIVE_TYPES.get(&**name) {
                Some(ty) => ty.to_owned(),
                None => Type::Named(name),
            },
            ast::Type::Ref { mutable, ref ty } => Type::Ref {
                mutable,
                ty: Box::new(Type::from(&**ty)),
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
    params: Vec<Param<'a>>,
    ret_ty: Type<'a>,
    body: Vec<ast::Statement>,
}

struct Field<'a> {
    name: &'a str,
    ty: Type<'a>,
}

struct StructDef<'a> {
    fields: Vec<Field<'a>>,
}

pub fn compile(ast: &ast::Ast) -> ir::Program {
    let mut func_defs = BTreeMap::<&str, FuncDef>::new();
    let mut struct_defs = BTreeMap::<&str, StructDef>::new();

    for item in STD.items.iter().chain(&ast.items) {
        match item {
            ast::Item::FuncDef {
                name,
                params,
                ret_ty,
                body,
            } => {
                let func_def = FuncDef {
                    params: params
                        .iter()
                        .map(|param| Param {
                            mutable: param.mutable,
                            name: &param.name,
                            ty: Type::from(&param.ty),
                        })
                        .collect(),
                    ret_ty: Type::from(ret_ty),
                    body: body.clone(),
                };
                if func_defs.insert(name, func_def).is_some() {
                    panic!("function `{name}` already defined");
                }
            }
            ast::Item::StructDef { name, fields } => {
                if PRIMITIVE_TYPES.contains_key(&**name) {
                    panic!("`{name}` is a primitive type and cannot be redefined");
                }
                if struct_defs.contains_key(&**name) {
                    panic!("`{name}` is defined multiple times");
                }
                let struct_def = StructDef {
                    fields: fields
                        .iter()
                        .map(|field| Field {
                            name: &field.name,
                            ty: Type::from(&field.ty),
                        })
                        .collect(),
                };
                struct_defs.insert(name, struct_def);
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
            ir::Instruction::SaveFrame { size: 1 },
            ir::Instruction::StoreImm {
                dst: ir::Place::Direct(ir::DirectPlace::StackFrame { offset: 0 }),
                value: main_func_id,
                store_mode: ir::StoreMode::Add,
            },
            ir::Instruction::While {
                cond: ir::Place::Direct(ir::DirectPlace::StackFrame { offset: 0 }),
                body: vec![ir::Instruction::Switch {
                    cond: ir::Place::Direct(ir::DirectPlace::StackFrame { offset: 0 }),
                    cases: global_state.frags,
                    // default should never run unless we implement dynamic dispatch and an invalid function pointer is called
                    // TODO: add some instructions to "panic" and exit in this case
                    default: vec![],
                }],
            },
        ],
    }
}
