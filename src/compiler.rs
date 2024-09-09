use {
    crate::{ast, ir},
    std::{cmp::Ordering, collections::BTreeMap, sync::LazyLock},
};

#[derive(Debug, Clone)]
struct Var<'src> {
    frame_offset: usize,
    mutable: bool,
    name: &'src str,
    ty: Type<'src>,
}

#[derive(Debug, Clone)]
#[must_use]
struct Typed<'src, T> {
    value: T,
    ty: Type<'src>,
}

impl<'src, T> Typed<'src, T> {
    fn new(value: T, ty: Type<'src>) -> Self {
        Self { value, ty }
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> Typed<'src, U> {
        Typed {
            value: f(self.value),
            ty: self.ty,
        }
    }

    fn expect_ty(self, expected_ty: &Type<'src>, context: &str) -> T {
        if self.ty.can_coerce_to(expected_ty) {
            self.value
        } else {
            panic!("{context}: expected `{expected_ty}`, found `{}`", self.ty);
        }
    }

    fn expect_ref(self, expected_ty: &Type<'src>, context: &str) -> T {
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

static MACRO_NAMES: &[&str] = &[
    "read_char",
    "print_char",
    "print_str",
    "print",
    "println",
    "exit",
    "panic",
    "boxed",
    "malloc",
];

fn compile_macro_call<'src>(
    name: &str,
    args: &[ast::Expr<'src>],
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::Value> {
    let orig_frame_offset = scope.frame_offset;
    match name {
        "read_char" => {
            if !args.is_empty() {
                panic!("`{name}` does not take any arguments");
            }
            scope.global.frags[*cur_frag].push(ir::Instruction::Input {
                dst: ir::Place::Direct(ir::DirectPlace::StackFrame {
                    offset: scope.frame_offset,
                }),
            });
            scope.frame_offset += 1;
            Typed::new(stack_value_at(orig_frame_offset), Type::Char)
        }
        "print_char" => {
            if args.is_empty() {
                panic!("`{name}` requires at least 1 argument");
            }
            for arg in args {
                let src = compile_expr(arg, scope, cur_frag).expect_ty(&Type::Char, name);
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
                        scope.global.frags[*cur_frag].extend(unescape(s).map(|byte| {
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

                        scope.global.frags[*cur_frag].push(ir::Instruction::SaveFrame {
                            size: scope.frame_offset - 1,
                        });
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
            let format_str = match first_arg {
                ast::Expr::Str(s) => s,
                _ => panic!("first argument to `{name}` must be a string literal"),
            };
            let mut format_bytes = unescape(format_str);
            while let Some(byte) = format_bytes.next() {
                if byte == b'%' {
                    let Some(format_specifier) = format_bytes.next() else {
                        panic!("unterminated format specifier");
                    };
                    if format_specifier == b'%' {
                        scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                            src: ir::Value::Immediate(b'%' as usize),
                        });
                        continue;
                    }
                    let Some((arg, rest)) = args.split_at_checked(1) else {
                        panic!("not enough arguments for format string `{format_str}`")
                    };
                    args = rest;
                    match format_specifier {
                        b'i' | b'd' => compile_func_call("print_int", arg, scope, cur_frag),
                        b'c' => compile_macro_call("print_char", arg, scope, cur_frag),
                        b's' => compile_macro_call("print_str", arg, scope, cur_frag),
                        _ => panic!("invalid format specifier"),
                    }
                    .expect_ty(&Type::unit(), name);
                } else {
                    scope.global.frags[*cur_frag].push(ir::Instruction::Output {
                        src: ir::Value::Immediate(byte as usize),
                    });
                }
            }
            scope.frame_offset = orig_frame_offset;
            if !args.is_empty() {
                panic!("too many arguments for format string `{format_str}`");
            }
            Typed::new(stack_value_at(orig_frame_offset), Type::unit())
        }
        "println" => {
            if !args.is_empty() {
                compile_macro_call("print", args, scope, cur_frag).expect_ty(&Type::unit(), name);
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
        "panic" => {
            scope.global.frags[*cur_frag].extend(b"panicked: ".map(|byte| {
                ir::Instruction::Output {
                    src: ir::Value::Immediate(byte as usize),
                }
            }));
            if args.is_empty() {
                scope.global.frags[*cur_frag].extend(b"explicit panic\n".map(|byte| {
                    ir::Instruction::Output {
                        src: ir::Value::Immediate(byte as usize),
                    }
                }));
            } else {
                compile_macro_call("println", args, scope, cur_frag).expect_ty(&Type::unit(), name);
            }
            compile_macro_call("exit", &[], scope, cur_frag)
        }
        "boxed" | "malloc" => {
            let [arg] = args else {
                panic!("`{name}` requires exactly 1 argument");
            };

            // store the address on the stack
            scope.frame_offset += 1;
            compile_single_move(
                ir::Place::Direct(ir::DirectPlace::StackFrame {
                    offset: orig_frame_offset,
                }),
                ir::Value::At(ir::Place::Direct(ir::DirectPlace::Address(0))),
                ir::StoreMode::Replace,
                1,
                &mut scope.global.frags[*cur_frag],
            );

            let arg = compile_expr(arg, scope, cur_frag);
            let (offset, ty) = match name {
                "boxed" => {
                    // store the value on the heap
                    let size = scope.size_of(&arg.ty);
                    compile_move(
                        ir::Place::Indirect(ir::IndirectPlace::Deref {
                            address: ir::DirectPlace::Address(0),
                        }),
                        arg.value,
                        ir::StoreMode::Add,
                        size,
                        scope,
                        *cur_frag,
                    );
                    (ir::Value::Immediate(size), arg.ty)
                }
                "malloc" => (
                    arg.expect_ty(&Type::Usize, name),
                    Type::Array {
                        len: None,
                        ty: Box::new(Type::Usize),
                    },
                ),
                _ => unreachable!("the only heap allocation macros are `boxed` and `malloc`"),
            };

            // increment the next free address
            compile_single_move(
                ir::Place::Direct(ir::DirectPlace::Address(0)),
                offset,
                ir::StoreMode::Add,
                2,
                &mut scope.global.frags[*cur_frag],
            );

            scope.frame_offset = orig_frame_offset + 1;
            Typed::new(
                stack_value_at(orig_frame_offset),
                Type::Ref {
                    mutable: true,
                    ty: Box::new(ty),
                },
            )
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
            let [if_same, if_diff] = match name {
                "==" => [1, 0],
                "!=" => [0, 1],
                _ => unreachable!("the only comparison operators are `==` and `!=`"),
            };
            let size = scope.size_of(&lhs.ty);
            match size {
                0 => return Typed::new(ir::Value::Immediate(if_same), Type::Bool),
                1 => {}
                2.. => todo!("comparison of large types"),
            }
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
            scope.global.frags[*cur_frag].push(ir::Instruction::Switch {
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
            });
            scope.frame_offset = orig_frame_offset + size;
            Typed::new(stack_value_at(orig_frame_offset), Type::Bool)
        }
        "&&" | "||" => {
            let [lhs, rhs] = args else {
                panic!("`{name}` requires exactly 2 arguments");
            };

            let short_circuit = scope.new_frag();
            let long_circuit = scope.new_frag();

            let lhs = compile_expr_and_push(lhs, scope, cur_frag).expect_ty(&Type::Bool, name);

            let [false_circuit, true_circuit] = match name {
                "&&" => [short_circuit, long_circuit],
                "||" => [long_circuit, short_circuit],
                _ => unreachable!("the only short-circuiting operators are `&&` and `||`"),
            };

            scope.global.frags[*cur_frag].push(ir::Instruction::Switch {
                cond: ir::Place::Direct(lhs),
                cases: vec![goto(false_circuit, scope.frame_call_offset).to_vec()],
                default: goto(true_circuit, scope.frame_call_offset).to_vec(),
            });

            *cur_frag = long_circuit;
            scope.frame_offset -= 1;
            compile_expr_and_push(rhs, scope, cur_frag).expect_ty(&Type::Bool, name);
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

fn compile_func_call<'src>(
    name: &'src str,
    args: &[ast::Expr<'src>],
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::Value> {
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
        compile_expr_and_push(arg, scope, cur_frag).expect_ty(&param.ty, name);
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

fn compile_call<'src>(
    call_expr: &ast::CallExpr<'src>,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::Value> {
    if call_expr.bang {
        compile_macro_call(call_expr.func, &call_expr.args, scope, cur_frag)
    } else {
        compile_func_call(call_expr.func, &call_expr.args, scope, cur_frag)
    }
}

fn compile_struct_expr<'src>(
    struct_expr: &ast::StructExpr<'src>,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::Value> {
    let struct_fields = scope.global.iter_fields_of_struct(struct_expr.name);

    if struct_expr.fields.len() != struct_fields.len() {
        panic!(
            "struct `{}` has {} fields, but {} were provided",
            struct_expr.name,
            struct_fields.len(),
            struct_expr.fields.len()
        );
    }

    struct FieldInfo<'src> {
        ty: &'src Type<'src>,
        frame_offset: usize,
    }

    let struct_frame_offset = scope.frame_offset;

    let mut expected_fields: BTreeMap<&str, Option<FieldInfo>> = struct_fields
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
        let Some(expected_field) = expected_fields.get_mut(field.name).map(Option::take) else {
            panic!(
                "field `{}` not found in struct `{}`",
                field.name, struct_expr.name
            );
        };
        let Some(field_info) = expected_field else {
            panic!("field `{}` specified more than once", field.name);
        };
        match scope.frame_offset.cmp(&field_info.frame_offset) {
            Ordering::Less | Ordering::Equal => {
                scope.frame_offset = field_info.frame_offset;
                compile_expr_and_push(&field.value, scope, cur_frag)
                    .expect_ty(field_info.ty, field.name);
            }
            Ordering::Greater => {
                let prev_frame_offset = scope.frame_offset;
                compile_expr_and_push(&field.value, scope, cur_frag)
                    .expect_ty(field_info.ty, field.name);
                compile_move(
                    ir::Place::Direct(ir::DirectPlace::StackFrame {
                        offset: field_info.frame_offset,
                    }),
                    stack_value_at(prev_frame_offset),
                    ir::StoreMode::Replace,
                    scope.size_of(field_info.ty),
                    scope,
                    *cur_frag,
                );
                scope.frame_offset = prev_frame_offset;
            }
        }
    }

    Typed::new(
        stack_value_at(struct_frame_offset),
        Type::Named(struct_expr.name),
    )
}

fn compile_tuple_expr<'src>(
    elements: &[ast::Expr<'src>],
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::Value> {
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

fn compile_array_expr<'src>(
    array_expr: &ast::ArrayExpr<'src>,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::Value> {
    Typed::new(
        stack_value_at(scope.frame_offset),
        match *array_expr {
            ast::ArrayExpr::List(ref elements) => Type::Array {
                len: Some(elements.len()),
                ty: Box::new(match elements.split_first() {
                    Some((first, rest)) => {
                        let first_ty = compile_expr_and_push(first, scope, cur_frag).ty;
                        for element in rest {
                            compile_expr_and_push(element, scope, cur_frag)
                                .expect_ty(&first_ty, "array element");
                        }
                        first_ty
                    }
                    None => Type::Usize,
                }),
            },
            ast::ArrayExpr::Repeat { ref value, len } => {
                let first_elem = compile_expr_and_push(value, scope, cur_frag);
                let elem_size = scope.size_of(&first_elem.ty);
                match len {
                    0 => scope.frame_offset -= elem_size,
                    _ => {
                        for _ in 1..len {
                            compile_move(
                                ir::Place::Direct(ir::DirectPlace::StackFrame {
                                    offset: scope.frame_offset,
                                }),
                                ir::Value::At(ir::Place::Direct(first_elem.value)),
                                ir::StoreMode::Replace,
                                elem_size,
                                scope,
                                *cur_frag,
                            );
                            scope.frame_offset += elem_size;
                        }
                    }
                }
                Type::Array {
                    len: Some(len),
                    ty: Box::new(first_elem.ty),
                }
            }
        },
    )
}

#[derive(Debug, Clone, Copy)]
enum Mutability {
    Immutable,
    MutableThroughRef,
    Mutable,
}

impl Mutability {
    fn of_var(mutable: bool) -> Self {
        if mutable {
            Self::Mutable
        } else {
            Self::MutableThroughRef
        }
    }

    fn through_ref(self, mutable: bool) -> Self {
        if mutable && matches!(self, Self::Mutable | Self::MutableThroughRef) {
            Self::Mutable
        } else {
            Self::Immutable
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PlaceMut {
    place: ir::Place,
    mutability: Mutability,
}

fn compile_var_access<'src>(name: &'src str, scope: &Scope<'src, '_>) -> Typed<'src, PlaceMut> {
    let Some(var) = scope.vars.iter().rev().find(|var| var.name == name) else {
        panic!("variable `{name}` not found");
    };
    Typed::new(
        PlaceMut {
            place: ir::Place::Direct(ir::DirectPlace::StackFrame {
                offset: var.frame_offset,
            }),
            mutability: Mutability::of_var(var.mutable),
        },
        var.ty.clone(),
    )
}

fn compile_field_access<'src>(
    base: &ast::Place<'src>,
    field_name: &'src str,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, PlaceMut> {
    let mut base = compile_place(base, scope, cur_frag);
    base = compile_deref(base, scope, *cur_frag, true);
    let found_field = match &base.ty {
        Type::Tuple(tys) => field_name.parse().ok().and_then(|index| {
            tys.get(index).map(|ty| {
                Typed::new(
                    tys[..index].iter().map(|ty| scope.size_of(ty)).sum(),
                    ty.clone(),
                )
            })
        }),
        Type::Named(ty_name) => scope
            .global
            .iter_fields_of_struct(ty_name)
            .scan(0, |offset, field| {
                let field_offset = *offset;
                *offset += scope.size_of(&field.ty);
                Some((field, field_offset))
            })
            .find_map(|(field, offset)| {
                (field.name == field_name).then(|| Typed::new(offset, field.ty.clone()))
            }),
        _ => None,
    };
    let Some(Typed {
        value: field_offset,
        ty: field_ty,
    }) = found_field
    else {
        panic!("no field `{}` on type `{}`", field_name, base.ty);
    };
    if field_offset > 0 {
        let field_size = scope.size_of(&field_ty);
        own_place(
            &mut base.value.place,
            &mut scope.global.frags[*cur_frag],
            scope.frame_offset + field_size,
        );
        increment_place(
            &mut base.value.place,
            &mut scope.global.frags[*cur_frag],
            field_offset,
        );
    }
    Typed::new(base.value, field_ty)
}

fn compile_deref<'src>(
    place: Typed<'src, PlaceMut>,
    scope: &mut Scope<'src, '_>,
    cur_frag: FragId,
    indexing: bool,
) -> Typed<'src, PlaceMut> {
    let Typed {
        ty: Type::Ref { mutable, ty },
        value: PlaceMut { place, mutability },
    } = place
    else {
        if indexing {
            return place;
        } else {
            panic!("cannot dereference value of type `{}`", place.ty);
        }
    };
    let deref = Typed::new(
        PlaceMut {
            place: match place {
                ir::Place::Direct(address) => {
                    ir::Place::Indirect(ir::IndirectPlace::Deref { address })
                }
                ir::Place::Indirect(_) => {
                    let offset = scope.frame_offset
                        + if indexing {
                            scope.indexed_size_of(&ty)
                        } else {
                            scope.size_of(&ty)
                        };
                    compile_single_move(
                        ir::Place::Direct(ir::DirectPlace::StackFrame { offset }),
                        ir::Value::At(place),
                        ir::StoreMode::Replace,
                        1,
                        &mut scope.global.frags[cur_frag],
                    );
                    ir::Place::Indirect(ir::IndirectPlace::Deref {
                        address: ir::DirectPlace::StackFrame { offset },
                    })
                }
            },
            mutability: mutability.through_ref(mutable),
        },
        *ty,
    );
    if indexing {
        compile_deref(deref, scope, cur_frag, indexing)
    } else {
        deref
    }
}

fn compile_index<'src>(
    base: &ast::Place<'src>,
    index: &ast::Expr<'src>,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, PlaceMut> {
    let base = compile_place(base, scope, cur_frag);
    let base = compile_deref(base, scope, *cur_frag, true);
    let Type::Array { ty: elem_ty, .. } = base.ty else {
        panic!("cannot index into value of type `{}`", base.ty);
    };
    let elem_size = scope.size_of(&elem_ty);
    scope.frame_offset += elem_size;
    let index = compile_expr(index, scope, cur_frag).expect_ty(&Type::Usize, "array index");
    let address = ir::DirectPlace::StackFrame {
        offset: scope.frame_offset,
    };
    scope.global.frags[*cur_frag].extend([
        ir::Instruction::LoadRef {
            src: base.value.place,
        },
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
        PlaceMut {
            place: ir::Place::Indirect(ir::IndirectPlace::Deref { address }),
            mutability: base.value.mutability,
        },
        *elem_ty,
    )
}

fn compile_place<'src>(
    place: &ast::Place<'src>,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, PlaceMut> {
    let orig_frame_offset = scope.frame_offset;
    let result = match place {
        ast::Place::Var(ident) => compile_var_access(ident, scope),
        ast::Place::FieldAccess { base, field } => {
            compile_field_access(base, field, scope, cur_frag)
        }
        ast::Place::Index { base, index } => compile_index(base, index, scope, cur_frag),
        ast::Place::Deref(expr) => compile_deref(
            compile_expr(expr, scope, cur_frag).map(|value| PlaceMut {
                place: match value {
                    ir::Value::At(place) => place,
                    ir::Value::Immediate(address) => {
                        ir::Place::Direct(ir::DirectPlace::Address(address))
                    }
                },
                mutability: Mutability::MutableThroughRef,
            }),
            scope,
            *cur_frag,
            false,
        ),
    };
    scope.frame_offset = orig_frame_offset;
    result
}

fn compile_ref<'src>(
    mutable: bool,
    place: &ast::Place<'src>,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::Value> {
    let place = compile_place(place, scope, cur_frag);
    if mutable && !matches!(place.value.mutability, Mutability::Mutable) {
        panic!(
            "cannot take mutable reference to immutable place of type `{}`",
            place.ty
        );
    }
    scope.global.frags[*cur_frag].extend([
        ir::Instruction::LoadRef {
            src: place.value.place,
        },
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

fn unescape(escaped_str: &str) -> impl Iterator<Item = u8> + '_ {
    let mut bytes = escaped_str.bytes();
    std::iter::from_fn(move || {
        let byte = bytes.next()?;
        match byte {
            b'\\' => match bytes.next().expect("unterminated character escape") {
                b'n' => Some(b'\n'),
                b'r' => Some(b'\r'),
                b't' => Some(b'\t'),
                b'\\' => Some(b'\\'),
                b'"' => Some(b'"'),
                b'\'' => Some(b'\''),
                _ => panic!("unknown character escape"),
            },
            _ => Some(byte),
        }
    })
}

fn compile_str<'src>(s: &'src str, scope: &mut Scope<'src, '_>) -> Typed<'src, ir::Value> {
    Typed::new(
        ir::Value::Immediate(*scope.global.str_ids.entry(s).or_insert_with(|| {
            scope.global.frags.push(
                [ir::Instruction::RestoreFrame { size: 1 }]
                    .into_iter()
                    .chain(unescape(s).map(|byte| ir::Instruction::Output {
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

fn compile_expr<'src>(
    expr: &ast::Expr<'src>,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::Value> {
    match *expr {
        ast::Expr::Int(i) => Typed::new(ir::Value::Immediate(i), Type::Usize),
        ast::Expr::Char(c) => Typed::new(ir::Value::Immediate(c as usize), Type::Char),
        ast::Expr::Bool(b) => Typed::new(ir::Value::Immediate(b as usize), Type::Bool),
        ast::Expr::Str(s) => compile_str(s, scope),
        ast::Expr::Place(ref place) => compile_place(place, scope, cur_frag)
            .map(|place| place.place)
            .map(ir::Value::At),
        ast::Expr::Ref { mutable, ref place } => compile_ref(mutable, place, scope, cur_frag),
        ast::Expr::Call(ref call_expr) => compile_call(call_expr, scope, cur_frag),
        ast::Expr::Struct(ref struct_expr) => compile_struct_expr(struct_expr, scope, cur_frag),
        ast::Expr::Tuple(ref elements) => compile_tuple_expr(elements, scope, cur_frag),
        ast::Expr::Array(ref array_expr) => compile_array_expr(array_expr, scope, cur_frag),
        ast::Expr::Cast { ref expr, ref ty } => {
            let src = compile_expr(expr, scope, cur_frag);
            let src_size = scope.size_of(&src.ty);
            let dst_ty = Type::from(ty);
            let dst_size = scope.size_of(&dst_ty);
            if src_size != dst_size {
                panic!(
                    "cannot cast `{}` to `{}`: size mismatch ({} != {})",
                    src.ty, dst_ty, src_size, dst_size
                );
            }
            Typed::new(src.value, dst_ty)
        }
    }
}

fn compile_expr_and_push<'src>(
    expr: &ast::Expr<'src>,
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) -> Typed<'src, ir::DirectPlace> {
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
            scope.size_of(&result.ty),
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
            cur_frag.push(ir::Instruction::StoreImm {
                dst,
                value: value * multiplier,
                store_mode,
            });
        }
        ir::Value::At(src) => {
            cur_frag.extend([
                ir::Instruction::Load { src, multiplier },
                ir::Instruction::Store { dst, store_mode },
            ]);
        }
    }
}

fn compile_move(
    mut dst: ir::Place,
    src: ir::Value,
    store_mode: ir::StoreMode,
    size: usize,
    scope: &mut Scope,
    cur_frag: FragId,
) {
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

struct GlobalState<'src> {
    func_defs: &'src BTreeMap<&'src str, FuncDef<'src>>,
    struct_defs: &'src BTreeMap<&'src str, StructDef<'src>>,
    frags: Vec<Vec<ir::Instruction>>,
    func_ids: BTreeMap<&'src str, FragId>,
    str_ids: BTreeMap<&'src str, FragId>,
}

// frag 0 is the exit psuedo-fragment (it never actually gets executed due to the while loop)
const EXIT_FRAG: FragId = 0;

impl<'src> GlobalState<'src> {
    fn new(
        func_defs: &'src BTreeMap<&'src str, FuncDef<'src>>,
        struct_defs: &'src BTreeMap<&'src str, StructDef<'src>>,
    ) -> Self {
        Self {
            func_defs,
            struct_defs,
            frags: vec![vec![]],
            func_ids: BTreeMap::new(),
            str_ids: BTreeMap::new(),
        }
    }

    fn add_frag(&mut self, instructions: Vec<ir::Instruction>) -> FragId {
        let frag_id = self.frags.len();
        self.frags.push(instructions);
        frag_id
    }

    fn iter_fields_of_struct(
        &self,
        name: &str,
    ) -> impl ExactSizeIterator<Item = &'src Field<'src>> {
        self.struct_defs
            .get(name)
            .unwrap_or_else(|| panic!("struct `{name}` not found"))
            .fields
            .iter()
    }

    fn indexed_size_of(&self, ty: &Type) -> usize {
        match ty {
            Type::Array { ty, .. } => self.size_of(ty),
            Type::Named(name) => self
                .iter_fields_of_struct(name)
                .map(|field| self.size_of(&field.ty))
                .max()
                .unwrap_or_default(),
            _ => self.size_of(ty),
        }
    }

    fn size_of(&self, ty: &Type) -> usize {
        match ty {
            Type::Usize | Type::Char | Type::Bool | Type::Ref { .. } => 1,
            Type::Tuple(tys) => tys.iter().map(|ty| self.size_of(ty)).sum(),
            Type::Array { ty, len: Some(len) } => len * self.size_of(ty),
            Type::Array { ty, len: None } if self.size_of(ty) == 0 => 0,
            Type::Named(name) => self
                .iter_fields_of_struct(name)
                .map(|field| self.size_of(&field.ty))
                .sum(),
            Type::Str | Type::Array { len: None, .. } => {
                panic!("cannot get size of unsized type `{ty}`")
            }
        }
    }
}

struct Scope<'src, 'b> {
    func_def: &'src FuncDef<'src>,
    vars: Vec<Var<'src>>,
    frame_offset: usize,
    frame_call_offset: usize,
    global: &'b mut GlobalState<'src>,
    loop_stack: Vec<FragPair>,
}

impl<'src, 'b> Scope<'src, 'b> {
    fn new(func_def: &'src FuncDef<'src>, global: &'b mut GlobalState<'src>) -> Self {
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
            Ordering::Less => {
                self.vars.truncate(
                    self.vars
                        .iter()
                        .rev()
                        .position(|var| var.frame_offset < target_frame_offset)
                        .map(|pos| self.vars.len() - pos)
                        .unwrap_or_default(),
                );
                self.frame_offset = target_frame_offset;
            }
            Ordering::Equal => {}
            Ordering::Greater => unreachable!("cannot shrink frame to a larger size"),
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

    fn indexed_size_of(&self, ty: &Type) -> usize {
        self.global.indexed_size_of(ty)
    }

    fn size_of(&self, ty: &Type) -> usize {
        self.global.size_of(ty)
    }
}

fn execute_block<'src>(
    body: &[ast::Statement<'src>],
    scope: &mut Scope<'src, '_>,
    cur_frag: &mut FragId,
) {
    let block = compile_block(body, scope);
    scope.global.frags[*cur_frag].extend(goto(block.start, scope.frame_call_offset));
    *cur_frag = block.end;
}

#[must_use]
struct FragPair {
    start: FragId,
    end: FragId,
}

fn compile_block<'src>(
    statements: &[ast::Statement<'src>],
    scope: &mut Scope<'src, '_>,
) -> FragPair {
    let orig_frame_offset = scope.frame_offset;
    let start_frag = scope.new_frag();
    let mut cur_frag = start_frag;
    for statement in statements {
        let prev_frame_offset = scope.frame_offset;
        match *statement {
            ast::Statement::Let {
                mutable,
                name,
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
                            compile_expr_and_push(value, scope, &mut cur_frag).expect_ty(&ty, name);
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
                let typed_src = compile_expr_and_push(value, scope, &mut cur_frag);
                let typed_dst = compile_place(place, scope, &mut cur_frag);
                let src = typed_src.expect_ty(&typed_dst.ty, "assignment");
                match typed_dst.value.mutability {
                    Mutability::Immutable => panic!(
                        "cannot assign value of type `{}` through immutable reference",
                        typed_dst.ty
                    ),
                    Mutability::MutableThroughRef => panic!(
                        "cannot assign value of type `{}` to immutable place",
                        typed_dst.ty
                    ),
                    Mutability::Mutable => {}
                }
                compile_move(
                    typed_dst.value.place,
                    ir::Value::At(ir::Place::Direct(src)),
                    compile_store_mode(mode),
                    scope.size_of(&typed_dst.ty),
                    scope,
                    cur_frag,
                );
                scope.frame_offset = prev_frame_offset;
            }
            ast::Statement::Loop(ref body) => {
                let after_loop = scope.new_frag();
                let loop_start = scope.global.frags.len();
                scope.loop_stack.push(FragPair {
                    start: loop_start,
                    end: after_loop,
                });
                let loop_body = compile_block(body, scope);
                assert!(loop_start == loop_body.start);
                scope.global.frags[cur_frag].extend(goto(loop_start, scope.frame_call_offset));
                scope.global.frags[loop_body.end].extend(goto(loop_start, scope.frame_call_offset));
                scope.loop_stack.pop();
                cur_frag = after_loop;
            }
            ast::Statement::Continue => {
                let Some(&FragPair { start, .. }) = scope.loop_stack.last() else {
                    panic!("`continue` may only be used inside a loop");
                };
                scope.global.frags[cur_frag].extend(goto(start, scope.frame_call_offset));
                cur_frag = EXIT_FRAG;
                break;
            }
            ast::Statement::Break => {
                let Some(&FragPair { end, .. }) = scope.loop_stack.last() else {
                    panic!("`break` may only be used inside a loop");
                };
                scope.global.frags[cur_frag].extend(goto(end, scope.frame_call_offset));
                cur_frag = EXIT_FRAG;
                break;
            }
            ast::Statement::If {
                ref cond,
                ref true_branch,
                ref false_branch,
            } => {
                match compile_expr(cond, scope, &mut cur_frag)
                    .expect_ty(&Type::Bool, "if condition")
                {
                    ir::Value::At(place) => {
                        let true_block = compile_block(true_branch, scope);
                        let false_block = compile_block(false_branch, scope);

                        scope.global.frags[cur_frag].push(ir::Instruction::Switch {
                            cond: place,
                            cases: vec![goto(false_block.start, scope.frame_call_offset).to_vec()],
                            default: goto(true_block.start, scope.frame_call_offset).to_vec(),
                        });

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
                        .entry(pat.expect_ty(&cond.ty, "match pattern"))
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
                        scope.global.frags[cur_frag].push(ir::Instruction::Switch {
                            cond: place,
                            cases: arm_blocks
                                .iter()
                                .map(|block| goto(block.start, scope.frame_call_offset).to_vec())
                                .collect(),
                            default: goto(default_block.start, scope.frame_call_offset).to_vec(),
                        });
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
                    .expect_ty(&scope.func_def.ret_ty, "return value");
                compile_move(
                    ir::Place::Direct(ir::DirectPlace::StackFrame { offset: 0 }),
                    src,
                    ir::StoreMode::Replace,
                    scope.size_of(&scope.func_def.ret_ty),
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
    FragPair {
        start: start_frag,
        end: cur_frag,
    }
}

fn compile_func<'src>(name: &'src str, global_state: &mut GlobalState<'src>) -> FragId {
    if let Some(&func_id) = global_state.func_ids.get(name) {
        // function already compiled
        return func_id;
    }

    let Some(func_def) = global_state.func_defs.get(name) else {
        if MACRO_NAMES.contains(&name) {
            panic!("macro `{name}` must be called with `!`");
        } else {
            panic!("function `{name}` not defined");
        }
    };

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
    let mut ast = ast::Ast::try_from(src).expect("failed to parse standard library");
    for item in &mut ast.items {
        let ast::Item::FuncDef { name, .. } = item else {
            continue;
        };
        *name = match *name {
            "add" => "+",
            "sub" => "-",
            "mul" => "*",
            "div" => "/",
            "rem" => "%",
            "lt" => "<",
            "le" => "<=",
            "gt" => ">",
            "ge" => ">=",
            "not" => "!",
            _ => continue,
        };
    }
    ast
});

#[derive(Debug, Clone, PartialEq, Eq)]
#[must_use]
enum Type<'src> {
    Usize,
    Char,
    Bool,
    Str,
    Tuple(Vec<Type<'src>>),
    Array {
        ty: Box<Type<'src>>,
        len: Option<usize>,
    },
    Named(&'src str),
    Ref {
        mutable: bool,
        ty: Box<Type<'src>>,
    },
}

impl Type<'_> {
    const fn unit() -> Self {
        Type::Tuple(vec![])
    }

    fn can_coerce_to(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Type::Ref {
                    mutable: mutable1,
                    ty: self_ty,
                },
                Type::Ref {
                    mutable: mutable2,
                    ty: ty2,
                },
            ) => self_ty == ty2 && mutable1 >= mutable2,
            _ => self == other,
        }
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
            Type::Array { ref ty, len } => match len {
                Some(len) => write!(f, "[{}; {}]", ty, len),
                None => write!(f, "[{}]", ty),
            },
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

impl<'src> From<&ast::Type<'src>> for Type<'src> {
    fn from(ty: &ast::Type<'src>) -> Self {
        match *ty {
            ast::Type::Tuple(ref tys) => Type::Tuple(tys.iter().map(Type::from).collect()),
            ast::Type::Array { ref ty, len } => Type::Array {
                ty: Box::new(Type::from(&**ty)),
                len,
            },
            ast::Type::Named(name) => match PRIMITIVE_TYPES.get(name) {
                Some(ty) => ty.clone(),
                None => Type::Named(name),
            },
            ast::Type::Ref { mutable, ref ty } => Type::Ref {
                mutable,
                ty: Box::new(Type::from(&**ty)),
            },
        }
    }
}

struct Param<'src> {
    mutable: bool,
    name: &'src str,
    ty: Type<'src>,
}

struct FuncDef<'src> {
    params: Vec<Param<'src>>,
    ret_ty: Type<'src>,
    body: Vec<ast::Statement<'src>>,
}

struct Field<'src> {
    name: &'src str,
    ty: Type<'src>,
}

struct StructDef<'src> {
    fields: Vec<Field<'src>>,
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
                            name: param.name,
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
                if PRIMITIVE_TYPES.contains_key(name) {
                    panic!("`{name}` is a primitive type and cannot be redefined");
                }
                if struct_defs.contains_key(name) {
                    panic!("`{name}` is defined multiple times");
                }
                let struct_def = StructDef {
                    fields: fields
                        .iter()
                        .map(|field| Field {
                            name: field.name,
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

    global_state.frags[EXIT_FRAG].clear(); // will never be executed

    ir::Program {
        instructions: vec![
            ir::Instruction::StoreImm {
                dst: ir::Place::Direct(ir::DirectPlace::Address(0)),
                value: 2, // next free address (for boxed values)
                store_mode: ir::StoreMode::Add,
            },
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
                    default: vec![], // should never be executed
                }],
            },
        ],
    }
}
