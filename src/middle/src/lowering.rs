use {
    super::ir,
    frontend::ast,
    std::{cmp::Ordering, collections::BTreeMap, sync::LazyLock},
};

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
                    mutable: self_mutable,
                    ty: self_ty,
                },
                Type::Ref {
                    mutable: other_mutable,
                    ty: other_ty,
                },
            ) => self_ty == other_ty && self_mutable >= other_mutable,
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
                    write!(f, "{first}")?;
                    if rest.is_empty() {
                        write!(f, ",")?;
                    } else {
                        for ty in rest {
                            write!(f, ", {ty}")?;
                        }
                    }
                }
                write!(f, ")")
            }
            Type::Array { ref ty, len } => match len {
                Some(len) => write!(f, "[{ty}; {len}]"),
                None => write!(f, "[{ty}]"),
            },
            Type::Named(name) => write!(f, "{name}"),
            Type::Ref { mutable, ref ty } => {
                write!(f, "&")?;
                if mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{ty}")
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

// frag 0 is is never actually executed and is only used to exit the program
const EXIT_FRAG: FragId = 0;

#[must_use]
struct FragPair {
    start: FragId,
    end: FragId,
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
    "size_of_val",
];

struct GlobalContext<'src> {
    func_defs: &'src BTreeMap<&'src str, FuncDef<'src>>,
    struct_defs: &'src BTreeMap<&'src str, StructDef<'src>>,
    frags: Vec<Vec<ir::Instruction>>,
    func_ids: BTreeMap<&'src str, FragId>,
    str_ids: BTreeMap<&'src str, FragId>,
}

impl<'src> GlobalContext<'src> {
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

    fn compile_func(&mut self, name: &'src str) -> FragId {
        if let Some(&func_id) = self.func_ids.get(name) {
            // function already compiled
            return func_id;
        }

        let Some(func_def) = self.func_defs.get(name) else {
            if MACRO_NAMES.contains(&name) {
                panic!("macro `{name}` must be called with `!`");
            } else {
                panic!("function `{name}` not defined");
            }
        };

        let block_start = self.frags.len();
        self.func_ids.insert(name, block_start);

        let mut context = FuncContext::new(func_def, self);

        context.compile_block(&func_def.body);
        context.emit_return();

        block_start
    }
}

fn unescape_char(chars: &mut impl Iterator<Item = char>) -> char {
    match chars.next().expect("unterminated character escape") {
        '"' => '"',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '\\' => '\\',
        '\'' => '\'',
        '0' => '\0',
        c => panic!("unknown character escape: `{c}`"),
    }
}

fn unescape_str(s: &str) -> impl Iterator<Item = u8> + '_ {
    let mut chars = s.chars();
    std::iter::from_fn(move || {
        Some(match chars.next()? {
            '\\' => unescape_char(&mut chars),
            c => c,
        })
    })
    .flat_map(|c| {
        let mut buf = [0; 4];
        c.encode_utf8(&mut buf);
        buf.into_iter().take_while(|&b| b != 0)
    })
}

fn goto(frag_id: FragId, frame_base_offset: usize) -> [ir::Instruction; 2] {
    [
        ir::Instruction::SaveFrame {
            size: frame_base_offset,
        },
        ir::Instruction::StoreImm {
            dst: ir::Place::Direct(ir::DirectPlace::StackFrame { offset: 0 }),
            value: frag_id,
            store_mode: ir::StoreMode::Replace,
        },
    ]
}

fn stack_value_at(offset: usize) -> ir::Value {
    ir::Value::At(ir::Place::Direct(ir::DirectPlace::StackFrame { offset }))
}

struct FuncContext<'a, 'src> {
    func_def: &'src FuncDef<'src>,
    vars: Vec<Var<'src>>,
    frame_offset: usize,
    frame_call_offset: usize,
    cur_frag: FragId,
    loop_stack: Vec<FragPair>,
    global: &'a mut GlobalContext<'src>,
}

impl<'a, 'src> FuncContext<'a, 'src> {
    fn new(func_def: &'src FuncDef<'src>, global: &'a mut GlobalContext<'src>) -> Self {
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
            cur_frag: global.add_frag(vec![ir::Instruction::RestoreFrame {
                size: frame_call_offset,
            }]),
            loop_stack: vec![],
            global,
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

    fn emit_return(&mut self) {
        self.push_frag(
            self.cur_frag,
            ir::Instruction::SaveFrame {
                size: self.frame_call_offset - 1,
            },
        );
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

    fn extend_frag(
        &mut self,
        frag_id: FragId,
        instructions: impl IntoIterator<Item = ir::Instruction>,
    ) {
        self.global.frags[frag_id].extend(instructions);
    }

    fn push_frag(&mut self, frag_id: FragId, instruction: ir::Instruction) {
        self.global.frags[frag_id].push(instruction)
    }

    fn extend_cur_frag(&mut self, instructions: impl IntoIterator<Item = ir::Instruction>) {
        self.extend_frag(self.cur_frag, instructions)
    }

    fn push_cur_frag(&mut self, instruction: ir::Instruction) {
        self.push_frag(self.cur_frag, instruction)
    }

    fn compile_single_move(
        &mut self,
        dst: ir::Place,
        src: ir::Value,
        store_mode: ir::StoreMode,
        multiplier: usize,
    ) {
        match src {
            ir::Value::Immediate(value) => {
                self.push_cur_frag(ir::Instruction::StoreImm {
                    dst,
                    value: value * multiplier,
                    store_mode,
                });
            }
            ir::Value::At(src) => {
                self.extend_cur_frag([
                    ir::Instruction::Load { src, multiplier },
                    ir::Instruction::Store { dst, store_mode },
                ]);
            }
        }
    }

    fn set_imm(&mut self, frame_offset: usize, value: usize) {
        self.push_cur_frag(ir::Instruction::StoreImm {
            dst: ir::Place::Direct(ir::DirectPlace::StackFrame {
                offset: frame_offset,
            }),
            value,
            store_mode: ir::StoreMode::Replace,
        });
    }

    fn compile_macro_call(
        &mut self,
        name: &str,
        args: &[ast::Expr<'src>],
    ) -> Typed<'src, ir::Value> {
        let orig_frame_offset = self.frame_offset;
        match name {
            "read_char" => {
                if !args.is_empty() {
                    panic!("`{name}` does not take any arguments");
                }
                self.push_cur_frag(ir::Instruction::Input {
                    dst: ir::Place::Direct(ir::DirectPlace::StackFrame {
                        offset: self.frame_offset,
                    }),
                });
                self.frame_offset += 1;
                Typed::new(stack_value_at(orig_frame_offset), Type::Char)
            }
            "print_char" => {
                if args.is_empty() {
                    panic!("`{name}` requires at least 1 argument");
                }
                for arg in args {
                    let src = self.compile_expr(arg).expect_ty(&Type::Char, name);
                    self.push_cur_frag(ir::Instruction::Output { src });
                    self.frame_offset = orig_frame_offset;
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
                            self.extend_cur_frag(unescape_str(s).map(|byte| {
                                ir::Instruction::Output {
                                    src: ir::Value::Immediate(byte as usize),
                                }
                            }));
                        }
                        _ => {
                            let return_frag =
                                self.global.add_frag(vec![ir::Instruction::RestoreFrame {
                                    size: self.frame_offset,
                                }]);

                            self.set_imm(self.frame_offset, return_frag);
                            self.frame_offset += 1;

                            self.compile_expr_and_push(arg).expect_ref(&Type::Str, name);

                            self.push_cur_frag(ir::Instruction::SaveFrame {
                                size: self.frame_offset - 1,
                            });
                            self.frame_offset = orig_frame_offset;

                            self.cur_frag = return_frag;
                        }
                    }
                }
                Typed::new(stack_value_at(orig_frame_offset), Type::unit())
            }
            "print" => {
                let mut args = args.iter();
                let Some(first_arg) = args.next() else {
                    panic!("`{name}` requires at least 1 argument");
                };
                let format_str = match first_arg {
                    ast::Expr::Str(s) => s,
                    _ => panic!("first argument to `{name}` must be a string literal"),
                };
                let mut chars = format_str.chars();
                while let Some(c) = chars.next() {
                    let c = match c {
                        '\\' => unescape_char(&mut chars),
                        '{' => match chars.clone().next() {
                            Some('{') => {
                                chars.next();
                                '{'
                            }
                            Some(_) => {
                                let Some((name, rest)) = chars.as_str().split_once('}') else {
                                    panic!("unterminated `{{` in format string");
                                };
                                chars = rest.chars();
                                if name.is_empty() {
                                    let Some(arg) = args.next() else {
                                        panic!(
                                            "not enough arguments for format string `{format_str}`"
                                        );
                                    };
                                    let var = Var {
                                        frame_offset: self.frame_offset,
                                        mutable: false,
                                        name,
                                        ty: self.compile_expr_and_push(arg).ty,
                                    };
                                    self.vars.push(var);
                                }
                                if name.starts_with(|c: char| !c.is_ascii_alphabetic() && c != '_')
                                    || name.chars().any(|c| !c.is_ascii_alphanumeric() && c != '_')
                                {
                                    panic!("invalid variable name `{name}`");
                                }
                                let outer_ty = self.compile_var_access(name).ty;
                                let mut ty = &outer_ty;
                                let mut arg = ast::Expr::Place(ast::Place::Var(name));
                                loop {
                                    match ty {
                                        Type::Usize => {
                                            break self.compile_func_call("print_int", &[arg]);
                                        }
                                        Type::Char => {
                                            break self.compile_macro_call("print_char", &[arg]);
                                        }
                                        Type::Ref { ty: inner_ty, .. } => match **inner_ty {
                                            Type::Str => {
                                                break self.compile_macro_call("print_str", &[arg]);
                                            }
                                            _ => {
                                                ty = inner_ty;
                                                arg = ast::Expr::Place(ast::Place::Deref(
                                                    Box::new(arg),
                                                ));
                                            }
                                        },
                                        _ => panic!("cannot print variable of type `{outer_ty}`"),
                                    }
                                }
                                .expect_ty(&Type::unit(), name);
                                self.shrink_frame(orig_frame_offset);
                                continue;
                            }
                            None => panic!("unterminated `{{` in format string"),
                        },
                        '}' => match chars.next() {
                            Some('}') => '}',
                            _ => panic!("unescaped `}}` in format string"),
                        },
                        _ => c,
                    };
                    for byte in c.encode_utf8(&mut [0; 4]).bytes() {
                        self.push_cur_frag(ir::Instruction::Output {
                            src: ir::Value::Immediate(byte as usize),
                        });
                    }
                }
                if args.next().is_some() {
                    panic!("too many arguments for format string `{format_str}`");
                }
                Typed::new(stack_value_at(orig_frame_offset), Type::unit())
            }
            "println" => {
                if !args.is_empty() {
                    self.compile_macro_call("print", args)
                        .expect_ty(&Type::unit(), name);
                }
                self.push_cur_frag(ir::Instruction::Output {
                    src: ir::Value::Immediate(b'\n' as usize),
                });
                Typed::new(stack_value_at(orig_frame_offset), Type::unit())
            }
            "exit" => {
                if !args.is_empty() {
                    panic!("`{name}` does not take any arguments");
                }
                self.extend_cur_frag(goto(EXIT_FRAG, self.frame_call_offset));
                self.cur_frag = EXIT_FRAG;
                Typed::new(stack_value_at(orig_frame_offset), Type::unit())
            }
            "panic" => {
                self.extend_cur_frag(b"panicked: ".map(|byte| ir::Instruction::Output {
                    src: ir::Value::Immediate(byte as usize),
                }));
                if args.is_empty() {
                    self.extend_cur_frag(b"explicit panic\n".map(|byte| ir::Instruction::Output {
                        src: ir::Value::Immediate(byte as usize),
                    }));
                } else {
                    self.compile_macro_call("println", args)
                        .expect_ty(&Type::unit(), name);
                }
                self.compile_macro_call("exit", &[])
            }
            "boxed" | "malloc" => {
                let [arg] = args else {
                    panic!("`{name}` requires exactly 1 argument");
                };

                // store the address on the stack
                self.frame_offset += 1;
                self.compile_single_move(
                    ir::Place::Direct(ir::DirectPlace::StackFrame {
                        offset: orig_frame_offset,
                    }),
                    ir::Value::At(ir::Place::Direct(ir::DirectPlace::Address(0))),
                    ir::StoreMode::Replace,
                    1,
                );

                let arg = self.compile_expr(arg);
                let (offset, ty) = match name {
                    "boxed" => {
                        // store the value on the heap
                        let size = self.size_of(&arg.ty);
                        self.compile_move(
                            ir::Place::Indirect(ir::IndirectPlace::Deref {
                                address: ir::DirectPlace::Address(0),
                            }),
                            arg.value,
                            ir::StoreMode::Add,
                            size,
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
                self.compile_single_move(
                    ir::Place::Direct(ir::DirectPlace::Address(0)),
                    offset,
                    ir::StoreMode::Add,
                    2,
                );

                self.frame_offset = orig_frame_offset + 1;
                Typed::new(
                    stack_value_at(orig_frame_offset),
                    Type::Ref {
                        mutable: true,
                        ty: Box::new(ty),
                    },
                )
            }
            "size_of_val" => {
                let [arg] = args else {
                    panic!("`{name}` requires exactly 1 argument");
                };
                let ty = self.compile_expr(arg).ty;
                self.frame_offset = orig_frame_offset;
                Typed::new(ir::Value::Immediate(self.size_of(&ty)), Type::Usize)
            }
            "==" | "!=" => {
                let [lhs, rhs] = args else {
                    panic!("`{name}` requires exactly 2 arguments");
                };
                let (ret_init, ret_modify) = match name {
                    "==" => (true, ir::StoreMode::Subtract),
                    "!=" => (false, ir::StoreMode::Add),
                    _ => unreachable!("the only comparison operators are `==` and `!=`"),
                };
                let ret = self
                    .compile_expr_and_push(&ast::Expr::Bool(ret_init))
                    .map(ir::Place::Direct);
                let lhs_base = self.frame_offset;
                let lhs_ty = self.compile_expr_and_push(lhs).ty;
                let rhs_base = self.frame_offset;
                self.compile_expr_and_push(rhs).expect_ty(&lhs_ty, name);
                let mut eq_test = vec![];
                for offset in 0..self.size_of(&lhs_ty) {
                    let lhs = ir::Place::Direct(ir::DirectPlace::StackFrame {
                        offset: lhs_base + offset,
                    });
                    let rhs = ir::Place::Direct(ir::DirectPlace::StackFrame {
                        offset: rhs_base + offset,
                    });
                    eq_test = vec![
                        ir::Instruction::While {
                            cond: lhs,
                            body: vec![
                                ir::Instruction::StoreImm {
                                    dst: lhs,
                                    value: 1,
                                    store_mode: ir::StoreMode::Subtract,
                                },
                                ir::Instruction::Switch {
                                    cond: rhs,
                                    cases: vec![vec![
                                        ir::Instruction::StoreImm {
                                            dst: lhs,
                                            value: 0,
                                            store_mode: ir::StoreMode::Replace,
                                        },
                                        ir::Instruction::StoreImm {
                                            dst: rhs,
                                            value: 1,
                                            store_mode: ir::StoreMode::Add,
                                        },
                                    ]],
                                    default: vec![ir::Instruction::StoreImm {
                                        dst: rhs,
                                        value: 1,
                                        store_mode: ir::StoreMode::Subtract,
                                    }],
                                },
                            ],
                        },
                        ir::Instruction::Switch {
                            cond: rhs,
                            cases: vec![eq_test],
                            default: vec![ir::Instruction::StoreImm {
                                dst: ret.value,
                                value: 1,
                                store_mode: ret_modify,
                            }],
                        },
                    ];
                }
                self.extend_cur_frag(eq_test);
                self.frame_offset = orig_frame_offset + 1;
                ret.map(ir::Value::At)
            }
            "&&" | "||" => {
                let [lhs, rhs] = args else {
                    panic!("`{name}` requires exactly 2 arguments");
                };

                let short_circuit = self.new_frag();
                let long_circuit = self.new_frag();

                let lhs = self.compile_expr_and_push(lhs).expect_ty(&Type::Bool, name);

                let [false_circuit, true_circuit] = match name {
                    "&&" => [short_circuit, long_circuit],
                    "||" => [long_circuit, short_circuit],
                    _ => unreachable!("the only short-circuiting operators are `&&` and `||`"),
                };

                self.push_cur_frag(ir::Instruction::Switch {
                    cond: ir::Place::Direct(lhs),
                    cases: vec![goto(false_circuit, self.frame_call_offset).to_vec()],
                    default: goto(true_circuit, self.frame_call_offset).to_vec(),
                });

                self.cur_frag = long_circuit;
                self.frame_offset -= 1;
                self.compile_expr_and_push(rhs).expect_ty(&Type::Bool, name);
                self.extend_cur_frag(goto(short_circuit, self.frame_call_offset));

                self.cur_frag = short_circuit;

                Typed::new(stack_value_at(orig_frame_offset), Type::Bool)
            }
            _ => {
                if self.global.func_defs.contains_key(name) {
                    panic!("function `{name}` must be called without `!`");
                } else {
                    panic!("macro `{name}` does not exist");
                }
            }
        }
    }

    fn compile_func_call(
        &mut self,
        name: &'src str,
        args: &[ast::Expr<'src>],
    ) -> Typed<'src, ir::Value> {
        let call_frag = self.global.compile_func(name);

        let func_def = &self.global.func_defs[name];
        if args.len() != func_def.params.len() {
            panic!(
                "function `{name}` expects {} arguments, but {} were provided",
                func_def.params.len(),
                args.len()
            );
        }

        // stack layout: [..., return value, return frag id, call frag id, arguments, locals]
        // frame timeline:
        // 1. caller:     ^^^^^^^^^^^^^^^^^
        // 2. call:                                          ^^^^^^^^^^^^^^^^^^^^^^^
        // 3. callee:          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        // 4. goto:                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        // 5. return:                        ^^^^^^^^^^^^^^
        // 6. returned:   ^^^^^^^^^^^^^^^^^

        let orig_frame_offset = self.frame_offset;

        // leave space for the return value
        let ret_size = self.size_of(&func_def.ret_ty);
        self.frame_offset += ret_size;

        // the return frag will restore everything up to this point
        let return_frag = self.global.add_frag(vec![ir::Instruction::RestoreFrame {
            size: self.frame_offset,
        }]);

        // push the return frag id
        self.set_imm(self.frame_offset, return_frag);
        self.frame_offset += 1;

        // push the call frag id
        self.set_imm(self.frame_offset, call_frag);
        self.frame_offset += 1;

        // push the arguments
        for (arg, param) in args.iter().zip(&func_def.params) {
            self.compile_expr_and_push(arg).expect_ty(&param.ty, name);
        }

        // make the call
        self.push_cur_frag(ir::Instruction::SaveFrame {
            size: orig_frame_offset + ret_size + 1,
        });

        // restore the frame after the call
        self.frame_offset = orig_frame_offset + ret_size;
        self.cur_frag = return_frag;

        Typed::new(stack_value_at(orig_frame_offset), func_def.ret_ty.clone())
    }

    fn compile_call(&mut self, call_expr: &ast::CallExpr<'src>) -> Typed<'src, ir::Value> {
        if call_expr.bang {
            self.compile_macro_call(call_expr.func, &call_expr.args)
        } else {
            self.compile_func_call(call_expr.func, &call_expr.args)
        }
    }

    fn compile_struct_expr(
        &mut self,
        struct_expr: &ast::StructExpr<'src>,
    ) -> Typed<'src, ir::Value> {
        let struct_fields = self.global.iter_fields_of_struct(struct_expr.name);

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

        let struct_frame_offset = self.frame_offset;

        let mut expected_fields: BTreeMap<&str, Option<FieldInfo>> = struct_fields
            .scan(struct_frame_offset, |frame_offset, field| {
                let info = Some((
                    field.name,
                    Some(FieldInfo {
                        ty: &field.ty,
                        frame_offset: *frame_offset,
                    }),
                ));
                *frame_offset += self.size_of(&field.ty);
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
            match self.frame_offset.cmp(&field_info.frame_offset) {
                Ordering::Less | Ordering::Equal => {
                    self.frame_offset = field_info.frame_offset;
                    self.compile_expr_and_push(&field.value)
                        .expect_ty(field_info.ty, field.name);
                }
                Ordering::Greater => {
                    let prev_frame_offset = self.frame_offset;
                    self.compile_expr_and_push(&field.value)
                        .expect_ty(field_info.ty, field.name);
                    self.compile_move(
                        ir::Place::Direct(ir::DirectPlace::StackFrame {
                            offset: field_info.frame_offset,
                        }),
                        stack_value_at(prev_frame_offset),
                        ir::StoreMode::Replace,
                        self.size_of(field_info.ty),
                    );
                    self.frame_offset = prev_frame_offset;
                }
            }
        }

        Typed::new(
            stack_value_at(struct_frame_offset),
            Type::Named(struct_expr.name),
        )
    }

    fn compile_tuple_expr(&mut self, elements: &[ast::Expr<'src>]) -> Typed<'src, ir::Value> {
        Typed::new(
            stack_value_at(self.frame_offset),
            Type::Tuple(
                elements
                    .iter()
                    .map(|expr| self.compile_expr_and_push(expr).ty)
                    .collect(),
            ),
        )
    }

    fn compile_array_expr(&mut self, array_expr: &ast::ArrayExpr<'src>) -> Typed<'src, ir::Value> {
        Typed::new(
            stack_value_at(self.frame_offset),
            match *array_expr {
                ast::ArrayExpr::List(ref elements) => Type::Array {
                    len: Some(elements.len()),
                    ty: Box::new(match elements.split_first() {
                        Some((first, rest)) => {
                            let first_ty = self.compile_expr_and_push(first).ty;
                            for element in rest {
                                self.compile_expr_and_push(element)
                                    .expect_ty(&first_ty, "array element");
                            }
                            first_ty
                        }
                        None => Type::Usize,
                    }),
                },
                ast::ArrayExpr::Repeat { ref value, len } => {
                    let first_elem = self.compile_expr_and_push(value);
                    let elem_size = self.size_of(&first_elem.ty);
                    match len {
                        0 => self.frame_offset -= elem_size,
                        _ => {
                            for _ in 1..len {
                                self.compile_move(
                                    ir::Place::Direct(ir::DirectPlace::StackFrame {
                                        offset: self.frame_offset,
                                    }),
                                    ir::Value::At(ir::Place::Direct(first_elem.value)),
                                    ir::StoreMode::Replace,
                                    elem_size,
                                );
                                self.frame_offset += elem_size;
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

    fn compile_var_access(&self, name: &'src str) -> Typed<'src, PlaceMut> {
        let Some(var) = self.vars.iter().rev().find(|var| var.name == name) else {
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

    fn increment_place(&mut self, place: &mut ir::Place, amount: usize) {
        match place {
            ir::Place::Direct(ir::DirectPlace::StackFrame { offset }) => {
                *offset += amount;
            }
            ir::Place::Direct(ir::DirectPlace::Address(address)) => *address += 2 * amount,
            ir::Place::Indirect(ir::IndirectPlace::Deref { address }) => {
                self.push_cur_frag(ir::Instruction::StoreImm {
                    dst: ir::Place::Direct(*address),
                    value: 2 * amount,
                    store_mode: ir::StoreMode::Add,
                });
            }
        }
    }

    fn own_place(&mut self, place: &mut ir::Place, new_frame_offset: usize) {
        let ir::Place::Indirect(ir::IndirectPlace::Deref { address }) = place else {
            return;
        };
        let owned_address = ir::DirectPlace::StackFrame {
            offset: new_frame_offset,
        };
        self.extend_cur_frag([
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

    fn compile_field_access(
        &mut self,
        base: &ast::Place<'src>,
        field_name: &'src str,
    ) -> Typed<'src, PlaceMut> {
        let mut base = self.compile_place(base);
        base = self.compile_deref(base, true);
        let found_field = match &base.ty {
            Type::Tuple(tys) => field_name.parse().ok().and_then(|index| {
                tys.get(index).map(|ty| {
                    Typed::new(
                        tys[..index].iter().map(|ty| self.size_of(ty)).sum(),
                        ty.clone(),
                    )
                })
            }),
            Type::Named(ty_name) => self
                .global
                .iter_fields_of_struct(ty_name)
                .scan(0, |offset, field| {
                    let field_offset = *offset;
                    *offset += self.size_of(&field.ty);
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
            let field_size = self.size_of(&field_ty);
            self.own_place(&mut base.value.place, self.frame_offset + field_size);
            self.increment_place(&mut base.value.place, field_offset);
        }
        Typed::new(base.value, field_ty)
    }

    fn compile_deref(
        &mut self,
        place: Typed<'src, PlaceMut>,
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
                        let offset = self.frame_offset
                            + if indexing {
                                self.indexed_size_of(&ty)
                            } else {
                                self.size_of(&ty)
                            };
                        self.compile_single_move(
                            ir::Place::Direct(ir::DirectPlace::StackFrame { offset }),
                            ir::Value::At(place),
                            ir::StoreMode::Replace,
                            1,
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
            self.compile_deref(deref, indexing)
        } else {
            deref
        }
    }

    fn compile_index(
        &mut self,
        base: &ast::Place<'src>,
        index: &ast::Expr<'src>,
    ) -> Typed<'src, PlaceMut> {
        let base = self.compile_place(base);
        let base = self.compile_deref(base, true);
        let Type::Array { ty: elem_ty, .. } = base.ty else {
            panic!("cannot index into value of type `{}`", base.ty);
        };
        let elem_size = self.size_of(&elem_ty);
        self.frame_offset += elem_size;
        let index = self
            .compile_expr(index)
            .expect_ty(&Type::Usize, "array index");
        let address = ir::DirectPlace::StackFrame {
            offset: self.frame_offset,
        };
        self.extend_cur_frag([
            ir::Instruction::LoadRef {
                src: base.value.place,
            },
            ir::Instruction::Store {
                dst: ir::Place::Direct(address),
                store_mode: ir::StoreMode::Replace,
            },
        ]);
        self.compile_single_move(
            ir::Place::Direct(address),
            index,
            ir::StoreMode::Add,
            elem_size * 2,
        );
        Typed::new(
            PlaceMut {
                place: ir::Place::Indirect(ir::IndirectPlace::Deref { address }),
                mutability: base.value.mutability,
            },
            *elem_ty,
        )
    }

    fn compile_place(&mut self, place: &ast::Place<'src>) -> Typed<'src, PlaceMut> {
        let orig_frame_offset = self.frame_offset;
        let result = match place {
            ast::Place::Var(ident) => self.compile_var_access(ident),
            ast::Place::FieldAccess { base, field } => self.compile_field_access(base, field),
            ast::Place::Index { base, index } => self.compile_index(base, index),
            ast::Place::Deref(expr) => {
                let place = self.compile_expr(expr).map(|value| PlaceMut {
                    place: match value {
                        ir::Value::At(place) => place,
                        ir::Value::Immediate(address) => {
                            ir::Place::Direct(ir::DirectPlace::Address(address))
                        }
                    },
                    mutability: Mutability::MutableThroughRef,
                });
                self.compile_deref(place, false)
            }
        };
        self.frame_offset = orig_frame_offset;
        result
    }

    fn compile_ref(&mut self, mutable: bool, place: &ast::Place<'src>) -> Typed<'src, ir::Value> {
        let place = self.compile_place(place);
        if mutable && !matches!(place.value.mutability, Mutability::Mutable) {
            panic!(
                "cannot take mutable reference to immutable place of type `{}`",
                place.ty
            );
        }
        self.extend_cur_frag([
            ir::Instruction::LoadRef {
                src: place.value.place,
            },
            ir::Instruction::Store {
                dst: ir::Place::Direct(ir::DirectPlace::StackFrame {
                    offset: self.frame_offset,
                }),
                store_mode: ir::StoreMode::Replace,
            },
        ]);
        self.frame_offset += 1;
        Typed::new(
            stack_value_at(self.frame_offset - 1),
            Type::Ref {
                mutable,
                ty: Box::new(place.ty),
            },
        )
    }

    fn compile_str(&mut self, s: &'src str) -> Typed<'src, ir::Value> {
        Typed::new(
            ir::Value::Immediate(*self.global.str_ids.entry(s).or_insert_with(|| {
                self.global.frags.push(
                    [ir::Instruction::RestoreFrame { size: 1 }]
                        .into_iter()
                        .chain(unescape_str(s).map(|byte| ir::Instruction::Output {
                            src: ir::Value::Immediate(byte as usize),
                        }))
                        .collect(),
                );
                self.global.frags.len() - 1
            })),
            Type::Ref {
                mutable: false,
                ty: Box::new(Type::Str),
            },
        )
    }

    fn compile_expr(&mut self, expr: &ast::Expr<'src>) -> Typed<'src, ir::Value> {
        match *expr {
            ast::Expr::Int(i) => Typed::new(ir::Value::Immediate(i), Type::Usize),
            ast::Expr::Char(c) => Typed::new(ir::Value::Immediate(c as usize), Type::Char),
            ast::Expr::Bool(b) => Typed::new(ir::Value::Immediate(b as usize), Type::Bool),
            ast::Expr::Str(s) => self.compile_str(s),
            ast::Expr::Place(ref place) => self
                .compile_place(place)
                .map(|place| place.place)
                .map(ir::Value::At),
            ast::Expr::Ref { mutable, ref place } => self.compile_ref(mutable, place),
            ast::Expr::Call(ref call_expr) => self.compile_call(call_expr),
            ast::Expr::Struct(ref struct_expr) => self.compile_struct_expr(struct_expr),
            ast::Expr::Tuple(ref elements) => self.compile_tuple_expr(elements),
            ast::Expr::Array(ref array_expr) => self.compile_array_expr(array_expr),
            ast::Expr::Cast { ref expr, ref ty } => {
                let src = self.compile_expr(expr);
                let src_size = self.size_of(&src.ty);
                let dst_ty = Type::from(ty);
                let dst_size = self.size_of(&dst_ty);
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

    fn compile_expr_and_push(&mut self, expr: &ast::Expr<'src>) -> Typed<'src, ir::DirectPlace> {
        let orig_frame_offset = self.frame_offset;
        let result = self.compile_expr(expr);
        let size = self.size_of(&result.ty);
        if self.frame_offset == orig_frame_offset && size > 0 {
            self.compile_move(
                ir::Place::Direct(ir::DirectPlace::StackFrame {
                    offset: self.frame_offset,
                }),
                result.value,
                ir::StoreMode::Replace,
                self.size_of(&result.ty),
            );
            self.frame_offset += size;
        }
        Typed::new(
            ir::DirectPlace::StackFrame {
                offset: orig_frame_offset,
            },
            result.ty,
        )
    }

    fn compile_move(
        &mut self,
        mut dst: ir::Place,
        src: ir::Value,
        store_mode: ir::StoreMode,
        size: usize,
    ) {
        match size {
            0 => {}
            1 => self.compile_single_move(dst, src, store_mode, 1),
            2.. => {
                let ir::Value::At(mut src) = src else {
                    unreachable!("expected source value to be a place in a move of size > 1");
                };
                self.own_place(&mut dst, self.frame_offset + size);
                self.own_place(&mut src, self.frame_offset + size + 1);
                for _ in 0..size {
                    self.extend_cur_frag([
                        ir::Instruction::Load { src, multiplier: 1 },
                        ir::Instruction::Store { dst, store_mode },
                    ]);
                    self.increment_place(&mut dst, 1);
                    self.increment_place(&mut src, 1);
                }
            }
        }
    }

    fn compile_statement(&mut self, statement: &ast::Statement<'src>) {
        let orig_frame_offset = self.frame_offset;
        match *statement {
            ast::Statement::Let {
                mutable,
                name,
                ref ty,
                ref value,
            } => {
                let var = Var {
                    frame_offset: self.frame_offset,
                    mutable,
                    name,
                    ty: match (ty.as_ref().map(Type::from), value) {
                        (None, None) => {
                            panic!("variable `{name}` needs a type annotation or a value")
                        }
                        (Some(ty), Some(value)) => {
                            self.compile_expr_and_push(value).expect_ty(&ty, name);
                            ty
                        }
                        (Some(ty), None) => {
                            if !mutable {
                                panic!("variable `{name}` must be mutable to be uninitialized")
                            }
                            self.frame_offset += self.size_of(&ty);
                            ty
                        }
                        (None, Some(value)) => self.compile_expr_and_push(value).ty,
                    },
                };
                self.vars.push(var);
            }
            ast::Statement::Assign {
                ref place,
                ref value,
                mode,
            } => {
                let typed_src = self.compile_expr_and_push(value);
                let typed_dst = self.compile_place(place);
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
                self.compile_move(
                    typed_dst.value.place,
                    ir::Value::At(ir::Place::Direct(src)),
                    match mode {
                        ast::AssignMode::Add => ir::StoreMode::Add,
                        ast::AssignMode::Subtract => ir::StoreMode::Subtract,
                        ast::AssignMode::Replace => ir::StoreMode::Replace,
                    },
                    self.size_of(&typed_dst.ty),
                );
                self.frame_offset = orig_frame_offset;
            }
            ast::Statement::Loop(ref body) => {
                let after_loop = self.new_frag();
                let loop_start = self.global.frags.len();
                self.loop_stack.push(FragPair {
                    start: loop_start,
                    end: after_loop,
                });
                let loop_body = self.create_block(body);
                assert!(loop_start == loop_body.start);
                self.extend_cur_frag(goto(loop_start, self.frame_call_offset));
                self.extend_frag(loop_body.end, goto(loop_start, self.frame_call_offset));
                self.loop_stack.pop();
                self.cur_frag = after_loop;
            }
            ast::Statement::Continue => {
                let Some(&FragPair { start, .. }) = self.loop_stack.last() else {
                    panic!("`continue` may only be used inside a loop");
                };
                self.extend_cur_frag(goto(start, self.frame_call_offset));
                self.cur_frag = EXIT_FRAG;
            }
            ast::Statement::Break => {
                let Some(&FragPair { end, .. }) = self.loop_stack.last() else {
                    panic!("`break` may only be used inside a loop");
                };
                self.extend_cur_frag(goto(end, self.frame_call_offset));
                self.cur_frag = EXIT_FRAG;
            }
            ast::Statement::If {
                ref cond,
                ref true_branch,
                ref false_branch,
            } => match self
                .compile_expr(cond)
                .expect_ty(&Type::Bool, "if condition")
            {
                ir::Value::At(place) => {
                    let true_block = self.create_block(true_branch);
                    let false_block = self.create_block(false_branch);

                    self.push_cur_frag(ir::Instruction::Switch {
                        cond: place,
                        cases: vec![goto(false_block.start, self.frame_call_offset).to_vec()],
                        default: goto(true_block.start, self.frame_call_offset).to_vec(),
                    });

                    self.cur_frag = self.new_frag();
                    for block in [true_block, false_block] {
                        self.extend_frag(block.end, goto(self.cur_frag, self.frame_call_offset));
                    }
                }
                ir::Value::Immediate(value) => self.compile_block(if value == 0 {
                    false_branch
                } else {
                    true_branch
                }),
            },
            ast::Statement::Match {
                ref scrutinee,
                ref arms,
            } => {
                let scrutinee = self.compile_expr(scrutinee);

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
                        .entry(pat.expect_ty(&scrutinee.ty, "match pattern"))
                        .or_insert(body);
                }

                let default_body = default_body.unwrap_or_default();

                let Some(&last_arm) = ordered_arms.keys().last() else {
                    self.compile_block(default_body);
                    return;
                };

                match scrutinee.value {
                    ir::Value::At(place) => {
                        let arm_blocks = (0..=last_arm)
                            .map(|value| {
                                self.create_block(
                                    ordered_arms.get(&value).copied().unwrap_or(default_body),
                                )
                            })
                            .collect::<Vec<_>>();
                        let default_block = self.create_block(default_body);
                        self.push_cur_frag(ir::Instruction::Switch {
                            cond: place,
                            cases: arm_blocks
                                .iter()
                                .map(|block| goto(block.start, self.frame_call_offset).to_vec())
                                .collect(),
                            default: goto(default_block.start, self.frame_call_offset).to_vec(),
                        });
                        self.cur_frag = self.new_frag();
                        for block in arm_blocks.into_iter().chain([default_block]) {
                            self.extend_frag(
                                block.end,
                                goto(self.cur_frag, self.frame_call_offset),
                            );
                        }
                    }
                    ir::Value::Immediate(value) => self
                        .compile_block(ordered_arms.get(&value).copied().unwrap_or(default_body)),
                };
            }
            ast::Statement::Block(ref body) => self.compile_block(body),
            ast::Statement::Return(ref value) => {
                let src = self
                    .compile_expr(value)
                    .expect_ty(&self.func_def.ret_ty, "return value");
                self.compile_move(
                    ir::Place::Direct(ir::DirectPlace::StackFrame { offset: 0 }),
                    src,
                    ir::StoreMode::Replace,
                    self.size_of(&self.func_def.ret_ty),
                );
                self.emit_return();
                self.cur_frag = EXIT_FRAG;
            }
            ast::Statement::Eval(ref expr) => {
                let _ = self.compile_expr(expr);
                self.frame_offset = orig_frame_offset;
            }
        }
    }

    fn compile_block(&mut self, body: &[ast::Statement<'src>]) {
        let orig_frame_offset = self.frame_offset;
        for statement in body {
            self.compile_statement(statement);
        }
        self.shrink_frame(orig_frame_offset);
    }

    fn create_block(&mut self, body: &[ast::Statement<'src>]) -> FragPair {
        let orig_frag = self.cur_frag;
        let start_frag = self.new_frag();
        self.cur_frag = start_frag;
        self.compile_block(body);
        let end_frag = self.cur_frag;
        self.cur_frag = orig_frag;
        FragPair {
            start: start_frag,
            end: end_frag,
        }
    }
}

static STD: LazyLock<ast::Ast> = LazyLock::new(|| {
    let src = include_str!("std.rs");
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

pub(super) fn lower(ast: &ast::Ast) -> ir::Program {
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

    let mut context = GlobalContext::new(&func_defs, &struct_defs);

    let main_func_id = context.compile_func("main");

    if context.func_defs["main"].ret_ty != Type::unit() {
        panic!("`main` must return `()`");
    }

    context.frags[EXIT_FRAG].clear(); // will never be executed

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
                    cases: context.frags,
                    default: vec![], // should never be executed
                }],
            },
        ],
    }
}
