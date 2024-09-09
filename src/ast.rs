use {
    crate::{lexer, parser},
    chumsky::prelude::*,
};

#[derive(Debug, Clone)]
pub enum Place<'src> {
    Var(&'src str),
    FieldAccess {
        base: Box<Place<'src>>,
        field: &'src str,
    },
    Index {
        base: Box<Place<'src>>,
        index: Box<Expr<'src>>,
    },
    Deref(Box<Expr<'src>>),
}

#[derive(Debug, Clone)]
pub struct CallExpr<'src> {
    pub func: &'src str,
    pub bang: bool,
    pub args: Vec<Expr<'src>>,
}

#[derive(Debug, Clone)]
pub struct Field<'src> {
    pub name: &'src str,
    pub value: Expr<'src>,
}

#[derive(Debug, Clone)]
pub struct StructExpr<'src> {
    pub name: &'src str,
    pub fields: Vec<Field<'src>>,
}

#[derive(Debug, Clone)]
pub enum ArrayExpr<'src> {
    List(Vec<Expr<'src>>),
    Repeat { value: Box<Expr<'src>>, len: usize },
}

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Int(usize),
    Char(char),
    Bool(bool),
    Str(&'src str),
    Place(Place<'src>),
    Ref {
        mutable: bool,
        place: Place<'src>,
    },
    Call(CallExpr<'src>),
    Struct(StructExpr<'src>),
    Tuple(Vec<Expr<'src>>),
    Array(ArrayExpr<'src>),
    Cast {
        expr: Box<Expr<'src>>,
        ty: Type<'src>,
    },
}

impl Default for Expr<'_> {
    fn default() -> Self {
        Expr::Tuple(vec![])
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AssignMode {
    Add,
    Subtract,
    Replace,
}

#[derive(Debug, Clone, Copy)]
pub enum Pattern {
    Int(usize),
    Char(char),
    Bool(bool),
    Wildcard,
}

#[derive(Debug, Clone)]
pub enum Statement<'src> {
    Let {
        mutable: bool,
        name: &'src str,
        ty: Option<Type<'src>>,
        value: Option<Expr<'src>>,
    },
    Assign {
        place: Place<'src>,
        value: Expr<'src>,
        mode: AssignMode,
    },
    Loop(Vec<Statement<'src>>),
    Continue,
    Break,
    If {
        cond: Expr<'src>,
        true_branch: Vec<Statement<'src>>,
        false_branch: Vec<Statement<'src>>,
    },
    Match {
        scrutinee: Expr<'src>,
        arms: Vec<(Pattern, Vec<Statement<'src>>)>,
    },
    Block(Vec<Statement<'src>>),
    Return(Expr<'src>),
    Eval(Expr<'src>),
}

impl<'src> From<Statement<'src>> for Vec<Statement<'src>> {
    fn from(statement: Statement<'src>) -> Self {
        match statement {
            Statement::Block(body) => body,
            _ => Vec::from([statement]),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type<'src> {
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
    // TODO: support function types
    // Func {
    //     param_tys: Vec<Type>,
    //     ret_ty: Box<Type>,
    // }
}

impl Default for Type<'_> {
    fn default() -> Self {
        Type::Tuple(vec![])
    }
}

impl std::fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
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
            Type::Array { ty, len } => match len {
                Some(len) => write!(f, "[{}; {}]", ty, len),
                None => write!(f, "[{}]", ty),
            },
            Type::Named(name) => write!(f, "{}", name),
            Type::Ref { mutable, ty } => {
                write!(f, "&")?;
                if *mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{}", ty)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param<'src> {
    pub mutable: bool,
    pub name: &'src str,
    pub ty: Type<'src>,
}

#[derive(Debug, Clone)]
pub struct FieldDef<'src> {
    pub name: &'src str,
    pub ty: Type<'src>,
}

#[derive(Debug, Clone)]
pub enum Item<'src> {
    FuncDef {
        name: &'src str,
        params: Vec<Param<'src>>,
        ret_ty: Type<'src>,
        body: Vec<Statement<'src>>,
    },
    StructDef {
        name: &'src str,
        fields: Vec<FieldDef<'src>>,
    },
}

#[derive(Debug, Clone)]
pub struct Ast<'src> {
    pub items: Vec<Item<'src>>,
}

impl<'src> TryFrom<&'src str> for Ast<'src> {
    type Error = anyhow::Error;

    fn try_from(src: &'src str) -> Result<Self, Self::Error> {
        let tokens = match lexer::lexer().parse(src).into_result() {
            Ok(tokens) => tokens,
            Err(errs) => anyhow::bail!(errs
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")),
        };
        let ast = match parser::ast_parser().parse(&tokens).into_result() {
            Ok(ast) => ast,
            Err(errs) => anyhow::bail!(errs
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")),
        };
        Ok(ast)
    }
}
