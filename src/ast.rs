use {
    crate::{lexer, parser},
    chumsky::prelude::*,
    derive_more::{Display, From},
};

// TODO: Use a &str instead of a String
pub type Ident = String;

#[derive(Debug, Clone)]
pub enum FieldIdent {
    Named(Ident),
    Index(usize),
}

#[derive(Debug, Clone)]
pub enum Place {
    Var(Ident),
    FieldAccess { base: Box<Place>, field: FieldIdent },
    Deref(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    // TODO: Instead of an Ident for the func, take an Expr to allow for dynamic dispatch.
    pub func: Ident,
    pub bang: bool,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct StructExpr {
    pub name: Ident,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(usize),
    String(String),
    Place(Place),
    Ref { mutable: bool, place: Place },
    Call(CallExpr),
    Struct(StructExpr),
    Tuple(Vec<Expr>),
}

impl Default for Expr {
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

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        mutable: bool,
        name: Ident,
        ty: Option<Type>,
        value: Option<Expr>,
    },
    Assign {
        place: Place,
        value: Expr,
        mode: AssignMode,
    },
    Loop(Vec<Statement>),
    Continue,
    Break,
    Switch {
        cond: Expr,
        cases: Vec<(usize, Vec<Statement>)>,
        default: Vec<Statement>,
    },
    Block(Vec<Statement>),
    Return(Expr),
    Eval(Expr),
}

impl From<Statement> for Vec<Statement> {
    fn from(statement: Statement) -> Self {
        match statement {
            Statement::Block(body) => body,
            _ => Vec::from([statement]),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Tuple(Vec<Type>),
    Named(Ident),
    Ref { mutable: bool, ty: Box<Type> },
    // TODO: Add support for function types
    // Func {
    //     param_tys: Vec<Type>,
    //     ret_ty: Box<Type>,
    // }
}

impl Default for Type {
    fn default() -> Self {
        Type::Tuple(vec![])
    }
}

impl std::fmt::Display for Type {
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
pub struct Param {
    pub mutable: bool,
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Item {
    FuncDef {
        name: Ident,
        params: Vec<Param>,
        ret_ty: Type,
        body: Vec<Statement>,
    },
    StructDef {
        name: Ident,
        fields: Vec<FieldDef>,
    },
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, From, Display)]
pub enum ParseOrLexError {
    #[display("Lex error: {_0:#?}")]
    LexError(Vec<Simple<char>>),
    #[display("Parse error: {_0:#?}")]
    ParseError(Vec<Simple<lexer::Token>>),
}

impl std::error::Error for ParseOrLexError {}

impl Ast {
    pub fn parse(src: &str) -> Result<Self, ParseOrLexError> {
        let tokens = lexer::lexer().parse(src)?;
        let ast = parser::ast_parser().parse(tokens)?;
        Ok(ast)
    }
}
