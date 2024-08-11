use {
    crate::{lexer, parser},
    chumsky::prelude::*,
    derive_more::{Display, From},
};

// TODO: Use a &str instead of a String
pub type Ident = String;

#[derive(Debug, Clone)]
pub enum Place {
    Var(Ident),
    Deref(Ident),
}

// TODO: Get rid of SimpleExpr and just use Expr.
#[derive(Debug, Clone)]
pub enum SimpleExpr {
    Int(usize),
    String(String),
    Place(Place),
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    // TODO: Instead of an Ident for the func, take an Expr to allow for dynamic dispatch.
    pub func: Ident,
    pub bang: bool,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Simple(SimpleExpr),
    Call(CallExpr),
}

#[derive(Debug, Clone, Copy)]
pub enum AssignMode {
    Add,
    Subtract,
    Replace,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub mutable: bool,
    pub name: Ident,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        decl: VarDecl,
        value: Expr,
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
pub struct Function {
    pub name: Ident,
    pub params: Vec<VarDecl>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub functions: Vec<Function>,
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
