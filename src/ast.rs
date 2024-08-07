// TODO: perhaps make this file into a separate crate to improve compile times

use chumsky::prelude::*;

// TODO: Use a reference to a string instead of a string.
pub type Ident = std::string::String;

#[derive(Debug, Clone)]
pub enum Place {
    Var(Ident),
    Deref(Ident),
}

// TODO: Get rid of SimpleExpr and just use Expr.
#[derive(Debug, Clone)]
pub enum SimpleExpr {
    Int(usize),
    Place(Place),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Simple(SimpleExpr),
    // TODO: Instead of an Ident for the func, take an Expr to allow for dynamic dispatch.
    Call { func: Ident, args: Vec<Expr> },
}

#[derive(Debug, Clone, Copy)]
pub enum AssignMode {
    Replace,
    Add,
    Subtract,
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
    Loop {
        body: Vec<Statement>,
    },
    Continue,
    Break,
    Switch {
        cond: Expr,
        cases: Vec<(usize, Vec<Statement>)>,
        default: Vec<Statement>,
    },
    Block {
        body: Vec<Statement>,
    },
    Return(Expr),
    Eval(Expr),
}

impl From<Statement> for Vec<Statement> {
    fn from(statement: Statement) -> Self {
        match statement {
            Statement::Block { body } => body,
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

impl Ast {
    pub fn parse(src: &str) -> Result<Self, Vec<Simple<char>>> {
        let parsed = ast_parser().parse(src);
        if crate::config::DEBUG {
            println!("{:#?}", parsed);
        }
        parsed
    }
}

#[allow(clippy::let_and_return)]
fn ast_parser() -> impl Parser<char, Ast, Error = Simple<char>> {
    let ident = text::ident().padded();

    let r#deref = just('*').ignore_then(ident).padded().map(Place::Deref);

    let place = ident.map(Place::Var).or(r#deref);

    let int = text::int(10)
        .map(|s: String| s.parse::<usize>().unwrap())
        .padded();

    let simple_value = place.map(SimpleExpr::Place).or(int.map(SimpleExpr::Int));

    let expr = recursive(|expr| {
        let call = ident
            .then(
                expr.separated_by(just(','))
                    .allow_trailing()
                    .delimited_by(just('('), just(')'))
                    .padded(),
            )
            .map(|(func, args)| Expr::Call { func, args });

        let expr = call.or(simple_value.map(Expr::Simple)).padded();

        expr
    });

    let maybe_mut = text::keyword("mut").padded().or_not().map(|m| m.is_some());

    let r#let = text::keyword("let")
        .ignore_then(maybe_mut.clone())
        .then(ident)
        .then_ignore(just('='))
        .then(expr.clone())
        .map(|((mutable, name), value)| Statement::Let {
            decl: VarDecl { mutable, name },
            value,
        });

    let assign = place
        .then(
            just('=')
                .map(|_| AssignMode::Replace)
                .or(just("+=").map(|_| AssignMode::Add))
                .or(just("-=").map(|_| AssignMode::Subtract)),
        )
        .then(expr.clone())
        .map(|((place, mode), value)| Statement::Assign { place, value, mode });

    let r#return = text::keyword("return")
        .ignore_then(expr.clone())
        .map(Statement::Return);

    // TODO: only allow break and continue in loops
    let r#break = text::keyword("break").map(|_| Statement::Break);
    let r#continue = text::keyword("continue").map(|_| Statement::Continue);

    let eval = expr.clone().map(Statement::Eval);

    let statement_without_block = r#let
        .or(assign)
        .or(r#return)
        .or(r#break)
        .or(r#continue)
        .or(eval)
        .padded();

    let semicolon = just(';').padded();
    let comma = just(',').padded();
    let lbrace = just('{').padded();
    let rbrace = just('}').padded();

    let block = recursive(|block| {
        let r#while = text::keyword("while")
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(cond, body)| Statement::Loop {
                body: vec![Statement::Switch {
                    cond,
                    cases: vec![(0, vec![Statement::Break])],
                    default: body,
                }],
            });

        let if_else = text::keyword("if")
            .ignore_then(expr.clone())
            .then(block.clone())
            .then(text::keyword("else").ignore_then(block.clone()).or_not())
            .map(|((cond, main_body), else_body)| Statement::Switch {
                cond,
                cases: vec![(0, else_body.unwrap_or_default())],
                default: main_body,
            });

        let statement_with_block = recursive(|statement_with_block| {
            let arm = just("=>").padded().ignore_then(
                statement_without_block
                    .clone()
                    .then_ignore(comma)
                    .or(statement_with_block.then_ignore(comma.or_not()))
                    .map(Vec::from),
            );

            let switch = text::keyword("match")
                .ignore_then(expr)
                .then(
                    int.then(arm.clone())
                        .repeated()
                        .padded()
                        .then(
                            just('_')
                                .padded()
                                .ignore_then(arm)
                                .or_not()
                                .map(Option::unwrap_or_default),
                        )
                        .delimited_by(lbrace, rbrace),
                )
                .map(|(cond, (cases, default))| Statement::Switch {
                    cond,
                    cases,
                    default,
                });

            let statement_with_block = if_else
                .or(r#while)
                .or(switch)
                .or(block.map(|body| Statement::Block { body }))
                .padded();

            statement_with_block
        });

        let statement = statement_without_block
            .then_ignore(semicolon)
            .or(statement_with_block.then_ignore(semicolon.or_not()));

        let block = statement.repeated().delimited_by(lbrace, rbrace).padded();

        block
    });

    let param = maybe_mut
        .then(ident)
        // .then_ignore(just(':').padded())
        // .then_ignore(just("usize").padded())
        .map(|(mutable, name)| VarDecl { mutable, name });

    let function = text::keyword("fn")
        .ignore_then(ident)
        .then(
            param
                .separated_by(just(','))
                .allow_trailing()
                .delimited_by(just('('), just(')'))
                .padded(),
        )
        .then(block)
        .map(|((name, params), body)| Function { name, params, body });

    let ast = function
        .repeated()
        .padded()
        .then_ignore(end())
        .map(|functions| Ast { functions });

    ast
}
