// TODO: perhaps make this file into a separate crate to improve compile times

use chumsky::prelude::*;

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
    // TODO: Instead of taking an Ident, take an Expr to allow for dynamic dispatch.
    // TODO: Generalize args to Vec<Expr>
    Call { func: Ident, args: Vec<SimpleExpr> },
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
    While {
        cond: Place, // TODO: Generalize to Expr
        body: Vec<Statement>,
    },
    IfElse {
        cond: Expr,
        main_body: Vec<Statement>,
        else_body: Vec<Statement>,
    },
    Switch {
        cond: Expr,
        cases: Vec<(usize, Vec<Statement>)>,
        default: Vec<Statement>,
    },
    Block {
        body: Vec<Statement>,
    },
    Return(Expr),
    // TODO: statements for IO
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
        // let parsed = ast_parser().parse(src);
        // println!("{:?}", parsed);
        // parsed
        use Statement::*;
        Ok(Ast {
            functions: vec![Function {
                name: "main".to_string(),
                params: vec![],
                body: vec![
                    Let {
                        decl: VarDecl {
                            mutable: true,
                            name: "a".to_string(),
                        },
                        value: Expr::Simple(SimpleExpr::Int(0)),
                    },
                    Let {
                        decl: VarDecl {
                            mutable: true,
                            name: "b".to_string(),
                        },
                        value: Expr::Simple(SimpleExpr::Int(1)),
                    },
                    Let {
                        decl: VarDecl {
                            mutable: true,
                            name: "temp".to_string(),
                        },
                        value: Expr::Simple(SimpleExpr::Int(0)),
                    },
                    Let {
                        decl: VarDecl {
                            mutable: true,
                            name: "iterations".to_string(),
                        },
                        value: Expr::Simple(SimpleExpr::Int(12)),
                    },
                    Let {
                        decl: VarDecl {
                            mutable: true,
                            name: "ptr".to_string(),
                        },
                        value: Expr::Simple(SimpleExpr::Int(0)),
                    },
                    While {
                        cond: Place::Var("iterations".to_string()),
                        body: vec![
                            Assign {
                                place: Place::Var("temp".to_string()),
                                value: Expr::Simple(SimpleExpr::Place(Place::Var("b".to_string()))),
                                mode: AssignMode::Replace,
                            },
                            Assign {
                                place: Place::Var("b".to_string()),
                                value: Expr::Simple(SimpleExpr::Place(Place::Var("a".to_string()))),
                                mode: AssignMode::Add,
                            },
                            Assign {
                                place: Place::Var("a".to_string()),
                                value: Expr::Simple(SimpleExpr::Place(Place::Var(
                                    "temp".to_string(),
                                ))),
                                mode: AssignMode::Replace,
                            },
                            Assign {
                                place: Place::Var("iterations".to_string()),
                                value: Expr::Simple(SimpleExpr::Int(1)),
                                mode: AssignMode::Subtract,
                            },
                            Assign {
                                place: Place::Deref("ptr".to_string()),
                                value: Expr::Simple(SimpleExpr::Place(Place::Var("b".to_string()))),
                                mode: AssignMode::Replace,
                            },
                            Assign {
                                place: Place::Var("ptr".to_string()),
                                value: Expr::Simple(SimpleExpr::Int(1)),
                                mode: AssignMode::Add,
                            },
                            Switch {
                                cond: Expr::Simple(SimpleExpr::Place(Place::Var(
                                    "iterations".to_string(),
                                ))),
                                cases: vec![
                                    (
                                        10,
                                        vec![
                                            Assign {
                                                place: Place::Deref("ptr".to_string()),
                                                value: Expr::Simple(SimpleExpr::Int(10)),
                                                mode: AssignMode::Replace,
                                            },
                                            Assign {
                                                place: Place::Var("ptr".to_string()),
                                                value: Expr::Simple(SimpleExpr::Int(1)),
                                                mode: AssignMode::Add,
                                            },
                                        ],
                                    ),
                                    (
                                        8,
                                        vec![
                                            Assign {
                                                place: Place::Deref("ptr".to_string()),
                                                value: Expr::Simple(SimpleExpr::Int(8)),
                                                mode: AssignMode::Replace,
                                            },
                                            Assign {
                                                place: Place::Var("ptr".to_string()),
                                                value: Expr::Simple(SimpleExpr::Int(1)),
                                                mode: AssignMode::Add,
                                            },
                                        ],
                                    ),
                                    (
                                        6,
                                        vec![
                                            Assign {
                                                place: Place::Deref("ptr".to_string()),
                                                value: Expr::Simple(SimpleExpr::Int(6)),
                                                mode: AssignMode::Replace,
                                            },
                                            Assign {
                                                place: Place::Var("ptr".to_string()),
                                                value: Expr::Simple(SimpleExpr::Int(1)),
                                                mode: AssignMode::Add,
                                            },
                                        ],
                                    ),
                                    (
                                        4,
                                        vec![
                                            Assign {
                                                place: Place::Deref("ptr".to_string()),
                                                value: Expr::Simple(SimpleExpr::Int(4)),
                                                mode: AssignMode::Replace,
                                            },
                                            Assign {
                                                place: Place::Var("ptr".to_string()),
                                                value: Expr::Simple(SimpleExpr::Int(1)),
                                                mode: AssignMode::Add,
                                            },
                                        ],
                                    ),
                                ],
                                default: vec![
                                    Assign {
                                        place: Place::Deref("ptr".to_string()),
                                        value: Expr::Simple(SimpleExpr::Int(0)),
                                        mode: AssignMode::Replace,
                                    },
                                    Assign {
                                        place: Place::Var("ptr".to_string()),
                                        value: Expr::Simple(SimpleExpr::Int(1)),
                                        mode: AssignMode::Add,
                                    },
                                ],
                            },
                        ],
                    },
                ],
            }],
        })
    }
}

// #[allow(clippy::let_and_return)]
// fn ast_parser() -> impl Parser<char, Ast, Error = Simple<char>> {
//     let ident = text::ident().padded();

//     let r#deref = just('*').ignore_then(ident).padded().map(Place::Deref);

//     let place = ident.map(Place::Var).or(r#deref);

//     let int = text::int(10)
//         .map(|s: String| s.parse::<usize>().unwrap())
//         .padded();

//     let simple_value = place.map(SimpleExpr::Place).or(int.map(SimpleExpr::Int));

//     let call = ident
//         .then(
//             simple_value
//                 .separated_by(just(','))
//                 .allow_trailing()
//                 .delimited_by(just('('), just(')'))
//                 .padded(),
//         )
//         .map(|(func, args)| Expr::Call { func, args });

//     let expr = call.or(simple_value.map(Expr::Simple)).padded();

//     let maybe_mut = text::keyword("mut").padded().or_not().map(|m| m.is_some());

//     let r#let = text::keyword("let")
//         .ignore_then(maybe_mut.clone())
//         .then(ident)
//         .then_ignore(just('='))
//         .then(expr)
//         .map(|((mutable, name), value)| Statement::Let {
//             decl: VarDecl { mutable, name },
//             value,
//         });

//     let assign = place
//         .then(
//             just('=')
//                 .map(|_| AssignMode::Replace)
//                 .or(just("+=").map(|_| AssignMode::Add))
//                 .or(just("-=").map(|_| AssignMode::Subtract)),
//         )
//         .then(expr)
//         .map(|((place, mode), value)| Statement::Assign { place, value, mode });

//     let r#return = text::keyword("return")
//         .ignore_then(expr)
//         .map(Statement::Return);

//     let semicolon = just(';').padded();
//     let comma = just(',').padded();
//     let lbrace = just('{').padded();
//     let rbrace = just('}').padded();

//     let block = recursive(|block| {
//         let r#while = text::keyword("while")
//             .ignore_then(expr)
//             .then(block.clone())
//             .map(|(cond, body)| Statement::While { cond, body });

//         let if_else = text::keyword("if")
//             .ignore_then(expr)
//             .then(block.clone())
//             .then(text::keyword("else").ignore_then(block.clone()).or_not())
//             .map(|((cond, main_body), else_body)| Statement::IfElse {
//                 cond,
//                 main_body,
//                 else_body: else_body.unwrap_or_default(),
//             });

//         let statement_without_block = r#let.or(assign).or(r#return).padded();

//         let statement_with_block = recursive(|statement_with_block| {
//             let arm = just("=>").padded().ignore_then(
//                 statement_without_block
//                     .clone()
//                     .then_ignore(comma)
//                     .or(statement_with_block.then_ignore(comma.or_not()))
//                     .map(Vec::from),
//             );

//             let switch = text::keyword("match")
//                 .ignore_then(expr)
//                 .then(
//                     int.then(arm.clone())
//                         .repeated()
//                         .padded()
//                         .then(
//                             just('_')
//                                 .padded()
//                                 .ignore_then(arm)
//                                 .or_not()
//                                 .map(Option::unwrap_or_default),
//                         )
//                         .delimited_by(lbrace, rbrace),
//                 )
//                 .map(|(cond, (cases, default))| Statement::Switch {
//                     cond,
//                     cases,
//                     default,
//                 });

//             let statement_with_block = if_else
//                 .or(r#while)
//                 .or(switch)
//                 .or(block.map(|body| Statement::Block { body }))
//                 .padded();

//             statement_with_block
//         });

//         let statement = statement_without_block
//             .then_ignore(semicolon)
//             .or(statement_with_block.then_ignore(semicolon.or_not()));

//         let block = statement.repeated().delimited_by(lbrace, rbrace).padded();

//         block
//     });

//     let param = maybe_mut
//         .then(ident)
//         // .then_ignore(just(':').padded())
//         // .then_ignore(just("usize").padded())
//         .map(|(mutable, name)| VarDecl { mutable, name });

//     let function = text::keyword("fn")
//         .ignore_then(ident)
//         .then(
//             param
//                 .separated_by(just(','))
//                 .allow_trailing()
//                 .delimited_by(just('('), just(')'))
//                 .padded(),
//         )
//         .then(block)
//         .map(|((name, params), body)| Function { name, params, body });

//     let ast = function
//         .repeated()
//         .padded()
//         .then_ignore(end())
//         .map(|functions| Ast { functions });

//     ast
// }
