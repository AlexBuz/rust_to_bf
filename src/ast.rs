use chumsky::prelude::*;
use derive_more::{Display, From};

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

#[derive(Debug, Clone, From, Display)]
pub enum ParseError {
    #[display("Lex error: {_0:#?}")]
    LexError(Vec<Simple<char>>),
    #[display("Parse error: {_0:#?}")]
    ParseError(Vec<Simple<Token>>),
}

impl std::error::Error for ParseError {}

impl Ast {
    pub fn parse(src: &str) -> Result<Self, ParseError> {
        let tokens = lexer().parse(src)?;
        let ast = parser().parse(tokens)?;
        Ok(ast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // delimiters
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Comma,
    Semi,
    // operators
    Star,
    Bang,
    Arrow,
    Eq,
    PlusEq,
    MinusEq,
    // keywords
    Let,
    Mut,
    Fn,
    If,
    Else,
    While,
    Loop,
    Match,
    Return,
    Break,
    Continue,
    // literals
    Int(usize),
    String(String),
    // identifiers
    Ident(Ident),
}

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let char_escape = just('\\').ignore_then(choice([
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('\\').to('\\'),
        just('"').to('"'),
        just('\'').to('\''),
    ]));

    let string_literal = just('"')
        .ignore_then(none_of("\"\\").or(char_escape).repeated().collect())
        .then_ignore(just('"'))
        .map(Token::String);

    let char_literal = just('\'')
        .ignore_then(none_of("'\\").or(char_escape))
        .then_ignore(just('\''))
        .map(|c| c as _)
        .map(Token::Int);

    let int_literal = text::int(10).from_str().unwrapped().map(Token::Int);

    let literal = string_literal.or(char_literal).or(int_literal);

    let delimiter = choice([
        just('{').to(Token::OpenBrace),
        just('}').to(Token::CloseBrace),
        just('(').to(Token::OpenParen),
        just(')').to(Token::CloseParen),
        just(',').to(Token::Comma),
        just(';').to(Token::Semi),
    ]);

    let operator = choice([
        just("*").to(Token::Star),
        just("!").to(Token::Bang),
        just("=>").to(Token::Arrow),
        just("=").to(Token::Eq),
        just("+=").to(Token::PlusEq),
        just("-=").to(Token::MinusEq),
    ]);

    let keyword = choice([
        text::keyword("let").to(Token::Let),
        text::keyword("mut").to(Token::Mut),
        text::keyword("fn").to(Token::Fn),
        text::keyword("if").to(Token::If),
        text::keyword("else").to(Token::Else),
        text::keyword("while").to(Token::While),
        text::keyword("loop").to(Token::Loop),
        text::keyword("match").to(Token::Match),
        text::keyword("return").to(Token::Return),
        text::keyword("break").to(Token::Break),
        text::keyword("continue").to(Token::Continue),
    ]);

    let ident = text::ident().map(Token::Ident);

    let token = choice((literal, delimiter, operator, keyword, ident));

    let comment = just("//").then(none_of('\n').repeated()).padded();

    token
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .then_ignore(end())
}

#[allow(clippy::let_and_return)]
fn parser() -> impl Parser<Token, Ast, Error = Simple<Token>> {
    let ident = select! { Token::Ident(ident) => ident };

    let r#deref = just(Token::Star).ignore_then(ident).map(Place::Deref);

    let place = ident.map(Place::Var).or(r#deref);

    let int = select! { Token::Int(int) => int };

    let string = select! { Token::String(string) => string };

    let simple_expr = place
        .clone()
        .map(SimpleExpr::Place)
        .or(int.map(SimpleExpr::Int))
        .or(string.map(SimpleExpr::String))
        .map(Expr::Simple);

    let expr = recursive(|expr| {
        let call = ident
            .then(just(Token::Bang).or_not().map(|i| i.is_some()))
            .then(
                expr.separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )
            .map(|((func, bang), args)| Expr::Call(CallExpr { func, bang, args }));

        let expr = call.or(simple_expr);

        expr
    });

    let maybe_mut = just(Token::Mut).or_not().map(|m| m.is_some());

    let r#let = just(Token::Let)
        .ignore_then(maybe_mut.clone())
        .then(ident)
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map(|((mutable, name), value)| Statement::Let {
            decl: VarDecl { mutable, name },
            value,
        });

    let assign = place
        .then(
            just(Token::Eq)
                .to(AssignMode::Replace)
                .or(just(Token::PlusEq).to(AssignMode::Add))
                .or(just(Token::MinusEq).to(AssignMode::Subtract)),
        )
        .then(expr.clone())
        .map(|((place, mode), value)| Statement::Assign { place, value, mode });

    let r#return = just(Token::Return)
        .ignore_then(expr.clone())
        .map(Statement::Return);

    let r#break = just(Token::Break).to(Statement::Break);

    let r#continue = just(Token::Continue).to(Statement::Continue);

    let eval = expr.clone().map(Statement::Eval);

    let statement_without_block = r#let
        .or(assign)
        .or(r#return)
        .or(r#break)
        .or(r#continue)
        .or(eval);

    let block = recursive(|block| {
        let if_else = just(Token::If)
            .ignore_then(expr.clone())
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .map(|((cond, main_body), else_body)| Statement::Switch {
                cond,
                cases: vec![(0, else_body.unwrap_or_default())],
                default: main_body,
            });

        let r#loop = just(Token::Loop)
            .ignore_then(block.clone())
            .map(|body| Statement::Loop { body });

        let r#while = just(Token::While)
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(cond, body)| Statement::Loop {
                body: vec![Statement::Switch {
                    cond,
                    cases: vec![(0, vec![Statement::Break])],
                    default: body,
                }],
            });

        let statement_with_block = recursive(|statement_with_block| {
            let arm = just(Token::Arrow).ignore_then(
                statement_without_block
                    .clone()
                    .then_ignore(just(Token::Comma))
                    .or(statement_with_block.then_ignore(just(Token::Comma).or_not()))
                    .map(Vec::from),
            );

            let switch = just(Token::Match)
                .ignore_then(expr)
                .then(
                    int.then(arm.clone())
                        .repeated()
                        .then(
                            select! { Token::Ident(ident) if ident == "_" => ident }
                                .ignore_then(arm)
                                .or_not()
                                .map(Option::unwrap_or_default),
                        )
                        .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
                )
                .map(|(cond, (cases, default))| Statement::Switch {
                    cond,
                    cases,
                    default,
                });

            let statement_with_block = if_else
                .or(r#loop)
                .or(r#while)
                .or(switch)
                .or(block.map(|body| Statement::Block { body }));

            statement_with_block
        });

        let statement = statement_without_block
            .then_ignore(just(Token::Semi))
            .or(statement_with_block.then_ignore(just(Token::Semi).or_not()));

        let block = statement
            .repeated()
            .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace));

        block
    });

    let param = maybe_mut
        .then(ident)
        .map(|(mutable, name)| VarDecl { mutable, name });

    let function = just(Token::Fn)
        .ignore_then(ident)
        .then(
            param
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        )
        .then(block)
        .map(|((name, params), body)| Function { name, params, body });

    let ast = function
        .repeated()
        .then_ignore(end())
        .map(|functions| Ast { functions });

    ast
}
