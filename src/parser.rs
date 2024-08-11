use {
    crate::{ast::*, lexer::Token},
    chumsky::prelude::*,
};

fn ident_parser() -> impl Parser<Token, Ident, Error = Simple<Token>> + Clone {
    select! { Token::Ident(ident) => ident }
}

fn place_parser() -> impl Parser<Token, Place, Error = Simple<Token>> + Clone {
    let r#deref = just(Token::Star)
        .ignore_then(ident_parser())
        .map(Place::Deref);

    ident_parser().map(Place::Var).or(r#deref)
}

fn int_parser() -> impl Parser<Token, usize, Error = Simple<Token>> + Clone {
    select! { Token::Int(int) => int }
}

fn string_parser() -> impl Parser<Token, String, Error = Simple<Token>> + Clone {
    select! { Token::String(string) => string }
}

fn simple_expr_parser() -> impl Parser<Token, SimpleExpr, Error = Simple<Token>> + Clone {
    place_parser()
        .map(SimpleExpr::Place)
        .or(int_parser().map(SimpleExpr::Int))
        .or(string_parser().map(SimpleExpr::String))
}

fn expr_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(move |expr| {
        let call = ident_parser()
            .then(just(Token::Bang).or_not().map(|i| i.is_some()))
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
            )
            .map(|((func, bang), args)| Expr::Call(CallExpr { func, bang, args }));

        let atom = call
            .or(simple_expr_parser().map(Expr::Simple))
            .or(expr.delimited_by(just(Token::OpenParen), just(Token::CloseParen)));

        let prec0 = just(Token::Bang)
            .to("!")
            .repeated()
            .then(atom)
            .foldr(|op, expr| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![expr],
                })
            });

        let prec1 = prec0
            .clone()
            .then(
                (choice([
                    just(Token::Star).to("*"),
                    just(Token::Slash).to("/"),
                    just(Token::Percent).to("%"),
                ]))
                .then(prec0)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec2 = prec1
            .clone()
            .then(
                (choice([just(Token::Plus).to("+"), just(Token::Minus).to("-")]))
                    .then(prec1)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec3 = prec2
            .clone()
            .then(
                (choice([
                    just(Token::LtEq).to("<="),
                    just(Token::Lt).to("<"),
                    just(Token::GtEq).to(">="),
                    just(Token::Gt).to(">"),
                    just(Token::EqEq).to("=="),
                    just(Token::BangEq).to("!="),
                ]))
                .then(prec2)
                .or_not(),
            )
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: false,
                    args: vec![lhs, rhs],
                })
            });

        let prec4 = prec3
            .clone()
            .then(just(Token::AndAnd).to("&&").then(prec3).repeated())
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: true,
                    args: vec![lhs, rhs],
                })
            });

        prec4
            .clone()
            .then(just(Token::OrOr).to("||").then(prec4).repeated())
            .foldl(|lhs, (op, rhs)| {
                Expr::Call(CallExpr {
                    func: op.to_string(),
                    bang: true,
                    args: vec![lhs, rhs],
                })
            })
    })
}

fn mutability_parser() -> impl Parser<Token, bool, Error = Simple<Token>> + Clone {
    just(Token::Mut).or_not().map(|m| m.is_some())
}

fn statement_without_block_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> + Clone
{
    let r#let = just(Token::Let)
        .ignore_then(mutability_parser())
        .then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|((mutable, name), value)| Statement::Let {
            decl: VarDecl { mutable, name },
            value,
        });

    let primitive_assign = place_parser()
        .then(choice([
            just(Token::PlusEq).to(AssignMode::Add),
            just(Token::MinusEq).to(AssignMode::Subtract),
            just(Token::Eq).to(AssignMode::Replace),
        ]))
        .then(expr_parser())
        .map(|((place, mode), value)| Statement::Assign { place, value, mode });

    let arithmetic_assign = place_parser()
        .then(choice([
            just(Token::StarEq).to("*"),
            just(Token::SlashEq).to("/"),
            just(Token::PercentEq).to("%"),
        ]))
        .then(expr_parser())
        .map(|((place, op), value)| Statement::Assign {
            place: place.clone(),
            value: Expr::Call(CallExpr {
                func: op.to_string(),
                bang: false,
                args: vec![Expr::Simple(SimpleExpr::Place(place)), value],
            }),
            mode: AssignMode::Replace,
        });

    #[derive(Clone)]
    enum ShortCircuitOp {
        And,
        Or,
    }

    let short_circuit_assign = place_parser()
        .then(choice([
            just(Token::AndAndEq).to(ShortCircuitOp::And),
            just(Token::OrOrEq).to(ShortCircuitOp::Or),
        ]))
        .then(expr_parser())
        .map(|((place, op), value)| {
            let [short_circuit, long_circuit] = [
                vec![],
                vec![Statement::Assign {
                    place: place.clone(),
                    value,
                    mode: AssignMode::Replace,
                }],
            ];
            let [false_case, true_case] = match op {
                ShortCircuitOp::And => [short_circuit, long_circuit],
                ShortCircuitOp::Or => [long_circuit, short_circuit],
            };
            Statement::Switch {
                cond: Expr::Simple(SimpleExpr::Place(place)),
                cases: vec![(0, false_case)],
                default: true_case,
            }
        });

    let assign = choice((primitive_assign, arithmetic_assign, short_circuit_assign));

    let r#return = just(Token::Return)
        .ignore_then(expr_parser())
        .map(Statement::Return);

    let r#break = just(Token::Break).to(Statement::Break);

    let r#continue = just(Token::Continue).to(Statement::Continue);

    let eval = expr_parser().map(Statement::Eval);

    choice((r#let, assign, r#return, r#break, r#continue, eval))
}

fn block_parser() -> impl Parser<Token, Vec<Statement>, Error = Simple<Token>> + Clone {
    recursive(move |block| {
        let if_else = recursive(|if_else| {
            just(Token::If)
                .ignore_then(expr_parser())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(choice((
                            if_else.map(|statement| vec![statement]),
                            block.clone(),
                        )))
                        .or_not(),
                )
                .map(|((cond, main_body), else_body)| Statement::Switch {
                    cond,
                    cases: vec![(0, else_body.unwrap_or_default())],
                    default: main_body,
                })
        });

        let r#loop = just(Token::Loop)
            .ignore_then(block.clone())
            .map(Statement::Loop);

        let r#while = just(Token::While)
            .ignore_then(expr_parser())
            .then(block.clone())
            .map(|(cond, body)| {
                Statement::Loop(vec![Statement::Switch {
                    cond,
                    cases: vec![(0, vec![Statement::Break])],
                    default: body,
                }])
            });

        let statement_with_block = recursive(move |statement_with_block| {
            let arm = just(Token::Arrow).ignore_then(
                statement_without_block_parser()
                    .then_ignore(just(Token::Comma))
                    .or(statement_with_block.then_ignore(just(Token::Comma).or_not()))
                    .map(Vec::from),
            );

            let switch = just(Token::Match)
                .ignore_then(expr_parser())
                .then(
                    int_parser()
                        .then(arm.clone())
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

            choice((
                if_else,
                r#loop,
                r#while,
                switch,
                block.map(Statement::Block),
            ))
        });

        let statement = statement_without_block_parser()
            .then_ignore(just(Token::Semi))
            .or(statement_with_block);

        statement
            .separated_by(just(Token::Semi).repeated())
            .allow_trailing()
            .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace))
    })
}

fn var_decl_parser() -> impl Parser<Token, VarDecl, Error = Simple<Token>> {
    mutability_parser()
        .then(ident_parser())
        .map(|(mutable, name)| VarDecl { mutable, name })
}

fn function_parser() -> impl Parser<Token, Function, Error = Simple<Token>> {
    just(Token::Fn)
        .ignore_then(ident_parser())
        .then(
            var_decl_parser()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::OpenParen), just(Token::CloseParen)),
        )
        .then(block_parser())
        .map(|((name, params), body)| Function { name, params, body })
}

pub fn ast_parser() -> impl Parser<Token, Ast, Error = Simple<Token>> {
    function_parser()
        .repeated()
        .then_ignore(end())
        .map(|functions| Ast { functions })
}
