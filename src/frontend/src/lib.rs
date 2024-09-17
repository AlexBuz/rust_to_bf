pub mod ast;
mod lexer;
mod parser;

impl<'src> TryFrom<&'src str> for ast::Ast<'src> {
    type Error = anyhow::Error;

    fn try_from(src: &'src str) -> Result<Self, Self::Error> {
        use chumsky::Parser;

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
