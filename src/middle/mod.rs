pub mod ir;
mod lowering;

use crate::frontend::ast::Ast;

impl From<&Ast<'_>> for ir::Program {
    fn from(ast: &Ast<'_>) -> Self {
        lowering::lower(ast)
    }
}
