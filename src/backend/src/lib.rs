pub mod bf;
mod codegen;

use middle::ir;

impl From<&ir::Program> for bf::Program {
    fn from(program: &ir::Program) -> Self {
        codegen::compile(program)
    }
}
