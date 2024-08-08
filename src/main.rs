mod ast;
mod bf;
mod compiler;
mod ir;
mod macros;

mod config {
    pub const DEBUG: bool = false;
    pub const EXECUTE_THROUGH_BF: bool = false;
    pub const PRINT_FINAL_STATE: bool = false;
}

use macros::debug_println;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let usage = "Usage: bfvm <path-to-source-file>";

    let path = std::env::args().nth(1).ok_or(usage)?;
    let src = std::fs::read_to_string(path)?;

    match ast::Ast::parse(&src) {
        Ok(ast) => {
            let program = compiler::compile(ast);
            debug_println!("{:#?}", program);
            let final_state = program.execute();
            if config::PRINT_FINAL_STATE {
                println!("Final memory state:\n{final_state}");
            }
            Ok(())
        }
        Err(parse_errs) => {
            for err in parse_errs {
                eprintln!("Parse error: {err}");
            }
            Err("Could not parse program".into())
        }
    }
}
