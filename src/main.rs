mod ast;
mod bf;
mod common;
mod compiler;
mod ir;
mod lexer;
mod parser;

use {
    clap::{Parser, ValueEnum},
    common::debug_println,
    ir::Program,
    std::{
        cell::LazyCell,
        sync::atomic::{AtomicBool, Ordering},
    },
};

static DEBUG: AtomicBool = AtomicBool::new(false);

#[derive(Clone, ValueEnum, Parser)]
enum ExecuteMode {
    Ir,
    Bf,
    None,
}

#[derive(Parser)]
struct Args {
    source_file: String,
    #[clap(short, long, help = "Print the generated BF code")]
    print_bf: bool,
    #[clap(
        short,
        long = "execute",
        default_value = "ir",
        help = "Execute the program, either by interpreting the intermediate representation (faster) or the generated BF code (slower), or don't execute it at all"
    )]
    execute_mode: ExecuteMode,
    #[clap(
        short = 'm',
        long,
        help = "Print the final memory state (stack and heap) after execution, if execution is enabled"
    )]
    print_final_memory: bool,
    #[arg(short, long, help = "Enable debug logging")]
    debug: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    DEBUG.store(args.debug, Ordering::Relaxed);
    let src = std::fs::read_to_string(args.source_file)?;
    let ast = ast::Ast::parse(&src)?;
    debug_println!("{ast:#?}");
    let program = compiler::compile(&ast);
    debug_println!("{program:#?}");
    let bf_code = LazyCell::new(|| program.convert_to_bf());
    if args.print_bf {
        println!("{}", *bf_code);
    }
    let final_state = match args.execute_mode {
        ExecuteMode::Ir => program.execute(),
        ExecuteMode::Bf => Program::execute_bf(&bf_code),
        ExecuteMode::None => return Ok(()),
    };
    if args.print_final_memory {
        println!("final memory state:\n{final_state}");
    }
    Ok(())
}
