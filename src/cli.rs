use {
    crate::{
        backend::bf,
        common::{debug_println, DEBUG},
        frontend::ast::Ast,
        middle::ir,
    },
    clap::{Parser, Subcommand, ValueEnum},
    std::{path::PathBuf, sync::atomic::Ordering},
};

#[derive(Debug, Parser)]
pub struct Cli {
    #[command(subcommand)]
    command: Command,

    /// Enable debug logging
    #[arg(short, long)]
    debug: bool,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Compile a program to BF
    Compile {
        /// Input source file
        input_path: PathBuf,

        #[command(flatten)]
        compile_options: CompileOptions,
    },
    /// Run a program
    Run {
        /// Input source file
        input_path: PathBuf,

        #[command(flatten)]
        run_options: RunOptions,
    },
}

#[derive(Debug, Parser)]
struct CompileOptions {
    /// Output file for generated BF code [leave unspecified for stdout]
    #[arg(short)]
    output_path: Option<PathBuf>,
}

#[derive(ValueEnum, Clone, Debug)]
enum Stage {
    Ir,
    Bf,
}

#[derive(Debug, Parser)]
struct RunOptions {
    /// Compilation stage to run
    #[arg(long, default_value = "ir")]
    stage: Stage,

    /// Print the final memory state after execution
    #[arg(long)]
    print_memory: bool,
}

pub(crate) fn run() -> anyhow::Result<()> {
    let cli = Cli::parse();
    DEBUG.store(cli.debug, Ordering::Relaxed);
    let (input_path, compile_options, run_options) = match cli.command {
        Command::Compile {
            input_path,
            compile_options,
        } => (input_path, Some(compile_options), None),
        Command::Run {
            input_path,
            run_options,
        } => (input_path, None, Some(run_options)),
    };
    let src = std::fs::read_to_string(input_path)?;
    let ast = Ast::try_from(&*src)?;
    debug_println!("{ast:#?}");
    let ir_program = ir::Program::from(&ast);
    debug_println!("{ir_program:#?}");
    let bf_program = bf::Program::from(&ir_program);
    if let Some(compile_options) = compile_options {
        if let Some(output_path) = compile_options.output_path {
            std::fs::write(output_path, bf_program.to_string())?;
        } else {
            println!("{bf_program}");
        }
    }
    if let Some(run_options) = run_options {
        let final_state = match run_options.stage {
            Stage::Ir => ir_program.execute(),
            Stage::Bf => bf_program.execute(),
        };
        if run_options.print_memory {
            println!("final memory state:\n{final_state}");
        }
    }
    Ok(())
}
