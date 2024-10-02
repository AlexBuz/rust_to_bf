use {
    backend::bf,
    clap::{Parser, Subcommand, ValueEnum},
    frontend::ast::Ast,
    middle::ir::{self, Execute},
    std::path::PathBuf,
};

#[derive(Debug, Parser)]
pub struct Cli {
    #[command(subcommand)]
    command: Command,
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
enum Interpreter {
    Ir,
    Bf,
}

#[derive(Debug, Parser)]
struct RunOptions {
    /// Interpreter to use
    #[arg(long, default_value = "ir")]
    interpreter: Interpreter,

    /// Print the final memory state after execution
    #[arg(long)]
    print_memory: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
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
    let ir_program = ir::Program::from(&ast);
    let bf_program = bf::Program::from(&ir_program);
    if let Some(compile_options) = compile_options {
        if let Some(output_path) = compile_options.output_path {
            std::fs::write(output_path, bf_program.to_string())?;
        } else {
            println!("{bf_program}");
        }
    }
    if let Some(run_options) = run_options {
        let stdin = &mut std::io::stdin().lock();
        let stdout = &mut std::io::stdout().lock();
        let final_state = match run_options.interpreter {
            Interpreter::Ir => ir_program.execute(stdin, stdout),
            Interpreter::Bf => bf_program.execute(stdin, stdout),
        };
        if run_options.print_memory {
            println!("{final_state:?}");
        }
    }
    Ok(())
}
