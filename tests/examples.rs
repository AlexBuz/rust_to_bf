use {
    middle::ir::Execute,
    rstest::rstest,
    rstest_reuse::{apply, template},
    std::path::PathBuf,
};

#[template]
#[rstest]
fn examples(#[files("example_programs/*.rs")] path: PathBuf) {}

#[apply(examples)]
fn execute_ir(path: PathBuf) {
    let example = ExampleProgram::from(path);
    let mut output = Vec::new();
    example
        .ir_program
        .execute(&mut example.input.as_bytes(), &mut output);
    assert_eq!(String::from_utf8(output).unwrap(), example.expected_output);
}

#[apply(examples)]
fn execute_bf(path: PathBuf) {
    let example = ExampleProgram::from(path);
    let bf_program = backend::bf::Program::from(&example.ir_program);
    let mut output = Vec::new();
    bf_program.execute(&mut example.input.as_bytes(), &mut output);
    assert_eq!(String::from_utf8(output).unwrap(), example.expected_output);
}

struct ExampleProgram {
    ir_program: middle::ir::Program,
    input: String,
    expected_output: String,
}

impl From<PathBuf> for ExampleProgram {
    fn from(mut path: PathBuf) -> Self {
        let code = std::fs::read_to_string(&path).unwrap();
        let ast = frontend::ast::Ast::try_from(&*code).unwrap();
        let ir_program = middle::ir::Program::from(&ast);

        path.set_extension("stdin");
        let input = std::fs::read_to_string(&path).unwrap_or_default();

        path.set_extension("stdout");
        let expected_output = std::fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("failed to read expected output file at path {path:?}: {e}")
        });

        ExampleProgram {
            ir_program,
            input,
            expected_output,
        }
    }
}
