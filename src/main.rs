mod ast;
mod bf;
mod compiler;
mod ir;

fn main() {
    // let program = ir::example_programs::fibonacci();
    // let final_state = program.execute();
    // println!("{final_state}");
    let usage = "Run `cargo run -- <path-to-source-file>`";
    let src = std::fs::read_to_string(std::env::args().nth(1).expect(usage)).expect(usage);
    // dbg!(&src);

    match ast::Ast::parse(&src) {
        Ok(ast) => {
            // println!("{:#?}", ast);
            let program = compiler::compile(ast);
            println!("{:#?}", program);
            let final_state = program.execute();
            println!("{final_state}");
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    };
}
