mod compiler;

use bumpalo::Bump;
use compiler::Compiler;
use parcer::lexer::Lexer;
use parcer::parser::Parser;

use std::{fs, io};

const TEST_PATH: &str = "tests/main.c";

fn main() -> io::Result<()> {
    let file_content = fs::read_to_string(TEST_PATH).unwrap();
    let lexer = Lexer::new(&file_content);
    let binding = Bump::new();
    let mut parser = Parser::new(lexer, &binding);
    let ast = parser.parse();
    let ir_stream = Compiler::compile_program(ast);
    fs::write("main.chir", ir_stream.to_string())?;
    Ok(())
}
