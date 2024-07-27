mod compiler;

use bumpalo::Bump;
use citadel_api::backend::asm::{AsmBackend, TargetX86_64};
use compiler::Compiler;
use parcer::lexer::Lexer;
use parcer::parser::Parser;

use std::{fs, io};

const TEST_PATH: &str = "tests/main.c";
const ASM_OUT_PATH: &str = "build/asm/out.asm";

fn main() -> io::Result<()> {
    let file_content = fs::read_to_string(TEST_PATH).unwrap();
    let lexer = Lexer::new(&file_content);

    let parser_arena = Bump::new();
    let mut parser = Parser::new(lexer, &parser_arena);
    let ast = parser.parse();

    let compiler_arena = Bump::new();
    let compiler = Compiler::new(&compiler_arena);
    let ir_stream = compiler.compile_program(ast);

    citadel_api::compile!(AsmBackend::new(TargetX86_64), ir_stream).to_file(ASM_OUT_PATH.into())?;
    Ok(())
}
