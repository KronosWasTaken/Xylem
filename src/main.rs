mod lexer;
mod parser;
mod ast;
mod semantic;
mod bytecode;
mod codegen_bytecode;

use std::fs;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <sourcefile.xl>", args[0]);
        std::process::exit(1);
    }
    let filename = &args[1];
    let source = std::fs::read_to_string(filename).expect("Failed to read source file");
    let mut lexer = lexer::Lexer::new(&source);
    let tokens = lexer.tokenize_all();
    // Remove or comment out any debug print of tokens or AST

    let mut parser = parser::Parser::new(tokens);
    match parser.parse_program() {
        Ok(program) => {
            // Semantic analysis
            let mut analyzer = semantic::SemanticAnalyzer::new();
            analyzer.analyze(&program);
            if !analyzer.errors.is_empty() {
                for err in analyzer.errors {
                    println!("Semantic error: {:?}", err);
                }
                return;
            }
            // Codegen: AST -> Bytecode
            let mut cg = codegen_bytecode::CodegenContext::new();
            cg.gen_program(&program);
            // Run in the VM with user-defined functions
            let mut vm = bytecode::VM::with_functions(cg.chunk, cg.functions);
            let result = vm.run();
            if result != bytecode::Value::None {
                println!("[VM Result] {}", result);
            }
        },
        Err(e) => println!("Parse error: {}", e),
    }
}
