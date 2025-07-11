mod lexer;
mod parser;
mod ast;
mod utils;
mod semantic;

use std::fs;

fn main() {
    let source = fs::read_to_string("example.xl").expect("Failed to read file");
    let mut lexer = lexer::Lexer::new(&source);
    let tokens = lexer.tokenize_all();
    println!("Tokens: {:#?}", tokens);

    let mut parser = parser::Parser::new(tokens);
    match parser.parse_program() {
        Ok(ast) => {
            println!("AST: {:#?}", ast);
            let mut analyzer = semantic::SemanticAnalyzer::new();
            analyzer.analyze(&ast);
            if analyzer.errors.is_empty() {
                println!("Semantic analysis passed: no scope errors.");
            } else {
                println!("Semantic errors:");
                for err in analyzer.errors {
                    println!("  {:?}", err);
                }
            }
        },
        Err(e) => println!("Parse error: {}", e),
    }
}
