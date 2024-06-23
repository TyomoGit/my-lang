use eval::Evaluator;
use parser::Parser;
use scanner::Scanner;

mod ast;
mod parser;
mod scanner;
mod token;
mod eval;
mod value;

fn main() {
    let source = "true && 1";
    let mut scanner = Scanner::new(source.chars().collect());
    let tokens = scanner.scan_tokens().unwrap();
    // println!("{:#?}", tokens);
    let mut parser = Parser::new(tokens);
    let expr = parser.parse().unwrap();
    println!("{:#?}", expr);
    
    let evaluator = Evaluator::new(expr);
    let result = evaluator.evaluate();
    println!("{}", result);
}
