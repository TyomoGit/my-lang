use std::path::PathBuf;

use error::fmt_errors;
use eval::Evaluator;
use parser::Parser;
use scanner::Scanner;

mod ast;
mod error;
mod eval;
mod parser;
mod scanner;
mod token;
mod value;

fn main() {
    let (source, file_path) = if std::env::args().len() > 1 {
        let path = std::env::args().nth(1).unwrap();
        (
            std::fs::read_to_string(&path)
                .unwrap()
                .trim_end()
                .to_string(),
            Some(PathBuf::from(path)),
        )
    } else {
        let mut source = String::new();
        std::io::stdin().read_line(&mut source).unwrap();
        (source.trim_end().to_string(), None)
    };

    let mut scanner = Scanner::new(source.chars().collect());
    let tokens = match scanner.scan_tokens() {
        Ok(tokens) => tokens,
        Err(errors) => {
            let msg = fmt_errors(source.lines(), &errors, file_path.as_deref());
            println!("{}", msg);
            return;
        }
    };
    // println!("{:#?}", tokens);
    let mut parser = Parser::new(tokens);
    let expr = match parser.parse() {
        Ok(expr) => expr,
        Err(errors) => {
            let msg = fmt_errors(source.lines(), &errors, file_path.as_deref());
            println!("{}", msg);
            return;
        }
    };
    // println!("{:#?}", expr);

    let mut evaluator = Evaluator::new();
    evaluator.evaluate(&expr);
}
