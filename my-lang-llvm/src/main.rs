use compiler::Compiler;
use inkwell::{context::Context, module::Module, OptimizationLevel};
use my_lang_parser::{parser::Parser, scanner::Scanner};

mod compiler;

fn main() {
    let source = "1 + 2 + 3 + 4";
    let mut scanner = Scanner::new(source.chars().collect());
    let tokens = scanner.scan_tokens().unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();

    let context = Context::create();
    let compiler = Compiler::new(&context);
    compiler.compile(&ast);

    let module = compiler.module;
    run_jit(module.clone());
    module.print_to_file("out.ll").unwrap();
}

fn run_jit(module: Module) {
    let engine = module
        .create_jit_execution_engine(OptimizationLevel::Aggressive)
        .unwrap();
    unsafe {
        engine
            .get_function::<unsafe extern "C" fn()>("main")
            .unwrap()
            .call();
    }
}
