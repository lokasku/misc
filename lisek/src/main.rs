mod parser;
mod declare;
mod eval;
mod formatter;
mod builtins;
mod arithmetic;
mod bool;

use parser::{Parser, Token};
use formatter::Formatter;
use eval::Evaluator;

use std::fs;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {panic!("You must provide filecode.")}

    let content = fs::read_to_string(args[1].clone()).expect("Cannot read file for some reasons.");
    let mut parser = Parser::new(content);
    parser.parse();
    // dbg!(&parser.output);
    
    let mut formated_ast: Vec<Token> = vec![];
    Formatter::formatter(parser.output, &mut formated_ast, 0, 0);
    // dbg!(&formated_ast);

    let mut evaluator = Evaluator::new(parser.values);
    evaluator.eval(formated_ast);
}