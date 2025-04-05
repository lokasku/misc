use crate::{
    parser::{Token, TType},
    formatter::Formatter
};

impl TType {
    pub fn displayable(&self) -> String {
        match self {
            TType::Integer(i) => format!("i:{}", i),
            TType::Float(fl) => format!("f:{}", fl),
            TType::String(s) => format!("{:?}", s),
            TType::SParen(_) => format!("Scope of Backets"),
            TType::SBrac(_) => format!("Scope of Braces"),
            TType::Ident(_) => format!("Identifier"),
            TType::Builtin(_) => format!("Builtin"),
            TType::Bool(b) => format!("b:{}", b),
            _ => "Unexpected".to_owned()
        }
    }
}

pub struct Evaluator {
    pub stack: Vec<TType>,
    pub values: Vec<Token>,
    builtins: Vec<fn(&mut Evaluator, usize, usize)>
}

impl Evaluator {
    pub fn new(values: Vec<Token>) -> Self {
        Self {
            stack: vec![],
            values,
            builtins: vec![
                Evaluator::add, Evaluator::sub, Evaluator::mul, Evaluator::div,
                Evaluator::mdl, Evaluator::exp, Evaluator::low, Evaluator::sup,
                Evaluator::eq, Evaluator::uneq, Evaluator::format, Evaluator::conc,
                Evaluator::cond, Evaluator::looop
            ]
        }
    }

    pub fn pop(&mut self) -> TType {
        match self.stack.pop() {
            Some(v) => return v,
            None => panic!("Stack Underflow.")
        }
    }
    
    pub fn advance(&mut self, tok: Token) {
        match tok {
            Token {ttype, line, column} => match ttype {
                TType::SParen(v) | TType::Expr(v) => for t in v {
                    self.advance(t);
                }
                TType::SBrac(c) => self.stack.push(TType::SBrac(c)),
                TType::String(s) => {self.stack.push(TType::String(s))},
                TType::Integer(i) => {self.stack.push(TType::Integer(i))},
                TType::Float(f) => {self.stack.push(TType::Float(f))},
                TType::Builtin(id) => {self.builtins[id](self, line, column)},
                TType::Ident(id) => {
                    self.advance(Formatter::unit_format(vec![self.values.get(id).unwrap().clone()], 0).0);
                    // self.advance(self.values.get(id).unwrap().clone())
                },
                _ => {}
            }
        }
    }
    
    pub fn eval(&mut self, input: Vec<Token>) {
        for t in input.clone() {
            self.advance(t);
        }
    }
}
