#![allow(unused)]

use crate::eval::Evaluator;
use crate::parser::TType;

impl Evaluator {
    pub fn format(&mut self, line: usize, column: usize) {
        let to_display = self.pop();
        println!("{}", to_display.displayable());
    }

    pub fn conc(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::String(ls) => match rm {
                TType::String(rs) => self.stack.push(TType::String(format!("{}{}", ls, rs))),
                t => panic!("Expected String, find {} ({}:{}).", t.displayable(), line, column)
            }
            t => panic!("Expected String, find {} ({}:{}).", t.displayable(), line, column)
        }
    }

    pub fn cond(&mut self, line: usize, column: usize) {
        let cond = self.pop();
        let then_do = self.pop();
        let else_do = self.pop();

        match cond {
            TType::Bool(true) => if let TType::SBrac(v) = then_do {
                self.eval(v);
            } else {panic!("eee")},
            TType::Bool(false) => if let TType::SBrac(v) = else_do {
                self.eval(v);
            } else {panic!("aaa")},
            t => panic!("Expected boolean statment, find {} ({}:{}).", t.displayable(), line, column)
        }
    }

    pub fn looop(&mut self, line: usize, column: usize) {
        if let TType::SBrac(v) = self.pop() {
            loop {
                self.eval(v.clone());
            }
        } else {
            panic!("Expected SBrac, find something else ({}:{}).", line, column);
        }
    }
}
