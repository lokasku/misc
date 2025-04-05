#![allow(unused)]

use crate::eval::Evaluator;
use crate::parser::TType;

impl Evaluator {
    pub fn low(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Bool(li < ri)),
                t => panic!("Expected integer, find {} ({}:{}).", t.displayable(), line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Bool(ld < rd)),
                t => panic!("Expected decimal, findd {} ({}:{}).", t.displayable(), line, column)
            }
            t => panic!("Expected integer or decimal, find {} ({}:{}).", t.displayable(), line, column)
        }
    }

    pub fn sup(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Bool(li > ri)),
                t => panic!("Expected integer, find {} ({}:{}).", t.displayable(), line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Bool(ld > rd)),
                t => panic!("Expected decimal, find {} ({}:{}).", t.displayable(), line, column)
            }
            t => panic!("Expected integer or decimal, find {} ({}:{}).", t.displayable(), line, column)
        }
    }

    pub fn eq(&mut self, line: usize, column: usize) {
        let rm = self.pop();
        let lm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Bool(li == ri)),
                t => panic!("Expected integer, find {} ({}:{}).", t.displayable(), line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Bool(ld == rd)),
                t => panic!("Expected decimal, find {} ({}:{}).", t.displayable(), line, column)
            },
            TType::String(ls) => match rm {
                TType::String(rs) => self.stack.push(TType::Bool(ls == rs)),
                t => panic!("Expected string, find {} ({}:{}).", t.displayable(), line, column)
            }
            t => panic!("Expected integer, decimal or string, find {} ({}:{}).", t.displayable(), line, column)
        }
    }

    pub fn uneq(&mut self, line: usize, column: usize) {
        let rm = self.pop();
        let lm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Bool(li != ri)),
                t => panic!("Expected integer, find {} ({}:{}).", t.displayable(), line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Bool(ld != rd)),
                t => panic!("Expected decimal, find {} ({}:{}).", t.displayable(), line, column)
            },
            TType::String(ls) => match rm {
                TType::String(rs) => self.stack.push(TType::Bool(ls != rs)),
                t => panic!("Expected string, find {} ({}:{}).", t.displayable(), line, column)
            }
            t => panic!("Expected integer, decimal or string, find {} ({}:{}).", t.displayable(), line, column)
        }
    }
}
