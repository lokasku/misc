#![allow(unused)]

use crate::eval::Evaluator;
use crate::parser::TType;

impl Evaluator {
    pub fn add(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Integer(li + ri)),
                v => panic!("Expected integer, found {:?} ({}:{}).", v, line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Float(ld + rd)),
                v => panic!("Expected Float, found {:?} ({}:{}).", v, line, column)
            }
            v => panic!("Expected integer or Float, found {:?} ({}:{}).", v, line, column)
        }
    }

    pub fn sub(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Integer(li - ri)),
                v => panic!("Expected integer, found {:?} ({}:{}).", v, line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Float(ld - rd)),
                v => panic!("Expected Float, found {:?} ({}:{}).", v, line, column)
            }
            v => panic!("Expected integer or Float, found {:?} ({}:{}).", v, line, column)
        }
    }

    pub fn mul(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Integer(li * ri)),
                v => panic!("Expected integer, found {:?} ({}:{}).", v, line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Float(ld * rd)),
                v => panic!("Expected Float, found {:?} ({}:{}).", v, line, column)
            }
            v => panic!("Expected integer or Float, found {:?} ({}:{}).", v, line, column)
        }
    }

    pub fn div(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => {
                    if ri == 0 {panic!("Division by zero ({}:{}).", line, column)}
                    self.stack.push(TType::Integer(li / ri))
                }
                v => panic!("Expected integer, found {:?} ({}:{}).", v, line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Float(ld / rd)),
                v => panic!("Expected Float, found {:?} ({}:{}).", v, line, column)
            }
            v => panic!("Expected integer or Float, found {:?} ({}:{}).", v, line, column)
        }
    }

    pub fn mdl(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Integer(li % ri)),
                v => panic!("Expected integer, found {:?} ({}:{}).", v, line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Float(ld % rd)),
                v => panic!("Expected Float, found {:?} ({}:{}).", v, line, column)
            }
            v => panic!("Expected integer or Float, found {:?} ({}:{}).", v, line, column)
        }
    }

    pub fn exp(&mut self, line: usize, column: usize) {
        let lm = self.pop();
        let rm = self.pop();

        match lm {
            TType::Integer(li) => match rm {
                TType::Integer(ri) => self.stack.push(TType::Integer(i32::pow(li, ri as u32))),
                v => panic!("Expected integer, found {:?} ({}:{}).", v, line, column)
            }
            TType::Float(ld) => match rm {
                TType::Float(rd) => self.stack.push(TType::Float(f32::powf(ld, rd))),
                v => panic!("Expected Float, found {:?} ({}:{}).", v, line, column)
            }
            v => panic!("Expected integer or Float, found {:?} ({}:{}).", v, line, column)
        }
    }
}
