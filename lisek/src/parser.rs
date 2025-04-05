#![allow(unused)]

use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter, Result};
use crate::eval::Evaluator;
use crate::declare::*;

#[derive(Clone)]
pub enum TType {
    Integer(i32),
    Float(f32),
    String(String),
    SParen(Vec<Token>), // ()
    SBrac(Vec<Token>),  // {}
    Ident(usize),
    Builtin(usize),
    Bool(bool),
    Expr(Vec<Token>)
}

impl Debug for TType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TType::Integer(i) => write!(f, "Integer({})", i),
            TType::Float(fl) => write!(f, "Float({})", fl),
            TType::String(s) => write!(f, "String({:?})", s),
            TType::SParen(t) => write!(f, "SParen({:?})", t),
            TType::SBrac(t) => write!(f, "SBrac({:?})", t),
            TType::Ident(i) => write!(f, "Ident({})", i),
            TType::Builtin(i) => write!(f, "Builtin({})", i),
            TType::Bool(b) => write!(f, "Bool({})", b),
            TType::Expr(v) => write!(f, "Expr({:#?})", v)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TType,
    pub line: usize,
    pub column: usize
}

impl Token {
    pub fn new(ttype: TType, line: usize, column: usize) -> Self {
        Self {
            ttype,
            line,
            column
        }
    }
}

pub struct Parser {
    pub input: String,
    pub output: Vec<Token>,
    pub line: usize,
    pub column: usize,
    pub current: usize,
    pub start: usize,
    pub symbols: Vec<String>,
    pub values: Vec<Token>,
    pub builtins: Vec<String>
}

impl Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "input: {:#?}, output: {:#?}, line: {}, column: {}, current: {}, start: {}, symbols: {:#?}, values: {:#?}, builtins: {:?}",
               self.input, self.output, self.line, self.column, self.current, self.start, self.symbols, self.values, self.builtins)
    }
}

impl Parser {
    pub fn new(input: String) -> Self {
        let mut parser = Self {
            input,
            output: vec![],
            line: 1,
            column: 1,
            current: 0,
            start: 0,
            symbols: vec![],
            values: vec![],
            builtins: vec![]
        };

        parser.add_builtin("+"); // 0
        parser.add_builtin("-"); // 1
        parser.add_builtin("*"); // 2
        parser.add_builtin("/"); // 3
        parser.add_builtin("%"); // 4
        parser.add_builtin("^"); // 5

        parser.add_builtin("<"); // 6
        parser.add_builtin(">"); // 7
        parser.add_builtin("="); // 8
        parser.add_builtin("!="); // 9

        parser.add_builtin("format"); // 10
        parser.add_builtin("conc"); // 11
        parser.add_builtin("cond"); // 12
        parser.add_builtin("loop"); // 13
        parser
    }

    fn add_builtin(&mut self, name: &str) {
        self.builtins.push(name.to_string());
    }

    pub fn is_eof(&self) -> bool {
        self.current >= self.input.chars().count() - 1
    }

    pub fn peek(&self, k: usize) -> char {
        self.input
        .chars()
        .nth(self.current + k)
        .expect("Unexpected EOF while peeking.")
    }

    pub fn advance(&mut self) -> char {
        self.current += 1;
        match self.input.chars().nth(self.current - 1).unwrap() {
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            _ => self.column += 1
        }
        self.input
            .chars()
            .nth(self.current)
            .expect("Unexpected EOF while advancing.")
    }

    pub fn skip_blanks(&mut self) {
        while self.peek(0) == ' ' {
            self.advance();
        }
        self.start = self.current
    }

    pub fn string(&mut self) -> TType {
        self.advance();
        self.start = self.current;
        while self.peek(0) != '"' {
            self.advance();
        }
        let string = self.input[self.start..self.current].to_string();

        TType::String(string)
    }

    pub fn number(&mut self) -> TType {
        if self.peek(0) == '-' {
            self.advance();
        }

        while self.peek(0).is_digit(10) || self.peek(0) == '.' {
            self.advance();
        }

        let num = self.input[self.start..self.current].to_string();

        match num.parse::<i32>() {
            Ok(n) => TType::Integer(n),
            Err(_) => TType::Float(num.parse::<f32>().unwrap())
        }
    }

    pub fn stuck(&mut self, delimiter: char) -> Vec<Token> {
        let mut content = vec![];
        while self.peek(0) != delimiter {
            match self.unit_parse() {
                Some(x) => content.push(x),
                None => {}
            }
        }
        self.current += 1;
        content
    }

    pub fn unit_parse(&mut self) -> Option<Token> {
        let cc = self.advance();
        if self.current != 1 {self.start = self.current};

        match cc {
            '\n' | '\r' | ' ' | ')' | '}' => None,
            '|' => {
                while self.peek(0) != '\n' {self.advance();}
                self.line += 1;
                None
            }
            '"' => Some(Token::new(self.string(), self.line, self.column)),
            '(' => Some(Token::new(TType::SParen(self.stuck(')')), self.line, self.column)),
            '{' => Some(Token::new(TType::SBrac(self.stuck('}')), self.line, self.column)),
            x => if x.is_digit(10) || x == '-' && self.peek(1).is_digit(10) {
                Some(Token::new(self.number(), self.line, self.column))
            } else {
                self.identifiers()
            }
        }
    }

    pub fn parse(&mut self) {
        while !self.is_eof() {
            match self.unit_parse() {
                Some(tok) => self.output.push(tok),
                None => {}
            }
        }
    }
}
