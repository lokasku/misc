use crate::parser::{Parser, Token, TType};

impl Parser {
    pub fn identifiers(&mut self) -> Option<Token> {
        let stop = vec![' ', ')', '(', ']', '[', '{', '}', '\n', '\r'];
        while !stop.contains(&self.peek(0)) {self.advance();}
        let ident = self.input[self.start..self.current].to_string();

        match ident.as_str() {
            "let" => self.declaration(),
            x if self.builtins.contains(&x.to_owned()) => Some(Token::new(
                TType::Builtin(self.builtins.iter().position(|i| i == x).unwrap() as usize),
                self.line,
                self.column)),
            x if self.symbols.contains(&x.to_owned()) => Some(Token::new(
                TType::Ident(self.symbols.iter().position(|v| v == x).unwrap() as usize),
                self.line,
                self.column)),
            x => panic!("Undefined variable: `{}` ({}:{})", x, self.line, self.column)
        }
    }
    pub fn declaration(&mut self) -> Option<Token> {
        self.skip_blanks();

        while self.peek(0) != ' ' {
            self.advance();
        }
        let name = self.input[self.start..self.current].to_string();
        self.skip_blanks();
        self.current -= 1;

        let token = self.unit_parse().unwrap();
        match token.ttype {
            TType::SParen(_) | TType::Integer(_) | TType::Float(_) | TType::String(_) | TType::Builtin(_) => {},
            t => panic!("Expected SParen, identifier, decimal, float or string, find `{:?}` ({}:{}).", t, self.line, self.column)
        }

        if self.symbols.contains(&name) {
            let index = self.symbols.iter().position(|i| i == &name).unwrap() as usize;
            self.values[index] = token;
        } else {
            self.symbols.push(name);
            self.values.push(token);
        }
        None
    }
}
