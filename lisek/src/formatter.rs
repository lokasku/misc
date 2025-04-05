use crate::parser::{Token, TType};

pub struct Formatter {}

impl Formatter {
    pub fn unit_format(tokens: Vec<Token>, start_to: usize) -> (Token, usize)  {
        let token = tokens[start_to].clone();
        match token {
            Token {ttype: TType::Builtin(func), line, column} => {
                let arity = match func {
                    0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 11 => 2,
                    10 | 13 => 1,
                    12 => 3,
                    _ => 0 // will never happen
                }; 
                let mut res: Vec<Token> = vec![token];
                let mut position = start_to + 1;
                for _ in 0..arity {
                    let r = Self::unit_format(tokens.clone(), position);
                    res.push(r.0);
                    position += 1; 
                }
                let expr = TType::Expr(res);
                (Token::new(expr, line, column), position as usize)
            }
            Token {ttype: TType::SParen(v), line, column} => {
                let mut result: Vec<Token> = vec![];
                Self::formatter(v, &mut result, 0, 0);
                (Token::new(TType::SParen(result.clone()), line, column), 0)
            }
            Token {ttype: TType::SBrac(v), line, column} => {
                let mut result: Vec<Token> = vec![];
                Self::formatter(v, &mut result, 0, 0);
                (Token::new(TType::SBrac(result), line, column), 0)
            }
            t => (t, 0)
        }

    }

    pub fn formatter(tokens: Vec<Token>, result: &mut Vec<Token>, start_to: usize, mut pos: usize) {
        let r = Self::unit_format(tokens.clone(), start_to);
        match r {
            (Token {ttype: TType::Expr(v), line, column}, n) => {
                result.push(Token::new(TType::Expr(v.into_iter().rev().collect()), line, column));
                pos += n;
                if pos >= tokens.len() - 1 {}
                else {
                    Self::formatter(tokens, result, pos, pos);
                }
            }
            _ => panic!("Every instruction must start with a Builtin, or an assignment.")
        }
    }
}
