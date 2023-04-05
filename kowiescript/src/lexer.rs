// This is lexer struct which converts the source code into tokens.
// lexer uses ChrIterator to read the source code.

use std::iter::Peekable;

use crate::io::{ChrIterator, Input};

#[derive(Debug, PartialEq)]
enum Token {
    // Keywords
    Let,
    True,
    False,
    If,
    Else,
    Fn,
    Return,

    // Symbols
    Equal,
    NotEqual,
    Lt,
    LEt,
    Gt,
    GEt,
    Bang,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Modulo,

    DoubleSlash,

    Assign,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Match,
    When,
    Then,
    Range,
    Either,
    As,

    // Identifiers and literals
    Identifier(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),

    // End of file
    EOF,

    // Temp token for match exhaustion
    Err,
}


pub struct Lexer<'a> {
    chr_iter: Peekable<ChrIterator<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: Input) -> Lexer<'a> {
        Lexer {
            chr_iter: ChrIterator::new(input).unwrap(),
        }
    }

    fn next_token(&mut self) -> Token {
        match self.chr_iter.next() {
            Some(chr) => match chr {
                '=' => {
                    if let Some('=') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                }
                '+' => Token::Plus,
                '-' => Token::Minus,
                '!' => {
                    if let Some('=') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        Token::NotEqual
                    } else {
                        Token::Bang
                    }
                }
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '<' => Token::Lt,
                '>' => Token::Gt,
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '(' => Token::Lparen,
                ')' => Token::Rparen,
                '{' => Token::Lbrace,
                '}' => Token::Rbrace,
                '[' => Token::Lbracket,
                ']' => Token::Rbracket,
                _ if chr.is_alphanumeric() || chr == '_' => {
                    let mut identifier = String::new();
                    identifier.push(chr);
                    while let Some(next_ch) = self.chr_iter.peek() {
                        if next_ch.is_alphanumeric() || *next_ch == '_' {
                            identifier.push(*next_ch);
                            self.chr_iter.next();
                        } else {
                            break;
                        }
                    }
                    match identifier.as_str() {
                        "let" => Token::Let,
                        "true" => Token::True,
                        "false" => Token::False,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "fn" => Token::Fn,
                        "return" => Token::Return,
                        _ => Token::Identifier(identifier),
                    }
                }
                _ => Token::Err,
            },
            None => Token::EOF,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token == Token::EOF {
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_string_input() {
        let string = String::from("let a = 5");
        let mut lexer = Lexer::new(Input::String(string));
        let token = lexer.tokenize();
    }
}
