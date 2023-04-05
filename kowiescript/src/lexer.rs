// This is lexer struct which converts the source code into tokens.
// lexer uses ChrIterator to read the source code.

use std::iter::Peekable;

use crate::io::{ChrIterator, Input, WHITESPACES};

#[derive(Debug, PartialEq, Clone)]
enum Token {
    // Keywords
    Let,
    Const,
    True,
    False,
    If,
    For,
    Else,
    Fn,
    Return,
    Match,
    When,
    Then,
    Range,
    Either,
    As,
    In,
    End,
    Next,
    Comment(String),

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
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    // Identifiers and literals
    Identifier(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Vector(Vec<Token>),

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
                '/' => {
                    if let Some('/') = self.chr_iter.peek() {
                        let mut comment = String::new();
                        // to end of line keep string
                        while let Some(next_ch) = self.chr_iter.peek() {
                            if *next_ch != '\n' {
                                comment.push(*next_ch);
                                self.chr_iter.next();
                            } else {
                                break;
                            }
                        }
                        Token::Comment(comment)
                    } else {
                        Token::Slash
                    }
                }
                '%' => Token::Modulo,
                '<' => {
                    if let Some('=') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        Token::LEt
                    } else {
                        Token::Lt
                    }
                }
                '>' => {
                    if let Some('=') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        Token::GEt
                    } else {
                        Token::Gt
                    }
                }
                ',' => Token::Comma,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,
                _string if chr == '"' => {
                    let mut string = String::new();
                    while let Some(next_ch) = self.chr_iter.peek() {
                        if *next_ch != '"' {
                            string.push(*next_ch);
                            self.chr_iter.next();
                        } else {
                            self.chr_iter.next();
                            break;
                        }
                    }
                    Token::String(string)
                }
                _number if chr.is_numeric() => {
                    let mut number = String::new();
                    number.push(chr);
                    while let Some(next_ch) = self.chr_iter.peek() {
                        if next_ch.is_numeric() {
                            number.push(*next_ch);
                            self.chr_iter.next();
                        } else {
                            break;
                        }
                    }
                    if let Some('.') = self.chr_iter.peek() {
                        number.push('.');
                        self.chr_iter.next();
                        while let Some(next_ch) = self.chr_iter.peek() {
                            if next_ch.is_numeric() {
                                number.push(*next_ch);
                                self.chr_iter.next();
                            } else {
                                break;
                            }
                        }
                        Token::Float(number.parse::<f64>().unwrap())
                    } else {
                        Token::Integer(number.parse::<i64>().unwrap())
                    }
                }
                _ident if chr.is_alphanumeric() || chr == '_' => {
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
                        "const" => Token::Const,
                        "true" => Token::True,
                        "false" => Token::False,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "fn" => Token::Fn,
                        "as" => Token::As,
                        "match" => Token::Match,
                        "when" => Token::When,
                        "then" => Token::Then,
                        "to" => Token::Range,
                        "either" => Token::Either,
                        "ret" => Token::Return,
                        "end" => Token::End,
                        "next" => Token::Next,
                        "for" => Token::For,
                        "in" => Token::In,
                        _ => Token::Identifier(identifier),
                    }
                }
                _whitespace if WHITESPACES.contains(&(chr as u8)) => {
                    while let Some(next_ch) = self.chr_iter.peek() {
                        if WHITESPACES.contains(&(*next_ch as u8)) {
                            self.chr_iter.next();
                        } else {
                            break;
                        }
                    }
                    self.next_token()
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
            println!("token: {:?}", token);
            tokens.push(token.clone());
            if token == Token::EOF {
                break;
            }
        }
        tokens
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_simple_string_input() {
        let string = String::from("let  a = 5");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier(String::from("a")),
                Token::Assign,
                Token::Integer(5),
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_double_line_input() {
        let string = String::from("let  a = 5\nlet b = 10.2");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier(String::from("a")),
                Token::Assign,
                Token::Integer(5),
                Token::Let,
                Token::Identifier(String::from("b")),
                Token::Assign,
                Token::Float(10.2),
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_vectors() {
        let string = String::from("let a = [1, 2, 3]");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier(String::from("a")),
                Token::Assign,
                Token::LeftBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RightBracket,
                Token::EOF
            ]
        );
    }
    #[test]
    fn test_fn() {
        let string = String::from("fn test() {\nprint(\"test\")\nret 2\n}");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("test".to_string()),
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::Identifier("print".to_string()),
                Token::LeftParen,
                Token::String("test".to_string()),
                Token::RightParen,
                Token::Return,
                Token::Integer(2),
                Token::RightBrace,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_basic_file() {
        let mut lexer = Lexer::new(Input::File(String::from("src/tests/data/basic_file.ks")));
        let tokens = lexer.tokenize();

        println!("{:?}", tokens);

        /* assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("test".to_string()),
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::Identifier("print".to_string()),
                Token::LeftParen,
                Token::String("test".to_string()),
                Token::RightParen,
                Token::Identifier("ret".to_string()),
                Token::Integer(2),
                Token::RightBrace,
                Token::Const,
                Token::Identifier("b".to_string()),
                Token::Equal,
                Token::Integer(3),
                Token::Const,
                Token::Identifier("files".to_string()),
                Token::Equal,
                Token::LeftBracket,
                Token::String("test1".to_string()),
                Token::Comma,
                Token::String("test2".to_string()),
                Token::RightBracket,
                Token::Let,
                Token::Identifier("a".to_string()),
                Token::Equal,
                Token::Identifier("test".to_string()),
                Token::LeftParen,
                Token::RightParen,
                Token::Identifier("a".to_string()),
                Token::Equal,
                Token::Identifier("a".to_string()),
                Token::Plus,
                Token::Integer(1),
                Token::If,
                Token::Identifier("a".to_string()),
                Token::NotEqual,
                Token::Integer(5),
                Token::LeftBrace,
                Token::Identifier("print".to_string()),
                Token::LeftParen,
                Token::String("'a' is not a five".to_string()),
                Token::RightParen,
                Token::RightBrace,
                Token::For,
                Token::Identifier("file".to_string()),
                Token::In,
                Token::Identifier("files".to_string()),
                Token::LeftBrace,
                Token::Comment("// do sth with file".to_string()),
                Token::If,
                Token::Identifier("file".to_string()),
                Token::Equal,
                Token::String("end".to_string()),
                Token::LeftBrace,
                Token::End,
                Token::RightBrace,
                Token::If,
                Token::Identifier("file".to_string()),
                Token::Equal,
                Token::String("skip".to_string()),
                Token::LeftBrace,
                Token::Next,
                Token::RightBrace,
                Token::RightBrace,
            ]
        ); */
    }
}
