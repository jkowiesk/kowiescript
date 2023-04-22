// This is lexer struct which converts the source code into tokens.
// lexer uses ChrIterator to read the source code.

use std::{iter::Peekable, error::Error, fmt::{Display, self, format}};

use crate::io::{ChrIterator, Input, WHITESPACES};

pub const ESCAPE_CHARACTERS: [char; 5] = ['\\', '\"', '\t', '\n', '\r'];


#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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
    String(String),
    Vector(String),

    // End of file
    EOF,

    // Temp token for match exhaustion
    Err,
}

pub struct Lexer<'a> {
    chr_iter: Peekable<ChrIterator<'a>>,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: Input) -> Lexer<'a> {
        Lexer {
            chr_iter: ChrIterator::new(input).unwrap(),
            line: 1,
        }
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        match self.chr_iter.next() {
            Some(chr) => match chr {
                '=' => {
                    if let Some('=') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        Ok(Token::Equal)
                    } else {
                        Ok(Token::Assign)
                    }
                }
                '+' => Ok(Token::Plus),
                '-' => Ok(Token::Minus),
                '!' => {
                    if let Some('=') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        Ok(Token::NotEqual)
                    } else {
                        Ok(Token::Bang)
                    }
                }
                '*' => Ok(Token::Asterisk),
                '/' => {
                    if let Some('/') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        let mut comment = String::new();

                        while let Some(next_ch) = self.chr_iter.peek() {
                            if *next_ch != '\n' {
                                comment.push(*next_ch);
                                self.chr_iter.next();
                            } else {
                                break;
                            }
                        }
                        Ok(Token::Comment(comment))
                    } else {
                        Ok(Token::Slash)
                    }
                }
                '%' => Ok(Token::Modulo),
                '<' => {
                    if let Some('=') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        Ok(Token::LEt)
                    } else {
                        Ok(Token::Lt)
                    }
                }
                '>' => {
                    if let Some('=') = self.chr_iter.peek() {
                        self.chr_iter.next();
                        Ok(Token::GEt)
                    } else {
                        Ok(Token::Gt)
                    }
                }
                ',' => Ok(Token::Comma),
                '(' => Ok(Token::LeftParen),
                ')' => Ok(Token::RightParen),
                '{' => Ok(Token::LeftBrace),
                '}' => Ok(Token::RightBrace),
                '[' => Ok(Token::LeftBracket),
                ']' => Ok(Token::RightBracket),
                ';' => Ok(Token::Semicolon),
                _whitespace if WHITESPACES.contains(&(chr as u8)) => {
                    while let Some(next_ch) = self.chr_iter.peek() {
                        if WHITESPACES.contains(&(*next_ch as u8)) {
                            self.chr_iter.next();
                        } else if *next_ch == '\n' {
                            self.line += 1;
                            self.chr_iter.next();
                        } else {
                            break;
                        }
                    }
                    self.next_token()
                },
                _string if chr == '"' => self.string_to_token(),
                _number if chr.is_numeric() => self.number_to_token(chr),
                _ident if chr.is_alphanumeric() => self.identifier_to_token(chr),
                _ => Ok(Token::Err),
            },
            None => Ok(Token::EOF),
        }
    }

    fn handle_escape_chars(&mut self, string: &mut String) {
        if let Some(next_ch) = self.chr_iter.peek() {
            match *next_ch {
                '\n' => {
                    string.push('\\');
                    string.push('n');
                }
                _ => {
                    string.push('#');
                }
            }
            self.chr_iter.next();
        }
    }

    fn string_to_token(&mut self) -> Result<Token, LexerError> {
            let mut string = String::new();
            while let Some(next_ch) = self.chr_iter.peek() {
                if *next_ch != '"' {
                    if *next_ch == '#' {
                        self.chr_iter.next();
                        self.handle_escape_chars(&mut string);

                    } else {
                        string.push(*next_ch);
                        self.chr_iter.next();
                    }
                } else {
                    self.chr_iter.next();
                    break;
                }
            }
            Ok(Token::String(string))
    }

    fn number_to_token(&mut self, chr: char) -> Result<Token, LexerError> {
        let mut accumulator: i64 = num_char_to_u8(chr) as i64;
        while let Some(next_ch) = self.chr_iter.peek() {
            if next_ch.is_numeric() {
                accumulator = accumulator * 10 + num_char_to_u8(*next_ch) as i64;
                self.chr_iter.next();
            } else if next_ch.is_alphabetic() {
                return Err(LexerError::new(self.line, LexerErrorKind::Spelling, accumulator.to_string()));
            } else {
                break;
            }
        }

        if let Some('.') = self.chr_iter.peek() {
            self.chr_iter.next();
            let mut decimal_part: f64 = 0.0;
            let mut decimal_place: f64 = 0.1;
            while let Some(next_ch) = self.chr_iter.peek() {
                if next_ch.is_numeric() {
                    decimal_part += num_char_to_u8(*next_ch) as f64 * decimal_place;
                    decimal_place /= 10.0;
                    self.chr_iter.next();
                } else {
                    break;
                }
            }
            Ok(Token::Float(accumulator as f64 + decimal_part))
        } else {
            Ok(Token::Integer(accumulator))
        }


    }

    fn identifier_to_token(&mut self, chr: char) -> Result<Token, LexerError> {
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
            "let" => Ok(Token::Let),
            "const" => Ok(Token::Const),
            "true" => Ok(Token::True),
            "false" => Ok(Token::False),
            "if" => Ok(Token::If),
            "else" => Ok(Token::Else),
            "fn" => Ok(Token::Fn),
            "as" => Ok(Token::As),
            "match" => Ok(Token::Match),
            "when" => Ok(Token::When),
            "then" => Ok(Token::Then),
            "to" => Ok(Token::Range),
            "either" => Ok(Token::Either),
            "ret" => Ok(Token::Return),
            "end" => Ok(Token::End),
            "next" => Ok(Token::Next),
            "for" => Ok(Token::For),
            "in" => Ok(Token::In),
            _ => Ok(Token::Identifier(identifier)),
        }
    }

}

pub fn tokenize(mut lexer: Lexer) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token()?;

        // println!("{:?} ", token);
        tokens.push(token.clone());

        if token == Token::EOF {
            break;
        }
    }
    Ok(tokens)
}

pub fn num_char_to_u8(chr: char) -> u8 {
    chr as u8 - '0' as u8
}
#[derive(Debug)]
pub enum LexerErrorKind {
    Spelling
}

#[derive(Debug)]
pub struct LexerError {
    description: String,
    line: usize,
    agent: String
}


impl LexerError {
    fn new(line: usize, kind: LexerErrorKind, agent: String) -> Self {
        use LexerErrorKind::*;
        let description = match kind {
            Spelling => format!("incorrect spelling of '{}'", agent)
        };

        LexerError { description, line, agent }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Lexical Error at line {}: {}", self.line, self.description)
    }
}

impl Error for LexerError {
    fn description(&self) -> &str {
        &self.description
    }
}


mod tests {
    use super::*;

    #[test]
    fn test_ident_starts_w_number() {
        let lexer = Lexer::new(Input::String(String::from("let 3a = 5;")));
        let tokens_res = tokenize(lexer);
        assert!('.'.is_alphabetic());

        match tokens_res {
            Ok(_) => panic!("Should panic"),
            Err(err) => println!("{}", err)
        }
    }

    #[test]
    fn test_escape_characters() {
        let lexer = Lexer::new(Input::File(String::from("src/tests/data/escape_characters.ks")));
        let tokens: Vec<Token> = tokenize(lexer).unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::String("\\n #".to_string()),
                Token::EOF
            ]
        );

    }

    #[test]
    fn test_comments() {
        let string = String::from("let a = 5; // this is a comment\n");
        let lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(lexer).unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier(String::from("a")),
                Token::Assign,
                Token::Integer(5),
                Token::Semicolon,
                Token::Comment(String::from(" this is a comment")),
                Token::EOF
            ]
        );

    }

    #[test]
    fn test_number_tokenize() {
        let string = String::from("5 10.323");
        let lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(lexer).unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Integer(5),
                Token::Float(10.323),
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_simple_string_input() {
        let string = String::from("let  a = 5;");
        let lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(lexer).unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier(String::from("a")),
                Token::Assign,
                Token::Integer(5),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_double_line_input() {
        let string = String::from("let  a = 5;let b = 10.2;");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(lexer).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier(String::from("a")),
                Token::Assign,
                Token::Integer(5),
                Token::Semicolon,
                Token::Let,
                Token::Identifier(String::from("b")),
                Token::Assign,
                Token::Float(10.2),
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_vectors() {
        let string = String::from("let a = [1, 2, 3];");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(lexer).unwrap();
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
                Token::Semicolon,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_fn() {
        let string = String::from("fn test(){print(\"test\");ret 2;}");
        let lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(lexer).unwrap();

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
                Token::Semicolon,
                Token::Return,
                Token::Integer(2),
                Token::Semicolon,
                Token::RightBrace,
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_basic_file() {
        let lexer = Lexer::new(Input::File(String::from("src/tests/data/basic_file.ks")));
        let tokens: Vec<Token> = tokenize(lexer).unwrap();


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
                Token::Semicolon,
                Token::Return,
                Token::Integer(2),
                Token::Semicolon,
                Token::RightBrace,
                Token::Const,
                Token::Identifier("b".to_string()),
                Token::Assign,
                Token::Integer(3),
                Token::Semicolon,
                Token::Const,
                Token::Identifier("files".to_string()),
                Token::Assign,
                Token::LeftBracket,
                Token::String("test1".to_string()),
                Token::Comma,
                Token::String("test2".to_string()),
                Token::RightBracket,
                Token::Semicolon,
                Token::Let,
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::Identifier("test".to_string()),
                Token::LeftParen,
                Token::RightParen,
                Token::Semicolon,
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::Identifier("a".to_string()),
                Token::Plus,
                Token::Integer(1),
                Token::Semicolon,
                Token::If,
                Token::Identifier("a".to_string()),
                Token::NotEqual,
                Token::Integer(5),
                Token::LeftBrace,
                Token::Identifier("print".to_string()),
                Token::LeftParen,
                Token::String("'a' is not a five".to_string()),
                Token::RightParen,
                Token::Semicolon,
                Token::RightBrace,
                Token::For,
                Token::Identifier("file".to_string()),
                Token::In,
                Token::Identifier("files".to_string()),
                Token::LeftBrace,
                Token::Comment(" do sth with file".to_string()),
                Token::If,
                Token::Identifier("file".to_string()),
                Token::Equal,
                Token::String("end".to_string()),
                Token::LeftBrace,
                Token::End,
                Token::Semicolon,
                Token::RightBrace,
                Token::If,
                Token::Identifier("file".to_string()),
                Token::Equal,
                Token::String("skip".to_string()),
                Token::LeftBrace,
                Token::Next,
                Token::Semicolon,
                Token::RightBrace,
                Token::RightBrace,
                Token::EOF
            ]
        );
    }
}
