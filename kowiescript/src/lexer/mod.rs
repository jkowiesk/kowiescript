// This is lexer struct which converts the source code into tokens.
// lexer uses ChrIterator to read the source code.

use std::{
    error::Error,
    fmt::{self},
    iter::Peekable,
};

use crate::io::{ChrIterator, Input, WHITESPACES};

use self::token::Token;

pub mod token;

pub struct Lexer<'a> {
    chr_iter: Peekable<ChrIterator<'a>>,
    pub line: usize,
    new_line: Option<Vec<char>>,
    peeked: Option<Token>,
    cur_token: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: Input) -> Lexer<'a> {
        Lexer {
            chr_iter: ChrIterator::new(input).unwrap(),
            line: 1,
            new_line: None,
            peeked: None,
            cur_token: None,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        if let Some(token) = self.peeked.take() {
            self.cur_token = Some(token.clone());

            // println!("peeked token: {:?}", token);
            return Ok(token);
        }

        let token = match self.chr_iter.next() {
            Some(chr) => match chr {
                '=' => Ok(self.simple_build('=', Token::Equal, Token::Assign)),
                '+' => Ok(Token::Plus),
                '!' => Ok(self.simple_build('=', Token::NotEqual, Token::Bang)),
                '*' => Ok(Token::Asterisk),
                '/' => Ok(self
                    .slash_to_token()?
                    .unwrap_or_else(|| self.next_token().unwrap())),
                '%' => Ok(Token::Modulo),
                '<' => Ok(self.simple_build('=', Token::LEt, Token::Lt)),
                '>' => Ok(self.simple_build('=', Token::GEt, Token::Gt)),
                ',' => Ok(Token::Comma),
                '(' => Ok(Token::LeftParen),
                ')' => Ok(Token::RightParen),
                '{' => Ok(Token::LeftBrace),
                '}' => Ok(Token::RightBrace),
                '[' => Ok(Token::LeftBracket),
                ']' => Ok(Token::RightBracket),
                ';' => Ok(Token::Semicolon),
                _whitespace if WHITESPACES.contains(&(chr as u8)) => self.skip_whitespace(chr),
                _string if chr == '"' => self.string_to_token(),
                _number_or_minus if chr.is_numeric() || chr == '-' => {
                    self.number_or_minus_to_token(chr)
                }
                _ident if chr.is_alphanumeric() => self.identifier_to_token(chr),
                _ => Ok(Token::Err),
            },
            None => Ok(Token::EOF),
        };

        // println!("token: {:?}", token);

        if let Ok(cur_token) = &token {
            self.cur_token = Some(cur_token.clone());
        }

        return token;
    }

    pub fn peek_token(&mut self) -> Result<Token, LexerError> {
        if let Some(token) = self.peeked.clone() {
            return Ok(token);
        }

        let token = self.next_token()?;
        self.peeked = Some(token.clone());
        Ok(token)
    }

    fn slash_to_token(&mut self) -> Result<Option<Token>, LexerError> {
        if let Some('/') = self.chr_iter.peek() {
            let mut comment = String::new();

            self.chr_iter.next();

            while let Some(next_chr) = self.peek_and_check_chr(|chr| chr != '\n') {
                self.chr_iter.next();
                comment.push(next_chr);
            }

            Ok(None)
        } else {
            Ok(Some(Token::Slash))
        }
    }

    fn simple_build(&mut self, guess_char: char, true_token: Token, false_token: Token) -> Token {
        if let Some(next_char) = self.chr_iter.peek() {
            if *next_char == guess_char {
                self.chr_iter.next();
                return true_token;
            }
        }
        false_token
    }

    fn skip_whitespace(&mut self, chr: char) -> Result<Token, LexerError> {
        if let Some(new_line) = &self.new_line {
            if new_line.len() == 2 {
                if chr == '\r' {
                    self.line += 1;
                    self.chr_iter.next();
                }
            } else if chr == new_line[0] {
                self.line += 1
            }
        } else {
            match chr {
                '\r' => {
                    if let Some(next_ch) = self.chr_iter.peek() {
                        if *next_ch == '\n' {
                            self.new_line = Some(vec!['\r', '\n']);
                        } else {
                            self.new_line = Some(vec!['\r']);
                        }
                        self.line += 1;
                    }
                }
                '\n' => {
                    self.new_line = Some(vec!['\n']);
                    self.line += 1;
                }
                _ => {} // Other whitespace that don't initalize new line property
            }
        }

        self.next_token()
    }

    fn handle_escape_chars(&mut self, string: &mut String) {
        if let Some(next_ch) = self.chr_iter.peek() {
            match *next_ch {
                'n' => {
                    string.push('\\');
                    string.push('n');
                }
                't' => {
                    string.push('\\');
                    string.push('t');
                }
                'r' => {
                    string.push('\\');
                    string.push('r');
                }
                '"' => {
                    string.push('"');
                }
                '#' => {
                    string.push('#');
                }
                _ => {}
            }
            self.chr_iter.next();
        }
    }

    fn string_to_token(&mut self) -> Result<Token, LexerError> {
        let mut string = String::new();
        while self.chr_iter.peek() != Some(&'"') {
            let next_chr = match self.chr_iter.peek() {
                Some(chr) => *chr,
                None => {
                    return Err(LexerError::new(
                        self.line,
                        LexerErrorKind::StringNotClosed,
                        string,
                    ))
                }
            };

            if next_chr == '#' {
                self.chr_iter.next();
                self.handle_escape_chars(&mut string);
            } else {
                string.push(next_chr);
                self.chr_iter.next();
            }
        }
        self.chr_iter.next();
        Ok(Token::String(string))
    }

    fn number_or_minus_to_token(&mut self, chr: char) -> Result<Token, LexerError> {
        let mut is_negative = false;
        let mut next_chr;

        if chr == '-' {
            match self.chr_iter.peek() {
                Some(chr) => {
                    if chr.is_numeric() {
                        is_negative = true;
                        next_chr = match self.chr_iter.next() {
                            Some(chr) => chr,
                            None => panic!("Lexer: number_or_minus_to_token: no next char"),
                        };
                    } else {
                        return Ok(Token::Minus);
                    }
                }
                None => return Ok(Token::Minus),
            }
        } else {
            next_chr = chr;
        }

        let mut accumulator: i64 = num_char_to_u8(next_chr) as i64;

        next_chr = match self.chr_iter.peek() {
            Some(chr) => *chr,
            None => return Ok(Token::Integer(accumulator)),
        };

        if next_chr.is_alphabetic() {
            return Err(LexerError::new(
                self.line,
                LexerErrorKind::Spelling,
                accumulator.to_string(),
            ));
        }

        while let Some(next_chr) = self.peek_and_check_chr(|chr| chr.is_numeric()) {
            accumulator = accumulator * 10 + num_char_to_u8(next_chr) as i64;
            self.chr_iter.next();
        }

        if let Some('.') = self.chr_iter.peek() {
            self.chr_iter.next();
            let mut decimal_part: f64 = 0.0;
            let mut decimal_place: f64 = 0.1;

            match self.chr_iter.peek() {
                Some(_) => {}
                None => {
                    if is_negative {
                        return Ok(Token::Float(-accumulator as f64));
                    }
                    return Ok(Token::Float(accumulator as f64));
                }
            };

            while let Some(next_chr) = self.peek_and_check_chr(|chr| chr.is_numeric()) {
                self.chr_iter.next();
                decimal_part += num_char_to_u8(next_chr) as f64 * decimal_place;
                decimal_place /= 10.0;
            }

            if is_negative {
                let value = accumulator as f64 + decimal_part;
                return Ok(Token::Float(-value));
            }
            Ok(Token::Float(accumulator as f64 + decimal_part))
        } else {
            if is_negative {
                return Ok(Token::Integer(-accumulator));
            }
            Ok(Token::Integer(accumulator))
        }
    }

    fn identifier_to_token(&mut self, chr: char) -> Result<Token, LexerError> {
        let mut identifier = String::new();
        identifier.push(chr);

        let _next_chr = match self.chr_iter.peek() {
            Some(chr) => *chr,
            None => return Ok(Token::Identifier(identifier)),
        };

        while let Some(next_chr) =
            self.peek_and_check_chr(|chr| chr.is_alphanumeric() || chr == '_')
        {
            self.chr_iter.next();
            identifier.push(next_chr);
        }
        let result = match identifier.as_str() {
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
            "int" => Ok(Token::IntType),
            "float" => Ok(Token::FloatType),
            "string" => Ok(Token::StringType),
            "bool" => Ok(Token::BoolType),
            "and" => Ok(Token::And),
            "or" => Ok(Token::Or),
            "default" => Ok(Token::Default),
            _ => Ok(Token::Identifier(identifier.clone())),
        };
        // check if left parenthesis right after identifier, it's a function call
        match self.chr_iter.peek() {
            Some('(') => Ok(Token::FnIdent(identifier)),
            Some('[') => Ok(Token::VecAcc(identifier)),
            _ => result,
        }
    }

    fn peek_and_check_chr<F>(&mut self, condition: F) -> Option<char>
    where
        F: FnOnce(char) -> bool,
    {
        match self.chr_iter.peek() {
            Some(chr) => {
                if condition(*chr) {
                    Some(*chr)
                } else {
                    None
                }
            }
            None => None,
        }
    }
}

pub fn num_char_to_u8(chr: char) -> u8 {
    chr as u8 - b'0'
}
#[derive(Debug)]
pub enum LexerErrorKind {
    Spelling,
    StringNotClosed,
}

#[derive(Debug)]
pub struct LexerError {
    description: String,
    line: usize,
    agent: String,
}

impl LexerError {
    fn new(line: usize, kind: LexerErrorKind, agent: String) -> Self {
        use LexerErrorKind::*;

        let description = match kind {
            Spelling => format!("incorrect spelling of ident '{}'", agent),
            StringNotClosed => format!("encountered EOF before closing '{}'", agent),
        };

        LexerError {
            description,
            line,
            agent,
        }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Lexical Error at line {}: {}",
            self.line, self.description
        )
    }
}

impl Error for LexerError {
    fn description(&self) -> &str {
        &self.description
    }
}

mod tests {
    use super::*;

    fn tokenize(lexer: &mut Lexer) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token()?;
            println!("{:?}", token);
            tokens.push(token.clone());

            if token == Token::EOF {
                break;
            }
        }
        Ok(tokens)
    }

    #[test]
    fn test_new_line() {
        let mut lexer = Lexer::new(Input::String(String::from(" \r\n\r\nlet")));
        let tokens_res = tokenize(&mut lexer);

        assert_eq!(tokens_res.unwrap(), vec![Token::Let, Token::EOF]);

        assert_eq!(lexer.line, 3);
    }

    #[test]
    fn test_ident_starts_w_number() {
        let mut lexer = Lexer::new(Input::String(String::from("3a")));
        let tokens_res = tokenize(&mut lexer);

        match tokens_res {
            Ok(_) => panic!("Should panic"),
            Err(err) => println!("{}", err),
        }
    }

    #[test]
    fn test_escape_characters_string() {
        let mut lexer = Lexer::new(Input::String(String::from("\"#n #r #t #\" ##\"")));
        let tokens: Vec<Token> = tokenize(&mut lexer).unwrap();

        assert_eq!(
            tokens,
            vec![Token::String("\\n \\r \\t \" #".to_string()), Token::EOF]
        );
    }

    #[test]
    fn test_comments() {
        let string = String::from("// this is a comment");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(&mut lexer).unwrap();
        assert_eq!(tokens, vec![Token::EOF]);

        let string = String::from("// this is a comment\n");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(&mut lexer).unwrap();
        assert_eq!(tokens, vec![Token::EOF]);

        let string = String::from("let a = 5; // this is a comment\n");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(&mut lexer).unwrap();
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
    fn test_number_tokenize() {
        let string = String::from("5 10.323");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(&mut lexer).unwrap();

        assert_eq!(
            tokens,
            vec![Token::Integer(5), Token::Float(10.323), Token::EOF]
        );
    }

    #[test]
    fn test_parsing_conjuction() {
        let mut lexer = Lexer::new(Input::String("1 == 2 and 2 == 2".to_string()));
        let _tokens = tokenize(&mut lexer).unwrap();
    }

    #[test]
    fn test_negative_nums() {
        let string = String::from("-5 -1.0");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(&mut lexer).unwrap();

        assert_eq!(
            tokens,
            vec![Token::Integer(-5), Token::Float(-1.0), Token::EOF]
        );
    }

    #[test]
    fn test_pass_minus() {
        let string = String::from("-5 - -1.0");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(&mut lexer).unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Integer(-5),
                Token::Minus,
                Token::Float(-1.0),
                Token::EOF
            ]
        );
    }

    #[test]
    fn test_simple_string_input() {
        let string = String::from("let  a = 5;");
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(&mut lexer).unwrap();

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
        let tokens = tokenize(&mut lexer).unwrap();
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
        let tokens = tokenize(&mut lexer).unwrap();
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
        let mut lexer = Lexer::new(Input::String(string));
        let tokens = tokenize(&mut lexer).unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::FnIdent("test".to_string()),
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::FnIdent("print".to_string()),
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
        let mut lexer = Lexer::new(Input::File(String::from("src/tests/data/basic_file.ks")));
        let tokens: Vec<Token> = tokenize(&mut lexer).unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::FnIdent("test".to_string()),
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::FnIdent("print".to_string()),
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
                Token::FnIdent("test".to_string()),
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
                Token::FnIdent("print".to_string()),
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
