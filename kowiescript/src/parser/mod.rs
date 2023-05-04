// parser based on my lexer and ebnf
// this parser has lookahead of 1 token

use crate::{
    io::Input,
    lexer::{Lexer, Token},
};
use std::{error::Error, fmt};

use self::ast::*;

mod ast;

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn new(input: Input) -> Parser<'a> {
        Parser {
            lexer: Lexer::new(input),
        }
    }

    // program = { statement };
    pub fn parse_program(&mut self) -> Result<Vec<Statement>, Box<dyn Error>> {
        let mut statements: Vec<Statement> = Vec::new();
        loop {
            match self.lexer.peek_token()? {
                Token::EOF => break,
                _ => {
                    let statement = self.parse_statement()?;
                    statements.push(statement);
                }
            }
        }

        Ok(statements)
    }

    /* statement = variable_declaration
    | constant_declaration
    | assign_declaration
    | loop_statement
    | loop_substatment
    | conditional_statement
    | function_statement
    | return_statement
    | pattern_match_stmt; */
    fn parse_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        match self.lexer.peek_token()? {
            Token::Let | Token::Const => self.parse_var_declaration(),
            Token::Identifier(name) => {
                let ident = self.lexer.next_token()?;
                match self.lexer.peek_token()? {
                    Token::Assign => self.parse_assignment(name),
                    _ => panic!("parse statment error"),
                }
            }
            _ => panic!("parse statment error"),
        }
    }

    fn parse_assignment(&mut self, name: String) -> Result<Statement, Box<dyn Error>> {
        let expression = self.parse_expression()?;
        Ok(Statement::Assignment(Assignment { name, expression }))
    }

    // variable_declaration  = "let" identifier "=" expression ";";
    fn parse_var_declaration(&mut self) -> Result<Statement, Box<dyn Error>> {
        let kind = match self.lexer.next_token()? {
            Token::Let => VarKind::Mutable,
            Token::Const => VarKind::Constant,
            _ => panic!("expected 'let' or 'const' token"),
        };

        let token = self.lexer.next_token()?;
        let name = match token {
            Token::Identifier(name) => name,
            name => {
                return Err(ParserError::boxed(
                    self.lexer.line,
                    ParserErrorKind::MissingIdent,
                    format!("{:?}", name),
                ))
            }
        };

        let token = self.lexer.next_token()?;
        match token {
            Token::Assign => (),
            _ => panic!("expected '=' token, got {:?}", token),
        }

        let expression = self.parse_expression()?;

        let token = self.lexer.next_token()?;
        match token {
            Token::Semicolon => (),
            _ => panic!("expected ';' token, got {:?}", token),
        }

        Ok(Statement::VarDeclaration(VarDeclaration {
            kind,
            name,
            expression,
        }))
    }

    // expression = conjuction { "or" conjuction };
    fn parse_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        let conjunction = self.parse_conjuction()?;

        let mut conjunctions = Vec::new();
        conjunctions.push(conjunction);

        loop {
            match self.lexer.peek_token()? {
                Token::Or => {
                    self.lexer.next_token()?;
                    conjunctions.push(self.parse_conjuction()?);
                }
                _ => break,
            }
        }

        Ok(Expression { conjunctions })
    }

    // conjuction = relation_expression { "and" relation_expression };
    fn parse_conjuction(&mut self) -> Result<Conjunction, Box<dyn Error>> {
        let relation = self.parse_relation()?;

        let mut relations = Vec::new();
        relations.push(relation);

        loop {
            match self.lexer.peek_token()? {
                Token::And => {
                    self.lexer.next_token()?;
                    relations.push(self.parse_relation()?);
                }
                _ => break,
            }
        }

        Ok(Conjunction { relations })
    }

    // relational_operator = "<" | "<=" | ">" | ">=" | "==" | "!=";
    // relation = simple_expression [relational_operator, simple_expression];
    fn parse_relation(&mut self) -> Result<Relation, Box<dyn Error>> {
        let lhs = self.parse_simple_expression()?;
        let mut op = None;
        let mut rhs = None;

        match self.lexer.peek_token()? {
            Token::Lt => {
                op = Some(RelationalOperator::Less);
                self.lexer.next_token()?;
                rhs = Some(self.parse_simple_expression()?);
            }
            Token::LEt => {
                op = Some(RelationalOperator::LessEqual);
                self.lexer.next_token()?;
                rhs = Some(self.parse_simple_expression()?);
            }
            Token::Gt => {
                op = Some(RelationalOperator::Greater);
                self.lexer.next_token()?;
                rhs = Some(self.parse_simple_expression()?);
            }
            Token::GEt => {
                op = Some(RelationalOperator::GreaterEqual);
                self.lexer.next_token()?;
                rhs = Some(self.parse_simple_expression()?);
            }
            Token::Equal => {
                op = Some(RelationalOperator::Equal);
                self.lexer.next_token()?;
                rhs = Some(self.parse_simple_expression()?);
            }
            Token::NotEqual => {
                op = Some(RelationalOperator::NotEqual);
                self.lexer.next_token()?;
                rhs = Some(self.parse_simple_expression()?);
            }
            _ => (),
        };

        Ok(Relation { lhs, op, rhs })
    }

    // simple_expression = term { lower_arithmetic term };
    fn parse_simple_expression(&mut self) -> Result<SimpleExpression, Box<dyn Error>> {
        let term = self.parse_term()?;

        let mut terms = Vec::new();
        let mut ops = Vec::new();
        terms.push(term);

        loop {
            match self.lexer.peek_token()? {
                Token::Plus => {
                    self.lexer.next_token()?;
                    ops.push(ArithmeticOperator::Add);
                    let conversion = self.parse_term()?;
                    terms.push(conversion);
                }
                Token::Minus => {
                    self.lexer.next_token()?;
                    ops.push(ArithmeticOperator::Subtract);
                    let conversion = self.parse_term()?;
                    terms.push(conversion);
                }
                _ => break,
            }
        }

        Ok(SimpleExpression { terms, ops })
    }

    /* higher_arithmetic = "*"
    |                      "/"
    |                      "%";
    */
    // term = type_conversion { higher_arithmetic type_conversion };
    fn parse_term(&mut self) -> Result<Term, Box<dyn Error>> {
        let conversion = self.parse_type_conversion()?;

        let mut conversions = Vec::new();
        let mut ops = Vec::new();
        conversions.push(conversion);

        loop {
            match self.lexer.peek_token()? {
                Token::Asterisk => {
                    self.lexer.next_token()?;
                    ops.push(ArithmeticOperator::Multiply);
                    let conversion = self.parse_type_conversion()?;
                    conversions.push(conversion);
                }
                Token::Slash => {
                    self.lexer.next_token()?;
                    ops.push(ArithmeticOperator::Divide);
                    let conversion = self.parse_type_conversion()?;
                    conversions.push(conversion);
                }
                Token::Modulo => {
                    self.lexer.next_token()?;
                    ops.push(ArithmeticOperator::Modulo);
                    let conversion = self.parse_type_conversion()?;
                    conversions.push(conversion);
                }
                _ => break,
            }
        }

        Ok(Term { conversions, ops })
    }

    // type_conversion = inversion [ cast_operator cast_type ];
    fn parse_type_conversion(&mut self) -> Result<Conversion, Box<dyn Error>> {
        let inversion = self.parse_inversion()?;

        if self.lexer.peek_token()? == Token::As {
            self.lexer.next_token()?;
            let cast_type = match self.lexer.next_token()? {
                Token::IntType => ConversionType::Int,
                Token::FloatType => ConversionType::Float,
                Token::StringType => ConversionType::String,
                Token::BoolType => ConversionType::Bool,
                _ => panic!("type conversion error"),
            };
            Ok(Conversion {
                inversion,
                to: Some(cast_type),
            })
        } else {
            Ok(Conversion {
                inversion,
                to: None,
            })
        }
    }

    // inversion = ["!"] factor;
    fn parse_inversion(&mut self) -> Result<Inversion, Box<dyn Error>> {
        let mut negated = false;
        if self.lexer.peek_token()? == Token::Bang {
            self.lexer.next_token()?;
            negated = true;
        }

        let value = self.parse_factor()?;
        Ok(Inversion { negated, value })
    }

    /* factor = literal
    | identifier
    | vector
    | function_call
    | vector_access
    | "(" expression ")"; */
    fn parse_factor(&mut self) -> Result<Factor, Box<dyn Error>> {
        match self.lexer.peek_token()? {
            Token::Integer(i) => {
                self.lexer.next_token()?;
                Ok(Factor::Literal(Literal::Int(i)))
            }
            Token::Float(f) => {
                self.lexer.next_token()?;
                Ok(Factor::Literal(Literal::Float(f)))
            }
            Token::String(string) => {
                self.lexer.next_token()?;
                Ok(Factor::Literal(Literal::String(string)))
            }
            Token::Identifier(ident) => self.parse_identifier(ident),
            Token::LeftParen => self.parse_paren_expression(),
            _ => panic!("expected factor, got {:?}", self.lexer.peek_token()?),
        }
    }

    fn parse_identifier(&mut self, ident: String) -> Result<Factor, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::LeftParen => self.parse_function_call(ident),
            Token::LeftBracket => self.parse_vector_access(ident),
            _ => {
                return Err(ParserError::boxed(
                    self.lexer.line,
                    ParserErrorKind::MissingIdent,
                    format!("{:?}", token),
                ))
            }
        }
    }

    // vector_access = (identifier | function_call) "[" expression "]";
    fn parse_vector_access(&mut self, ident: String) -> Result<Factor, Box<dyn Error>> {
        let expression = self.parse_expression()?;

        Ok(Factor::VectorAccess(VectorAccess {
            name: ident,
            index: expression,
        }))
    }

    // function_call = identifier "(" [ expression { "," expression } ] ")";
    fn parse_function_call(&mut self, ident: String) -> Result<Factor, Box<dyn Error>> {
        let mut arguments: Vec<Expression> = Vec::new();
        loop {
            match self.lexer.peek_token()? {
                Token::RightParen => break,
                _ => {
                    let expression = self.parse_expression()?;
                    arguments.push(expression);
                }
            }
        }

        Ok(Factor::FunctionCall(FunctionCall {
            name: ident,
            arguments: arguments,
        }))
    }

    // "(" expression ")";
    fn parse_paren_expression(&mut self) -> Result<Factor, Box<dyn Error>> {
        let expression = self.parse_expression()?;
        Ok(Factor::Parenthesized(expression))
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    MissingIdent,
}

#[derive(Debug)]
pub struct ParserError {
    description: String,
    line: usize,
    agent: String,
}

impl ParserError {
    fn new(line: usize, kind: ParserErrorKind, agent: String) -> Self {
        use ParserErrorKind::*;

        let description = match kind {
            MissingIdent => format!("incorrect spelling of ident '{}'", agent),
        };

        ParserError {
            description,
            line,
            agent,
        }
    }

    fn boxed(line: usize, kind: ParserErrorKind, agent: String) -> Box<Self> {
        use ParserErrorKind::*;

        let description = match kind {
            MissingIdent => format!("incorrect spelling of ident '{}'", agent),
        };
        let err = ParserError {
            description,
            line,
            agent,
        };

        Box::new(err)
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Syntax Error at line {}: {}",
            self.line, self.description
        )
    }
}

impl Error for ParserError {
    fn description(&self) -> &str {
        &self.description
    }
}

mod tests {
    use super::*;

    #[test]
    fn simple_parse() {
        let mut parser = Parser::new(Input::String("let a = 1 + 2 * 3;".to_string()));
        let expression = parser.parse_program().unwrap();
        println!("{:?}", ron::to_string(&expression));
    }
}
