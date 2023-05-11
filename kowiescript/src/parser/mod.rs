// parser based on my lexer and ebnf
// this parser has lookahead of 1 token

use crate::{
    io::Input,
    lexer::{
        token::{self, Token},
        Lexer,
    },
};
use std::{error::Error, fmt, string::ParseError};

use self::ast::*;

pub mod ast;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: Input) -> Parser<'a> {
        Parser {
            lexer: Lexer::new(input),
        }
    }

    // program = { statement };
    pub fn parse_program(&mut self) -> Result<Vec<Statement>, Box<dyn Error>> {
        let mut statements: Vec<Statement> = Vec::new();

        while let Some(_) = self.peek_and_check_is_not(Token::EOF)? {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(statements)
    }

    fn peek_and_check_is_not(&mut self, token: Token) -> Result<Option<Token>, Box<dyn Error>> {
        let peeked = self.lexer.peek_token()?;

        if peeked != token {
            Ok(Some(token))
        } else {
            Ok(None)
        }
    }

    /*     fn peek_and_check_token<F>(&mut self, condition: F) -> Result<Option<Token>, Box<dyn Error>>
    where
        F: FnOnce(Token) -> bool,
    {
        let token = self.lexer.peek_token()?;
        if condition(token) {
            Ok(Some(token))
        } else {
            Ok(None)
        }
    } */

    /* statement = variable_declaration
    | constant_declaration
    | assign_declaration
    | loop_statement
    | loop_substatement
    | conditional_statement
    | function_statement
    | return_statement
    | pattern_match_stmt
    | expression_stmt;  */
    fn parse_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        let statement = self
            .parse_var_declaration()
            .transpose()
            .or_else(|| self.parse_assignment().transpose())
            .or_else(|| self.parse_for_loop().transpose())
            .or_else(|| self.parse_subloop().transpose())
            .or_else(|| self.parse_if().transpose())
            .or_else(|| self.parse_fn().transpose())
            .or_else(|| self.parse_return().transpose())
            .or_else(|| self.parse_match().transpose())
            .or_else(|| self.parse_expression_stmt().transpose());

        match statement {
            Some(stmt) => Ok(stmt?),
            None => {
                let token = self.lexer.next_token()?;
                Err(ParserError::boxed(
                    self.lexer.line,
                    ParserErrorKind::UnrecognizedStmt,
                    token,
                ))
            }
        }
    }

    //expression_stmt = expression ";" ;
    fn parse_expression_stmt(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        let expression = self.parse_expression()?;

        if let Token::Semicolon = self.lexer.peek_token()? {
            self.lexer.next_token()?;
        }

        Ok(Some(Statement::Expression(expression)))
    }

    // pattern_match_stmt = "match" expression "{" { when_branch } default_branch "}";
    fn parse_match(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        if self.check_token(Token::Match)?.is_none() {
            return Ok(None);
        } else {
            self.lexer.next_token()?;
        }

        let expression = self.parse_expression()?;

        self.parse_token(Token::LeftBrace)?;

        let mut when_branches: Vec<WhenBranch> = Vec::new();

        while self.peek_and_check_is_not(Token::Default)?.is_some() {
            when_branches.push(self.parse_when_branch()?);
        }

        let default_branch = self.parse_default_branch()?;

        self.parse_token(Token::RightBrace)?;

        Ok(Some(Statement::PatternMatch(PatternMatch {
            expression,
            when_branches,
            default_branch,
        })))
    }

    // when_branch = "when" match_expression "then" "{" { statement } "}";
    fn parse_when_branch(&mut self) -> Result<WhenBranch, Box<dyn Error>> {
        self.parse_token(Token::When)?;

        let pattern = self.parse_match_expression()?;

        self.parse_token(Token::Then)?;

        let body = self.parse_body()?;

        Ok(WhenBranch { pattern, body })
    }

    // when_expression = simple_expression { "either" simple_expression };
    fn parse_match_expression(&mut self) -> Result<WhenExpression, Box<dyn Error>> {
        let mut simple_exprs: Vec<SimpleExpression> = vec![self.parse_simple_expression()?];

        while let Token::Either = self.lexer.peek_token()? {
            self.lexer.next_token()?;
            let simple_expression = self.parse_simple_expression()?;
            simple_exprs.push(simple_expression);
        }

        Ok(WhenExpression { simple_exprs })
    }

    // default_branch = "default" "{" { statement } "}";
    fn parse_default_branch(&mut self) -> Result<Vec<Statement>, Box<dyn Error>> {
        self.parse_token(Token::Default)?;

        self.parse_body()
    }

    // return_statement = "ret" [expression] ";";
    fn parse_return(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        if self.check_token(Token::Return)?.is_none() {
            return Ok(None);
        } else {
            self.lexer.next_token()?;
        }

        let value = match self.lexer.peek_token()? {
            Token::Semicolon => None,
            _ => Some(self.parse_expression()?),
        };

        self.parse_token(Token::Semicolon)?;

        Ok(Some(Statement::Return(Return { value })))
    }

    // function_statement = "fn" identifier "(" [ parameter_list ] ")" "{" { statement } "}";
    fn parse_fn(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        if self.check_token(Token::Fn)?.is_none() {
            return Ok(None);
        } else {
            self.lexer.next_token()?;
        }

        let name = match self.lexer.next_token()? {
            Token::Identifier(name) => name,
            Token::FnIdent(name) => name,
            _token => Err(ParserError::boxed(
                self.lexer.line,
                ParserErrorKind::ExpectedIndent,
                _token,
            ))?,
        };

        self.parse_token(Token::LeftParen)?;

        let parameters = self.parse_parameters()?;

        self.parse_token(Token::RightParen)?;

        let body = self.parse_body()?;

        Ok(Some(Statement::Function(Function {
            name,
            parameters,
            body,
        })))
    }

    // argument_list = expression { "," expression };
    fn parse_arguments(&mut self) -> Result<Vec<Expression>, Box<dyn Error>> {
        let mut arguments: Vec<Expression> = Vec::new();

        while self.peek_and_check_is_not(Token::RightParen)?.is_some() {
            let expression = self.parse_expression()?;
            arguments.push(expression);

            match self.lexer.peek_token()? {
                Token::Comma => {
                    self.lexer.next_token()?;
                }
                Token::RightParen => break,
                _token => Err(ParserError::boxed(
                    self.lexer.line,
                    ParserErrorKind::UnexpectedTokens(vec![Token::Comma, Token::RightParen]),
                    _token,
                ))?,
            }
        }

        Ok(arguments)
    }

    // parameter_list = parameter { "," parameter };
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, Box<dyn Error>> {
        let mut parameters: Vec<Parameter> = Vec::new();

        while self.peek_and_check_is_not(Token::RightParen)?.is_some() {
            let parameter = self.parse_parameter()?;
            parameters.push(parameter);

            match self.lexer.peek_token()? {
                Token::Comma => {
                    self.lexer.next_token()?;
                }
                Token::RightParen => break,
                _token => Err(ParserError::boxed(
                    self.lexer.line,
                    ParserErrorKind::UnexpectedTokens(vec![Token::Comma, Token::RightParen]),
                    _token,
                ))?,
            }
        }

        Ok(parameters)
    }
    // parameter = ["const"] identifier;
    fn parse_parameter(&mut self) -> Result<Parameter, Box<dyn Error>> {
        let kind = match self.lexer.peek_token()? {
            Token::Const => {
                self.lexer.next_token()?;
                VarKind::Constant
            }
            _ => VarKind::Mutable,
        };

        let name = match self.lexer.next_token()? {
            Token::Identifier(name) => name,
            _token => {
                return Err(ParserError::boxed(
                    self.lexer.line,
                    ParserErrorKind::ExpectedIndent,
                    _token,
                ))
            }
        };

        Ok(Parameter { name, kind })
    }

    // loop_substatement = "end;"
    //                     "next;"
    fn parse_subloop(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        let sub_loop_kind = match self.lexer.peek_token()? {
            Token::Next => SubLoopKind::Next,
            Token::End => SubLoopKind::End,
            _ => return Ok(None),
        };
        self.lexer.next_token()?;

        self.parse_token(Token::Semicolon)?;

        Ok(Some(Statement::SubLoop(SubLoop {
            kind: sub_loop_kind,
        })))
    }

    // conditional_statement = "if" expression "{" { statement } "}" [ "else" "{" { statement } "}" ];
    fn parse_if(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        if self.check_token(Token::If)?.is_none() {
            return Ok(None);
        } else {
            self.lexer.next_token()?;
        }

        let condition = self.parse_expression()?;

        let then_body = self.parse_body()?;

        let else_body = match self.lexer.peek_token()? {
            Token::Else => {
                self.lexer.next_token()?;

                let body = self.parse_body()?;

                Some(body)
            }
            _ => None,
        };

        Ok(Some(Statement::Conditional(Conditional {
            condition,
            then_body,
            else_body,
        })))
    }

    // loop_statement = "for" identifier "in" iterator_expression "{" { statement } "}";
    fn parse_for_loop(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        if self.check_token(Token::For)?.is_none() {
            return Ok(None);
        } else {
            self.lexer.next_token()?;
        }

        let iter_var = match self.lexer.next_token()? {
            Token::Identifier(name) => name,
            _token => Err(ParserError::boxed(
                self.lexer.line,
                ParserErrorKind::MissingIdent,
                _token,
            ))?,
        };
        self.parse_token(Token::In)?;

        let iterator = self.parse_iterator_expression()?;

        let body = self.parse_body()?;

        Ok(Some(Statement::ForLoop(ForLoop {
            iter_var,
            iterator,
            body,
        })))
    }

    // iterator_expression = vector | identifier | range_expression | function_call;
    fn parse_iterator_expression(&mut self) -> Result<IteratorExpression, Box<dyn Error>> {
        match self.lexer.peek_token()? {
            Token::Integer(_) => {
                let start = self.parse_range_factor()?;
                let token = self.lexer.next_token()?;
                match token {
                    Token::Range => {
                        let end = self.parse_range_factor()?;
                        Ok(IteratorExpression::Range(RangeExpression { start, end }))
                    }
                    _ => Err(ParserError::boxed(
                        self.lexer.line,
                        ParserErrorKind::UnexpectedTokens(vec![Token::Range]),
                        token,
                    ))?,
                }
            }
            Token::Identifier(name) => {
                self.lexer.next_token()?;
                match self.lexer.peek_token()? {
                    Token::Range => {
                        self.lexer.next_token()?;
                        let end = self.parse_range_factor()?;
                        Ok(IteratorExpression::Range(RangeExpression {
                            start: Factor::Identifier(name),
                            end,
                        }))
                    }
                    _ => Ok(IteratorExpression::Identifier(name)),
                }
            }
            Token::FnIdent(name) => {
                self.lexer.next_token()?;
                Ok(IteratorExpression::FunctionCall(FunctionCall {
                    name,
                    args: self.parse_arguments()?,
                }))
            }
            Token::LeftBrace => {
                self.lexer.next_token()?;
                Ok(IteratorExpression::Vector(self.parse_vector()?))
            }
            _token => Err(ParserError::boxed(
                self.lexer.line,
                ParserErrorKind::UnexpectedExpr(vec![
                    "factor".to_string(),
                    "identifier".to_string(),
                    "vector".to_string(),
                ]),
                _token,
            ))?,
        }
    }

    // range_expression = range_factor "to" range_factor;
    fn parse_range_factor(&mut self) -> Result<Factor, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::Integer(i) => {
                if i < 0 {
                    panic!("range 'factor', must be positive")
                }
                Ok(Factor::Literal(Literal::Int(i)))
            }
            Token::Identifier(name) => Ok(Factor::Identifier(name)),
            _ => panic!("expected 'factor', got {:?}", token),
        }
    }

    fn parse_body(&mut self) -> Result<Vec<Statement>, Box<dyn Error>> {
        self.parse_token(Token::LeftBrace)?;

        let mut statements: Vec<Statement> = Vec::new();
        loop {
            match self.lexer.peek_token()? {
                Token::EOF => panic!("unexpected EOF"),
                Token::RightBrace => {
                    self.lexer.next_token()?;
                    break;
                }
                _ => {
                    let statement = self.parse_statement()?;
                    statements.push(statement);
                }
            }
        }

        Ok(statements)
    }

    // assign_declaration = identifier "=" expression ";";
    fn parse_assignment(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        let name = match self.lexer.peek_token()? {
            Token::Identifier(name) => name,
            _ => return Ok(None),
        };
        self.lexer.next_token()?;

        self.parse_token(Token::Assign)?;

        let expression = self.parse_expression()?;

        self.parse_token(Token::Semicolon)?;

        Ok(Some(Statement::Assignment(Assignment { name, expression })))
    }

    // variable_declaration  = "let" identifier "=" expression ";";
    fn parse_var_declaration(&mut self) -> Result<Option<Statement>, Box<dyn Error>> {
        let kind = match self.lexer.peek_token()? {
            Token::Let => VarKind::Mutable,
            Token::Const => VarKind::Constant,
            _token => return Ok(None),
        };
        self.lexer.next_token()?;

        let token = self.lexer.next_token()?;
        let name = match token {
            Token::Identifier(name) => name,
            name => {
                return Err(ParserError::boxed(
                    self.lexer.line,
                    ParserErrorKind::MissingIdent,
                    name,
                ))
            }
        };

        self.parse_token(Token::Assign)?;

        let expression = self.parse_expression()?;

        self.parse_token(Token::Semicolon)?;

        Ok(Some(Statement::VarDeclaration(VarDeclaration {
            kind,
            name,
            expression,
        })))
    }

    // vector = "[" [ expression { "," expression } ] "]";
    fn parse_vector(&mut self) -> Result<Vector, Box<dyn Error>> {
        self.parse_token(Token::LeftBracket)?;

        let mut values = Vec::new();

        loop {
            let expression = self.parse_expression()?;
            values.push(expression);

            let token = self.lexer.next_token()?;
            match token {
                Token::Comma => (),
                Token::RightBracket => break,
                _ => panic!("expected ',' or ']' token, got {:?}", token),
            }
        }

        Ok(Vector { values })
    }

    // expression = conjuction { "or" conjuction };
    pub fn parse_expression(&mut self) -> Result<Expression, Box<dyn Error>> {
        let conjunction = self.parse_conjuction()?;

        let mut conjunctions = Vec::new();
        conjunctions.push(conjunction);

        while let Token::Or = self.lexer.peek_token()? {
            self.lexer.next_token()?;
            conjunctions.push(self.parse_conjuction()?);
        }

        Ok(Expression { conjunctions })
    }

    // conjuction = relation_expression { "and" relation_expression };
    fn parse_conjuction(&mut self) -> Result<Conjunction, Box<dyn Error>> {
        let relation = self.parse_relation()?;

        let mut relations = Vec::new();
        relations.push(relation);

        while let Token::And = self.lexer.peek_token()? {
            self.lexer.next_token()?;
            relations.push(self.parse_relation()?);
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
            Token::LeftBracket => Ok(Factor::Vector(self.parse_vector()?)),
            Token::Identifier(_) => self.parse_identifier(),
            Token::FnIdent(name) => Ok(Factor::FunctionCall(self.parse_function_call(name)?)),
            Token::VecAcc(name) => self.parse_vector_access(name),
            Token::LeftParen => self.parse_paren_expression(),
            _ => panic!("expected factor, got {:?}", self.lexer.peek_token()?),
        }
    }

    fn parse_identifier(&mut self) -> Result<Factor, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        let ident = match token {
            Token::Identifier(ident) => ident,
            _ => panic!("expected identifier, got {:?}", token),
        };

        Ok(Factor::Identifier(ident))
    }

    // vector_access = (identifier | function_call) "[" expression "]";
    fn parse_vector_access(&mut self, ident: String) -> Result<Factor, Box<dyn Error>> {
        let vector_expr = match self.lexer.peek_token()? {
            Token::FnIdent(ident) => VectorExpr::Identifier(ident),
            Token::LeftBrace => VectorExpr::FunctionCall(self.parse_function_call(ident)?),
            _ => panic!(
                "expected identifier or function call, got {:?}",
                self.lexer.peek_token()?
            ),
        };

        match self.lexer.next_token()? {
            Token::LeftBracket => (),
            _token => panic!("expected '[', got {:?}", _token),
        }
        let index = self.parse_expression()?;

        match self.lexer.next_token()? {
            Token::RightBracket => (),
            _token => panic!("expected ']', got {:?}", _token),
        }

        Ok(Factor::VectorAccess(VectorAccess { vector_expr, index }))
    }

    // function_call = identifier "(" [ argument_list ] ")";
    fn parse_function_call(&mut self, ident: String) -> Result<FunctionCall, Box<dyn Error>> {
        self.lexer.next_token()?;
        self.parse_token(Token::LeftParen)?;

        let args = self.parse_arguments()?;

        self.parse_token(Token::RightParen)?;

        Ok(FunctionCall { name: ident, args })
    }

    // "(" expression ")";
    fn parse_paren_expression(&mut self) -> Result<Factor, Box<dyn Error>> {
        self.lexer.next_token()?;
        let expression = self.parse_expression()?;
        self.lexer.next_token()?;
        Ok(Factor::Parenthesized(expression))
    }

    fn parse_token(&mut self, expected_token: Token) -> Result<(), Box<dyn Error>> {
        let given_token = self.lexer.next_token()?;
        if given_token == expected_token {
            Ok(())
        } else {
            Err(ParserError::boxed(
                self.lexer.line,
                ParserErrorKind::UnexpectedTokens(vec![expected_token]),
                given_token,
            ))
        }
    }

    fn check_token(&mut self, expected_token: Token) -> Result<Option<()>, Box<dyn Error>> {
        let given_token = self.lexer.peek_token()?;
        if given_token == expected_token {
            Ok(Some(()))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    MissingIdent,
    UnexpectedTokens(Vec<Token>),
    UnexpectedExpr(Vec<String>),
    UnrecognizedStmt,
    ExpectedIndent,
}

#[derive(Debug)]
pub struct ParserError {
    description: String,
    line: usize,
    agent: Token,
}

impl ParserError {
    fn new(line: usize, kind: ParserErrorKind, agent: Token) -> Self {
        use ParserErrorKind::*;

        let description = match kind {
            MissingIdent => format!("incorrect spelling of ident '{}'", agent),
            ExpectedIndent => format!("expected identifier got '{}'", agent),
            UnrecognizedStmt => format!(
                "Unrecognized statement started with '{}', are u the syntax is correct ?",
                agent
            ),
            UnexpectedTokens(given_tokens) => {
                let given_tokens = given_tokens
                    .iter()
                    .map(|token| format!("{}", token))
                    .collect::<Vec<String>>()
                    .join(", ");

                format!(
                    "Unexpected token: got '{}' expected {}",
                    agent, given_tokens,
                )
            }
            UnexpectedExpr(given_strings) => {
                let given_strings = given_strings
                    .iter()
                    .map(|string| string.to_owned())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!(
                    "Unexpected expression: got '{}' expected {}",
                    agent, given_strings,
                )
            }
        };

        ParserError {
            description,
            line,
            agent,
        }
    }

    fn boxed(line: usize, kind: ParserErrorKind, agent: Token) -> Box<Self> {
        Box::new(ParserError::new(line, kind, agent))
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
    use core::panic;

    use super::*;
    #[test]
    fn test_parse_statements() {
        let mut parser = Parser::new(Input::String("let a = 1 ;".to_string()));
        let statement = parser.parse_statement().unwrap();
        assert!(matches!(statement, Statement::VarDeclaration(_)));

        let mut parser = Parser::new(Input::String("b = 3;".to_string()));
        let statement = parser.parse_statement().unwrap();
        assert!(matches!(statement, Statement::Assignment(_)));

        let mut parser = Parser::new(Input::String("for a in 1 to 2 {}".to_string()));
        let statement = parser.parse_statement().unwrap();
        assert!(matches!(statement, Statement::ForLoop(_)));

        let mut parser = Parser::new(Input::String("if a == 1 {}".to_string()));
        let statement = parser.parse_statement().unwrap();
        assert!(matches!(statement, Statement::Conditional(_)));

        let mut parser = Parser::new(Input::String("fn test(const a, b, c) {}".to_string()));
        let statement = parser.parse_statement().unwrap();
        assert!(matches!(statement, Statement::Function(_)));

        let mut parser = Parser::new(Input::String("match x {default {}};".to_string()));
        let statement = parser.parse_statement().unwrap();
        assert!(matches!(statement, Statement::PatternMatch(_)));

        let mut parser = Parser::new(Input::String("ret 1;".to_string()));
        let statement = parser.parse_statement().unwrap();
        assert!(matches!(statement, Statement::Return(_)));

        let mut parser = Parser::new(Input::String("next;".to_string()));
        let statement = parser.parse_statement().unwrap();
        assert!(matches!(statement, Statement::SubLoop(_)));
    }

    #[test]
    fn test_parse_simple() {
        let mut parser = Parser::new(Input::String("let a = 1 + 2 * 3;".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "VarDeclaration((name:\"a\",kind:Mutable,expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[]),(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None),(inversion:(value:Literal(Int(3)),negated:false),to:None)],ops:[Multiply])],ops:[Add]),op:None,rhs:None)])])))");
    }

    #[test]
    fn test_parse_paren_expression() {
        let mut parser = Parser::new(Input::String("let a = (1 + 2) * 3;".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "VarDeclaration((name:\"a\",kind:Mutable,expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Parenthesized((conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[]),(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[Add]),op:None,rhs:None)])])),negated:false),to:None),(inversion:(value:Literal(Int(3)),negated:false),to:None)],ops:[Multiply])],ops:[]),op:None,rhs:None)])])))");
    }

    #[test]
    fn test_parse_for_loop_simple_range_no_body() {
        let mut parser = Parser::new(Input::String("for t in 1 to 2 {}".to_string()));
        let statement = parser.parse_statement().unwrap();

        println!("{}", ron::to_string(&statement).unwrap());
        assert_eq!(ron::to_string(&statement).unwrap(), "ForLoop((iter_var:\"t\",iterator:Range((start:Literal(Int(1)),end:Literal(Int(2)))),body:[]))");
    }

    #[test]
    fn test_parse_for_loop_with_ident_start_range_no_body() {
        let mut parser = Parser::new(Input::String("for t in i to 2 {}".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "ForLoop((iter_var:\"t\",iterator:Range((start:Identifier(\"i\"),end:Literal(Int(2)))),body:[]))");
    }

    #[test]
    fn test_parse_for_loop_with_ident_no_body() {
        let mut parser = Parser::new(Input::String("for t in m {}".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(
            ron::to_string(&statement).unwrap(),
            "ForLoop((iter_var:\"t\",iterator:Identifier(\"m\"),body:[]))"
        );
    }

    #[test]
    fn test_parse_if_no_body() {
        let mut parser = Parser::new(Input::String("if 1 == 2 {}".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "Conditional((condition:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:Some(Equal),rhs:Some((terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[])))])]),then_body:[],else_body:None))");
    }
    #[test]
    fn test_parse_if_in_if_body() {
        let mut parser = Parser::new(Input::String(
            "if 1 == 2 { if 2 != 3 {} else {} }".to_string(),
        ));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "Conditional((condition:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:Some(Equal),rhs:Some((terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[])))])]),then_body:[Conditional((condition:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[]),op:Some(NotEqual),rhs:Some((terms:[(conversions:[(inversion:(value:Literal(Int(3)),negated:false),to:None)],ops:[])],ops:[])))])]),then_body:[],else_body:Some([])))],else_body:None))")
    }

    #[test]
    fn test_parse_if_simple() {
        let mut parser = Parser::new(Input::String("if 1 == 2 { a = 5 + 1;}".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "Conditional((condition:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:Some(Equal),rhs:Some((terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[])))])]),then_body:[Assignment((name:\"a\",expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(5)),negated:false),to:None)],ops:[]),(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[Add]),op:None,rhs:None)])])))],else_body:None))");
    }

    #[test]
    fn test_parse_fn_no_body() {
        let mut parser = Parser::new(Input::String("fn mark(const a, b) {}".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "Function((name:\"mark\",parameters:[(name:\"a\",kind:Constant),(name:\"b\",kind:Mutable)],body:[]))");
    }

    #[test]
    fn test_parse_match_no_body() {
        let mut parser = Parser::new(Input::String("match x {default {}} ".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "PatternMatch((expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Identifier(\"x\"),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),when_branches:[],default_branch:[]))");
    }

    #[test]
    fn test_parse_match_simple() {
        let mut parser = Parser::new(Input::String(
            "match x {when 2 + 3 then {a = 2;} default {a = 1;}} ".to_string(),
        ));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "PatternMatch((expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Identifier(\"x\"),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),when_branches:[(pattern:(simple_exprs:[(terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[]),(conversions:[(inversion:(value:Literal(Int(3)),negated:false),to:None)],ops:[])],ops:[Add])]),body:[Assignment((name:\"a\",expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])])))])],default_branch:[Assignment((name:\"a\",expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])])))]))");
    }

    #[test]
    fn test_parse_expression_stmt() {
        let mut parser = Parser::new(Input::String("23 + 1;".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "Expression((conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(23)),negated:false),to:None)],ops:[]),(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[Add]),op:None,rhs:None)])]))")
    }

    #[test]
    fn test_parse_vector() {
        let mut parser = Parser::new(Input::String("[1, 2, i]".to_string()));
        let vector = parser.parse_vector().unwrap();

        assert_eq!(ron::to_string(&vector).unwrap(), "(values:[(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Identifier(\"i\"),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])])])");
    }

    #[test]
    fn test_parse_starts_ident_function_call() {
        let mut parser = Parser::new(Input::String("test(1, 2, 3)".to_string()));
        // let starts_ident = parser.parse_function_call().unwrap();

        // assert_eq!(ron::to_string(&starts_ident).unwrap(), "FunctionCall((name:\"test\",args:[(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(3)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])])]))");
    }

    #[test]
    fn test_parse_function_call_stmt() {
        let mut parser = Parser::new(Input::String("test(1, 2);".to_string()));
        let statement = parser.parse_statement().unwrap();

        println!("{}", ron::to_string(&statement).unwrap());
    }

    #[test]
    fn test_parse_statement_assignment() {
        let mut parser = Parser::new(Input::String("a = 1;".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(
            ron::to_string(&statement).unwrap(),
            "Assignment((name:\"a\",expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])])))"
        );
    }

    #[test]
    fn test_parse_next_and_end() {
        let mut parser = Parser::new(Input::String("end;next;".to_string()));
        let statements = parser.parse_program().unwrap();

        assert_eq!(
            ron::to_string(&statements[0]).unwrap(),
            "SubLoop((kind:End))"
        );
        assert_eq!(
            ron::to_string(&statements[1]).unwrap(),
            "SubLoop((kind:Next))"
        );
    }

    #[test]
    fn test_error_ident() {
        let mut parser = Parser::new(Input::String("fn 23".to_string()));
        let result = parser.parse_program();

        if let Err(err) = result {
            assert_eq!(
                err.to_string(),
                "Syntax Error at line 1: expected identifier got '23'"
            );
        } else {
            panic!("Expected error");
        }
    }

    #[test]
    fn test_error_parse_iterator_expression() {
        let mut parser = Parser::new(Input::String("1.0".to_string()));
        let result = parser.parse_iterator_expression();

        if let Err(err) = result {
            assert_eq!(
                err.to_string(),
                "Syntax Error at line 1: Unexpected expression: got '1' expected factor, identifier, vector"
            );
        } else {
            panic!("Expected error");
        }
    }

    fn copy() {
        let mut parser = Parser::new(Input::String("for t in 1 to 2 {}".to_string()));
        let statement = parser.parse_statement().unwrap();

        println!("{}", ron::to_string(&statement).unwrap());
        //assert_eq!(ron::to_string(&statement).unwrap(), );
    }
}
