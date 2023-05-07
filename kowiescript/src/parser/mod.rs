// parser based on my lexer and ebnf
// this parser has lookahead of 1 token

use crate::{
    io::Input,
    lexer::{Lexer, Token},
};
use std::{error::Error, fmt};

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
    | loop_substatement
    | conditional_statement
    | function_statement
    | return_statement
    | pattern_match_stmt; */
    fn parse_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        match self.lexer.peek_token()? {
            Token::Let | Token::Const => self.parse_var_declaration(),
            Token::Identifier(name) => {
                self.lexer.next_token()?;
                match self.lexer.peek_token()? {
                    Token::Assign => self.parse_assignment(name),
                    _ => panic!("parse statment error"),
                }
            }
            Token::For => self.parse_for_loop(),
            Token::Next | Token::End => self.parse_subloop(),
            Token::If => self.parse_if(),
            Token::Fn => self.prase_fn(),
            Token::Return => self.parse_return(),
            Token::Match => self.parse_match(),
            Token::Comment(_) => {
                self.lexer.next_token()?;
                self.parse_statement()
            }
            _ => panic!("parse statment error"),
        }
    }

    // pattern_match_stmt = "match" expression "{" { when_branch } default_branch "}";
    fn parse_match(&mut self) -> Result<Statement, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::Match => (),
            _ => panic!("expected 'match' token, got {:?}", token),
        }

        let expression = self.parse_expression()?;

        let token = self.lexer.next_token()?;
        match token {
            Token::LeftBrace => (),
            _ => panic!("expected '{{' token, got {:?}", token),
        }

        let mut when_branches: Vec<WhenBranch> = Vec::new();

        loop {
            match self.lexer.peek_token()? {
                Token::Default => break,
                _ => {
                    let when_branch = self.parse_when_branch()?;
                    when_branches.push(when_branch);
                }
            }
        }

        let default_branch = self.parse_default_branch()?;

        let token = self.lexer.next_token()?;
        match token {
            Token::RightBrace => (),
            _ => panic!("expected '}}' token, got {:?}", token),
        }

        Ok(Statement::PatternMatch(PatternMatch {
            expression,
            when_branches,
            default_branch,
        }))
    }

    // when_branch = "when" match_expression "then" "{" { statement } "}";
    fn parse_when_branch(&mut self) -> Result<WhenBranch, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::When => (),
            _ => panic!("expected 'when' token, got {:?}", token),
        }

        let pattern = self.parse_match_expression()?;

        let token = self.lexer.next_token()?;
        match token {
            Token::Then => (),
            _ => panic!("expected 'then' token, got {:?}", token),
        }

        let mut body = self.parse_body()?;

        Ok(WhenBranch { pattern, body })
    }

    // default_branch = "default" "{" { statement } "}";
    fn parse_default_branch(&mut self) -> Result<Vec<Statement>, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::Default => (),
            _ => panic!("expected 'default' token, got {:?}", token),
        }

        self.parse_body()
    }

    // when_expression = simple_expression { "either" simple_expression };
    fn parse_match_expression(&mut self) -> Result<WhenExpression, Box<dyn Error>> {
        let mut simple_exprs: Vec<SimpleExpression> = Vec::new();
        loop {
            let simple_expression = self.parse_simple_expression()?;
            simple_exprs.push(simple_expression);

            match self.lexer.peek_token()? {
                Token::Either => {
                    self.lexer.next_token()?;
                }
                _ => break,
            }
        }

        Ok(WhenExpression { simple_exprs })
    }

    // return_statement = "ret" [expression] ";";
    fn parse_return(&mut self) -> Result<Statement, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::Return => (),
            _ => panic!("expected 'ret' token, got {:?}", token),
        }

        let value = match self.lexer.peek_token()? {
            Token::Semicolon => None,
            _ => Some(self.parse_expression()?),
        };

        let token = self.lexer.next_token()?;
        match token {
            Token::Semicolon => (),
            _ => panic!("expected ';' token, got {:?}", token),
        }

        Ok(Statement::Return(Return { value }))
    }

    // function_statement = "fn" identifier "(" [ parameter_list ] ")" "{" { statement } "}";
    fn prase_fn(&mut self) -> Result<Statement, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::Fn => (),
            _ => panic!("expected 'fn' token, got {:?}", token),
        }

        let name = match self.lexer.next_token()? {
            Token::Identifier(name) => name,
            _ => panic!("expected identifier token, got {:?}", token),
        };

        let token = self.lexer.next_token()?;
        match token {
            Token::LeftParen => (),
            _ => panic!("expected '(' token, got {:?}", token),
        }

        let parameters = self.parse_parameters()?;

        let body = self.parse_body()?;

        Ok(Statement::Function(Function {
            name,
            parameters,
            body,
        }))
    }

    // parameter_list = parameter { "," parameter };
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, Box<dyn Error>> {
        let mut parameters: Vec<Parameter> = Vec::new();

        loop {
            let parameter = self.parse_parameter()?;
            parameters.push(parameter);

            match self.lexer.next_token()? {
                Token::Comma => (),
                Token::RightParen => break,
                _token => panic!("expected ',' or ')' token, got {:?}", _token),
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
            _token => panic!("expected identifier token, got {:?}", _token),
        };

        Ok(Parameter { name, kind })
    }

    // loop_substatement = "end;"
    //                     "next;"
    fn parse_subloop(&mut self) -> Result<Statement, Box<dyn Error>> {
        let sub_loop_kind = match self.lexer.next_token()? {
            Token::Next => SubLoopKind::Next,
            Token::End => SubLoopKind::End,
            _ => panic!("parse subloop error"),
        };

        let token = self.lexer.next_token()?;
        match token {
            Token::Semicolon => (),
            _ => panic!("expected ';' token, got {:?}", token),
        }

        Ok(Statement::SubLoop(SubLoop {
            kind: sub_loop_kind,
        }))
    }

    // conditional_statement = "if" expression "{" { statement } "}" [ "else" "{" { statement } "}" ];
    fn parse_if(&mut self) -> Result<Statement, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::If => (),
            _ => panic!("expected 'if' token, got {:?}", token),
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

        Ok(Statement::Conditional(Conditional {
            condition,
            then_body,
            else_body,
        }))
    }

    // loop_statement = "for" identifier "in" iterator_expression "{" { statement } "}";
    fn parse_for_loop(&mut self) -> Result<Statement, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::For => (),
            _ => panic!("expected 'for' token, got {:?}", token),
        }

        let iter_var = match self.lexer.next_token()? {
            Token::Identifier(name) => name,
            _ => panic!("expected 'identifier' token, got {:?}", token),
        };

        let token = self.lexer.next_token()?;
        match token {
            Token::In => (),
            _ => panic!("expected 'in' token, got {:?}", token),
        }

        let iterator = self.parse_iterated()?;

        let body = self.parse_body()?;

        Ok(Statement::ForLoop(ForLoop {
            iter_var,
            iterator,
            body,
        }))
    }

    // iterator_expression = vector | identifier | range_expression | function_call;
    fn parse_iterated(&mut self) -> Result<IteratorExpression, Box<dyn Error>> {
        match self.lexer.peek_token()? {
            Token::Integer(_) => {
                let start = self.parse_range_factor()?;
                let token = self.lexer.next_token()?;
                match token {
                    Token::Range => {
                        let end = self.parse_range_factor()?;
                        Ok(IteratorExpression::Range(RangeExpression { start, end }))
                    }
                    _ => panic!("expected '..' token, got {:?}", token),
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
            Token::LeftBrace => {
                self.lexer.next_token()?;
                Ok(IteratorExpression::Vector(self.parse_vector()?))
            }
            _token => panic!("expected 'Factor' or 'identifier' token, got {:?}", _token),
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
        let token = self.lexer.next_token()?;
        match token {
            Token::LeftBrace => (),
            _ => panic!("expected '{{' token, got {:?}", token),
        }

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
    fn parse_assignment(&mut self, name: String) -> Result<Statement, Box<dyn Error>> {
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

    // vector = "[" [ expression { "," expression } ] "]";
    fn parse_vector(&mut self) -> Result<Vector, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        match token {
            Token::LeftBracket => (),
            _ => panic!("expected '[' token, got {:?}", token),
        }

        let mut values = Vec::new();

        loop {
            let expression = self.parse_expression()?;
            values.push(expression);

            match self.lexer.next_token()? {
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
            Token::Identifier(_) => self.parse_starts_identifier(),
            Token::LeftParen => self.parse_paren_expression(),
            _ => panic!("expected factor, got {:?}", self.lexer.peek_token()?),
        }
    }

    fn parse_starts_identifier(&mut self) -> Result<Factor, Box<dyn Error>> {
        let token = self.lexer.next_token()?;
        let ident = match token {
            Token::Identifier(ident) => ident,
            _ => panic!("expected identifier, got {:?}", token),
        };

        match self.lexer.peek_token()? {
            Token::LeftParen => Ok(Factor::FunctionCall(self.parse_function_call(ident)?)),
            Token::LeftBracket => self.parse_vector_access(ident),
            _ => Ok(Factor::Identifier(ident)),
        }
    }

    // vector_access = (identifier | function_call) "[" expression "]";
    fn parse_vector_access(&mut self, ident: String) -> Result<Factor, Box<dyn Error>> {
        let vector_expr = match self.lexer.peek_token()? {
            Token::Identifier(ident) => VectorExpr::Identifier(ident),
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

    // function_call = identifier "(" [ expression { "," expression } ] ")";
    fn parse_function_call(&mut self, ident: String) -> Result<FunctionCall, Box<dyn Error>> {
        let mut arguments: Vec<Expression> = Vec::new();
        self.lexer.next_token()?;

        loop {
            let expression = self.parse_expression()?;
            arguments.push(expression);

            match self.lexer.next_token()? {
                Token::Comma => (),
                Token::RightParen => break,
                _token => panic!("expected ',' or ')' token, got {:?}", _token),
            }
        }

        Ok(FunctionCall {
            name: ident,
            arguments,
        })
    }

    // "(" expression ")";
    fn parse_paren_expression(&mut self) -> Result<Factor, Box<dyn Error>> {
        self.lexer.next_token()?;
        let expression = self.parse_expression()?;
        self.lexer.next_token()?;
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
    fn test_parse_simple() {
        let mut parser = Parser::new(Input::String("let a = 1 + 2 * 3;".to_string()));
        let statement = parser.parse_statement().unwrap();

        assert_eq!(ron::to_string(&statement).unwrap(), "VarDeclaration((name:\"a\",kind:Mutable,expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[]),(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None),(inversion:(value:Literal(Int(3)),negated:false),to:None)],ops:[Multiply])],ops:[Add]),op:None,rhs:None)])])))");
    }

    #[test]
    fn test_parse_paren_expression() {
        let mut parser = Parser::new(Input::String("let a = (1 + 2) * 3;".to_string()));
        let statement = parser.parse_statement().unwrap();

        println!("{}", ron::to_string(&statement).unwrap());

        assert_eq!(ron::to_string(&statement).unwrap(), "VarDeclaration((name:\"a\",kind:Mutable,expression:(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Parenthesized((conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[]),(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[Add]),op:None,rhs:None)])])),negated:false),to:None),(inversion:(value:Literal(Int(3)),negated:false),to:None)],ops:[Multiply])],ops:[]),op:None,rhs:None)])])))");
    }

    #[test]
    fn test_parse_for_loop_simple_range_no_body() {
        let mut parser = Parser::new(Input::String("for t in 1 to 2 {}".to_string()));
        let statement = parser.parse_statement().unwrap();

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
    fn test_parse_vector() {
        let mut parser = Parser::new(Input::String("[1, 2, i]".to_string()));
        let vector = parser.parse_vector().unwrap();

        assert_eq!(ron::to_string(&vector).unwrap(), "(values:[(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Identifier(\"i\"),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])])])");
    }

    #[test]
    fn test_parse_starts_ident_function_call() {
        let mut parser = Parser::new(Input::String("test(1, 2, 3)".to_string()));
        let starts_ident = parser.parse_starts_identifier().unwrap();

        assert_eq!(ron::to_string(&starts_ident).unwrap(), "FunctionCall((name:\"test\",arguments:[(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(1)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(2)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])]),(conjunctions:[(relations:[(lhs:(terms:[(conversions:[(inversion:(value:Literal(Int(3)),negated:false),to:None)],ops:[])],ops:[]),op:None,rhs:None)])])]))");
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

    fn copy() {
        let mut parser = Parser::new(Input::String("for t in 1 to 2 {}".to_string()));
        let statement = parser.parse_statement().unwrap();

        println!("{}", ron::to_string(&statement).unwrap());
        //assert_eq!(ron::to_string(&statement).unwrap(), );
    }
}
