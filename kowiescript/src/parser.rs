// parser based on my lexer and ebnf

#[derive(Debug, Clone)]
pub struct ParseNode {
    pub children: Vec<ParseNode>,
    pub value: Token,
}

impl ParseNode {
    pub fn new(token: Token) -> ParseNode {
        ParseNode {
            children: Vec::new(),
            value: token,
        }
    }
}

/* statement          = variable_declaration
                      | constant_declaration
                      | assign_declaration
                      | loop_statement
                      | loop_substatment
                      | conditional_statement
                      | function_statement
                      | return_statement
                      | pattern_match_stmt; */
enum Statement {
    VarDeclaration(ParseNode),
    ConstDeclaration(ParseNode),
    Assignment(ParseNode),
    Loop(ParseNode),
    SubLoop(ParseNode),
    Conditional(ParseNode),
    Function(ParseNode),
    Return(ParseNode),
    PatternMatch(ParseNode),
    Expression(ParseNode),
}

struct Block {
    statements: Vec<Statement>,

}

struct Parser {
    lexer: Lexer,
}

impl Parser {
    fn new(input: Input) -> Parser {
        Parser {
            lexer: Lexer::new(input),
        }
    }

    fn parse(&mut self) -> Result<(), Error> {
        while let token = self.lexer.next_token()? {
            match token {

            }
        }
    }


}