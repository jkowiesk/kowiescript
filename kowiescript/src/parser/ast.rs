use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum VarKind {
    Constant,
    Mutable,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Vector(Vec<Value>),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VarDeclaration {
    pub name: String,
    pub kind: VarKind,
    pub expression: Expression,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Assignment {
    pub name: String,
    pub expression: Expression,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ForLoop {
    pub iter_var: String,
    pub iterator: IteratorExpression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SubLoop {
    pub kind: SubLoopKind,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum SubLoopKind {
    End,
    Next,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Conditional {
    pub condition: Expression,
    pub then_branch: Vec<Statement>,
    pub else_branch: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Parameter {
    pub name: String,
    pub kind: VarKind,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Return {
    pub value: Option<Expression>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PatternMatch {
    pub expression: Expression,
    pub when_branches: Vec<WhenBranch>,
    pub default_branch: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct WhenBranch {
    pub pattern: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Statement {
    VarDeclaration(VarDeclaration),
    Assignment(Assignment),
    ForLoop(ForLoop),
    SubLoop(SubLoop),
    Conditional(Conditional),
    Function(Function),
    Return(Return),
    PatternMatch(PatternMatch),
    Expression(Expression),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum RelationalOperator {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Expression {
    pub conjunctions: Vec<Conjunction>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Conjunction {
    pub relations: Vec<Relation>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Relation {
    pub lhs: SimpleExpression,
    pub op: Option<RelationalOperator>,
    pub rhs: Option<SimpleExpression>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SimpleExpression {
    pub terms: Vec<Term>,
    pub ops: Vec<ArithmeticOperator>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Term {
    pub conversions: Vec<Conversion>,
    pub ops: Vec<ArithmeticOperator>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Conversion {
    pub inversion: Inversion,
    pub to: Option<ConversionType>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum ConversionType {
    Int,
    Float,
    String,
    Bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Inversion {
    pub value: Factor,
    pub negated: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Factor {
    Literal(Literal),
    Identifier(String),
    Vector(Vec<Expression>),
    FunctionCall(FunctionCall),
    VectorAccess(VectorAccess),
    Parenthesized(Expression),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VectorAccess {
    pub name: String,
    pub index: Expression,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum IteratorExpression {
    Range(RangeExpression),
    Vector(VectorExpression),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RangeExpression {
    pub start: Option<Expression>,
    pub end: Expression,
    pub step: Option<Expression>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VectorExpression {
    pub values: Vec<Expression>,
}
