use std::{
    fmt,
    ops::{Add, Div, Mul, Rem, Sub},
};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum VarKind {
    Constant,
    Mutable,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, PartialOrd)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Vector(Vec<Value>),
    Void,
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
    pub then_body: Vec<Statement>,
    pub else_body: Option<Vec<Statement>>,
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
    pub default_branch: Vec<Statement>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct WhenBranch {
    pub pattern: WhenExpression,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct WhenExpression {
    pub simple_exprs: Vec<SimpleExpression>,
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
    Vector(Vector),
    FunctionCall(FunctionCall),
    VectorAccess(VectorAccess),
    Parenthesized(Expression),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum VectorExpr {
    FunctionCall(FunctionCall),
    Identifier(String),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct VectorAccess {
    pub vector_expr: VectorExpr,
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
    Vector(Vector),
    Identifier(String),
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RangeExpression {
    pub start: Factor,
    pub end: Factor,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Vector {
    pub values: Vec<Expression>,
}

// impl traits

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match self {
            Value::Int(left) => match other {
                Value::Int(right) => Value::Int(left + right),
                Value::Float(right) => Value::Float(left as f64 + right),
                _ => unimplemented!("Cannot add {:?} and {:?}", self, other),
            },
            Value::Float(left) => match other {
                Value::Int(right) => Value::Float(left + right as f64),
                Value::Float(right) => Value::Float(left + right),
                _ => unimplemented!("Cannot add {:?} and {:?}", self, other),
            },
            Value::String(left) => match other {
                Value::String(right) => Value::String(left + &right),
                _ => unimplemented!("Cannot add {:?} and {:?}", left, other),
            },
            _ => unimplemented!("Cannot add {:?} and {:?}", self, other),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match self {
            Value::Int(left) => match other {
                Value::Int(right) => Value::Int(left - right),
                Value::Float(right) => Value::Float(left as f64 - right),
                _ => unimplemented!("Cannot subtract {:?} and {:?}", self, other),
            },
            Value::Float(left) => match other {
                Value::Int(right) => Value::Float(left - right as f64),
                Value::Float(right) => Value::Float(left - right),
                _ => unimplemented!("Cannot subtract {:?} and {:?}", self, other),
            },
            _ => unimplemented!("Cannot subtract {:?} and {:?}", self, other),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match self {
            Value::Int(left) => match other {
                Value::Int(right) => Value::Int(left * right),
                Value::Float(right) => Value::Float(left as f64 * right),
                _ => unimplemented!("Cannot multiply {:?} and {:?}", self, other),
            },
            Value::Float(left) => match other {
                Value::Int(right) => Value::Float(left * right as f64),
                Value::Float(right) => Value::Float(left * right),
                _ => unimplemented!("Cannot multiply {:?} and {:?}", self, other),
            },
            _ => unimplemented!("Cannot multiply {:?} and {:?}", self, other),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match self {
            Value::Int(left) => match other {
                Value::Int(right) => Value::Int(left / right),
                Value::Float(right) => Value::Float(left as f64 / right),
                _ => unimplemented!("Cannot divide {:?} and {:?}", self, other),
            },
            Value::Float(left) => match other {
                Value::Int(right) => Value::Float(left / right as f64),
                Value::Float(right) => Value::Float(left / right),
                _ => unimplemented!("Cannot divide {:?} and {:?}", self, other),
            },
            _ => unimplemented!("Cannot divide {:?} and {:?}", self, other),
        }
    }
}

impl Rem for Value {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        match self {
            Value::Int(left) => match other {
                Value::Int(right) => Value::Int(left % right),
                Value::Float(right) => Value::Float(left as f64 % right),
                _ => unimplemented!("Cannot modulo {:?} and {:?}", self, other),
            },
            Value::Float(left) => match other {
                Value::Int(right) => Value::Float(left % right as f64),
                Value::Float(right) => Value::Float(left % right),
                _ => unimplemented!("Cannot modulo {:?} and {:?}", self, other),
            },
            _ => unimplemented!("Cannot modulo {:?} and {:?}", self, other),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Vector(vector) => {
                write!(f, "[")?;
                for (i, value) in vector.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Value::Void => write!(f, "void"),
        }
    }
}
