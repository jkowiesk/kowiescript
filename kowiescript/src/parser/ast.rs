use std::{
    collections::HashMap,
    error::Error,
    fmt::{self},
    ops::{Add, Div, Mul, Not, Rem, Sub},
};

use serde::{Deserialize, Serialize};

use crate::interpreter::{Interpreter, InterpreterErrorKind, Variable};

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
    pub body: Vec<SourceStatement>,
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
    pub then_body: Vec<SourceStatement>,
    pub else_body: Option<Vec<SourceStatement>>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Vec<SourceStatement>,
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
    pub default_branch: Vec<SourceStatement>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct WhenBranch {
    pub pattern: WhenExpression,
    pub body: Vec<SourceStatement>,
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
pub struct SourceStatement {
    pub stmt: Statement,
    pub line: usize,
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
    VectorAccess(VectorAccess),
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

// interpreters types

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum InternalFunction {
    Print,
    Push,
    Remove,
}

impl InternalFunction {
    pub fn get_string(&self) -> String {
        match self {
            InternalFunction::Print => String::from("print"),
            InternalFunction::Push => String::from("push"),
            InternalFunction::Remove => String::from("remove"),
        }
    }
}

#[derive(Clone)]
pub struct CustomFunction<F> {
    pub f: F,
}

impl<F> Call for CustomFunction<F>
where
    F: Fn(&mut Interpreter, Vec<Value>) -> Result<Value, Box<dyn Error>>,
{
    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        (self.f)(ctx, args)
    }
}

impl<F> Fun for CustomFunction<F>
where
    F: Fn(&mut Interpreter, Vec<Value>) -> Result<Value, Box<dyn Error>> + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn Fun> {
        Box::new(self.clone())
    }
}

pub trait Call {
    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>>;
}

pub trait Fun: Call {
    fn clone_box(&self) -> Box<dyn Fun>;
}

impl Fun for InternalFunction {
    fn clone_box(&self) -> Box<dyn Fun> {
        Box::new(self.clone())
    }
}

impl Fun for Function {
    fn clone_box(&self) -> Box<dyn Fun> {
        Box::new(self.clone())
    }
}
use InterpreterErrorKind::*;

impl Call for InternalFunction {
    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match self {
            InternalFunction::Push => {
                let vector_name = match &args[0] {
                    Value::String(name) => name,
                    _ => {
                        return Err(
                            ctx.error(Error(String::from("First argument must be a string")))
                        );
                    }
                };
                let vector = ctx.variables.last_mut().unwrap().get_mut(vector_name);
                let vector = match vector {
                    Some(vector) => vector,
                    None => {
                        return Err(ctx.error(VariableNotDeclared(vector_name.to_string())));
                    }
                };

                let value = args[1].clone();
                match vector {
                    Variable {
                        kind: VarKind::Mutable,
                        value: Value::Vector(vec),
                    } => {
                        vec.push(value);
                        Ok(Value::Void)
                    }
                    _ => Err(ctx.error(Error(
                        "Second argument must be a valid vector element".to_string(),
                    ))),
                }
            }
            InternalFunction::Remove => {
                let vector_name = match &args[0] {
                    Value::String(name) => name,
                    _ => {
                        return Err(
                            ctx.error(Error(String::from("First argument must be a string")))
                        );
                    }
                };
                let vector = ctx.variables.last_mut().unwrap().get_mut(vector_name);
                let vector = match vector {
                    Some(vector) => vector,
                    None => {
                        return Err(ctx.error(VariableNotDeclared(vector_name.to_string())));
                    }
                };

                let index = match &args[1] {
                    Value::Int(index) => *index as usize,
                    _ => {
                        return Err(
                            ctx.error(Error(String::from("Second argument must be an integer")))
                        );
                    }
                };

                match vector {
                    Variable {
                        kind: VarKind::Mutable,
                        value: Value::Vector(vec),
                    } => {
                        vec.remove(index);
                        Ok(Value::Void)
                    }
                    _ => Err(ctx.error(Error(
                        "Second argument must be a valid vector element".to_string(),
                    ))),
                }
            }
            InternalFunction::Print => {
                for arg in args {
                    print!("{}", arg);
                }
                println!();
                Ok(Value::Void)
            }
        }
    }
}

impl Call for Function {
    fn call(&self, ctx: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
        // Create a new scope for this function call
        ctx.variables.push(HashMap::new());

        for (i, param) in self.parameters.iter().enumerate() {
            ctx.variables.last_mut().unwrap().insert(
                param.name.clone(),
                Variable {
                    kind: param.kind.clone(),
                    value: args[i].clone(),
                },
            );
        }

        let mut ret = None;

        for statement in &self.body {
            ctx.interpret_statement(statement)?;
            ret = ctx.variables.last().unwrap().get("ret").cloned();
            if ret.is_some() {
                break;
            }
        }

        //remove local variables from the scope
        for param in self.parameters.iter() {
            ctx.variables.last_mut().unwrap().remove(&param.name);
        }

        let ret = ret.unwrap_or(Variable {
            kind: VarKind::Mutable, // Or the default kind you wish to use
            value: Value::Void,
        });

        // Remove the function's scope
        ctx.variables.pop();

        Ok(ret.value)
    }
}

// impl traits for Value

impl Add for Value {
    type Output = Result<Self, String>;

    fn add(self, other: Self) -> Self::Output {
        match (self.clone(), other.clone()) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left + right)),
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left + right)),
            (Value::String(left), Value::String(right)) => Ok(Value::String(left + &right)),
            _ => Err(format!("Cannot add {:?} and {:?}", self, other)),
        }
    }
}

impl Sub for Value {
    type Output = Result<Self, String>;

    fn sub(self, other: Self) -> Self::Output {
        match (self.clone(), other.clone()) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left - right)),
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left - right)),
            _ => Err(format!("Cannot subtract {:?} and {:?}", self, other)),
        }
    }
}

impl Mul for Value {
    type Output = Result<Self, String>;

    fn mul(self, other: Self) -> Self::Output {
        match (self.clone(), other.clone()) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left * right)),
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left * right)),
            _ => Err(format!("Cannot multiply {:?} and {:?}", self, other)),
        }
    }
}

impl Div for Value {
    type Output = Result<Self, String>;

    fn div(self, other: Self) -> Self::Output {
        if other == Value::Int(0) || other == Value::Float(0.0) {
            return Err(format!("Cannot divide {:?} by zero", self));
        }
        match (self.clone(), other.clone()) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left / right)),
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left / right)),
            _ => Err(format!("Cannot divide {:?} and {:?}", self, other)),
        }
    }
}

impl Rem for Value {
    type Output = Result<Self, String>;

    fn rem(self, other: Self) -> Self::Output {
        match (self.clone(), other.clone()) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left % right)),
            (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left % right)),
            _ => Err(format!("Cannot modulo {:?} and {:?}", self, other)),
        }
    }
}

impl Not for Value {
    type Output = Result<Self, String>;

    fn not(self) -> Self::Output {
        match self {
            Value::Int(int) => Ok(Value::Int(-int)),
            Value::Float(float) => Ok(Value::Float(-float)),
            Value::String(string) => Ok(Value::String(string)),
            Value::Bool(bool) => Ok(Value::Bool(!bool)),
            Value::Void => Err("Cannot negate void".to_string()),
            Value::Vector(_) => Err("Cannot negate vector".to_string()),
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
