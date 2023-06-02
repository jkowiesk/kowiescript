use std::{collections::HashMap, error::Error, fmt, vec};

use crate::{
    parser::ast::{ArithmeticOperator, *},
    Program,
};

#[derive(Debug, Clone)]
pub struct Variable {
    pub kind: VarKind,
    pub value: Value,
}

pub struct Interpreter {
    pub variables: Vec<HashMap<String, Variable>>,
    pub functions: HashMap<String, Box<dyn Fun>>,
    pub lines: Vec<usize>,
}

impl Clone for Interpreter {
    fn clone(&self) -> Self {
        Interpreter {
            variables: self.variables.clone(),
            functions: self
                .functions
                .iter()
                .map(|(k, v)| (k.clone(), v.clone_box()))
                .collect(),
            lines: self.lines.clone(),
        }
    }
}

use InterpreterErrorKind::*;

impl Default for Interpreter {
    fn default() -> Self {
        let mut inter = Self::new();
        inter.functions.insert(
            InternalFunction::Print.get_string(),
            Box::new(InternalFunction::Print),
        );
        inter.functions.insert(
            InternalFunction::Push.get_string(),
            Box::new(InternalFunction::Push),
        );
        inter
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: vec![HashMap::new()],
            functions: HashMap::new(),
            lines: Vec::new(),
        }
    }

    pub fn interpret_program(&mut self, program: &Program) -> Result<(), Box<dyn Error>> {
        for statement in program {
            self.interpret_statement(statement)?;
        }

        Ok(())
    }

    pub fn interpret_statement(
        &mut self,
        statement: &SourceStatement,
    ) -> Result<(), Box<dyn Error>> {
        self.lines.push(statement.line);
        match &statement.stmt {
            Statement::VarDeclaration(var_declaration) => {
                self.interpret_var_declaration(var_declaration)?
            }
            Statement::Assignment(assignment) => self.interpret_assignment(assignment)?,
            Statement::ForLoop(for_loop) => self.interpret_for_loop(for_loop)?,
            Statement::SubLoop(sub_loop) => self.interpret_sub_loop(sub_loop)?,
            Statement::Function(function) => self.interpret_function(function)?,
            Statement::PatternMatch(pattern_match) => {
                self.interpret_pattern_match(pattern_match)?
            }
            Statement::Return(return_stmt) => self.interpret_return(return_stmt)?,
            Statement::Expression(expression) => {
                self.evaluate_expression(expression)?;
            }
            Statement::Conditional(cond) => self.interpret_if(cond)?,
        };
        self.lines.pop();
        Ok(())
    }

    fn interpret_function(&mut self, function: &Function) -> Result<(), Box<dyn Error>> {
        self.functions
            .insert(function.name.clone(), Box::new(function.clone()));

        Ok(())
    }

    fn inject_iternal_function(
        &mut self,
        name: String,
        func: CustomFunction<
            impl Fn(&mut Interpreter, Vec<Value>) -> Result<Value, Box<dyn Error>> + Clone + 'static,
        >,
    ) {
        self.functions.insert(name, Box::new(func));
    }

    fn interpret_return(&mut self, return_stmt: &Return) -> Result<(), Box<dyn Error>> {
        if let Some(expr) = &return_stmt.value {
            let value = self.evaluate_expression(expr)?;
            self.variables.last_mut().unwrap().insert(
                "ret".to_string(),
                Variable {
                    kind: VarKind::Constant,
                    value,
                },
            );
        } else {
            self.variables.last_mut().unwrap().insert(
                "ret".to_string(),
                Variable {
                    kind: VarKind::Constant,
                    value: Value::Void,
                },
            );
        }

        Ok(())
    }

    fn interpret_pattern_match(
        &mut self,
        pattern_match: &PatternMatch,
    ) -> Result<(), Box<dyn Error>> {
        let parent = self.evaluate_expression(&pattern_match.expression)?;
        let mut matched = false;
        for when_branch in &pattern_match.when_branches {
            let mut matched_branch = false;
            for simple_expr in &when_branch.pattern.simple_exprs {
                let child = self.evaluate_simple_expression(simple_expr)?;
                if parent == child {
                    matched_branch = true;
                    break;
                }
            }
            if matched_branch {
                matched = true;
                for statement in &when_branch.body {
                    self.interpret_statement(statement)?;
                }
                break;
            }
        }
        if !matched {
            for statement in &pattern_match.default_branch {
                self.interpret_statement(statement)?;
            }
        }

        Ok(())
    }

    fn interpret_if(&mut self, cond: &Conditional) -> Result<(), Box<dyn Error>> {
        let expr = self.evaluate_expression(&cond.condition)?;
        if let Value::Bool(val) = expr {
            if val {
                for source_stmt in &cond.then_body {
                    self.interpret_statement(source_stmt)?;
                }
            } else if let Some(else_body) = &cond.else_body {
                for source_stmt in else_body {
                    self.interpret_statement(source_stmt)?;
                }
            }
        }

        Ok(())
    }

    fn interpret_var_declaration(
        &mut self,
        var_declaration: &VarDeclaration,
    ) -> Result<(), Box<dyn Error>> {
        let value = self.evaluate_expression(&var_declaration.expression)?;
        self.variables.last_mut().unwrap().insert(
            var_declaration.name.clone(),
            Variable {
                kind: var_declaration.kind.clone(),
                value,
            },
        );

        Ok(())
    }
    fn interpret_assignment(&mut self, assignment: &Assignment) -> Result<(), Box<dyn Error>> {
        let value = self.evaluate_expression(&assignment.expression)?;
        let var = self.variables.last_mut().unwrap().get_mut(&assignment.name);

        match var {
            Some(var) => {
                if let VarKind::Constant = var.kind {
                    return Err(self.error(ConstantReassignment(assignment.name.clone())));
                } else {
                    var.value = value;
                }
            }
            None => {
                return Err(self.error(VariableNotDeclared(assignment.name.clone())));
            }
        }

        Ok(())
    }

    fn interpret_for_loop(&mut self, for_loop: &ForLoop) -> Result<(), Box<dyn Error>> {
        let iterable = self.evaluate_iterator_expression(&for_loop.iterator)?;

        'outer: for value in iterable {
            self.variables.last_mut().unwrap().insert(
                for_loop.iter_var.clone(),
                Variable {
                    kind: VarKind::Mutable,
                    value,
                },
            );
            for statement in &for_loop.body {
                match &statement.stmt {
                    Statement::SubLoop(sub_loop) => match sub_loop.kind {
                        SubLoopKind::End => {
                            self.variables
                                .last_mut()
                                .unwrap()
                                .remove(&for_loop.iter_var);
                            break 'outer;
                        }
                        SubLoopKind::Next => break,
                    },
                    _ => {
                        self.interpret_statement(statement)?;
                    }
                }
            }

            self.variables
                .last_mut()
                .unwrap()
                .remove(&for_loop.iter_var);
        }

        Ok(())
    }

    fn interpret_sub_loop(&mut self, sub_loop: &SubLoop) -> Result<(), Box<dyn Error>> {
        match sub_loop.kind {
            SubLoopKind::End => Ok(()),
            SubLoopKind::Next => Ok(()),
        }
    }

    fn evaluate_expression(&mut self, expression: &Expression) -> Result<Value, Box<dyn Error>> {
        match expression.conjunctions.len() {
            0 => Ok(Value::Bool(false)),
            1 => self.evaluate_conjunction(&expression.conjunctions[0]),
            _ => {
                let mut result = self.evaluate_conjunction(&expression.conjunctions[0])?;
                for conjunction in &expression.conjunctions[1..] {
                    let conjunction_value = self.evaluate_conjunction(conjunction)?;

                    result = match result {
                        Value::Bool(false) => Value::Bool(false),
                        Value::Bool(true) => conjunction_value,
                        _ => unimplemented!("Conjunction evaluation not implemented."),
                    };
                }
                Ok(result)
            }
        }
    }

    fn evaluate_conjunction(&mut self, conjunction: &Conjunction) -> Result<Value, Box<dyn Error>> {
        match conjunction.relations.len() {
            0 => Ok(Value::Bool(true)),
            1 => self.evaluate_relation(&conjunction.relations[0]),
            _ => {
                let mut result = self.evaluate_relation(&conjunction.relations[0])?;
                for relation in &conjunction.relations[1..] {
                    let relation_value = self.evaluate_relation(relation)?;
                    // Evaluate using short-circuit evaluation rules
                    result = match result {
                        Value::Bool(true) => relation_value,
                        Value::Bool(false) => Value::Bool(false),
                        _ => unimplemented!("Relation evaluation not implemented."),
                    };
                }
                Ok(result)
            }
        }
    }

    fn evaluate_relation(&mut self, relation: &Relation) -> Result<Value, Box<dyn Error>> {
        let lhs = self.evaluate_simple_expression(&relation.lhs);
        if let Some(op) = &relation.op {
            let rhs = relation
                .rhs
                .as_ref()
                .map(|expr| self.evaluate_simple_expression(expr))
                .transpose()?;
            let lhs = lhs?;

            match op {
                RelationalOperator::Less => self.compare_values(lhs, rhs, |a, b| a < b),
                RelationalOperator::LessEqual => self.compare_values(lhs, rhs, |a, b| a <= b),
                RelationalOperator::Greater => self.compare_values(lhs, rhs, |a, b| a > b),
                RelationalOperator::GreaterEqual => self.compare_values(lhs, rhs, |a, b| a >= b),
                RelationalOperator::Equal => self.compare_values(lhs, rhs, |a, b| a == b),
                RelationalOperator::NotEqual => self.compare_values(lhs, rhs, |a, b| a != b),
            }
        } else {
            lhs // No relational operator, return the value as is
        }
    }

    fn evaluate_simple_expression(
        &mut self,
        simple_expression: &SimpleExpression,
    ) -> Result<Value, Box<dyn Error>> {
        let mut result = self.evaluate_term(&simple_expression.terms[0])?;
        let mut term_iter = simple_expression.terms.iter().skip(1).peekable();

        for op in &simple_expression.ops {
            let term = term_iter
                .next()
                .expect("Mismatched number of terms and operators");
            let term_value = self.evaluate_term(term)?;

            result = match op {
                ArithmeticOperator::Add => match result + term_value {
                    Ok(val) => val,
                    Err(msg) => {
                        return Err(self.error(Error(msg)));
                    }
                },
                ArithmeticOperator::Subtract => match result - term_value {
                    Ok(val) => val,
                    Err(msg) => {
                        return Err(self.error(Error(msg)));
                    }
                },
                _ => Err(self.error(ArithmeticOperatorErr(op.clone())))?,
            };
        }

        Ok(result)
    }

    fn evaluate_term(&mut self, term: &Term) -> Result<Value, Box<dyn Error>> {
        let mut result = self.evaluate_conversion(&term.conversions[0])?;
        let mut conversion_iter = term.conversions.iter().skip(1).peekable();

        for op in &term.ops {
            let conversion = conversion_iter
                .next()
                .expect("Mismatched number of conversions and operators");
            let conversion_value = self.evaluate_conversion(conversion)?;

            result = match op {
                ArithmeticOperator::Multiply => match result * conversion_value {
                    Ok(val) => val,
                    Err(msg) => {
                        return Err(self.error(Error(msg)));
                    }
                },
                ArithmeticOperator::Divide => match result / conversion_value {
                    Ok(val) => val,
                    Err(msg) => {
                        return Err(self.error(Error(msg)));
                    }
                },
                ArithmeticOperator::Modulo => match result % conversion_value {
                    Ok(val) => val,
                    Err(msg) => {
                        return Err(self.error(Error(msg)));
                    }
                },
                _ => Err(self.error(ArithmeticOperatorErr(op.clone())))?,
            };
        }

        Ok(result)
    }

    fn evaluate_conversion(&mut self, conversion: &Conversion) -> Result<Value, Box<dyn Error>> {
        let inverted_value = self.evaluate_inversion(&conversion.inversion)?;

        if let Some(conversion_type) = &conversion.to {
            Ok(self.convert_value(inverted_value, conversion_type)?)
        } else {
            Ok(inverted_value) // No conversion specified, return the inverted value as is
        }
    }

    fn evaluate_inversion(&mut self, inversion: &Inversion) -> Result<Value, Box<dyn Error>> {
        let value = self.evaluate_factor(&inversion.value)?;

        if inversion.negated {
            self.negate_value(value)
        } else {
            Ok(value) // No negation, return the value as is
        }
    }

    fn evaluate_factor(&mut self, factor: &Factor) -> Result<Value, Box<dyn Error>> {
        match factor {
            Factor::Literal(literal) => self.evaluate_literal(literal),
            Factor::Identifier(name) => self.evaluate_identifier(name),
            Factor::Vector(vector) => {
                let mut values = Vec::new();
                for expr in &vector.values {
                    values.push(self.evaluate_expression(expr)?);
                }

                Ok(Value::Vector(values))
            }
            Factor::FunctionCall(func_call) => self.evaluate_function_call(func_call),
            Factor::VectorAccess(vector_access) => self.evaluate_vector_access(vector_access),
            Factor::Parenthesized(expression) => self.evaluate_expression(expression),
        }
    }

    fn evaluate_identifier(&mut self, name: &str) -> Result<Value, Box<dyn Error>> {
        for scope in self.variables.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Ok(var.value.clone());
            }
        }
        return Err(self.error(VariableNotDeclared(name.to_string())));
    }

    fn evaluate_literal(&mut self, literal: &Literal) -> Result<Value, Box<dyn Error>> {
        match literal {
            Literal::Int(value) => Ok(Value::Int(*value)),
            Literal::Float(value) => Ok(Value::Float(*value)),
            Literal::String(value) => Ok(Value::String(value.clone())),
            Literal::Bool(value) => Ok(Value::Bool(*value)),
        }
    }

    fn evaluate_function_call(
        &mut self,
        func_call: &FunctionCall,
    ) -> Result<Value, Box<dyn Error>> {
        let name = &func_call.name;

        let mut args = Vec::new();
        for expr in &func_call.args {
            args.push(self.evaluate_expression(expr)?);
        }
        let ctx = self.clone();
        let func = match ctx.functions.get(name) {
            Some(func) => func,
            None => {
                return Err(self.error(FunctionNotDeclared(name.to_string())));
            }
        };

        func.call(self, args)
    }

    fn evaluate_vector_access(
        &mut self,
        vector_access: &VectorAccess,
    ) -> Result<Value, Box<dyn Error>> {
        let index = match self.evaluate_expression(&vector_access.index)? {
            Value::Int(int) => int,
            _ => {
                return Err(self.error(Error("Index must be an integer value".to_string())));
            }
        };

        match &vector_access.vector_expr {
            VectorExpr::Identifier(ident) => {
                for scope in self.variables.iter().rev() {
                    if let Some(vector) = scope.get(ident) {
                        match &vector.value {
                            Value::Vector(values) => {
                                if index < 0 || index >= values.len() as i64 {
                                    return Err(self.error(IndexOutOfBounds(index as usize)));
                                }
                                return Ok(values[index as usize].clone());
                            }
                            Value::String(string) => {
                                if index < 0 || index >= string.len() as i64 {
                                    return Err(self.error(IndexOutOfBounds(index as usize)));
                                }
                                return Ok(Value::String(
                                    string.chars().nth(index as usize).unwrap().to_string(),
                                ));
                            }
                            _ => {
                                return Err(
                                    self.error(Error("Variable is not a vector".to_string()))
                                );
                            }
                        }
                    }
                }
                return Err(self.error(VariableNotDeclared(ident.to_string())));
            }
            VectorExpr::FunctionCall(func_call) => {
                let vector = self.evaluate_function_call(func_call)?;
                match vector {
                    Value::Vector(values) => {
                        if index < 0 || index >= values.len() as i64 {
                            return Err(self.error(IndexOutOfBounds(index as usize)));
                        }
                        Ok(values[index as usize].clone())
                    }
                    _ => {
                        return Err(self.error(Error("Function doesn't return vector".to_string())))
                    }
                }
            }
        }
    }

    fn evaluate_iterator_expression(
        &mut self,
        iterator_expression: &IteratorExpression,
    ) -> Result<Vec<Value>, Box<dyn Error>> {
        match &iterator_expression {
            IteratorExpression::Range(range) => {
                let start = match self.evaluate_factor(&range.start)? {
                    Value::Int(value) => {
                        if value < 0 {
                            0
                        } else {
                            value
                        }
                    }
                    _ => {
                        return Err(self.error(RangeExpressionExpected));
                    }
                };
                let end = match self.evaluate_factor(&range.end)? {
                    Value::Int(value) => {
                        if value < 0 {
                            0
                        } else {
                            value
                        }
                    }
                    _ => {
                        return Err(self.error(RangeExpressionExpected));
                    }
                };

                // range to Vec<value>
                Ok((start..=end).map(Value::Int).collect())
            }
            IteratorExpression::Vector(vector) => {
                let mut result = Vec::new();
                for expression in &vector.values {
                    result.push(self.evaluate_expression(expression)?);
                }
                Ok(result)
            }
            IteratorExpression::Identifier(ident) => {
                for scope in self.variables.iter().rev() {
                    if let Some(var) = scope.get(ident) {
                        match &var.value {
                            Value::Vector(values) => return Ok(values.clone()),
                            _ => {
                                return Err(
                                    self.error(InterpreterErrorKind::RangeExpressionExpected)
                                )
                            }
                        }
                    }
                }
                Err(self.error(Error("Empty iterator expression".to_string())))
            }
            IteratorExpression::FunctionCall(func_call) => {
                let vector = self.evaluate_function_call(func_call)?;
                match vector {
                    Value::Vector(values) => Ok(values),
                    _ => {
                        return Err(self.error(Error("Function doesn't return vector".to_string())))
                    }
                }
            }
            IteratorExpression::VectorAccess(vector_access) => {
                let vector = self.evaluate_vector_access(vector_access)?;
                match vector {
                    Value::Vector(values) => Ok(values),
                    _ => {
                        return Err(self.error(Error("Cannot iterate over not-vector".to_string())))
                    }
                }
            }
        }
    }

    fn compare_values<F>(
        &self,
        lhs: Value,
        rhs: Option<Value>,
        comparator: F,
    ) -> Result<Value, Box<dyn Error>>
    where
        F: FnOnce(Value, Value) -> bool,
    {
        match rhs {
            Some(value) => Ok(Value::Bool(comparator(lhs, value))),
            None => Ok(Value::Bool(false)), // If rhs is None, treat it as false
        }
    }

    fn convert_value(
        &self,
        value: Value,
        conversion_type: &ConversionType,
    ) -> Result<Value, Box<dyn Error>> {
        Ok(match value {
            Value::Int(int) => match conversion_type {
                ConversionType::Int => Value::Int(int),
                ConversionType::Float => Value::Float(int as f64),
                ConversionType::String => Value::String(int.to_string()),
                ConversionType::Bool => Value::Bool(int != 0),
            },
            Value::Float(float) => match conversion_type {
                ConversionType::Int => Value::Int(float as i64),
                ConversionType::Float => Value::Float(float),
                ConversionType::String => Value::String(float.to_string()),
                ConversionType::Bool => Value::Bool(float != 0.0),
            },
            Value::String(string) => match conversion_type {
                ConversionType::Int => Value::Int(string.parse().unwrap()),
                ConversionType::Float => Value::Float(string.parse().unwrap()),
                ConversionType::String => Value::String(string),
                ConversionType::Bool => Value::Bool(!string.is_empty()),
            },
            Value::Bool(bool) => match conversion_type {
                ConversionType::Int => Value::Int(bool as i64),
                ConversionType::Float => Value::Float(bool as i64 as f64),
                ConversionType::String => Value::String(bool.to_string()),
                ConversionType::Bool => Value::Bool(bool),
            },
            Value::Void => return Err(self.error(Error("Cannot convert void".to_string()))),
            Value::Vector(_) => return Err(self.error(Error("Cannot convert vector".to_string()))),
        })
    }

    fn negate_value(&self, value: Value) -> Result<Value, Box<dyn Error>> {
        match value {
            Value::Int(int) => Ok(Value::Int(-int)),
            Value::Float(float) => Ok(Value::Float(-float)),
            Value::String(string) => Ok(Value::String(string)),
            Value::Bool(bool) => Ok(Value::Bool(!bool)),
            Value::Void => Err(self.error(Error("Cannot negate void".to_string()))),
            Value::Vector(_) => Err(self.error(Error("Cannot negate vector".to_string()))),
        }
    }

    pub fn error(&self, kind: InterpreterErrorKind) -> Box<dyn Error> {
        InterpreterError::boxed(*self.lines.last().unwrap(), kind)
    }
}

pub enum InterpreterErrorKind {
    Error(String),
    IndexOutOfBounds(usize),
    VariableNotDeclared(String),
    FunctionNotDeclared(String),
    RangeExpressionExpected,
    ArithmeticOperatorErr(ArithmeticOperator),
    ConstantReassignment(String),
}

#[derive(Debug)]
pub struct InterpreterError {
    description: String,
    line: usize,
}

impl InterpreterError {
    fn new(line: usize, kind: InterpreterErrorKind) -> Self {
        use InterpreterErrorKind::*;

        let description = match kind {
            Error(str) => str,
            IndexOutOfBounds(index) => format!("Index '{}' is out of bounds", index),
            VariableNotDeclared(ident) => format!("Variable '{}' not defined", ident),
            FunctionNotDeclared(ident) => format!("Function '{}' not defined", ident),
            ArithmeticOperatorErr(op) => format!("Cannot use arithmetic operator {:?}", op),
            ConstantReassignment(ident) => format!("Cannot reassign constant '{}'", ident),
            RangeExpressionExpected => "Cannot iterate over non-int range.".to_string(),
        };

        InterpreterError { description, line }
    }

    fn boxed(line: usize, kind: InterpreterErrorKind) -> Box<Self> {
        Box::new(InterpreterError::new(line, kind))
    }
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Runtime Error at line {}: {}",
            self.line, self.description
        )
    }
}

impl Error for InterpreterError {
    fn description(&self) -> &str {
        &self.description
    }
}

fn dev_print(ctx: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>> {
    Ok(Value::Void)
}

mod tests {
    use crate::{io::Input, parser::Parser};

    use super::*;

    #[test]
    fn test_interpret_plus_statement() {
        let mut parser = Parser::new(Input::String("1 + 2 - 4".to_string()));
        let expression = parser.parse_expression().unwrap();

        let mut interpreter = Interpreter::new();
        let result = interpreter.evaluate_expression(&expression).unwrap();
        assert_eq!(result, Value::Int(-1));
    }

    #[test]
    fn test_strong_typed() {
        let mut parser = Parser::new(Input::String("1 + 2.0;".to_string()));
        let statement = parser.parse_statement().unwrap();

        let mut interpreter = Interpreter::new();
        let result = interpreter.interpret_statement(&statement.unwrap());

        if result.is_ok() {
            panic!();
        }
    }

    #[test]
    fn test_interpret_modulo_w_plus_statement() {
        let mut parser = Parser::new(Input::String("1 +  3 % 2 ".to_string()));
        let expression = parser.parse_expression().unwrap();

        let mut interpreter = Interpreter::new();
        let result = interpreter.evaluate_expression(&expression).unwrap();
        assert_eq!(result, Value::Int(2));
    }

    #[test]
    fn test_const_assignment() {
        let mut parser = Parser::new(Input::String("const a = 1; a = 2;".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        let result = interpreter.interpret_program(&statements);

        if result.is_ok() {
            panic!();
        }
    }

    #[test]
    fn test_conjuction_evaluete() {
        let mut parser = Parser::new(Input::String("let a = 2 == 2 and 3 == 1;".to_string()));
        let statement = parser.parse_statement().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter
            .interpret_statement(&statement.unwrap())
            .unwrap();
        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Bool(false)
        );
    }

    #[test]
    fn test_if_cond_fn_call() {
        let mut parser = Parser::new(Input::String(
            "let a = 1;fn t() {ret true;} if t() {a = 2;}".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_program(&statements).unwrap();
        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(2)
        );
    }

    #[test]
    fn test_interpret_var_declaration_statement() {
        let mut parser = Parser::new(Input::String("let a = 2 * 3 + 1;".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_statement(&statements[0]).unwrap();
        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(7)
        );
    }

    #[test]
    fn test_interpret_function() {
        let mut parser = Parser::new(Input::String("fn b(a) {}".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        match &statements[0].stmt {
            Statement::Function(function) => {
                interpreter.interpret_function(function).unwrap();
            }
            _ => panic!("Expected function declaration."),
        }

        assert!(interpreter.functions.contains_key("b"));
    }

    #[test]
    fn test_print() {
        let mut parser = Parser::new(Input::String("print(2 + 1);".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.inject_iternal_function("print".to_string(), CustomFunction { f: dev_print });

        match interpreter.interpret_statement(&statements[0]) {
            Ok(_) => {}
            Err(err) => panic!("Error: {}", err),
        }
    }

    #[test]
    fn test_recursion_fn() {
        let mut parser = Parser::new(Input::String(
            "fn recursion(n) {if n == 0 {ret 1;} ret 1 + recursion(n - 1);} let a = recursion(3);"
                .to_string(),
        ));

        let statements = parser.parse_program().unwrap();
        let mut interpreter = Interpreter::new();

        interpreter.interpret_statement(&statements[0]).unwrap();
        interpreter.interpret_statement(&statements[1]).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(4)
        );
    }

    #[test]
    fn test_variable_fn() {
        let mut parser = Parser::new(Input::String(
            "fn fib(n) {if n <= 1 {ret 2;}} let a = fib(0);".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        match &statements[0].stmt {
            Statement::Function(function) => {
                interpreter.interpret_function(function).unwrap();
            }
            _ => panic!("Expected function declaration."),
        }

        interpreter.interpret_statement(&statements[1]).unwrap();
        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(2)
        );
    }
    #[test]
    fn test_fn_print() {
        let mut parser = Parser::new(Input::String(
            "fn show(a) { print(a); } show(\"c\");".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.inject_iternal_function("print".to_string(), CustomFunction { f: dev_print });

        match &statements[0].stmt {
            Statement::Function(function) => {
                interpreter.interpret_function(function).unwrap();
            }
            _ => panic!("Expected function declaration."),
        }

        match &statements[1].stmt {
            Statement::Expression(expression) => {
                interpreter.evaluate_expression(expression).unwrap();
            }
            _ => panic!("Expected expression statement."),
        }
    }

    #[test]
    fn test_for_loop_fn_call() {
        let mut parser = Parser::new(Input::String(
            "let a = 0;fn f() {ret [1,2];} for i in f() {a = a + i;}".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_program(&statements).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(3)
        );
    }

    #[test]
    fn test_for_loop_vec_acc() {
        let mut parser = Parser::new(Input::String(
            "let t = [[1, 2]];let a = 0;for i in t[0] {a = a + i;}".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_program(&statements).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(3)
        );
    }

    #[test]
    fn test_for_loop() {
        let mut parser = Parser::new(Input::String(
            "let a = 0; for i in 1 to 3 {a = a + 1;}".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_program(&statements).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(3)
        );
    }

    #[test]
    fn test_if_simple() {
        let mut parser = Parser::new(Input::String(
            "let a = 2; if 1 == 1 and 2 == 2 {a = 3;}".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_program(&statements).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(3)
        );
    }

    #[test]
    fn test_if_fn_call() {
        let mut parser = Parser::new(Input::String(
            "let a = 2; fn test() {ret true;} if test() {a = 3;}".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_program(&statements).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(3)
        );
    }

    #[test]
    fn test_match_simple() {
        let mut parser = Parser::new(Input::String(
            "let a = 2; match a { when 1 then {a = 3;} when 2 then {a = 4;} default {}}"
                .to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_program(&statements).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(4)
        );
    }

    #[test]
    fn test_match_either() {
        let mut parser = Parser::new(Input::String(
            "let a = 0; let x = \"b\"; match x {when \"a\" either \"b\" then {a = 2;} default {a = 3;}}"
                .to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_program(&statements).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("a")
                .unwrap()
                .value,
            Value::Int(2)
        );
    }

    #[test]
    fn test_vecs_and_strs() {
        let mut parser = Parser::new(Input::File("src/tests/data/vecs_and_strs.ks".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::default();
        interpreter.interpret_statement(&statements[0]).unwrap();
        interpreter.interpret_statement(&statements[1]).unwrap();
        interpreter.interpret_statement(&statements[2]).unwrap();
        interpreter.interpret_statement(&statements[3]).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("vowel_words")
                .unwrap()
                .value,
            Value::Vector(vec![
                Value::String("apple".to_string()),
                Value::String("egg".to_string())
            ])
        );
    }

    #[test]
    fn test_fib() {
        let mut parser = Parser::new(Input::File("src/tests/data/fib.ks".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_statement(&statements[0]).unwrap();
        interpreter.interpret_statement(&statements[1]).unwrap();
        interpreter.interpret_statement(&statements[2]).unwrap();

        assert_eq!(
            interpreter
                .variables
                .last()
                .unwrap()
                .get("fib_number")
                .unwrap()
                .value,
            Value::Int(55)
        );
    }

    #[test]
    fn test_xd() {
        let mut interpreter = Interpreter::new();
    }

    #[test]
    fn test_pattern() {
        let mut parser = Parser::new(Input::File("src/tests/data/pattern.ks".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::default();
        interpreter.interpret_statement(&statements[0]).unwrap();
        interpreter.interpret_statement(&statements[1]).unwrap();
    }
}
