use std::{collections::HashMap, error::Error, result};

use crate::{parser::ast::*, Program};

#[derive(Debug, Clone)]
pub struct Variable {
    kind: VarKind,
    value: Value,
}

pub struct Interpreter {
    pub variables: HashMap<String, Variable>,
    pub functions: HashMap<String, Function>,
    output: Option<String>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            output: None,
        }
    }

    pub fn interpret_program(
        &mut self,
        program: &Program,
    ) -> Result<Option<String>, Box<dyn Error>> {
        for statement in program {
            self.interpret_statement(statement)?;
        }
        let output = self.output.clone();
        self.output = None;

        Ok(output)
    }

    pub fn interpret_statement(&mut self, statement: &Statement) -> Result<(), Box<dyn Error>> {
        match statement {
            Statement::VarDeclaration(var_declaration) => {
                self.interpret_var_declaration(var_declaration)
            }
            Statement::Assignment(assignment) => self.interpret_assignment(assignment),
            Statement::ForLoop(for_loop) => self.interpret_for_loop(for_loop),
            Statement::SubLoop(sub_loop) => self.interpret_sub_loop(sub_loop),
            Statement::Function(function) => self.interpret_function(function),
            Statement::Expression(expression) => {
                self.evaluate_expression(expression)?;
                Ok(())
            }
            _ => unimplemented!("Statement interpretation not implemented."),
        }
    }

    fn interpret_function(&mut self, function: &Function) -> Result<(), Box<dyn Error>> {
        self.functions
            .insert(function.name.clone(), function.clone());

        Ok(())
    }

    fn interpret_var_declaration(
        &mut self,
        var_declaration: &VarDeclaration,
    ) -> Result<(), Box<dyn Error>> {
        let value = self.evaluate_expression(&var_declaration.expression)?;
        self.variables.insert(
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
        // TODO: Remove panic
        self.variables.get_mut(&assignment.name).unwrap().value = value;

        Ok(())
    }

    fn interpret_for_loop(&mut self, for_loop: &ForLoop) -> Result<(), Box<dyn Error>> {
        let iterable = self.evaluate_iterator_expression(&for_loop.iterator)?;
        for value in iterable {
            self.variables.insert(
                for_loop.iter_var.clone(),
                Variable {
                    kind: VarKind::Mutable,
                    value,
                },
            );
            for statement in &for_loop.body {
                match statement {
                    Statement::SubLoop(sub_loop) => match sub_loop.kind {
                        SubLoopKind::End => break,
                        SubLoopKind::Next => continue,
                    },
                    _ => {
                        self.interpret_statement(statement);
                    }
                }
                self.interpret_statement(statement);
            }
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
                ArithmeticOperator::Add => result + term_value,
                ArithmeticOperator::Subtract => result - term_value,
                _ => unimplemented!("Arithmetic operation not implemented."),
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
                ArithmeticOperator::Multiply => result * conversion_value,
                ArithmeticOperator::Divide => result / conversion_value,
                ArithmeticOperator::Modulo => result % conversion_value,
                _ => unimplemented!("Arithmetic operation not implemented."),
            };
        }

        Ok(result)
    }

    fn evaluate_conversion(&mut self, conversion: &Conversion) -> Result<Value, Box<dyn Error>> {
        let inverted_value = self.evaluate_inversion(&conversion.inversion)?;

        if let Some(conversion_type) = &conversion.to {
            Ok(self.convert_value(inverted_value, conversion_type))
        } else {
            Ok(inverted_value) // No conversion specified, return the inverted value as is
        }
    }

    fn evaluate_inversion(&mut self, inversion: &Inversion) -> Result<Value, Box<dyn Error>> {
        let value = self.evaluate_factor(&inversion.value)?;

        if inversion.negated {
            Ok(self.negate_value(value))
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
        let var = self
            .variables
            .get(name)
            .expect(&format!("Variable '{}' is not defined.", name));

        Ok(var.value.clone())
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

        self.function_call(name, &args)
    }

    fn function_call(&mut self, name: &str, args: &Vec<Value>) -> Result<Value, Box<dyn Error>> {
        match name {
            "print" => {
                for arg in args {
                    print!("{}", arg);
                }
                println!();
                Ok(Value::Void)
            }
            _name => {
                let func = self
                    .functions
                    .get(name)
                    .expect(&format!("Function '{}' is not defined.", name));

                let parent_variables = self.variables.clone();

                for (i, param) in func.parameters.iter().enumerate() {
                    self.variables.insert(
                        param.name.clone(),
                        Variable {
                            kind: param.kind.clone(),
                            value: args[i].clone(),
                        },
                    );
                }

                for statement in func.body.clone() {
                    match statement {
                        Statement::Return(ret_expr) => match ret_expr.value {
                            Some(expr) => return self.evaluate_expression(&expr),
                            None => return Ok(Value::Void),
                        },
                        _ => self.interpret_statement(&statement)?,
                    };
                }

                Ok(Value::Void)
            }
        }
    }

    fn evaluate_vector_access(
        &mut self,
        vector_access: &VectorAccess,
    ) -> Result<Value, Box<dyn Error>> {
        unimplemented!("Vector access is not implemented yet.")
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
                    _ => panic!("Cannot iterate over non-int range."),
                };
                let end = match self.evaluate_factor(&range.end)? {
                    Value::Int(value) => {
                        if value < 0 {
                            0
                        } else {
                            value
                        }
                    }
                    _ => panic!("Cannot iterate over non-int range."),
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
            IteratorExpression::Identifier(ident) => match self.variables.get(ident) {
                Some(var) => match &var.value {
                    Value::Vector(values) => Ok(values.clone()),
                    _ => panic!("Cannot iterate over non-vector."),
                },
                None => unimplemented!("Variable doesn't exist."),
            },
            _ => unimplemented!("Iterator expression evaluation not implemented."),
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

    fn convert_value(&self, value: Value, conversion_type: &ConversionType) -> Value {
        match value {
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
            Value::Void => unimplemented!("Void conversion is not implemented yet."),
            Value::Vector(_) => unimplemented!("Vector conversion is not implemented yet."),
        }
    }

    fn negate_value(&self, value: Value) -> Value {
        unimplemented!("Negation is not implemented yet.")
    }
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
    fn test_interpret_modulo_w_plus_statement() {
        let mut parser = Parser::new(Input::String("1 +  2 * 3 ".to_string()));
        let expression = parser.parse_expression().unwrap();

        let mut interpreter = Interpreter::new();
        let result = interpreter.evaluate_expression(&expression).unwrap();
        assert_eq!(result, Value::Int(7));
    }

    #[test]
    fn test_interpret_var_declaration_statement() {
        let mut parser = Parser::new(Input::String("let a = 2 * 3 + 1;".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        interpreter.interpret_statement(&statements[0]);
        assert_eq!(interpreter.variables["a"].value, Value::Int(7));
    }

    #[test]
    fn test_interpret_function() {
        let mut parser = Parser::new(Input::String("fn b(a) {}".to_string()));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        match &statements[0] {
            Statement::Function(function) => {
                interpreter.interpret_function(function);
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
        match interpreter.interpret_statement(&statements[0]) {
            Ok(_) => {}
            Err(err) => panic!("Error: {}", err),
        }
    }
    #[test]
    fn test_fn_print() {
        let mut parser = Parser::new(Input::String(
            "fn show(a) { print(a); } show(\"c\");".to_string(),
        ));
        let statements = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new();
        match &statements[0] {
            Statement::Function(function) => {
                interpreter.interpret_function(function).unwrap();
            }
            _ => panic!("Expected function declaration."),
        }

        match &statements[1] {
            Statement::Expression(expression) => {
                interpreter.evaluate_expression(expression).unwrap();
            }
            _ => panic!("Expected expression statement."),
        }
    }
}
