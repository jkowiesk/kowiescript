use std::{error::Error, io::BufRead};

use crate::interpreter::InterpreterErrorKind;
use interpreter::Interpreter;
use io::Input;
use parser::{
    ast::{CustomFunction, SourceStatement, Value},
    Parser,
};

// This is the main file of the ks library. It contains all imports and exports of the library.

mod interpreter;
pub mod io;
mod lexer;
mod parser;

pub type Program = Vec<SourceStatement>;

pub fn run_program(input: Input) -> Result<(), String> {
    let mut parser = parser::Parser::new(input);
    let ast = parser.parse_program();

    let program = match ast {
        Ok(program) => program,
        Err(err) => return Err(err.to_string()),
    };

    let mut interpreter = Interpreter::default();
    interpreter.inject_internal_function(
        "input".to_string(),
        CustomFunction {
            f: |_ctx: &mut Interpreter, _args: Vec<Value>| -> Result<Value, Box<dyn Error>> {
                let mut input = String::new();
                let mut handle = std::io::stdin().lock();
                handle.read_line(&mut input)?;

                let input = input.trim();
                let value = match input.parse::<i64>() {
                    Ok(value) => Value::Int(value),
                    Err(_) => match input.parse::<f64>() {
                        Ok(value) => Value::Float(value),
                        Err(_) => Value::String(input.to_string()),
                    },
                };
                Ok(value)
            },
        },
    );

    match interpreter.interpret_program(&program) {
        Ok(_) => (),
        Err(err) => return Err(err.to_string()),
    }

    Ok(())
}

pub fn interpret_cli() -> Result<(), String> {
    let mut interpreter = Interpreter::default();
    interpreter.inject_internal_function(
        "input".to_string(),
        CustomFunction {
            f: |ctx: &mut Interpreter, _args: Vec<Value>| -> Result<Value, Box<dyn Error>> {
                let input_var = ctx.variables.last().unwrap().get("input").unwrap().clone();

                if let Value::String(input) = input_var.value {
                    let input = input.trim();
                    match input.parse::<i64>() {
                        Ok(value) => Ok(Value::Int(value)),
                        Err(_) => match input.parse::<f64>() {
                            Ok(value) => Ok(Value::Float(value)),
                            Err(_) => Ok(Value::String(input.to_string())),
                        },
                    }
                } else {
                    Err(ctx.error(InterpreterErrorKind::UnexpectedBehavior))
                }
            },
        },
    );
    let mut rl = rustyline::DefaultEditor::new().unwrap();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if line.trim() == "exit" {
                    break;
                }

                let mut parser = Parser::new(Input::String(line.clone()));
                let result = parser.parse_program();

                let ast = match result {
                    Ok(ast) => ast,
                    Err(err) => {
                        eprintln!("{}", err);
                        continue;
                    }
                };

                if line.contains("input") {
                    let raw_line = rl.readline("").unwrap();
                    interpreter.insert_input(raw_line);
                }

                let result = interpreter.interpret_program(&ast);
                match result {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("{}", err);
                    }
                }
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                break;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {}
