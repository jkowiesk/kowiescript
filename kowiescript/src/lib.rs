use std::io::{BufRead};

use interpreter::Interpreter;
use io::Input;
use parser::{ast::Statement, Parser};

// Author: Jakub Kowieski
//
// This is the main file of the kowiescript library. It contains all imports and exports of the library.
mod interpreter;
pub mod io;
mod lexer;
mod parser;

pub type Program = Vec<Statement>;

pub fn run_program(input: Input) -> Result<(), String> {
    let mut parser = parser::Parser::new(input);
    let ast = parser.parse_program();

    let program = match ast {
        Ok(program) => program,
        Err(err) => return Err(err.to_string()),
    };

    let mut interpreter = interpreter::Interpreter::new();
    match interpreter.interpret_program(&program) {
        Ok(_) => (),
        Err(err) => return Err(err.to_string()),
    }

    Ok(())
}

pub fn interpret() -> Result<(), String> {
    let mut interpreter = Interpreter::new();
    let stdin = std::io::stdin();
    let input = stdin.lock();
    let mut lines = input.lines();

    loop {
        if let Some(Ok(line)) = lines.next() {
            if line.trim() == "exit" {
                break;
            }

            let mut parser = Parser::new(Input::String(line));
            let ast = parser.parse_program().unwrap_or_else(|err| {
                eprintln!("{}", err);
                std::process::exit(1);
            });

            interpreter.interpret_program(&ast);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    
}
