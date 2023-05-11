use io::Input;
use parser::ast::Statement;

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

    let mut program = match ast {
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

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
