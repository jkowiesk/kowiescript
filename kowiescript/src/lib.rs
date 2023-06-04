use std::{error::Error, io::BufRead};

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

pub fn run_program(input: String) -> Result<String, String> {
    let input = Input::String(input);

    let mut parser = Parser::new(input);
    let ast = parser.parse_program();

    let program = match ast {
        Ok(program) => program,
        Err(err) => return Err(err.to_string()),
    };

    let mut interpreter = Interpreter::default_without_io();
    interpreter.inject_internal_function(
        "print".to_string(),
        CustomFunction {
            f: |ctx: &mut Interpreter, args: Vec<Value>| -> Result<Value, Box<dyn Error>> {
                ctx.output.push_str(args[0].to_string().as_str());
                ctx.output.push('\n');
                Ok(Value::Void)
            },
        },
    );

    match interpreter.interpret_program(&program) {
        Ok(_) => Ok(interpreter.output),
        Err(err) => Err(err.to_string()),
    }
}

#[cfg(test)]
mod tests {}
