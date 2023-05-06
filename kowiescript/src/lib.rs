use io::Input;

// Author: Jakub Kowieski
//
// This is the main file of the kowiescript library. It contains all imports and exports of the library.
mod io;
mod lexer;
mod parser;

struct Interpreter<'a> {
    lexer: lexer::Lexer<'a>,
}

impl<'a> Interpreter<'a> {
    fn new(input: Input) -> Interpreter<'a> {
        Interpreter {
            lexer: lexer::Lexer::new(input),
        }
    }
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
