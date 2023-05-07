use io::Input;

// Author: Jakub Kowieski
//
// This is the main file of the kowiescript library. It contains all imports and exports of the library.
mod interpreter;
mod io;
mod lexer;
mod parser;

struct Main<'a> {
    lexer: lexer::Lexer<'a>,
}

impl<'a> Main<'a> {
    fn new(input: Input) -> Main<'a> {
        Main {
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
