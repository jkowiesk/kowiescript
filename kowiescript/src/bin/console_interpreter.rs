use kowiescript::{io::Input, run_program};

fn main() {
    run_program(Input::String("print(\"Hello, world!\")".to_string())).unwrap();
}
