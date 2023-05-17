use kowiescript::io::Input;

fn main() {
    kowiescript::run_program(Input::File(("src/tests/data/fib.ks".to_string())));
}
