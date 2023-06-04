fn main() {
    // make nice print to welcome to kowiescript and tell about exit command
    println!("|---------------------------------|");
    println!("Welcome to kowiescript interpreter!");
    println!("|---------------------------------|");
    println!("Type 'exit' to exit the interpreter");
    kowiescript::interpret_cli().unwrap();
}
