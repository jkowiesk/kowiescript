use kowiescript::io::Input;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: ks <filename>");
        std::process::exit(1);
    }

    let filename = &args[1];
    let input = Input::File(filename.clone());
    if let Err(err) = kowiescript::run_program_cli(input) {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}
