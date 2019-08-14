extern crate vm;

use std::{env, fs, io};

fn main() {
    let args: Vec<String> = env::args().collect();
    dbg!(&args);
    match &args[..] {
        [_] => repl(),
        [_, file] => run_file(file),
        _ => panic!("Usage: rox [path]"),
    }
}

macro_rules! print_and_flush {
    ( $( $x:expr ),* ) => {{
        print!($($x),*);
        io::Write::flush(&mut io::stdout()).unwrap();
    }}
}

// TODO bubble errors up more gracefully

fn repl() {
    'repl: loop {
        print_and_flush!("> ");
        let mut line: String = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(_) => {
                if line.trim() == "quit" {
                    break 'repl;
                }
                match interpret(&line) {
                    Ok(_) => {}
                    // TODO figure out why an error seems to mess up reading in the next line
                    Err(e) => println!("error: {:?}", e)
                }
            }
            Err(e) => panic!("error reading line: {}", e),
        }
    }
}

fn interpret(line: &str) -> vm::vm::InterpretResult {
    println!("interpreting {}", line);
    let mut context = vm::context::LoxContext::new();
    let chunk = vm::compiler::compile(line, &mut context)?;
    let mut vm = vm::vm::VM::new(&chunk, &mut context);
    return vm.run();
}

fn run_file(path: &str) {
    let text = fs::read_to_string(path).unwrap();
    interpret(path);
}
