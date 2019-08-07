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
    ( $( $x:expr),* ) => {
        print!($($x)*);
        io::Write::flush(&mut io::stdout()).unwrap();
    }
}

// TODO bubble errors up more gracefully

fn repl() {
    let mut buffer: String = String::new();
    'repl: loop {
        print_and_flush!("> ");
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                if buffer.trim() == "quit" {
                    break 'repl;
                }
                interpret(&buffer);
            }
            Err(e) => panic!("error reading line: {}", e),
        }
        buffer.clear();
    }
}

fn interpret(line: &str) -> vm::vm::InterpretResult {
    println!("{}", line);
    // TODO compiler call goes here
    // TODO invoke chunk goes here
    Ok(())
}

fn run_file(path: &str) {
    let text = fs::read_to_string(path).unwrap();
    interpret(path).unwrap();
}