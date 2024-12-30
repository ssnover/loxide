use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

use loxide::scanning::Token;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = std::env::args();
    match args.len() {
        1 => run_repl(),
        2 => scan_file(args.nth(1).unwrap()).map(|_| ()),
        _ => {
            eprintln!("Usage: loxide [script]");
            Err(Box::new(std::io::Error::from(
                std::io::ErrorKind::InvalidInput,
            )))
        }
    }
}

fn run_repl() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = std::io::stdin();
    let mut input_line = String::new();
    while let Ok(_) = stdin.read_line(&mut input_line) {
        print!("> ");
        match loxide::scanning::scan_tokens(&input_line) {
            Ok(tokens) => {
                if !tokens.is_empty() {
                    println!("{tokens:?}");
                }
            }
            Err(errs) => {
                for err in errs {
                    eprintln!("{err}");
                }
            }
        }

        input_line.clear();
    }
    Ok(())
}

fn scan_file(path: impl AsRef<Path>) -> Result<Vec<Token>, Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    match loxide::scanning::scan_tokens(&contents) {
        Ok(tokens) => {
            for token in &tokens {
                println!("{token:?}");
            }
            Ok(tokens)
        }
        Err(errs) => {
            for err in errs {
                eprintln!("{err}");
            }
            Err(Box::new(std::io::Error::from(
                std::io::ErrorKind::InvalidInput,
            )))
        }
    }
}
