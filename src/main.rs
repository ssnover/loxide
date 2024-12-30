use std::{
    fs::File,
    io::{stdout, BufReader, Read, Write},
    path::Path,
};

use loxide::{
    interpreter::{self, interpreter::Interpreter},
    parsing, scanning,
};

#[derive(Debug, Clone)]
pub enum Error {
    ScanError(scanning::ScanError),
    ParseError(parsing::Error),
    RuntimeError(interpreter::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ScanError(err) => err.fmt(f),
            Error::ParseError(err) => err.fmt(f),
            Error::RuntimeError(err) => err.fmt(f),
        }
    }
}

impl From<scanning::ScanError> for Error {
    fn from(value: scanning::ScanError) -> Self {
        Self::ScanError(value)
    }
}

impl From<parsing::Error> for Error {
    fn from(value: parsing::Error) -> Self {
        Self::ParseError(value)
    }
}

impl From<interpreter::Error> for Error {
    fn from(value: interpreter::Error) -> Self {
        Self::RuntimeError(value)
    }
}

impl std::error::Error for Error {}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = std::env::args();
    match args.len() {
        1 => run_repl(),
        2 => run_script(args.nth(1).unwrap()),
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
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        stdout().flush()?;
        let Ok(_) = stdin.read_line(&mut input_line) else {
            break;
        };
        match loxide::scanning::scan_tokens(&input_line) {
            Ok(tokens) => {
                if !tokens.is_empty() {
                    match loxide::parsing::parse(&tokens) {
                        Ok(program) => {
                            interpreter.interpret(&program)?;
                        }
                        Err(errs) => {
                            eprintln!("Failed to parse");
                            for err in errs {
                                eprintln!("{err}");
                            }
                        }
                    }
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

fn run_script(path: impl AsRef<Path>) -> Result<(), Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    if let Err(errs) = loxide::scanning::scan_tokens(&contents)
        .map_err(|errs| errs.into_iter().map(Error::from).collect::<Vec<_>>())
        .and_then(|tokens| {
            loxide::parsing::parse(&tokens)
                .map_err(|errs| errs.into_iter().map(Error::from).collect::<Vec<_>>())
                .and_then(|program| {
                    let mut interpreter = Interpreter::new();
                    interpreter
                        .interpret(&program)
                        .map_err(|err| vec![Error::from(err)])
                })
        })
    {
        for err in errs {
            eprintln!("{err}");
        }
        std::process::exit(1);
    }

    Ok(())
}
