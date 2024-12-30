use interpreter::Interpreter;

pub mod ast;
pub mod interpreter;
pub mod parsing;
pub mod scanning;

#[derive(Clone, Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn bounding(start: &Span, end: &Span) -> Self {
        Self {
            start: start.start,
            end: end.end,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: Position::new(1, 0),
            end: Position::new(1, 0),
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Position {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    pub fn next(&mut self) {
        self.col += 1;
    }

    pub fn forward(&mut self, cols: usize) {
        self.col += cols;
    }

    pub fn next_line(&mut self) {
        self.col = 0;
        self.line += 1;
    }
}

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

pub fn interpret_src<W: std::io::Write>(src: &str, writer: &mut W) -> Result<(), Vec<Error>> {
    scanning::scan_tokens(&src)
        .map_err(|errs| errs.into_iter().map(Error::from).collect::<Vec<_>>())
        .and_then(|tokens| {
            parsing::parse(&tokens)
                .map_err(|errs| errs.into_iter().map(Error::from).collect::<Vec<_>>())
                .and_then(|program| {
                    let mut interpreter = Interpreter::new(writer);
                    interpreter
                        .interpret(&program)
                        .map_err(|err| vec![Error::from(err)])
                })
        })
}

#[cfg(test)]
mod test {
    use super::*;

    fn run_script_and_assert_output(input: &str, output: &str) {
        let output_str = vec![];
        let mut writer = std::io::BufWriter::new(output_str);
        interpret_src(input, &mut writer).unwrap();
        assert_eq!(output, std::str::from_utf8(writer.buffer()).unwrap())
    }

    #[test]
    fn test_variable_scoping() {
        let input_src = include_str!("../test_scripts/variable_scoping.lox");
        let output_txt = include_str!("../test_scripts/variable_scoping.out");
        run_script_and_assert_output(input_src, output_txt);
    }

    #[test]
    fn test_conditions() {
        let input_src = include_str!("../test_scripts/conditions.lox");
        let output_txt = include_str!("../test_scripts/conditions.out");
        run_script_and_assert_output(input_src, output_txt);
    }

    #[test]
    fn test_logical_ops() {
        let input_src = include_str!("../test_scripts/logical_ops.lox");
        let output_txt = include_str!("../test_scripts/logical_ops.out");
        run_script_and_assert_output(input_src, output_txt);
    }

    #[test]
    fn test_while_loop() {
        let input_src = include_str!("../test_scripts/while.lox");
        let output_txt = include_str!("../test_scripts/while.out");
        run_script_and_assert_output(input_src, output_txt);
    }

    #[test]
    fn test_for_loop() {
        let input_src = include_str!("../test_scripts/for.lox");
        let output_txt = include_str!("../test_scripts/for.out");
        run_script_and_assert_output(input_src, output_txt);
    }
}
