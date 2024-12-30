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
