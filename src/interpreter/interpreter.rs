use crate::ast::Statement;

use super::{expression::evaluate, Error};

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn interpret(&self, program: &[Statement]) -> Result<(), Error> {
        for stmt in program {
            self.execute(stmt)?;
        }
        Ok(())
    }

    pub fn execute(&self, stmt: &Statement) -> Result<(), Error> {
        match stmt {
            Statement::Expression(expr) => {
                let _ = evaluate(expr)?;
            }
            Statement::Print(expr) => {
                let value = evaluate(expr)?;
                println!("{value}");
            }
        }

        Ok(())
    }
}
