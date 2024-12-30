use super::{environment::Environment, expression::evaluate, Error, Object};
use crate::ast::Statement;
use std::{cell::RefCell, collections::VecDeque, rc::Rc};

pub struct Interpreter {
    scoped_envs: VecDeque<Rc<RefCell<Environment>>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut envs = VecDeque::new();
        envs.push_back(Rc::new(RefCell::new(Environment::new())));
        Interpreter { scoped_envs: envs }
    }

    pub fn interpret(&mut self, program: &[Statement]) -> Result<(), Error> {
        for stmt in program {
            self.execute(stmt)?;
        }
        Ok(())
    }

    pub fn execute(&mut self, stmt: &Statement) -> Result<(), Error> {
        match stmt {
            Statement::Expression(expr) => {
                let _ = evaluate(expr, &mut self.scoped_envs.back().unwrap().borrow_mut())?;
                Ok(())
            }
            Statement::Print(expr) => {
                let value = evaluate(expr, &mut self.scoped_envs.back().unwrap().borrow_mut())?;
                println!("{value}");
                Ok(())
            }
            Statement::VarDeclaration((name, initializer)) => {
                let initial_val = if let Some(expr) = initializer {
                    evaluate(expr, &mut self.scoped_envs.back().unwrap().borrow_mut())?
                } else {
                    Object::Nil
                };

                self.scoped_envs
                    .back()
                    .unwrap()
                    .borrow_mut()
                    .define(name, initial_val);

                Ok(())
            }
            Statement::Block(stmts) => {
                self.scoped_envs
                    .push_back(Rc::new(RefCell::new(Environment::with_parent(
                        self.scoped_envs.back().unwrap().clone(),
                    ))));

                let result = stmts.iter().try_for_each(|stmt| self.execute(stmt));

                self.scoped_envs.pop_back();
                result
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{Expression, ObjectValue};

    use super::*;

    #[test]
    fn test_var_declaration() {
        let mut interpreter = Interpreter::new();
        let stmt = Statement::VarDeclaration((String::from("value"), None));
        interpreter.execute(&stmt).unwrap();
        assert_eq!(
            Object::Nil,
            interpreter
                .scoped_envs
                .back()
                .unwrap()
                .borrow()
                .get("value")
                .unwrap()
        );

        let stmt = Statement::VarDeclaration((
            String::from("test"),
            Some(Expression::Literal(ObjectValue::Number(1.))),
        ));
        interpreter.execute(&stmt).unwrap();
        assert_eq!(
            Object::Number(1.),
            interpreter
                .scoped_envs
                .back()
                .unwrap()
                .borrow()
                .get("test")
                .unwrap()
        );
    }
}
