use super::{environment::Environment, expression::evaluate, Error, Object};
use crate::ast::Statement;
use std::{cell::RefCell, collections::VecDeque, io::Write, rc::Rc};

pub struct Interpreter<'a, W> {
    scoped_envs: VecDeque<Rc<RefCell<Environment>>>,
    print_writer: &'a mut W,
}

impl<'a, W: Write> Interpreter<'a, W> {
    pub fn new(writer: &'a mut W) -> Interpreter<'a, W> {
        let mut envs = VecDeque::new();
        envs.push_back(Rc::new(RefCell::new(Environment::new())));
        Interpreter {
            scoped_envs: envs,
            print_writer: writer,
        }
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
                let _ = writeln!(&mut self.print_writer, "{value}");
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
            Statement::If(stmt) => {
                let condition_result = evaluate(
                    &stmt.condition,
                    &mut self.scoped_envs.back().unwrap().borrow_mut(),
                )?;
                if condition_result.is_truthy() {
                    self.execute(&stmt.then_branch)
                } else if let Some(else_branch) = &stmt.else_branch {
                    self.execute(else_branch)
                } else {
                    Ok(())
                }
            }
            Statement::While(stmt) => {
                while evaluate(
                    &stmt.condition,
                    &mut self.scoped_envs.back().unwrap().borrow_mut(),
                )?
                .is_truthy()
                {
                    self.execute(&stmt.body)?;
                }

                Ok(())
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
        let mut stdout = std::io::stdout();
        let mut interpreter = Interpreter::new(&mut stdout);
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
