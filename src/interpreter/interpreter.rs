use super::{environment::Environment, expression::evaluate, Callable, Error, Object};
use crate::ast::Statement;
use std::{cell::RefCell, collections::VecDeque, io::Write, rc::Rc};

pub struct Interpreter<'a, W> {
    scoped_envs: VecDeque<Rc<RefCell<Environment>>>,
    print_writer: &'a mut W,
}

impl<'a, W: Write> Interpreter<'a, W> {
    pub fn new(writer: &'a mut W) -> Interpreter<'a, W> {
        let mut envs = VecDeque::new();
        let mut global_env = Environment::new();
        add_native_functions(&mut global_env);
        envs.push_back(Rc::new(RefCell::new(global_env)));
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

fn add_native_functions(env: &mut Environment) {
    env.define(
        "clock",
        Object::Callable(Callable {
            name: String::from("clock"),
            arity: 0,
            function: Rc::new(Box::new(|_args, _env| {
                let now = std::time::UNIX_EPOCH.elapsed().unwrap().as_millis() as f64 / 1000.;
                return Ok(Object::Number(now));
            })),
        }),
    );
}

#[cfg(test)]
mod test {
    use crate::ast::{CallExpr, Expression, ObjectValue, Variable};

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

    #[test]
    fn test_function_call() {
        let mut stdout = std::io::stdout();
        let mut interpreter = Interpreter::new(&mut stdout);
        {
            let mut env = interpreter.scoped_envs.front_mut().unwrap().borrow_mut();
            env.define(
                "test",
                Object::Callable(Callable {
                    name: "test".into(),
                    arity: 1,
                    function: Rc::new(Box::new(|args: &[Object], _env| {
                        return Ok(args[0].clone());
                    })),
                }),
            );
            env.define("test_var", Object::Nil);
        }

        interpreter
            .scoped_envs
            .front()
            .unwrap()
            .borrow()
            .get("test")
            .unwrap();

        let stmt = Statement::Expression(Expression::Assignment((
            "test_var".into(),
            Box::new(Expression::Call(Box::new(CallExpr {
                callee: Expression::Variable(Variable {
                    name: "test".into(),
                }),
                args: vec![Expression::Literal(ObjectValue::Number(1.))],
            }))),
        )));

        interpreter.execute(&stmt).unwrap();

        let value = interpreter
            .scoped_envs
            .back()
            .unwrap()
            .borrow()
            .get("test_var")
            .unwrap();
        assert!(matches!(value, Object::Number(1.)));
    }
}
