use super::{environment::Environment, Callable, Class, Error, ErrorKind, Object};
use crate::ast::{BinaryOperator, Expression, LogicalOperator, Statement, UnaryOperator, Variable};
use std::{cell::RefCell, collections::VecDeque, io::Write, rc::Rc};

pub trait StatementExecutor {
    fn execute(&mut self, stmt: &Statement) -> Result<(), Error>;

    fn execute_with_env(
        &mut self,
        stmt: &Statement,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), Error>;
}

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

    pub fn execute_stmt_with_env(
        &mut self,
        stmt: &Statement,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), Error> {
        match stmt {
            Statement::Expression(expr) => {
                let _ = self.evaluate(expr, &env)?;
                Ok(())
            }
            Statement::Print(expr) => {
                let value = self.evaluate(expr, &env)?;
                let _ = writeln!(&mut self.print_writer, "{value}");
                Ok(())
            }
            Statement::VarDeclaration((name, initializer)) => {
                let initial_val = if let Some(expr) = initializer {
                    self.evaluate(expr, &env)?
                } else {
                    Object::Nil
                };

                env.borrow_mut().define(name, initial_val);

                Ok(())
            }
            Statement::Block(stmts) => {
                self.scoped_envs
                    .push_back(Rc::new(RefCell::new(Environment::with_parent(env.clone()))));

                let result = stmts.iter().try_for_each(|stmt| self.execute(stmt));

                self.scoped_envs.pop_back();
                result
            }
            Statement::If(stmt) => {
                let condition_result = self.evaluate(&stmt.condition, &env)?;
                if condition_result.is_truthy() {
                    self.execute(&stmt.then_branch)
                } else if let Some(else_branch) = &stmt.else_branch {
                    self.execute(else_branch)
                } else {
                    Ok(())
                }
            }
            Statement::While(stmt) => {
                while self.evaluate(&stmt.condition, &env)?.is_truthy() {
                    self.execute(&stmt.body)?;
                }

                Ok(())
            }
            Statement::FnDeclaration(decl) => {
                let decl_params = decl.params.clone();
                let decl_body = decl.body.clone();
                let scoped_env = env.clone();
                let func = Object::Callable(Callable {
                    name: decl.name.clone(),
                    arity: decl.params.len(),
                    function: Rc::new(Box::new(move |args: &[Object], interpreter| {
                        let scoped_env = scoped_env.clone();
                        let decl_params = decl_params.clone();
                        let decl_body = decl_body.clone();

                        let mut env = Environment::with_parent(scoped_env);
                        decl_params
                            .into_iter()
                            .zip(args.iter())
                            .for_each(|(param, arg)| {
                                env.define(param, arg.clone());
                            });

                        let env = Rc::new(RefCell::new(env));
                        match interpreter.execute_with_env(&decl_body, &env) {
                            Ok(()) => Ok(Object::Nil),
                            Err(Error {
                                kind: ErrorKind::ReturnValue(value),
                            }) => Ok(value),
                            Err(err) => Err(err),
                        }
                    })),
                });

                env.borrow_mut().define(decl.name.clone(), func);

                Ok(())
            }
            Statement::Return(expr) => {
                let ret_val = if let Some(expr) = expr {
                    self.evaluate(expr, env)?
                } else {
                    Object::Nil
                };
                Err(Error {
                    kind: ErrorKind::ReturnValue(ret_val),
                })
            }
            Statement::ClassDeclaration(decl) => {
                let class = Object::Class(Class {
                    name: decl.name.clone(),
                });
                env.borrow_mut().define(decl.name.clone(), class);
                Ok(())
            }
        }
    }

    pub fn evaluate(
        &mut self,
        expr: &Expression,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Object, Error> {
        match expr {
            Expression::Assignment((Variable { name, .. }, expr)) => {
                let value = self.evaluate(&expr, env)?;
                let mut env = self.scoped_envs.back().unwrap().borrow_mut();
                env.assign(name, value.clone())?;
                Ok(value)
            }
            Expression::Binary(expr) => {
                let left = self.evaluate(&expr.left, env)?;
                let right = self.evaluate(&expr.right, env)?;
                self.perform_binary_op(expr.operator.clone(), &left, &right)
            }
            Expression::Call(expr) => {
                let callee = self.evaluate(&expr.callee, env)?;

                let args = expr
                    .args
                    .iter()
                    .map(|arg| self.evaluate(arg, env))
                    .collect::<Result<Vec<Object>, Error>>()?;

                let Object::Callable(function) = callee else {
                    return Err(Error {
                        kind: ErrorKind::NotCallable,
                    });
                };
                function.call(&args, self)
            }
            Expression::Grouping(expr) => self.evaluate(expr, env),
            Expression::Logical(expr) => {
                self.perform_logical_op(expr.operator.clone(), &expr.left, &expr.right, env)
            }
            Expression::Unary(expr) => {
                let right = self.evaluate(&expr.right, env)?;
                self.perform_unary_op(expr.operator.clone(), &right)
            }
            Expression::Literal(val) => Ok(Object::from(val.clone())),
            Expression::Variable(var) => {
                let value = if let Some(distance) = var.distance {
                    env.borrow().get_at(&var.name, distance)
                } else {
                    env.borrow().get(&var.name)
                };
                if let Some(value) = value {
                    Ok(value)
                } else {
                    Err(Error {
                        kind: ErrorKind::UndefinedVariable(var.name.clone()),
                    })
                }
            }
        }
    }

    fn perform_binary_op(
        &self,
        op: BinaryOperator,
        lhs: &Object,
        rhs: &Object,
    ) -> Result<Object, Error> {
        match op {
            BinaryOperator::Addition => lhs.add(rhs),
            BinaryOperator::Subtraction => lhs.subtract(rhs),
            BinaryOperator::Multiplication => lhs.multiply(rhs),
            BinaryOperator::Division => lhs.divide(rhs),
            BinaryOperator::Equals => lhs.equals(rhs),
            BinaryOperator::NotEquals => lhs.not_equals(rhs),
            BinaryOperator::LessThan => lhs.less_than(rhs),
            BinaryOperator::LessThanOrEqual => lhs.less_than_equal(rhs),
            BinaryOperator::GreaterThan => lhs.greater_than(rhs),
            BinaryOperator::GreaterThanOrEqual => lhs.greater_than_equal(rhs),
        }
    }

    fn perform_logical_op(
        &mut self,
        op: LogicalOperator,
        lhs: &Expression,
        rhs: &Expression,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Object, Error> {
        let lhs = self.evaluate(lhs, env)?;

        match op {
            LogicalOperator::And => {
                if lhs.is_truthy() {
                    self.evaluate(rhs, env)
                } else {
                    Ok(lhs)
                }
            }
            LogicalOperator::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    self.evaluate(rhs, env)
                }
            }
        }
    }

    fn perform_unary_op(&self, op: UnaryOperator, val: &Object) -> Result<Object, Error> {
        match op {
            UnaryOperator::LogicalNot => val.invert(),
            UnaryOperator::Negation => val.negate(),
        }
    }
}

impl<'a, W: Write> StatementExecutor for Interpreter<'a, W> {
    fn execute(&mut self, stmt: &Statement) -> Result<(), Error> {
        let env = self.scoped_envs.back().unwrap();
        self.execute_stmt_with_env(stmt, &env.clone())
    }

    fn execute_with_env(
        &mut self,
        stmt: &Statement,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), Error> {
        self.execute_stmt_with_env(stmt, env)
    }
}

fn add_native_functions(env: &mut Environment) {
    env.define(
        "clock",
        Object::Callable(Callable {
            name: String::from("clock"),
            arity: 0,
            function: Rc::new(Box::new(|_args, _| {
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
            Variable {
                name: "test_var".into(),
                distance: None,
            },
            Box::new(Expression::Call(Box::new(CallExpr {
                callee: Expression::Variable(Variable {
                    name: "test".into(),
                    distance: None,
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
