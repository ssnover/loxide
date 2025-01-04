use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::ast::{self, Expression, Statement, Variable};

use super::{environment::Environment, Object};

pub fn resolve(program: &mut [Statement]) {
    let mut resolver = Resolver::new();
    resolver.resolve(program)
}

pub struct Resolver {
    envs: VecDeque<Rc<RefCell<Environment>>>,
}

impl Resolver {
    pub fn new() -> Resolver {
        let mut envs = VecDeque::new();
        let mut global_env = Environment::new();
        global_env.define("clock", Object::Nil);
        envs.push_back(Rc::new(RefCell::new(global_env)));
        Resolver { envs }
    }

    pub fn resolve(&mut self, program: &mut [Statement]) {
        for stmt in program.iter_mut() {
            self.resolve_stmt(stmt);
        }
    }

    pub fn resolve_stmt(&mut self, stmt: &mut Statement) {
        let env = self.envs.back().unwrap().clone();
        self.resolve_stmt_with_env(stmt, &env);
    }

    fn resolve_stmt_with_env(&mut self, stmt: &mut ast::Statement, env: &Rc<RefCell<Environment>>) {
        match stmt {
            Statement::Expression(expr) => self.resolve_expr(expr, env),
            Statement::Print(expr) => self.resolve_expr(expr, env),
            Statement::VarDeclaration((name, initializer)) => {
                if let Some(expr) = initializer {
                    self.resolve_expr(expr, env);
                }
                env.borrow_mut().define(name.clone(), Object::Nil);
            }
            Statement::Block(stmts) => {
                self.envs
                    .push_back(Rc::new(RefCell::new(Environment::with_parent(env.clone()))));
                stmts.iter_mut().for_each(|stmt| self.resolve_stmt(stmt));
                self.envs.pop_back();
            }
            Statement::If(stmt) => {
                self.resolve_expr(&mut stmt.condition, env);
                self.resolve_stmt(&mut stmt.then_branch);
                if let Some(stmt) = &mut stmt.else_branch {
                    self.resolve_stmt(stmt);
                }
            }
            Statement::While(stmt) => {
                self.resolve_expr(&mut stmt.condition, env);
                self.resolve_stmt(&mut stmt.body);
            }
            Statement::FnDeclaration(decl) => {
                let mut fn_local_env = Environment::with_parent(env.clone());
                decl.params
                    .iter()
                    .for_each(|param| fn_local_env.define(param, Object::Nil));
                let fn_local_env = Rc::new(RefCell::new(fn_local_env));
                self.resolve_stmt_with_env(&mut decl.body, &fn_local_env);
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr, env);
                }
            }
        }
    }

    fn resolve_expr(&self, expr: &mut Expression, env: &Rc<RefCell<Environment>>) {
        match expr {
            Expression::Assignment((var, expr)) => {
                self.resolve_expr(expr, env);
                self.resolve_variable(var, env);
            }
            Expression::Binary(expr) => {
                self.resolve_expr(&mut expr.left, env);
                self.resolve_expr(&mut expr.right, env);
            }
            Expression::Call(expr) => {
                self.resolve_expr(&mut expr.callee, env);
                expr.args
                    .iter_mut()
                    .for_each(|arg| self.resolve_expr(arg, env));
            }
            Expression::Grouping(expr) => self.resolve_expr(expr, env),
            Expression::Literal(_) => {}
            Expression::Logical(expr) => {
                self.resolve_expr(&mut expr.left, env);
                self.resolve_expr(&mut expr.right, env);
            }
            Expression::Unary(expr) => self.resolve_expr(&mut expr.right, env),
            Expression::Variable(var) => self.resolve_variable(var, env),
        }
    }

    fn resolve_variable(&self, var: &mut Variable, env: &Rc<RefCell<Environment>>) {
        var.distance = env.borrow().find_distance(&var.name);
    }
}
