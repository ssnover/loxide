use crate::ast::{BinaryOperator, Expression, LogicalOperator, UnaryOperator};

use super::{environment::Environment, Error, ErrorKind, Object};

pub fn evaluate(expr: &Expression, env: &mut Environment) -> Result<Object, Error> {
    match expr {
        Expression::Assignment((name, expr)) => {
            let value = evaluate(&expr, env)?;
            env.assign(name, value.clone())?;
            Ok(value)
        }
        Expression::Binary(expr) => perform_binary_op(
            expr.operator.clone(),
            &evaluate(&expr.left, env)?,
            &evaluate(&expr.right, env)?,
        ),
        Expression::Grouping(expr) => evaluate(&*expr, env),
        Expression::Logical(expr) => {
            perform_logical_op(expr.operator.clone(), &expr.left, &expr.right, env)
        }
        Expression::Unary(expr) => {
            perform_unary_op(expr.operator.clone(), &evaluate(&expr.right, env)?)
        }
        Expression::Literal(val) => Ok(Object::from(val.clone())),
        Expression::Variable(var) => {
            if let Some(value) = env.get(&var.name) {
                Ok(value)
            } else {
                Err(Error {
                    kind: ErrorKind::UndefinedVariable,
                })
            }
        }
    }
}

fn perform_binary_op(op: BinaryOperator, lhs: &Object, rhs: &Object) -> Result<Object, Error> {
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
    op: LogicalOperator,
    lhs: &Expression,
    rhs: &Expression,
    env: &mut Environment,
) -> Result<Object, Error> {
    let lhs = evaluate(lhs, env)?;

    match op {
        LogicalOperator::And => {
            if lhs.is_truthy() {
                evaluate(rhs, env)
            } else {
                Ok(lhs)
            }
        }
        LogicalOperator::Or => {
            if lhs.is_truthy() {
                Ok(lhs)
            } else {
                evaluate(rhs, env)
            }
        }
    }
}

fn perform_unary_op(op: UnaryOperator, val: &Object) -> Result<Object, Error> {
    match op {
        UnaryOperator::LogicalNot => val.invert(),
        UnaryOperator::Negation => val.negate(),
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Variable;

    use super::*;

    #[test]
    fn print_var() {
        let mut env = Environment::new();
        env.define("a", Object::Number(5.));

        let res = evaluate(
            &Expression::Variable(Variable {
                name: "a".to_string(),
            }),
            &mut env,
        )
        .unwrap();
        assert_eq!(Object::Number(5.), res);
    }
}
