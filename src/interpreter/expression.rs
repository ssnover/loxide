use crate::ast::{BinaryOperator, Expression, UnaryOperator};

use super::{Error, Object};

pub fn evaluate(expr: &Expression) -> Result<Object, Error> {
    match expr {
        Expression::Binary(expr) => perform_binary_op(
            expr.operator.clone(),
            &evaluate(&expr.left)?,
            &evaluate(&expr.right)?,
        ),
        Expression::Grouping(expr) => evaluate(&*expr),
        Expression::Unary(expr) => perform_unary_op(expr.operator.clone(), &evaluate(&expr.right)?),
        Expression::Literal(val) => Ok(Object::from(val.clone())),
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

fn perform_unary_op(op: UnaryOperator, val: &Object) -> Result<Object, Error> {
    match op {
        UnaryOperator::LogicalNot => val.invert(),
        UnaryOperator::Negation => val.negate(),
    }
}
