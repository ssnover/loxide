use crate::scanning::TokenKind;

#[derive(Clone, Debug)]
pub enum Expression {
    Binary(Box<BinaryExpr>),
    Grouping(Box<Expression>),
    Literal(ObjectValue),
    Unary(Box<UnaryExpr>),
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub left: Expression,
    pub operator: BinaryOperator,
    pub right: Expression,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl TryFrom<&TokenKind> for BinaryOperator {
    type Error = String;

    fn try_from(value: &TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::BangEqual => Ok(BinaryOperator::NotEquals),
            TokenKind::EqualEquals => Ok(BinaryOperator::Equals),
            TokenKind::Plus => Ok(BinaryOperator::Addition),
            TokenKind::Minus => Ok(BinaryOperator::Subtraction),
            TokenKind::Star => Ok(BinaryOperator::Multiplication),
            TokenKind::Slash => Ok(BinaryOperator::Division),
            TokenKind::Less => Ok(BinaryOperator::LessThan),
            TokenKind::LessEqual => Ok(BinaryOperator::LessThanOrEqual),
            TokenKind::Greater => Ok(BinaryOperator::GreaterThan),
            TokenKind::GreaterEqual => Ok(BinaryOperator::GreaterThanOrEqual),
            kind => Err(format!("Unexpected token {kind:?}")),
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Addition => f.write_str("+"),
            BinaryOperator::Subtraction => f.write_str("-"),
            BinaryOperator::Multiplication => f.write_str("*"),
            BinaryOperator::Division => f.write_str("/"),
            BinaryOperator::Equals => f.write_str("=="),
            BinaryOperator::NotEquals => f.write_str("!="),
            BinaryOperator::LessThan => f.write_str("<"),
            BinaryOperator::LessThanOrEqual => f.write_str("<="),
            BinaryOperator::GreaterThan => f.write_str(">"),
            BinaryOperator::GreaterThanOrEqual => f.write_str(">="),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ObjectValue {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
}

impl std::fmt::Display for ObjectValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectValue::Nil => f.write_str("nil"),
            ObjectValue::Number(number) => f.write_str(&number.to_string()),
            ObjectValue::String(str) => f.write_str(str),
            ObjectValue::Boolean(val) => f.write_str(if *val { "true" } else { "false" }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub right: Expression,
}

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    LogicalNot,
    Negation,
}

impl TryFrom<&TokenKind> for UnaryOperator {
    type Error = String;

    fn try_from(value: &TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Bang => Ok(UnaryOperator::LogicalNot),
            TokenKind::Minus => Ok(UnaryOperator::Negation),
            _ => Err(format!("Unexpected token: {value:?}")),
        }
    }
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::LogicalNot => f.write_str("!"),
            UnaryOperator::Negation => f.write_str("-"),
        }
    }
}

pub fn print(expr: &Expression) -> String {
    match expr {
        Expression::Binary(expr) => {
            format!(
                "({} {} {})",
                expr.operator,
                print(&expr.left),
                print(&expr.right)
            )
        }
        Expression::Grouping(expr) => {
            format!("(group {})", print(&expr))
        }
        Expression::Unary(expr) => {
            format!("({} {})", expr.operator, print(&expr.right))
        }
        Expression::Literal(obj) => format!("{obj}"),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_basic() {
        let tree = Expression::Binary(Box::new(BinaryExpr {
            left: Expression::Unary(Box::new(UnaryExpr {
                operator: UnaryOperator::Negation,
                right: Expression::Literal(ObjectValue::Number(123.)),
            })),
            operator: BinaryOperator::Multiplication,
            right: Expression::Grouping(Box::new(Expression::Literal(ObjectValue::Number(45.67)))),
        }));

        assert_eq!("(* (- 123) (group 45.67))", print(&tree).as_str());
    }
}
