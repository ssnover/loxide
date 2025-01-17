pub use expr::*;
pub use stmt::*;

mod expr;
mod stmt;

pub fn print(expr: &Expression) -> String {
    match expr {
        Expression::Assignment((Variable { name, .. }, expr)) => {
            format!("({name} = {})", print(&expr))
        }
        Expression::Binary(expr) => {
            format!(
                "({} {} {})",
                expr.operator,
                print(&expr.left),
                print(&expr.right)
            )
        }
        Expression::Call(expr) => {
            format!(
                "(call {} {})",
                print(&expr.callee),
                expr.args.iter().fold(String::new(), |mut acc, arg| {
                    acc.push_str(&print(arg));
                    acc
                })
            )
        }
        Expression::Get(expr) => {
            format!("(get {} {})", expr.property_name, print(&expr.object))
        }
        Expression::Grouping(expr) => {
            format!("(group {})", print(&expr))
        }
        Expression::Logical(expr) => {
            format!(
                "({} {} {}",
                expr.operator,
                print(&expr.left),
                print(&expr.right)
            )
        }
        Expression::Set(expr) => {
            format!(
                "(set {}.{} = {})",
                print(&expr.object),
                expr.property_name,
                print(&expr.value)
            )
        }
        Expression::Unary(expr) => {
            format!("({} {})", expr.operator, print(&expr.right))
        }
        Expression::Literal(obj) => format!("{obj}"),
        Expression::Variable(var) => var.name.clone(),
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
