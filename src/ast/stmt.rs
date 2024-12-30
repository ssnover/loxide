use super::Expression;

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Block(Vec<Statement>),
    VarDeclaration((String, Option<Expression>)),
}
