use super::Expression;

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Block(Vec<Statement>),
    VarDeclaration((String, Option<Expression>)),
    FnDeclaration(FnDeclaration),
    While(Box<WhileStatement>),
    If(Box<IfStatement>),
    Return(Option<Expression>),
}

#[derive(Clone, Debug)]
pub struct FnDeclaration {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Statement,
    pub else_branch: Option<Statement>,
}

#[derive(Clone, Debug)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Statement,
}
