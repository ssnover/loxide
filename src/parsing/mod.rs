use crate::{
    ast::{
        BinaryExpr, BinaryOperator, Expression, ObjectValue, Statement, UnaryExpr, UnaryOperator,
    },
    scanning::{Token, TokenKind},
    Span,
};
use expr::parse_expr;
use stmt::parse_statement;

mod expr;
mod stmt;

#[derive(Debug, Clone)]
pub struct Error {
    span: Span,
    err: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.err.fmt(f)
    }
}

impl std::error::Error for Error {}

pub fn parse(tokens: &[Token]) -> Option<Vec<Statement>> {
    let mut statements = vec![];
    let mut consumed = 0;
    while consumed < tokens.len() {
        match parse_statement(&tokens[consumed..]) {
            Ok((0, _)) => break,
            Ok((stmt_consumed, stmt)) => {
                consumed += stmt_consumed;
                statements.push(stmt);
            }
            Err(err) => {
                eprintln!("{err}");
                break;
            }
        }
    }

    Some(statements)
}

fn synchronize(tokens: &[Token]) -> Option<usize> {
    let mut consumed = 0;
    let mut tokens = tokens.iter().peekable();
    while let Some(token) = tokens.next() {
        if matches!(token.kind, TokenKind::Semicolon) {
            return Some(consumed);
        }
        if matches!(
            tokens.peek().map(|token| &token.kind),
            Some(
                TokenKind::Class
                    | TokenKind::Fun
                    | TokenKind::Var
                    | TokenKind::For
                    | TokenKind::If
                    | TokenKind::While
                    | TokenKind::Print
                    | TokenKind::Return
            )
        ) {
            return Some(consumed);
        }
        consumed += 1;
    }

    None
}

#[cfg(test)]
mod test {
    use crate::Span;

    use super::*;

    fn token_sans_context(kind: TokenKind) -> Token {
        Token {
            kind,
            span: Span::default(),
        }
    }

    #[test]
    fn test_arithmetic_precedence() {
        let tokens = [
            TokenKind::Number(6.),
            TokenKind::Slash,
            TokenKind::Number(3.),
            TokenKind::Minus,
            TokenKind::Number(1.),
        ]
        .into_iter()
        .map(token_sans_context)
        .collect::<Vec<_>>();

        let (_, expr) = parse_expr(&tokens).unwrap();
        let Expression::Binary(expr) = expr else {
            panic!("Expect binary expression, got {expr:?} instead");
        };
        assert!(matches!(
            expr.right,
            Expression::Literal(ObjectValue::Number(1.))
        ));
        assert!(matches!(expr.operator, BinaryOperator::Subtraction));
        let Expression::Binary(expr) = expr.left else {
            panic!("Expected binary expression, got {expr:?} instead");
        };
        assert!(matches!(
            expr.right,
            Expression::Literal(ObjectValue::Number(3.))
        ));
        assert!(matches!(expr.operator, BinaryOperator::Division));
        assert!(matches!(
            expr.left,
            Expression::Literal(ObjectValue::Number(6.))
        ));
    }

    #[test]
    fn test_invalid_unary() {
        let tokens = [TokenKind::Star, TokenKind::Number(1.)]
            .into_iter()
            .map(token_sans_context)
            .collect::<Vec<_>>();

        assert!(parse(&tokens).is_none());
    }
}
