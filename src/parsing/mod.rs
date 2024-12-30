use crate::{
    ast::Statement,
    scanning::{Token, TokenKind},
    Span,
};
use stmt::parse_declaration;

mod expr;
mod stmt;

#[derive(Debug, Clone)]
pub struct Error {
    pub span: Span,
    pub err: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.err.fmt(f)
    }
}

impl std::error::Error for Error {}

pub fn parse(tokens: &[Token]) -> Result<Vec<Statement>, Vec<Error>> {
    let mut statements = vec![];
    let mut errors = vec![];
    let mut consumed = 0;
    while consumed < tokens.len() {
        match parse_declaration(&tokens[consumed..]) {
            Ok((0, _)) => break,
            Ok((stmt_consumed, stmt)) => {
                consumed += stmt_consumed;
                statements.push(stmt);
            }
            Err(err) => {
                errors.push(err);
                let forwarded = synchronize(&tokens[consumed..]);
                if forwarded == 0 {
                    break;
                } else {
                    consumed += forwarded;
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}

fn synchronize(tokens: &[Token]) -> usize {
    let mut consumed = 0;
    let mut tokens = tokens.iter().peekable();
    while let Some(token) = tokens.next() {
        if matches!(token.kind, TokenKind::Semicolon) {
            return consumed;
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
            return consumed;
        }
        consumed += 1;
    }

    tokens.len()
}

#[macro_export]
macro_rules! token_matches {
    ($expr:expr, $kinds:pat) => {
        match $expr {
            Some(Token { kind: $kinds, .. }) => true,
            _ => false,
        }
    };
}

#[cfg(test)]
mod test {
    use crate::ast::{Expression, ObjectValue};

    use super::*;

    fn token_sans_context(kind: TokenKind) -> Token {
        Token {
            kind,
            span: Span::default(),
        }
    }

    #[test]
    fn test_var_decl() {
        let tokens = [
            TokenKind::Var,
            TokenKind::Ident(String::from("value")),
            TokenKind::Equals,
            TokenKind::Number(5.),
            TokenKind::Semicolon,
        ]
        .into_iter()
        .map(token_sans_context)
        .collect::<Vec<_>>();

        let program = parse(&tokens).unwrap();
        assert_eq!(1, program.len());
        let stmt = &program[0];
        let Statement::VarDeclaration((name, Some(initializer))) = stmt else {
            panic!("Expected variable declaration, got {stmt:?} instead");
        };
        assert_eq!("value", name.as_str());
        let Expression::Literal(ObjectValue::Number(5.)) = initializer else {
            panic!("Expected number initializer, got {initializer:?}");
        };
    }
}
