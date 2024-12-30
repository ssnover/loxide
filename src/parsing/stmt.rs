use super::{expr::parse_expr, Error};
use crate::{
    ast::{IfStatement, Statement},
    scanning::{Token, TokenKind},
    token_matches, Span,
};

pub fn parse_declaration(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    if token_matches!(tokens.get(0), TokenKind::Var) {
        parse_var_declaration(tokens)
    } else {
        parse_statement(tokens)
    }
}

pub fn parse_statement(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    match tokens.get(0).map(|token| &token.kind) {
        Some(TokenKind::Print) => parse_print_statement(tokens),
        Some(TokenKind::LeftBrace) => parse_block(tokens),
        Some(TokenKind::If) => parse_if(tokens),
        _ => parse_expr_statement(tokens),
    }
}

fn parse_print_statement(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    let (consumed, expr) = parse_expr(&tokens[1..])?;
    if token_matches!(tokens.get(consumed + 1), TokenKind::Semicolon) {
        Ok((consumed + 2, Statement::Print(expr)))
    } else {
        Err(Error {
            span: Span::new(
                tokens.get(0).unwrap().span.start,
                tokens.get(1 + consumed - 1).unwrap().span.end,
            ),
            err: String::from("Expected ';' after value."),
        })
    }
}

fn parse_expr_statement(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    let (consumed, expr) = parse_expr(tokens)?;
    if token_matches!(tokens.get(consumed), TokenKind::Semicolon) {
        Ok((consumed + 1, Statement::Expression(expr)))
    } else {
        Err(Error {
            span: Span::new(
                tokens.get(0).unwrap().span.start,
                tokens.get(consumed - 1).unwrap().span.end,
            ),
            err: String::from("Expected ';' after value."),
        })
    }
}

fn parse_block(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    let mut consumed = 1; // we assume the first token is left brace
    let mut stmts = vec![];

    while consumed < tokens.len() {
        if token_matches!(tokens.get(consumed), TokenKind::RightBrace) {
            break;
        }
        let (decl_consumed, stmt) = parse_declaration(&tokens[consumed..])?;
        stmts.push(stmt);
        consumed += decl_consumed;
    }

    if token_matches!(tokens.get(consumed), TokenKind::RightBrace) {
        consumed += 1;
        Ok((consumed, Statement::Block(stmts)))
    } else {
        Err(Error {
            span: Span::bounding(&tokens.get(0).unwrap().span, &tokens.last().unwrap().span),
            err: String::from("Expected '}' after block."),
        })
    }
}

fn parse_if(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    let mut consumed = 1;
    if !token_matches!(tokens.get(consumed), TokenKind::LeftParen) {
        return Err(Error {
            span: tokens.get(0).unwrap().span.clone(),
            err: String::from("Expected '(' after if condition"),
        });
    }
    consumed += 1;

    let (expr_consumed, expr) = parse_expr(&tokens[consumed..])?;
    consumed += expr_consumed;

    if !token_matches!(tokens.get(consumed), TokenKind::RightParen) {
        return Err(Error {
            span: Span::bounding(
                &tokens.get(0).unwrap().span,
                &tokens.get(consumed - 1).unwrap().span,
            ),
            err: String::from("Expected ')' after if condition"),
        });
    }
    consumed += 1;

    let (stmt_consumed, then_branch) = parse_statement(&tokens[consumed..])?;
    consumed += stmt_consumed;

    let else_branch = if token_matches!(tokens.get(consumed), TokenKind::Else) {
        consumed += 1;
        let (else_consumed, else_branch) = parse_statement(&tokens[consumed..])?;
        consumed += else_consumed;
        Some(else_branch)
    } else {
        None
    };

    Ok((
        consumed,
        Statement::If(Box::new(IfStatement {
            condition: expr,
            then_branch,
            else_branch,
        })),
    ))
}

fn parse_var_declaration(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    let mut consumed = 1;
    if let Some(Token {
        kind: TokenKind::Ident(ident),
        ..
    }) = tokens.get(consumed)
    {
        consumed += 1;
        let initializer = if token_matches!(tokens.get(consumed), TokenKind::Equals) {
            consumed += 1;
            let (expr_consumed, expr) = parse_expr(&tokens[consumed..])?;
            consumed += expr_consumed;
            Some(expr)
        } else {
            None
        };

        if token_matches!(tokens.get(consumed), TokenKind::Semicolon) {
            consumed += 1;
            Ok((
                consumed,
                Statement::VarDeclaration((ident.clone(), initializer)),
            ))
        } else {
            Err(Error {
                span: Span::bounding(
                    &tokens.get(0).unwrap().span,
                    &tokens.get(consumed - 1).unwrap().span,
                ),
                err: String::from("Expected ';' after variable declaration."),
            })
        }
    } else {
        Err(Error {
            span: tokens.get(0).unwrap().span.clone(),
            err: String::from("Expected ident after 'var'."),
        })
    }
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

        let (consumed, stmt) = parse_declaration(&tokens).unwrap();
        assert_eq!(tokens.len(), consumed);
        let Statement::VarDeclaration((name, Some(initializer))) = stmt else {
            panic!("Expected variable declaration, got {stmt:?} instead");
        };
        assert_eq!("value", name.as_str());
        let Expression::Literal(ObjectValue::Number(5.)) = initializer else {
            panic!("Expected number initializer, got {initializer:?}");
        };
    }
}
