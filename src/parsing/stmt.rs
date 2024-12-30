use super::{expr::parse_expr, Error};
use crate::{
    ast::Statement,
    scanning::{Token, TokenKind},
    Span,
};

pub fn parse_statement(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    if let Some(Token {
        kind: TokenKind::Print,
        ..
    }) = tokens.get(0)
    {
        parse_print_statement(&tokens[1..]).map(|(consumed, stmt)| (consumed + 1, stmt))
    } else {
        parse_expr_statement(tokens)
    }
}

fn parse_print_statement(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    let (consumed, expr) = parse_expr(tokens)?;
    if let Some(Token {
        kind: TokenKind::Semicolon,
        ..
    }) = tokens.get(consumed)
    {
        Ok((consumed + 1, Statement::Print(expr)))
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

fn parse_expr_statement(tokens: &[Token]) -> Result<(usize, Statement), Error> {
    let (consumed, expr) = parse_expr(tokens)?;
    if let Some(Token {
        kind: TokenKind::Semicolon,
        ..
    }) = tokens.get(consumed)
    {
        Ok((consumed + 1, Statement::Expression(expr)))
    } else {
        Err(Error {
            span: Span::new(
                tokens.get(0).unwrap().span.start,
                tokens.get(consumed).unwrap().span.end,
            ),
            err: String::from("Expected ';' after value."),
        })
    }
}
