use super::Error;
use crate::{
    ast::{BinaryExpr, BinaryOperator, Expression, ObjectValue, UnaryExpr, UnaryOperator},
    scanning::{Token, TokenKind},
};

pub fn parse_expr(tokens: &[Token]) -> Result<(usize, Expression), Error> {
    parse_equality(tokens)
}

fn parse_equality(tokens: &[Token]) -> Result<(usize, Expression), Error> {
    let (mut consumed, mut expr) = parse_comparison(tokens)?;

    let mut tokens_iter = tokens[consumed..].iter().peekable();
    while let Some(Token { kind, .. }) = tokens_iter
        .next_if(|token| matches!(token.kind, TokenKind::BangEqual | TokenKind::EqualEquals))
    {
        consumed += 1; // for the operator
        let (right_consumed, right_expr) = parse_comparison(&tokens[consumed..])?;
        consumed += right_consumed;
        expr = Expression::Binary(Box::new(BinaryExpr {
            left: expr,
            operator: BinaryOperator::try_from(kind)
                .expect("Should be able to convert BinOp from two matched"),
            right: right_expr,
        }));
        tokens_iter = tokens[consumed..].iter().peekable();
    }

    Ok((consumed, expr))
}

fn parse_comparison(tokens: &[Token]) -> Result<(usize, Expression), Error> {
    let (mut consumed, mut expr) = parse_term(tokens)?;

    let mut tokens_iter = tokens[consumed..].iter().peekable();
    while let Some(Token { kind, .. }) = tokens_iter.next_if(|token| {
        matches!(
            token.kind,
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        )
    }) {
        consumed += 1;
        let (right_consumed, right_expr) = parse_term(&tokens[consumed..])?;
        consumed += right_consumed;
        expr = Expression::Binary(Box::new(BinaryExpr {
            left: expr,
            operator: BinaryOperator::try_from(kind)
                .expect("Should be able to convert BinOp from four matched"),
            right: right_expr,
        }));
        tokens_iter = tokens[consumed..].iter().peekable();
    }

    Ok((consumed, expr))
}

fn parse_term(tokens: &[Token]) -> Result<(usize, Expression), Error> {
    let (mut consumed, mut expr) = parse_factor(tokens)?;

    let mut tokens_iter = tokens[consumed..].iter().peekable();
    while let Some(Token { kind, .. }) =
        tokens_iter.next_if(|token| matches!(token.kind, TokenKind::Minus | TokenKind::Plus))
    {
        consumed += 1;
        let (right_consumed, right_expr) = parse_factor(&tokens[consumed..])?;
        consumed += right_consumed;
        expr = Expression::Binary(Box::new(BinaryExpr {
            left: expr,
            operator: BinaryOperator::try_from(kind)
                .expect("Should be able to parse BinOp from two matched"),
            right: right_expr,
        }));
        tokens_iter = tokens[consumed..].iter().peekable();
    }

    Ok((consumed, expr))
}

fn parse_factor(tokens: &[Token]) -> Result<(usize, Expression), Error> {
    let (mut consumed, mut expr) = parse_unary(tokens)?;

    let mut tokens_iter = tokens[consumed..].iter().peekable();
    while let Some(Token { kind, .. }) =
        tokens_iter.next_if(|token| matches!(token.kind, TokenKind::Slash | TokenKind::Star))
    {
        consumed += 1;
        let (right_consumed, right_expr) = parse_unary(&tokens[consumed..])?;
        consumed += right_consumed;
        expr = Expression::Binary(Box::new(BinaryExpr {
            left: expr,
            operator: BinaryOperator::try_from(kind)
                .expect("Should be to convert BinOp from two matched"),
            right: right_expr,
        }));
        tokens_iter = tokens[consumed..].iter().peekable();
    }

    Ok((consumed, expr))
}

fn parse_unary(tokens: &[Token]) -> Result<(usize, Expression), Error> {
    if let Some(Token { kind, .. }) = tokens.get(0) {
        if matches!(kind, TokenKind::Minus | TokenKind::Bang) {
            let (right_consumed, right_expr) = parse_unary(&tokens[1..])?;
            return Ok((
                right_consumed + 1,
                Expression::Unary(Box::new(UnaryExpr {
                    operator: UnaryOperator::try_from(kind)
                        .expect("Should be able to convert Unary Op from matched"),
                    right: right_expr,
                })),
            ));
        }
    }

    parse_primary(tokens)
}

fn parse_primary(tokens: &[Token]) -> Result<(usize, Expression), Error> {
    match tokens.get(0).map(|token| &token.kind) {
        Some(TokenKind::False) => Ok((1, Expression::Literal(ObjectValue::Boolean(false)))),
        Some(TokenKind::True) => Ok((1, Expression::Literal(ObjectValue::Boolean(true)))),
        Some(TokenKind::Nil) => Ok((1, Expression::Literal(ObjectValue::Nil))),
        Some(TokenKind::Number(num)) => Ok((1, Expression::Literal(ObjectValue::Number(*num)))),
        Some(TokenKind::String(str)) => {
            Ok((1, Expression::Literal(ObjectValue::String(str.clone()))))
        }
        Some(TokenKind::LeftParen) => {
            let (consumed, expr) = parse_expr(&tokens[1..])?;
            if let Some(Token {
                kind: TokenKind::RightParen,
                ..
            }) = tokens.get(consumed + 1)
            {
                Ok((2 + consumed, expr))
            } else {
                let left_paren = tokens.get(0).unwrap();
                Err(Error {
                    err: format!("Unmatched left parenthesis, expected ')' after expression"),
                    span: left_paren.span.clone(),
                })
            }
        }
        Some(_) => {
            let token = tokens.get(0).unwrap();
            Err(Error {
                err: format!("Expected literal, got unexpected token: {token:?}"),
                span: token.span.clone(),
            })
        }
        None => Err(Error {
            err: format!("Expected expression, but reached end of token stream"),
            span: Default::default(),
        }),
    }
}
