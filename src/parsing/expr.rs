use super::Error;
use crate::{
    ast::{
        BinaryExpr, BinaryOperator, Expression, LogicalExpr, LogicalOperator, ObjectValue,
        UnaryExpr, UnaryOperator, Variable,
    },
    scanning::{Token, TokenKind},
    token_matches, Span,
};

type ExprResult = Result<(usize, Expression), Error>;

pub fn parse_expr(tokens: &[Token]) -> ExprResult {
    parse_assignment(tokens)
}

fn parse_assignment(tokens: &[Token]) -> ExprResult {
    let (expr_consumed, expr) = parse_or(tokens)?;

    if token_matches!(tokens.get(expr_consumed), TokenKind::Equals) {
        let mut consumed = expr_consumed + 1;
        let (inner_consumed, value) = parse_assignment(&tokens[consumed..])?;
        consumed += inner_consumed;
        if let Expression::Variable(name) = expr {
            Ok((
                consumed,
                Expression::Assignment((name.name, Box::new(value))),
            ))
        } else {
            Err(Error {
                span: Span::bounding(&tokens[0].span, &tokens[consumed - 1].span),
                err: String::from("Invalid assignment target."),
            })
        }
    } else {
        Ok((expr_consumed, expr))
    }
}

fn parse_or(tokens: &[Token]) -> ExprResult {
    let (mut consumed, mut expr) = parse_and(tokens)?;

    let mut tokens_iter = tokens[consumed..].iter().peekable();
    while let Some(_) = tokens_iter.next_if(|token| matches!(token.kind, TokenKind::Or)) {
        consumed += 1;
        let (right_consumed, right_expr) = parse_and(&tokens[consumed..])?;
        consumed += right_consumed;
        expr = Expression::Logical(Box::new(LogicalExpr {
            left: expr,
            operator: LogicalOperator::Or,
            right: right_expr,
        }));
        tokens_iter = tokens[consumed..].iter().peekable();
    }

    Ok((consumed, expr))
}

fn parse_and(tokens: &[Token]) -> ExprResult {
    let (mut consumed, mut expr) = parse_equality(tokens)?;

    let mut tokens_iter = tokens[consumed..].iter().peekable();
    while let Some(_) = tokens_iter.next_if(|token| matches!(token.kind, TokenKind::And)) {
        consumed += 1;
        let (right_consumed, right_expr) = parse_equality(&tokens[consumed..])?;
        consumed += right_consumed;
        expr = Expression::Logical(Box::new(LogicalExpr {
            left: expr,
            operator: LogicalOperator::Or,
            right: right_expr,
        }));
        tokens_iter = tokens[consumed..].iter().peekable();
    }

    Ok((consumed, expr))
}

fn parse_equality(tokens: &[Token]) -> ExprResult {
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

fn parse_comparison(tokens: &[Token]) -> ExprResult {
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

fn parse_term(tokens: &[Token]) -> ExprResult {
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

fn parse_factor(tokens: &[Token]) -> ExprResult {
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

fn parse_unary(tokens: &[Token]) -> ExprResult {
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

fn parse_primary(tokens: &[Token]) -> ExprResult {
    match tokens.get(0).map(|token| &token.kind) {
        Some(TokenKind::False) => Ok((1, Expression::Literal(ObjectValue::Boolean(false)))),
        Some(TokenKind::True) => Ok((1, Expression::Literal(ObjectValue::Boolean(true)))),
        Some(TokenKind::Nil) => Ok((1, Expression::Literal(ObjectValue::Nil))),
        Some(TokenKind::Ident(ident_name)) => Ok((
            1,
            Expression::Variable(Variable {
                name: ident_name.clone(),
            }),
        )),
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

        assert!(parse_expr(&tokens).is_err());
    }
}
