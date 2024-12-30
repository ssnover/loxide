use crate::{
    ast::{BinaryExpr, BinaryOperator, Expression, ObjectValue, UnaryExpr, UnaryOperator},
    scanning::{Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct Error {
    start_span: (usize, usize),
    end_span: Option<(usize, usize)>,
    err: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.err.fmt(f)
    }
}

impl std::error::Error for Error {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }

    fn description(&self) -> &str {
        &self.err
    }

    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

pub fn parse(tokens: &[Token]) -> Option<Expression> {
    match parse_expr(tokens) {
        Ok((_, expr)) => Some(expr),
        Err(_err) => {
            // todo: Synchronize here
            None
        }
    }
}

fn parse_expr(tokens: &[Token]) -> Result<(usize, Expression), Error> {
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
                    start_span: left_paren.start_span,
                    end_span: None,
                })
            }
        }
        Some(_) => {
            let token = tokens.get(0).unwrap();
            Err(Error {
                err: format!("Expected literal, got unexpected token: {token:?}"),
                start_span: token.start_span,
                end_span: Some(token.end_span),
            })
        }
        None => Err(Error {
            err: format!("Expected expression, but reached end of token stream"),
            start_span: Default::default(),
            end_span: None,
        }),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn token_sans_context(kind: TokenKind) -> Token {
        Token {
            kind,
            start_span: (0, 0),
            end_span: (0, 0),
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

        let expr = parse(&tokens).unwrap();
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
