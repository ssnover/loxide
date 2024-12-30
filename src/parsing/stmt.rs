use super::{expr::parse_expr, Error};
use crate::{
    ast::{Expression, IfStatement, ObjectValue, Statement, WhileStatement},
    scanning::{Token, TokenKind},
    token_matches, Span,
};

type StmtResult = Result<(usize, Statement), Error>;

pub fn parse_declaration(tokens: &[Token]) -> StmtResult {
    if token_matches!(tokens.get(0), TokenKind::Var) {
        parse_var_declaration(tokens)
    } else {
        parse_statement(tokens)
    }
}

pub fn parse_statement(tokens: &[Token]) -> StmtResult {
    match tokens.get(0).map(|token| &token.kind) {
        Some(TokenKind::Print) => parse_print_statement(tokens),
        Some(TokenKind::While) => parse_while_loop(tokens),
        Some(TokenKind::For) => parse_for_loop(tokens),
        Some(TokenKind::LeftBrace) => parse_block(tokens),
        Some(TokenKind::If) => parse_if(tokens),
        _ => parse_expr_statement(tokens),
    }
}

fn parse_print_statement(tokens: &[Token]) -> StmtResult {
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

fn parse_while_loop(tokens: &[Token]) -> StmtResult {
    let mut consumed = 1;
    if !token_matches!(tokens.get(consumed), TokenKind::LeftParen) {
        return Err(Error {
            span: tokens.get(0).unwrap().span.clone(),
            err: String::from("Expected '(' after while condition"),
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
            err: String::from("Expected ')' after while condition"),
        });
    }
    consumed += 1;

    let (stmt_consumed, stmt) = parse_statement(&tokens[consumed..])?;
    consumed += stmt_consumed;

    Ok((
        consumed,
        Statement::While(Box::new(WhileStatement {
            condition: expr,
            body: stmt,
        })),
    ))
}

fn parse_for_loop(tokens: &[Token]) -> StmtResult {
    let mut consumed = 1;
    if !token_matches!(tokens.get(consumed), TokenKind::LeftParen) {
        return Err(Error {
            span: tokens.get(0).unwrap().span.clone(),
            err: String::from("Expected '(' after for statement"),
        });
    }
    consumed += 1;

    let initializer = match tokens.get(consumed).map(|token| &token.kind) {
        Some(TokenKind::Semicolon) => {
            consumed += 1;
            None
        }
        Some(TokenKind::Var) => {
            let (initializer_consumed, initializer) = parse_var_declaration(&tokens[consumed..])?;
            consumed += initializer_consumed;
            Some(initializer)
        }
        Some(_) => {
            let (expr_consumed, expr) = parse_expr_statement(&tokens[consumed..])?;
            consumed += expr_consumed;
            Some(expr)
        }
        _ => {
            return Err(Error {
                span: Span::bounding(
                    &tokens.get(0).unwrap().span,
                    &tokens.get(consumed - 1).unwrap().span,
                ),
                err: String::from("Unexpected end of stream while parsing for loop"),
            });
        }
    };

    let condition = if !token_matches!(tokens.get(consumed), TokenKind::Semicolon) {
        let (cond_consumed, cond) = parse_expr(&tokens[consumed..])?;
        consumed += cond_consumed;
        if !token_matches!(tokens.get(consumed), TokenKind::Semicolon) {
            return Err(Error {
                span: Span::bounding(
                    &tokens.get(0).unwrap().span,
                    &tokens.get(consumed - 1).unwrap().span,
                ),
                err: String::from("Expected ';' after for condition expression"),
            });
        }
        consumed += 1;
        Some(cond)
    } else {
        consumed += 1;
        None
    };

    let iteration = if !token_matches!(tokens.get(consumed), TokenKind::RightParen) {
        let (iter_consumed, iter) = parse_expr(&tokens[consumed..])?;
        consumed += iter_consumed;
        if !token_matches!(tokens.get(consumed), TokenKind::RightParen) {
            return Err(Error {
                span: Span::bounding(
                    &tokens.get(0).unwrap().span,
                    &tokens.get(consumed - 1).unwrap().span,
                ),
                err: String::from("Expected ')' after for iteration expression"),
            });
        }
        consumed += 1;
        Some(iter)
    } else {
        consumed += 1;
        None
    };

    let (body_consumed, body) = parse_statement(&tokens[consumed..])?;
    consumed += body_consumed;

    let body = match iteration {
        Some(iteration) => Statement::Block(vec![body, Statement::Expression(iteration)]),
        None => body,
    };

    let while_loop = Statement::While(Box::new(WhileStatement {
        condition: condition.unwrap_or(Expression::Literal(ObjectValue::Boolean(true))),
        body,
    }));

    let for_loop = match initializer {
        Some(initializer) => Statement::Block(vec![initializer, while_loop]),
        None => while_loop,
    };

    Ok((consumed, for_loop))
}

fn parse_expr_statement(tokens: &[Token]) -> StmtResult {
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

fn parse_block(tokens: &[Token]) -> StmtResult {
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

fn parse_if(tokens: &[Token]) -> StmtResult {
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

fn parse_var_declaration(tokens: &[Token]) -> StmtResult {
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
