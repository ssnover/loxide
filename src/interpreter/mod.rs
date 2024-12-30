use crate::ast::ObjectValue;

pub mod expression;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Nil,
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Object {
    pub fn add(&self, other: &Object) -> Result<Object, Error> {
        match (self, other) {
            (Object::Number(lhs), Object::Number(rhs)) => Ok(Object::Number(*lhs + *rhs)),
            (Object::String(lhs), Object::String(rhs)) => Ok(Object::String(format!("{lhs}{rhs}"))),
            _ => Err(Error {
                kind: ErrorKind::TypeError,
            }),
        }
    }

    pub fn subtract(&self, other: &Object) -> Result<Object, Error> {
        match (self, other) {
            (Object::Number(lhs), Object::Number(rhs)) => Ok(Object::Number(*lhs - *rhs)),
            _ => Err(Error {
                kind: ErrorKind::TypeError,
            }),
        }
    }

    pub fn multiply(&self, other: &Object) -> Result<Object, Error> {
        match (self, other) {
            (Object::Number(lhs), Object::Number(rhs)) => Ok(Object::Number(*lhs * *rhs)),
            _ => Err(Error {
                kind: ErrorKind::TypeError,
            }),
        }
    }

    pub fn divide(&self, other: &Object) -> Result<Object, Error> {
        match (self, other) {
            (Object::Number(lhs), Object::Number(rhs)) => Ok(Object::Number(*lhs / *rhs)),
            _ => Err(Error {
                kind: ErrorKind::TypeError,
            }),
        }
    }

    fn equals_inner(&self, other: &Object) -> Result<bool, Error> {
        match (self, other) {
            (Object::Nil, Object::Nil) => Ok(true),
            (Object::Boolean(lhs), Object::Boolean(rhs)) => Ok(lhs == rhs),
            (Object::String(lhs), Object::String(rhs)) => Ok(lhs == rhs),
            (Object::Number(lhs), Object::Number(rhs)) => Ok(lhs == rhs),
            _ => Err(Error {
                kind: ErrorKind::TypeError,
            }),
        }
    }

    pub fn equals(&self, other: &Object) -> Result<Object, Error> {
        self.equals_inner(other).map(|val| Object::Boolean(val))
    }

    pub fn not_equals(&self, other: &Object) -> Result<Object, Error> {
        self.equals_inner(other).map(|val| Object::Boolean(!val))
    }

    fn less_than_inner(&self, other: &Object) -> Result<bool, Error> {
        match (self, other) {
            (Object::Nil, Object::Nil) => Ok(false),
            (Object::Boolean(_), Object::Boolean(_)) => Ok(false),
            (Object::Number(lhs), Object::Number(rhs)) => Ok(lhs < rhs),
            (Object::String(lhs), Object::String(rhs)) => Ok(lhs < rhs),
            _ => Err(Error {
                kind: ErrorKind::TypeError,
            }),
        }
    }

    pub fn less_than(&self, other: &Object) -> Result<Object, Error> {
        self.less_than_inner(other).map(|val| Object::Boolean(val))
    }

    pub fn less_than_equal(&self, other: &Object) -> Result<Object, Error> {
        Ok(Object::Boolean(
            self.less_than_inner(other)? || self.equals_inner(other)?,
        ))
    }

    fn greater_than_inner(&self, other: &Object) -> Result<bool, Error> {
        match (self, other) {
            (Object::Nil, Object::Nil) => Ok(false),
            (Object::Boolean(_), Object::Boolean(_)) => Ok(false),
            (Object::Number(lhs), Object::Number(rhs)) => Ok(lhs > rhs),
            (Object::String(lhs), Object::String(rhs)) => Ok(lhs > rhs),
            _ => Err(Error {
                kind: ErrorKind::TypeError,
            }),
        }
    }

    pub fn greater_than(&self, other: &Object) -> Result<Object, Error> {
        self.greater_than_inner(other)
            .map(|val| Object::Boolean(val))
    }

    pub fn greater_than_equal(&self, other: &Object) -> Result<Object, Error> {
        Ok(Object::Boolean(
            self.greater_than_inner(other)? || self.equals_inner(other)?,
        ))
    }

    fn is_truthy(&self) -> bool {
        match self {
            Object::Nil => false,
            Object::Boolean(val) => *val,
            Object::Number(_) => false,
            Object::String(_) => false,
        }
    }

    pub fn invert(&self) -> Result<Object, Error> {
        Ok(Object::Boolean(!self.is_truthy()))
    }

    pub fn negate(&self) -> Result<Object, Error> {
        match self {
            Object::Number(num) => Ok(Object::Number(num * -1.)),
            _ => Err(Error {
                kind: ErrorKind::TypeError,
            }),
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Nil => "nil".fmt(f),
            Object::Boolean(val) => val.fmt(f),
            Object::Number(num) => num.fmt(f),
            Object::String(str) => str.fmt(f),
        }
    }
}

impl From<ObjectValue> for Object {
    fn from(value: ObjectValue) -> Self {
        match value {
            ObjectValue::Boolean(val) => Object::Boolean(val),
            ObjectValue::Nil => Object::Nil,
            ObjectValue::Number(num) => Object::Number(num),
            ObjectValue::String(str) => Object::String(str),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    TypeError,
}

#[derive(Clone, Debug)]
pub struct Error {
    kind: ErrorKind,
}