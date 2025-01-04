use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{Error, ErrorKind, Object};

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    class_name: String,
    inner: Rc<RefCell<Inner>>,
}

impl Instance {
    pub fn new(class_name: &str) -> Self {
        Self {
            class_name: class_name.to_string(),
            inner: Rc::new(RefCell::new(Inner {
                fields: HashMap::new(),
            })),
        }
    }

    pub fn class_name(&self) -> &str {
        &self.class_name
    }

    pub fn get(&self, name: &str) -> Result<Object, Error> {
        self.inner.borrow().get(name)
    }

    pub fn set(&self, name: &str, value: &Object) {
        self.inner.borrow_mut().set(name, value)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Inner {
    fields: HashMap<String, Object>,
}

impl Inner {
    pub fn get(&self, name: &str) -> Result<Object, Error> {
        match self.fields.get(name) {
            Some(object) => Ok(object.to_owned()),
            None => Err(Error {
                kind: ErrorKind::UndefinedProperty(name.to_string()),
            }),
        }
    }

    pub fn set(&mut self, name: &str, value: &Object) {
        self.fields.insert(name.to_string(), value.clone());
    }
}
