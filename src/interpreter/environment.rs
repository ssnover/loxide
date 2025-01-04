use super::{Error, ErrorKind, Object};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Debug)]
pub struct Environment {
    values: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: Default::default(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: Default::default(),
            parent: Some(parent),
        }
    }

    pub fn define(&mut self, name: impl Into<String>, value: Object) {
        let _ = self.values.insert(name.into(), value);
    }

    pub fn get(&self, name: impl Into<String>) -> Option<Object> {
        let name = name.into();
        if let Some(value) = self.values.get(&name) {
            Some(value.clone())
        } else if let Some(value) = self
            .parent
            .as_ref()
            .map(|parent| parent.borrow().get(&name))
            .flatten()
        {
            Some(value.clone())
        } else {
            None
        }
    }

    pub fn get_at(&self, name: impl Into<String>, distance: usize) -> Option<Object> {
        if distance == 0 {
            let name = name.into();
            self.values.get(&name).cloned()
        } else {
            self.parent
                .as_ref()
                .map(|parent| parent.borrow().get_at(name, distance - 1))
                .flatten()
        }
    }

    pub fn assign(&mut self, name: impl Into<String>, value: Object) -> Result<(), Error> {
        let name = name.into();
        match (self.values.get_mut(&name), self.parent.as_ref()) {
            (Some(obj), _) => {
                *obj = value;
                Ok(())
            }
            (None, Some(parent)) => parent.borrow_mut().assign(name, value),
            (None, None) => Err(Error {
                kind: ErrorKind::UndefinedVariable(name),
            }),
        }
    }

    pub fn find_distance(&self, name: impl Into<String>) -> Option<usize> {
        let name = name.into();
        match (self.values.get(&name), self.parent.as_ref()) {
            (Some(_), _) => Some(0),
            (None, Some(parent)) => parent.borrow().find_distance(name).map(|dist| dist + 1),
            (None, None) => None,
        }
    }
}
