use std::collections::HashMap;
use crate::value::ObjRef;

pub struct LoxContext {
    pub(crate) strings: Strings
}

impl LoxContext {
    pub fn new() -> LoxContext {
        LoxContext {
            strings: Strings::new()
        }
    }
}

pub(crate) struct Strings {
    interned: HashMap<String, ObjRef>
}

impl Strings {
    fn new() -> Strings {
        Strings {
            interned: HashMap::new()
        }
    }

    pub(crate) fn intern<F: FnOnce() -> ObjRef>(&mut self, key: String, factory: F) -> ObjRef {
        self.interned.entry(key).or_insert_with(factory).clone()
    }
}