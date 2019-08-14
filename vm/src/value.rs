use std::fmt;
use std::fmt::{Display};
use std::rc::Rc;
use crate::context::LoxContext;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    Object(ObjRef)
}

// TODO make this a proper type instead of a type alias
pub(crate) type ObjRef = Rc<Obj>;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(d) => write!(f, "{}", d),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Object(o) => {
                match o.deref() {
                    Obj::String { value, .. } => write!(f, "\"{}\"", value),
                    _ => write!(f, "{:?}", o)
                }
            }
        }
    }
}

macro_rules! fn_is_x {
    ($sel:ident, $name:ident, $variant:pat) => {
        pub fn $name(&$sel) -> bool {
            match $sel {
                $variant => true,
                _ => false
            }
        }
    };
}

macro_rules! fn_is_obj {
    ($sel:ident, $name:ident, $matcher:pat) => {
        pub fn $name(&$sel) -> bool {
            match $sel {
                Value::Object(r) => match **r {
                    $matcher => true,
                    _ => false
                }
                _ => false
            }
        }
    };
}

macro_rules! fn_as_obj {
    ( $sel:ident, $name:ident, $p:pat => $result:expr, $ret_type:ty ) => {
        pub fn $name(&$sel) -> $ret_type {
            match $sel {
                Value::Object(r) => match &**r {
                    $p => $result,
                    v => panic!("expected object to be type {}, but got {:?}", stringify!(ty), v)
                },
                v => panic!("value was not an object: {:?}", v)
            }
        }
    };
}

impl Value {
    fn_is_x!(self, is_nil, Value::Nil);
    fn_is_x!(self, is_double, Value::Number(_));
    fn_is_x!(self, is_bool, Value::Boolean(_));
    fn_is_x!(self, is_obj, Value::Object(_));

    fn_is_obj!(self, is_string, Obj::String { .. });
    fn_as_obj!(self, as_string, Obj::String { value, .. } => &value, &str);

}

pub fn allocate_string(value: &str, context: &mut LoxContext) -> ObjRef {
    context.strings.intern(value.to_owned(), || Rc::new(Obj::String {
        header: ObjHeader::new(),
        value: value.to_owned()
    }))
}

#[derive(Debug, Copy, Clone)]
pub struct ObjHeader {}

impl ObjHeader {
    // don't need the header quite yet since all it can do is report the type of the value, which is
    // already encoded in the ObjectValue enum. will need it eventually though once we get to GC.
    fn new() -> ObjHeader {
        ObjHeader {}
    }
}

impl Display for ObjHeader {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        // headers shouldn't ever be displayed
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Obj {
    String { header: ObjHeader, value: String }
}

impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Obj::String { value: s1, .. }, Obj::String { value: s2, .. }) => s1 == s2,
        }
    }
}

impl Eq for Obj {}