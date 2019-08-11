use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Nil,
    Number(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Number(d) => write!(f, "{}", d),
        }
    }
}
