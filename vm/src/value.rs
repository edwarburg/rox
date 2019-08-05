use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Garbage,
    Double(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Garbage => write!(f, "literal garbage"),
            Value::Double(d) => write!(f, "{}", d),
        }
    }
}
