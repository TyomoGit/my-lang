use std::{ fmt::Display, ops};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Void,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(number) => write!(f, "{}", number),
            Value::String(string) => write!(f, "{}", string),
            Value::Bool(bool) => write!(f, "{}", bool),
            Value::Void => write!(f, "void"),
        }
    }
}

impl ops::Add for Value {
    type Output = Value;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
            (Value::String(lhs), Value::String(rhs)) => Value::String(format!("{}{}", lhs, rhs)),
            (left, right) => panic!("cannot add {:?} and {:?}", left, right),
        }
    }
}

macro_rules! impl_binary_op_value {
    ($trait:path, $op:tt, $method:tt, $op_name:literal) => {
        impl $trait for Value {
            type Output = Value;
            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs $op rhs),
                    (left, right) => panic!(concat!("cannot ", $op_name, " {:?} and {:?}"), left, right),
                }
            }
        }
    };
}

impl_binary_op_value!(ops::Sub, -, sub, "subtract");
impl_binary_op_value!(ops::Mul, *, mul, "multiply");
impl_binary_op_value!(ops::Div, /, div, "divide");
impl_binary_op_value!(ops::Rem, %, rem, "modulus");

// logical and
