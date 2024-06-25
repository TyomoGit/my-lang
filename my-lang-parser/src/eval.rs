use std::collections::HashMap;

use crate::{
    ast::{Expr, Statement},
    value::Value,
};

#[derive(Debug)]
pub struct Evaluator<'a> {
    environment: Environment<'a>,
}

#[derive(Debug)]
pub struct Environment<'a> {
    variables: HashMap<String, Value>,
    parent: Option<&'a mut Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: &'a mut Environment<'a>) -> Self {
        Self {
            variables: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        if let Some(value) = self.variables.get(name) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }
}

impl<'a> Default for Environment<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Evaluator<'a> {
    pub fn new() -> Self {
        Self { environment: Environment::new() }
    }

    pub fn evaluate(&mut self, program: &Vec<Statement>) {
        for stmt in program {
            self.eval_stmt(stmt);
        }
    }

    fn eval_stmt(&mut self, stmt: &Statement) -> Value {
        match stmt {
            Statement::Empty => Value::Void,
            Statement::Expr(expr) => {
                let _ = self.eval_expr(expr);
                Value::Void
            }
            Statement::Let(name, expr) => {
                let value = self.eval_expr(expr);
                self.environment.define(name.to_string(), value.clone());
                Value::Void
            }
            Statement::Print(expr) => {
                let value = self.eval_expr(expr);
                println!("{}", value);
                Value::Void
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn eval_expr(&self, expr: &Expr) -> Value {
        macro_rules! impl_binary_op {
            ($lhs:ident, $rhs:ident, $op:tt, $type:tt) => {
                {
                    let Value::$type(lhs) = self.eval_expr($lhs.as_ref()) else {
                        panic!(concat!("expected ", stringify!($type), ", got: {:?}"), $lhs)
                    };
                    let Value::$type(rhs) = self.eval_expr($rhs.as_ref()) else {
                        panic!(concat!("expected ", stringify!($type), ", got: {:?}"), $rhs)
                    };
                    Value::$type(lhs $op rhs)
                }
            };
        }

        match expr {
            Expr::Number(number) => Value::Number(*number),
            Expr::String(string) => Value::String(string.to_string()),
            Expr::Block(statements) => unimplemented!(),
            Expr::Ident(ident) => {
                if let Some(value) = self.environment.get(ident) {
                    value.clone()
                } else {
                    panic!("undefined variable: {}", ident)
                }
            }
            Expr::Bool(bool) => Value::Bool(*bool),
            Expr::Minus(expr) => {
                let Value::Number(number) = self.eval_expr(expr.as_ref()) else {
                    panic!("expected number, got: {:?}", expr)
                };
                Value::Number(-number)
            }
            Expr::Assign(_left, _right) => unimplemented!(),
            Expr::Add(lhs, rhs) => match self.eval_expr(lhs.as_ref()) {
                Value::Number(lhs) => {
                    let Value::Number(rhs) = self.eval_expr(rhs.as_ref()) else {
                        panic!("expected number, got: {:?}", rhs)
                    };
                    Value::Number(lhs + rhs)
                }
                Value::String(lhs) => {
                    let Value::String(rhs) = self.eval_expr(rhs.as_ref()) else {
                        panic!("expected string, got: {:?}", rhs)
                    };
                    Value::String(lhs + &rhs)
                }
                _ => panic!("expected number or string, got: {:?}", lhs),
            },
            Expr::Sub(lhs, rhs) => {
                let Value::Number(lhs) = self.eval_expr(lhs.as_ref()) else {
                    panic!("expected number, got: {:?}", lhs)
                };
                let Value::Number(rhs) = self.eval_expr(rhs.as_ref()) else {
                    panic!("expected number, got: {:?}", rhs)
                };
                Value::Number(lhs - rhs)
            }
            Expr::Mul(lhs, rhs) => impl_binary_op!(lhs, rhs, *, Number),
            Expr::Div(lhs, rhs) => impl_binary_op!(lhs, rhs, /, Number),
            Expr::Eq(lhs, rhs) => impl_binary_op!(lhs, rhs, ==, Bool),
            Expr::Ne(lhs, rhs) => impl_binary_op!(lhs, rhs, !=, Bool),
            Expr::Gt(lhs, rhs) => impl_binary_op!(lhs, rhs, >, Bool),
            Expr::Ge(lhs, rhs) => impl_binary_op!(lhs, rhs, >=, Bool),
            Expr::Lt(lhs, rhs) => impl_binary_op!(lhs, rhs, <, Bool),
            Expr::Le(lhs, rhs) => impl_binary_op!(lhs, rhs, <=, Bool),
            Expr::And(lhs, rhs) => impl_binary_op!(lhs, rhs, &&, Bool),
            Expr::Or(lhs, rhs) => impl_binary_op!(lhs, rhs, ||, Bool),
        }
    }
}

impl<'a> Default for Evaluator<'a> {
    fn default() -> Self {
        Self::new()
    }
}
