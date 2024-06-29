use std::collections::HashMap;

use crate::{
    ast::{Decl, DeclKind, Expr, ExprKind, Stmt, StmtKind},
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

    pub fn into_parent(self) -> Option<&'a mut Environment<'a>> {
        self.parent
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
        Self {
            environment: Environment::new(),
        }
    }

    pub fn evaluate(&mut self, program: &Vec<Decl>) {
        for decl in program {
            self.eval_decl(decl);
        }
    }

    fn eval_decl(&mut self, decl: &Decl) {
        match decl.kind() {
            DeclKind::Stmt(stmt) => {
                self.eval_stmt(stmt);
            }
            DeclKind::Let(ident, expr) => {
                let value = self.eval_stmt(expr);
                self.environment.define(ident.name.clone(), value.clone());
            }
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Value {
        match stmt.kind() {
            StmtKind::Empty => Value::Void,
            StmtKind::Expr(expr) => {
                unreachable!("unreachable expr: {:?} in eval_stmt", expr)
            }
            StmtKind::SemiExpr(expr) => {
                let _ = self.eval_expr(expr);
                Value::Void
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn eval_expr(&mut self, expr: &Expr) -> Value {
        macro_rules! impl_binary_op {
            ($lhs:ident, $rhs:ident, $op:tt, $type:tt) => {
                {
                    let Value::$type(lhs) = self.eval_expr(&$lhs) else {
                        panic!(concat!("expected ", stringify!($type), ", got: {:?}"), $lhs)
                    };
                    let Value::$type(rhs) = self.eval_expr(&$rhs) else {
                        panic!(concat!("expected ", stringify!($type), ", got: {:?}"), $rhs)
                    };
                    Value::$type(lhs $op rhs)
                }
            };
        }

        match expr.kind() {
            ExprKind::Number(number) => Value::Number(*number),
            ExprKind::String(string) => Value::String(string.to_string()),
            ExprKind::Block { decls, return_expr } => {
                for decl in decls {
                    self.eval_decl(decl);
                }
                if let Some(return_expr) = return_expr {
                    self.eval_expr(&return_expr)
                } else {
                    Value::Void
                }
            }
            ExprKind::Ident(ident) => {
                let ident = &ident.name;
                if let Some(value) = self.environment.get(ident) {
                    value.clone()
                } else {
                    panic!("undefined variable: {}", ident)
                }
            }
            ExprKind::Bool(bool) => Value::Bool(*bool),
            ExprKind::Minus(expr) => {
                let Value::Number(number) = self.eval_expr(&expr) else {
                    panic!("expected number, got: {:?}", expr)
                };
                Value::Number(-number)
            }
            ExprKind::Assign(_left, _right) => unimplemented!(),
            ExprKind::Add(lhs, rhs) => match self.eval_expr(&lhs) {
                Value::Number(lhs) => {
                    let Value::Number(rhs) = self.eval_expr(rhs) else {
                        panic!("expected number, got: {:?}", rhs)
                    };
                    Value::Number(lhs + rhs)
                }
                Value::String(lhs) => {
                    let Value::String(rhs) = self.eval_expr(rhs) else {
                        panic!("expected string, got: {:?}", rhs)
                    };
                    Value::String(lhs + &rhs)
                }
                _ => panic!("expected number or string, got: {:?}", lhs),
            },
            ExprKind::Sub(lhs, rhs) => {
                let Value::Number(lhs) = self.eval_expr(lhs) else {
                    panic!("expected number, got: {:?}", lhs)
                };
                let Value::Number(rhs) = self.eval_expr(rhs) else {
                    panic!("expected number, got: {:?}", rhs)
                };
                Value::Number(lhs - rhs)
            }
            ExprKind::Mul(lhs, rhs) => impl_binary_op!(lhs, rhs, *, Number),
            ExprKind::Div(lhs, rhs) => impl_binary_op!(lhs, rhs, /, Number),
            ExprKind::Eq(lhs, rhs) => impl_binary_op!(lhs, rhs, ==, Bool),
            ExprKind::Ne(lhs, rhs) => impl_binary_op!(lhs, rhs, !=, Bool),
            ExprKind::Gt(lhs, rhs) => impl_binary_op!(lhs, rhs, >, Bool),
            ExprKind::Ge(lhs, rhs) => impl_binary_op!(lhs, rhs, >=, Bool),
            ExprKind::Lt(lhs, rhs) => impl_binary_op!(lhs, rhs, <, Bool),
            ExprKind::Le(lhs, rhs) => impl_binary_op!(lhs, rhs, <=, Bool),
            ExprKind::And(lhs, rhs) => impl_binary_op!(lhs, rhs, &&, Bool),
            ExprKind::Or(lhs, rhs) => impl_binary_op!(lhs, rhs, ||, Bool),

            ExprKind::Print(expr) => {
                let value = self.eval_expr(expr);
                println!("{}", value);
                Value::Void
            }
        }
    }
}

impl<'a> Default for Evaluator<'a> {
    fn default() -> Self {
        Self::new()
    }
}
