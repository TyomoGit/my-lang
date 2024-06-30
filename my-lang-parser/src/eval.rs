use std::collections::HashMap;

use crate::{
    ast::{Block, Decl, DeclKind, Expr, ExprKind, Stmt, StmtKind},
    value::Value,
};

#[derive(Debug)]
pub struct Evaluator {
    environment: Environment,
}

#[derive(Debug)]
pub struct Environment {
    variables: HashMap<String, Value>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Box<Environment>) -> Self {
        Self {
            variables: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn into_parent(self) -> Option<Box<Environment>> {
        self.parent
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
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

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
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
            StmtKind::Expr(expr) => self.eval_expr(expr),
            StmtKind::SemiExpr(expr) => {
                let _ = self.eval_expr(expr);
                Value::Void
            }
        }
    }

    fn eval_block(&mut self, block: &Block) -> Value {
        self.enter_scope();
        for decl in &block.decls {
            self.eval_decl(decl);
        }

        let value = if let Some(return_expr) = &block.return_expr {
            self.eval_expr(return_expr)
        } else {
            Value::Void
        };

        self.exit_scope();
        value
    }

    fn enter_scope(&mut self) {
        let old_environment = std::mem::take(&mut self.environment);
        self.environment = Environment::with_parent(old_environment.into());
    }

    fn exit_scope(&mut self) {
        if !self.environment.has_parent() {
            return;
        }

        let old_environment = std::mem::take(&mut self.environment);
        let Some(parent) = old_environment.into_parent() else {
            unreachable!("parent is None");
        };
        self.environment = *parent;
    }

    #[allow(clippy::only_used_in_recursion)]
    fn eval_expr(&mut self, expr: &Expr) -> Value {
        macro_rules! impl_binary_op {
            ($lhs:ident, $rhs:ident, $op:tt, $type:tt) => {
                {
                    let lhs = self.eval_expr($lhs);
                    let rhs = self.eval_expr($rhs);

                    let result = lhs $op rhs;
                    result
                }
            };
        }
        macro_rules! impl_binary_op_bool {
            ($lhs:ident, $rhs:ident, $op:tt, $type:tt) => {
                {
                    let lhs = self.eval_expr($lhs);
                    let rhs = self.eval_expr($rhs);
                    Value::Bool(lhs $op rhs)
                }
            };
        }

        macro_rules! impl_logical_op {
            ($lhs:ident, $rhs:ident, $op:tt, $type:tt) => {
                {
                    let Value::Bool(lhs) = self.eval_expr($lhs) else {
                        panic!("expected bool, got: {:?}", $lhs)
                    };
                    let Value::Bool(rhs) = self.eval_expr($rhs) else {
                        panic!("expected bool, got: {:?}", $rhs)
                    };
                    Value::Bool(lhs $op rhs)
                }
            };
        }



        match expr.kind() {
            ExprKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let Value::Bool(cond) = self.eval_expr(cond) else {
                    panic!("expected bool, got: {:?}", cond)
                };

                if cond {
                    self.eval_block(then_block)
                } else if let Some(else_block) = else_block {
                    self.eval_block(else_block)
                } else {
                    Value::Void
                }
            }
            ExprKind::Number(number) => Value::Number(*number),
            ExprKind::String(string) => Value::String(string.to_string()),
            ExprKind::Block(block) => self.eval_block(block),
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
                let Value::Number(number) = self.eval_expr(expr) else {
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
            ExprKind::Eq(lhs, rhs) => impl_binary_op_bool!(lhs, rhs, ==, Bool),
            ExprKind::Ne(lhs, rhs) => impl_binary_op_bool!(lhs, rhs, !=, Bool),
            ExprKind::Gt(lhs, rhs) => impl_binary_op_bool!(lhs, rhs, >, Bool),
            ExprKind::Ge(lhs, rhs) => impl_binary_op_bool!(lhs, rhs, >=, Bool),
            ExprKind::Lt(lhs, rhs) => impl_binary_op_bool!(lhs, rhs, <, Bool),
            ExprKind::Le(lhs, rhs) => impl_binary_op_bool!(lhs, rhs, <=, Bool),
            ExprKind::And(lhs, rhs) => impl_logical_op!(lhs, rhs, &&, Bool),
            ExprKind::Or(lhs, rhs) => impl_logical_op!(lhs, rhs, ||, Bool),

            ExprKind::Print(expr) => {
                let value = self.eval_expr(expr);
                println!("{}", value);
                Value::Void
            }
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}
