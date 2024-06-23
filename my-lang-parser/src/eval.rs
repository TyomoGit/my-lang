use crate::{ast::Expr, value::Value};

pub type Program = Expr;

#[derive(Debug, Clone)]
pub struct Evaluator {
    program: Program,
}

impl Evaluator {
    pub fn new(program: Program) -> Self {
        Self { program }
    }

    pub fn evaluate(&self) -> Value {
        self.eval_expr(&self.program)
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
            Expr::Ident(ident) => panic!("undefined variable: {}", ident),
            Expr::Bool(bool) => Value::Bool(*bool),
            Expr::Minus(expr) => {
                let Value::Number(number) = self.eval_expr(expr.as_ref()) else {
                    panic!("expected number, got: {:?}", expr)
                };
                Value::Number(-number)
            }
            Expr::Assign(_left, _right) => unimplemented!(),
            Expr::Add(lhs, rhs) => {
                match self.eval_expr(lhs.as_ref()) {
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
                }
            }
            Expr::Sub(lhs, rhs) => {
                let Value::Number(lhs) = self.eval_expr(lhs.as_ref()) else {
                    panic!("expected number, got: {:?}", lhs)
                };
                let Value::Number(rhs) = self.eval_expr(rhs.as_ref()) else {
                    panic!("expected number, got: {:?}", rhs)
                };
                Value::Number(lhs - rhs)
            },
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
