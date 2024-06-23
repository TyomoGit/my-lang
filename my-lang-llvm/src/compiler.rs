use std::{error::Error, fmt::Display};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, BasicValueEnum},
    AddressSpace,
};
use my_lang_parser::ast::Expr;

pub type Program = Expr;

#[derive(Debug, Clone)]
pub enum CompileError {}

impl Error for CompileError {}
impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct Compiler<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let printf_fn_type = context.void_type().fn_type(
            &[
                context.i8_type().ptr_type(AddressSpace::default()).into(),
                // context.f64_type().into(),
            ],
            true,
        );
        let _printf_fn = module.add_function("printf", printf_fn_type, None);
        let main_fn_type = context.i32_type().fn_type(&[], false);
        let main_fn = module.add_function("main", main_fn_type, None);

        let builder = context.create_builder();
        let entry_block = context.append_basic_block(main_fn, "entry");
        builder.position_at_end(entry_block);

        Self {
            context,
            module,
            builder,
        }
    }

    pub fn compile(&self, program: &Program) {
        let value = self.compile_expr(program).unwrap();
        let msg = self
            .builder
            .build_global_string_ptr("Value: %f\n", "msg")
            .unwrap();

        let printf_fn = self.module.get_function("printf").unwrap();
        let _call = self
            .builder
            .build_call(
                printf_fn,
                &[msg.as_pointer_value().into(), value.into()],
                "call",
            )
            .unwrap();
        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)))
            .unwrap();
    }

    fn compile_expr(&self, expr: &Expr) -> Result<BasicValueEnum, CompileError> {
        let value = match expr {
            Expr::Number(number) => self
                .context
                .f64_type()
                .const_float(*number)
                .as_basic_value_enum(),
            Expr::String(_) => todo!(),
            Expr::Ident(_) => todo!(),
            Expr::Bool(bool) => self
                .context
                .bool_type()
                .const_int(*bool as u64, false)
                .as_basic_value_enum(),
            Expr::Minus(number) => {
                let BasicValueEnum::FloatValue(value) = self.compile_expr(number)? else {
                    panic!("expected float value");
                };
                let value = self.builder.build_float_neg(value, "neg").unwrap();
                BasicValueEnum::FloatValue(value)
            }
            Expr::Assign(_, _) => todo!(),
            Expr::Add(lhs, rhs) => {
                let BasicValueEnum::FloatValue(lhs) = self.compile_expr(lhs)? else {
                    panic!("expected float value");
                };
                let BasicValueEnum::FloatValue(rhs) = self.compile_expr(rhs)? else {
                    panic!("expected float value");
                };
                let value = self.builder.build_float_add(lhs, rhs, "add").unwrap();
                BasicValueEnum::FloatValue(value)
            }
            Expr::Sub(_, _) => todo!(),
            Expr::Mul(_, _) => todo!(),
            Expr::Div(_, _) => todo!(),
            Expr::Eq(_, _) => todo!(),
            Expr::Ne(_, _) => todo!(),
            Expr::Gt(_, _) => todo!(),
            Expr::Ge(_, _) => todo!(),
            Expr::Lt(_, _) => todo!(),
            Expr::Le(_, _) => todo!(),
            Expr::And(_, _) => todo!(),
            Expr::Or(_, _) => todo!(),
        };
        Ok(value)
    }
}
