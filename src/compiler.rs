use std::mem;

use bumpalo::Bump;
use citadel_api::frontend::ir::{
    self,
    irgen::{HIRStream, IRGenerator},
    FuncStmt, IRStmt, IRTypedIdent, VarStmt, FLOAT32_T, FLOAT64_T, INT16_T, INT32_T, INT64_T,
};
use parcer::ast::{
    expr::Expression,
    stmt::{BlockStmt, Field, FunctionStmt, Statement, VariableStmt},
    types::Type,
    Ident,
};

pub struct Compiler<'c> {
    pub arena: &'c Bump,
    pub out: IRGenerator<'c>,

    global_ctx: Option<CompileCtx<'c>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CompileCtx<'ctx> {
    // int f() { return 100; }
    // ^^^              ^^^
    // |                Expression that is affected
    // type that is passed
    FuncRetType(ir::Type<'ctx>),
    // int x = 100;
    // ^^^     ^^^
    // |       Expression that is affected
    // type that is passed
    VarType(ir::Type<'ctx>),
}

impl<'c> Compiler<'c> {
    pub fn new(arena: &'c Bump) -> Self {
        Self {
            arena,
            out: IRGenerator::default(),
            global_ctx: Option::default(),
        }
    }

    pub fn compile_program(mut self, ast: Vec<Statement<'c>>) -> HIRStream<'c> {
        for stmt in ast {
            self.compile_stmt(stmt);
        }
        self.out.stream()
    }

    fn compile_stmt(&mut self, stmt: Statement<'c>) {
        match stmt {
            Statement::Struct(_) => todo!(),
            Statement::Enum(_) => todo!(),
            Statement::Union(_) => todo!(),
            Statement::Label(_) => todo!(),
            Statement::Function(node) => self.compile_func_stmt(node),
            Statement::Variable(node) => self.compile_var_stmt(node),
            Statement::If(_) => todo!(),
            Statement::Switch(_) => todo!(),
            Statement::While(_) => todo!(),
            Statement::DoWhile(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::Typedef(_) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::Break(_) => todo!(),
            Statement::Continue(_) => todo!(),
            Statement::Goto(_) => todo!(),
            Statement::Block(_) => todo!(),
            Statement::Expression(_) => todo!(),
        }
    }

    fn compile_func_stmt(&mut self, node: FunctionStmt<'c>) {
        if let Some(block) = node.body {
            let block = self.compile_block_stmt(block);
            self.out.gen_ir(IRStmt::Function(FuncStmt {
                name: Self::compile_typed_ident(&node.name, node.ret_type),
                args: Self::compile_fields_to_tis(node.args),
                block,
            }));
        }
    }

    fn compile_var_stmt(&mut self, node: VariableStmt<'c>) {
        let typed_ident = Self::compile_typed_ident(node.name, node._type);
        let val = self.compile_expr(
            node.val.unwrap(),
            Some(CompileCtx::VarType(typed_ident._type)),
        );
        self.out.gen_ir(IRStmt::Variable(VarStmt {
            name: typed_ident,
            val,
            is_const: node.is_const,
        }));
    }

    fn compile_block_stmt(&mut self, node: BlockStmt<'c>) -> ir::BlockStmt<'c> {
        let mut block = Vec::new();
        mem::swap(self.out.mut_stream_ref().mut_stream_ref(), &mut block);
        for stmt in node.block {
            self.compile_stmt(stmt);
        }
        mem::swap(self.out.mut_stream_ref().mut_stream_ref(), &mut block);
        ir::BlockStmt { stmts: block }
    }

    fn compile_expr(&self, val: Expression<'c>, ctx: Option<CompileCtx<'c>>) -> ir::IRExpr<'c> {
        match val {
            Expression::LiteralString(_) => todo!(),
            Expression::LiteralChar(_) => todo!(),
            Expression::LiteralShort(node) => {
                self.compile_lit_expr(ir::Literal::Int16(node), ctx, ir::Type::Ident(INT16_T))
            },
            Expression::LiteralInt(node) => {
                self.compile_lit_expr(ir::Literal::Int32(node), ctx, ir::Type::Ident(INT32_T))
            }
            Expression::LiteralLong(_) => todo!(),
            Expression::LiteralFloat(_) => todo!(),
            Expression::LiteralDouble(_) => todo!(),
            Expression::Ident(_) => todo!(),
            Expression::Prefix(_) => todo!(),
            Expression::Infix(_) => todo!(),
            Expression::Post(_) => todo!(),
            Expression::Call(_) => todo!(),
        }
    }

    fn compile_lit_expr(
        &self,
        lit: ir::Literal<'c>,
        ctx: Option<CompileCtx<'c>>,
        default_type: ir::Type<'c>,
    ) -> ir::IRExpr<'c> {
        let default_type = match self.global_ctx {
            Some(CompileCtx::FuncRetType(t)) => t,
            _ => default_type,
        };
        let lit_type = match ctx {
            Some(CompileCtx::VarType(t)) => t,
            _ => default_type,
        };
        ir::IRExpr::Literal(lit, lit_type)
    }

    fn compile_typed_ident(ident: Ident<'c>, _type: Type<'c>) -> IRTypedIdent<'c> {
        IRTypedIdent {
            ident,
            _type: Self::compile_type(_type),
        }
    }

    fn compile_type(_type: Type<'c>) -> ir::Type<'c> {
        match _type {
            Type::Ident(id) => ir::Type::Ident(Self::compile_ident_type(id)),
            Type::Pointer { .. } => todo!(),
            Type::Array { .. } => todo!(),
            Type::Struct(_) => todo!(),
            Type::Union(_) => todo!(),
            Type::Enum(_) => todo!(),
        }
    }

    fn compile_fields_to_tis(fields: Vec<Field<'c>>) -> Vec<IRTypedIdent<'c>> {
        let mut tis = Vec::with_capacity(fields.len());
        for field in fields {
            tis.push(IRTypedIdent {
                ident: field.name,
                _type: Self::compile_type(field.field_type),
            })
        }
        tis
    }

    #[inline]
    fn compile_ident_type(ident: Ident<'c>) -> ir::Ident<'c> {
        match ident {
            "short" => INT16_T,
            "int" => INT32_T,
            "long" => INT64_T,
            "float" => FLOAT32_T,
            "double" => FLOAT64_T,
            ident => ident,
        }
    }
}
