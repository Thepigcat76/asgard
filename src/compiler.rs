use std::mem;

use bumpalo::Bump;
use citadel_api::frontend::ir::{
    self,
    irgen::{HIRStream, IRGenerator},
    FuncStmt, IRStmt, IRTypedIdent,
};
use parcer::ast::{
    stmt::{BlockStmt, Field, FunctionStmt, Statement},
    types::Type,
    Ident,
};

pub struct Compiler<'c> {
    pub arena: &'c Bump,
    pub out: IRGenerator<'c>,
}

impl<'c> Compiler<'c> {
    pub fn compile_program(ast: Vec<Statement<'c>>, arena: &'c Bump) -> HIRStream<'c> {
        let mut compiler = Compiler {
            arena,
            out: IRGenerator::default(),
        };
        for stmt in ast {
            compiler.compile_stmt(stmt);
        }
        compiler.out.stream()
    }

    fn compile_stmt(&mut self, stmt: Statement<'c>) {
        match stmt {
            Statement::Struct(_) => todo!(),
            Statement::Enum(_) => todo!(),
            Statement::Union(_) => todo!(),
            Statement::Label(_) => todo!(),
            Statement::Function(node) => self.compile_func_stmt(node),
            Statement::Variable(_) => todo!(),
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

    fn compile_typed_ident(ident: Ident<'c>, _type: Type<'c>) -> IRTypedIdent<'c> {
        IRTypedIdent {
            ident,
            _type: Self::compile_type(_type),
        }
    }

    fn compile_type(_type: Type<'c>) -> ir::Type<'c> {
        match _type {
            Type::Ident(id) => ir::Type::Ident(id),
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
    
    fn compile_block_stmt(&mut self, node: BlockStmt<'c>) -> ir::BlockStmt<'c> {
        let mut block = Vec::new();
        mem::swap(self.out.mut_stream_ref().mut_stream_ref(), &mut block);
        for stmt in node.block {
            self.compile_stmt(stmt);
        }
        mem::swap(self.out.mut_stream_ref().mut_stream_ref(), &mut block);
        ir::BlockStmt { stmts: block }
    }
}
