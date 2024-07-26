use citadel_api::frontend::ir::{
    self,
    irgen::{IRGenerator, HIRStream},
    FuncStmt, IRExpr, IRStmt, IRTypedIdent,
};
use parcer::ast::{
    expr::{CallExpr, Expression},
    stmt::{BlockStmt, Field, FunctionStmt, Statement},
    types::Type,
};

#[derive(Default)]
pub struct Compiler;

impl<'c> Compiler {
    pub fn compile_program(ast: Vec<Statement<'c>>) -> HIRStream<'c> {
        let mut ir_gen = IRGenerator::default();
        for stmt in ast {
            ir_gen.gen_ir(Self::compile_stmt(stmt));
        }
        ir_gen.stream()
    }

    pub fn compile_stmt(stmt: Statement<'c>) -> IRStmt<'c> {
        match stmt {
            Statement::Struct(node) => todo!(),
            Statement::Enum(node) => todo!(),
            Statement::Union(node) => todo!(),
            Statement::Label(node) => todo!(),
            Statement::Function(node) => Self::compile_function_stmt(node),
            Statement::Variable(node) => todo!(),
            Statement::If(node) => todo!(),
            Statement::Switch(node) => todo!(),
            Statement::While(node) => todo!(),
            Statement::DoWhile(node) => todo!(),
            Statement::For(node) => todo!(),
            Statement::Typedef(node) => todo!(),
            Statement::Return(node) => todo!(),
            Statement::Break(node) => todo!(),
            Statement::Continue(node) => todo!(),
            Statement::Goto(node) => todo!(),
            Statement::Block(node) => todo!(),
            Statement::Expression(node) => match Self::compile_expr(node) {
                IRExpr::Call(node) => IRStmt::Call(node),
                IRExpr::Literal(_, _) => todo!(),
                IRExpr::Ident(_) => todo!(),
                IRExpr::ArithOp(_) => todo!(),
                IRExpr::StructInit(_) => todo!(),
            },
        }
    }

    pub fn compile_expr(node: Expression<'c>) -> IRExpr<'c> {
        match node {
            Expression::Call(node) => IRExpr::Call(Self::compile_call_expr(node)),
            Expression::LiteralString(_) => todo!(),
            Expression::LiteralChar(_) => todo!(),
            Expression::LiteralShort(_) => todo!(),
            Expression::LiteralInt(_) => todo!(),
            Expression::LiteralLong(_) => todo!(),
            Expression::LiteralFloat(_) => todo!(),
            Expression::LiteralDouble(_) => todo!(),
            Expression::Ident(_) => todo!(),
            Expression::Prefix(_) => todo!(),
            Expression::Infix(_) => todo!(),
            Expression::Post(_) => todo!(),
        }
    }

    pub fn compile_function_stmt(node: FunctionStmt<'c>) -> IRStmt<'c> {
        IRStmt::Function(FuncStmt {
            name: IRTypedIdent {
                ident: ir::Ident(node.name),
                _type: Self::compile_type(node.ret_type),
            },
            args: Self::compile_field_list(node.args),
            block: Self::compile_block_stmt(node.body.unwrap()),
        })
    }

    pub fn compile_call_expr(node: CallExpr<'c>) -> ir::CallExpr<'c> {
        ir::CallExpr {
            name: ir::Ident(node.val),
            args: Self::compile_expr_list(node.args),
        }
    }

    pub fn compile_type(node: Type<'c>) -> ir::Ident<'c> {
        match node {
            Type::Struct(_) => todo!(),
            Type::Enum(_) => todo!(),
            Type::Union(_) => todo!(),
            Type::Auto => todo!(),
            Type::Ident(ident) => ir::Ident(Self::compile_builtin_type(ident)),
            Type::Pointer(_) => todo!(),
        }
    }

    fn compile_builtin_type(ident: &str) -> &str {
        match ident {
            "char" => "i8",
            "short" => "i16",
            "int" => "i32",
            "long" => "i64",
            "float" => "f32",
            "double" => "f64",
            ident => ident,
        }
    }

    pub fn compile_block_stmt(node: BlockStmt<'c>) -> ir::BlockStmt<'c> {
        ir::BlockStmt {
            stmts: node
                .block
                .into_iter()
                .map(|stmt| Self::compile_stmt(stmt))
                .collect(),
        }
    }

    pub fn compile_field_list(node: Vec<Field<'c>>) -> Vec<IRTypedIdent<'c>> {
        node.into_iter()
            .map(|field| IRTypedIdent {
                ident: citadel_frontend::ir::Ident(field.name),
                _type: Self::compile_type(field._type),
            })
            .collect()
    }

    pub fn compile_expr_list(node: Vec<Expression<'c>>) -> Vec<IRExpr<'c>> {
        node.into_iter()
            .map(|expr| Self::compile_expr(expr))
            .collect()
    }
}
