use crate::ast::{Statement, Expr, BinOp, UnOp};
use crate::bytecode::{Chunk, Bytecode, Value};
use std::collections::HashMap;

pub struct CodegenContext {
    pub chunk: Chunk,
    pub functions: HashMap<String, Chunk>,
}

impl CodegenContext {
    pub fn new() -> Self {
        CodegenContext {
            chunk: Chunk::new(),
            functions: HashMap::new(),
        }
    }

    pub fn gen_program(&mut self, program: &[Statement]) {
        for stmt in program {
            self.gen_stmt(stmt);
        }
        self.chunk.code.push(Bytecode::Halt);
    }

    pub fn gen_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::LetDecl { name, value, .. } => {
                self.gen_expr(value);
                self.chunk.code.push(Bytecode::StoreVar(name.clone()));
            }
            Statement::Assignment { name, value, .. } => {
                self.gen_expr(value);
                self.chunk.code.push(Bytecode::StoreVar(name.clone()));
            }
            Statement::ExprStmt(expr, _) => {
                self.gen_expr(expr);
                self.chunk.code.push(Bytecode::Pop);
            }
            Statement::Return(expr, _) => {
                // If expr is None, push Value::None before Return
                if let Expr::Identifier(name, _) = expr {
                    if name == "None" {
                        let idx = self.chunk.add_const(Value::None);
                        self.chunk.code.push(Bytecode::PushConst(idx));
                    } else {
                        self.gen_expr(expr);
                    }
                } else {
                    self.gen_expr(expr);
                }
                self.chunk.code.push(Bytecode::Return);
            }
            Statement::IfStmt { cond, then_branch, elif_branches, else_branch, .. } => {
                self.gen_expr(cond);
                let jump_if_false_pos = self.chunk.code.len();
                self.chunk.code.push(Bytecode::JumpIfFalse(usize::MAX)); // placeholder
                self.chunk.code.push(Bytecode::EnterScope);
                for stmt in then_branch {
                    self.gen_stmt(stmt);
                }
                self.chunk.code.push(Bytecode::ExitScope);
                let mut after_if_jumps = vec![];
                after_if_jumps.push(self.chunk.code.len());
                self.chunk.code.push(Bytecode::Jump(usize::MAX)); // placeholder
                // Elif branches
                let mut elif_jump_positions = vec![];
                for elif in elif_branches {
                    let elif_start = self.chunk.code.len();
                    self.gen_expr(&elif.cond);
                    let elif_jump_if_false = self.chunk.code.len();
                    self.chunk.code.push(Bytecode::JumpIfFalse(usize::MAX));
                    self.chunk.code.push(Bytecode::EnterScope);
                    for stmt in &elif.body {
                        self.gen_stmt(stmt);
                    }
                    self.chunk.code.push(Bytecode::ExitScope);
                    after_if_jumps.push(self.chunk.code.len());
                    self.chunk.code.push(Bytecode::Jump(usize::MAX));
                    elif_jump_positions.push((elif_jump_if_false, elif_start));
                }
                // Else branch
                let else_start = self.chunk.code.len();
                if let Some(else_branch) = else_branch {
                    self.chunk.code.push(Bytecode::EnterScope);
                    for stmt in else_branch {
                        self.gen_stmt(stmt);
                    }
                    self.chunk.code.push(Bytecode::ExitScope);
                }
                let after_if = self.chunk.code.len();
                self.chunk.code[jump_if_false_pos] = Bytecode::JumpIfFalse(else_start);
                for jump_pos in after_if_jumps {
                    self.chunk.code[jump_pos] = Bytecode::Jump(after_if);
                }
                for (elif_jump_pos, _elif_start) in elif_jump_positions {
                    self.chunk.code[elif_jump_pos] = Bytecode::JumpIfFalse(else_start);
                }
            }
            Statement::WhileStmt { cond, body, .. } => {
                let loop_start = self.chunk.code.len();
                self.gen_expr(cond);
                let jump_if_false_pos = self.chunk.code.len();
                self.chunk.code.push(Bytecode::JumpIfFalse(usize::MAX));
                let push_loop_exit_pos = self.chunk.code.len();
                self.chunk.code.push(Bytecode::PushLoopExit(usize::MAX));
                self.chunk.code.push(Bytecode::EnterScope);
                for stmt in body {
                    self.gen_stmt(stmt);
                }
                self.chunk.code.push(Bytecode::ExitScope);
                self.chunk.code.push(Bytecode::Jump(loop_start));
                let after_loop = self.chunk.code.len();
                self.chunk.code[jump_if_false_pos] = Bytecode::JumpIfFalse(after_loop);
                self.chunk.code[push_loop_exit_pos] = Bytecode::PushLoopExit(after_loop);
            }
            Statement::FnDecl { name, params, body, .. } => {
                // Generate a new chunk for the function body
                let mut fn_ctx = CodegenContext::new();
                fn_ctx.chunk.code.push(Bytecode::EnterScope);
                // Map argN to parameter names
                for (i, param) in params.iter().enumerate() {
                    fn_ctx.chunk.code.push(Bytecode::LoadVar(format!("arg{}", i)));
                    fn_ctx.chunk.code.push(Bytecode::StoreVar(param.name.clone()));
                }
                for stmt in body {
                    fn_ctx.gen_stmt(stmt);
                }
                fn_ctx.chunk.code.push(Bytecode::ExitScope);
                fn_ctx.chunk.code.push(Bytecode::Return); // Ensure function returns
                self.functions.insert(name.clone(), fn_ctx.chunk);
            }
            Statement::Break(_) => {
                self.chunk.code.push(Bytecode::Break);
            }
            Statement::ForStmt { init, cond, step, body, .. } => {
                // for (init; cond; step) { body }
                self.gen_stmt(init);
                let loop_start = self.chunk.code.len();
                self.gen_expr(cond);
                let jump_if_false_pos = self.chunk.code.len();
                self.chunk.code.push(Bytecode::JumpIfFalse(usize::MAX));
                let push_loop_exit_pos = self.chunk.code.len();
                self.chunk.code.push(Bytecode::PushLoopExit(usize::MAX));
                self.chunk.code.push(Bytecode::EnterScope);
                for stmt in body {
                    self.gen_stmt(stmt);
                }
                self.chunk.code.push(Bytecode::ExitScope);
                self.gen_stmt(step);
                self.chunk.code.push(Bytecode::Jump(loop_start));
                let after_loop = self.chunk.code.len();
                self.chunk.code[jump_if_false_pos] = Bytecode::JumpIfFalse(after_loop);
                self.chunk.code[push_loop_exit_pos] = Bytecode::PushLoopExit(after_loop);
            }
        }
    }

    pub fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLiteral(val, _) => {
                let idx = self.chunk.add_const(Value::Int(*val));
                self.chunk.code.push(Bytecode::PushConst(idx));
            }
            Expr::FloatLiteral(val, _) => {
                let idx = self.chunk.add_const(Value::Float(*val));
                self.chunk.code.push(Bytecode::PushConst(idx));
            }
            Expr::BoolLiteral(val, _) => {
                let idx = self.chunk.add_const(Value::Bool(*val));
                self.chunk.code.push(Bytecode::PushConst(idx));
            }
            Expr::StrLiteral(val, _) => {
                let idx = self.chunk.add_const(Value::Str(val.clone()));
                self.chunk.code.push(Bytecode::PushConst(idx));
            }
            Expr::Identifier(name, _) => {
                self.chunk.code.push(Bytecode::LoadVar(name.clone()));
            }
            Expr::BinaryOp(left, op, right, _) => {
                self.gen_expr(left);
                self.gen_expr(right);
                match op {
                    BinOp::Add => self.chunk.code.push(Bytecode::Add),
                    BinOp::Sub => self.chunk.code.push(Bytecode::Sub),
                    BinOp::Mul => self.chunk.code.push(Bytecode::Mul),
                    BinOp::Div => self.chunk.code.push(Bytecode::Div),
                    BinOp::Eq => self.chunk.code.push(Bytecode::Eq),
                    BinOp::NotEq => self.chunk.code.push(Bytecode::Neq),
                    BinOp::Lt => self.chunk.code.push(Bytecode::Lt),
                    BinOp::Gt => self.chunk.code.push(Bytecode::Gt),
                    BinOp::Le => self.chunk.code.push(Bytecode::Lte),
                    BinOp::Ge => self.chunk.code.push(Bytecode::Gte),
                    BinOp::And => {
                        let end = self.chunk.code.len();
                        self.chunk.code.push(Bytecode::JumpIfFalse(usize::MAX));
                        self.gen_expr(right);
                        self.chunk.code[end] = Bytecode::JumpIfFalse(self.chunk.code.len());
                    }
                    BinOp::Or => {
                        let end = self.chunk.code.len();
                        self.chunk.code.push(Bytecode::JumpIfFalse(usize::MAX));
                        self.gen_expr(right);
                        self.chunk.code[end] = Bytecode::JumpIfFalse(self.chunk.code.len());
                    }
                }
            }
            Expr::UnaryOp(op, expr, _) => {
                self.gen_expr(expr);
                match op {
                    UnOp::Neg => self.chunk.code.push(Bytecode::Neg),
                    UnOp::Not => self.chunk.code.push(Bytecode::Not),
                }
            }
            Expr::Call(name, args, _) => {
                for arg in args {
                    self.gen_expr(arg);
                }
                self.chunk.code.push(Bytecode::Call(name.clone(), args.len()));
                // Ensure every call leaves a value on the stack
            }
        }
    }
} 