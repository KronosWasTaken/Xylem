use crate::ast::{Statement, Expr, Type};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Variable,
    Function { param_types: Vec<Option<Type>>, return_type: Option<Type> },
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub ty: Option<Type>,
    pub line: usize,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone)]
pub enum SemanticError {
    Redeclaration(String, usize),
    UndeclaredVariable(String, usize),
    TypeMismatch { expected: String, found: String, line: usize },
    FunctionArgMismatch { name: String, expected: usize, found: usize, line: usize },
    FunctionReturnMismatch { name: String, expected: String, found: String, line: usize },
}

#[derive(Default)]
pub struct SymbolTable {
    symbols: HashMap<String, SymbolInfo>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { symbols: HashMap::new() }
    }
    pub fn declare(&mut self, name: &str, info: SymbolInfo) -> Result<(), SemanticError> {
        if self.symbols.contains_key(name) {
            Err(SemanticError::Redeclaration(name.to_string(), info.line))
        } else {
            self.symbols.insert(name.to_string(), info);
            Ok(())
        }
    }
    pub fn get(&self, name: &str) -> Option<&SymbolInfo> {
        self.symbols.get(name)
    }
}

pub struct SemanticAnalyzer {
    pub errors: Vec<SemanticError>,
    pub scopes: Vec<SymbolTable>,
    pub current_function: Option<(String, Option<Type>)>,
    pub in_loop: usize, // track loop nesting
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            scopes: vec![SymbolTable::new()],
            current_function: None,
            in_loop: 0,
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(SymbolTable::new());
    }
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }
    pub fn analyze(&mut self, program: &Vec<Statement>) {
        // First pass: collect all function signatures
        for stmt in program {
            if let Statement::FnDecl { name, params, return_type, line, .. } = stmt {
                let param_types: Vec<Option<Type>> = params.iter().map(|p| p.ty.clone()).collect();
                let info = SymbolInfo {
                    ty: None,
                    line: *line,
                    kind: SymbolKind::Function { param_types: param_types.clone(), return_type: return_type.clone() },
                };
                // Only add to the global scope (first scope)
                if let Err(e) = self.scopes[0].declare(name, info) {
                    self.errors.push(e);
                }
            }
        }
        // Second pass: analyze all statements (including function bodies)
        for stmt in program {
            self.visit_stmt(stmt);
        }
    }

    fn type_of_expr(&self, expr: &Expr) -> Result<Type, Vec<SemanticError>> {
        match expr {
            Expr::IntLiteral(_, line)
            | Expr::FloatLiteral(_, line)
            | Expr::BoolLiteral(_, line)
            | Expr::StrLiteral(_, line) => Ok(match expr {
                Expr::IntLiteral(_, _) => Type::Int,
                Expr::FloatLiteral(_, _) => Type::Float,
                Expr::BoolLiteral(_, _) => Type::Bool,
                Expr::StrLiteral(_, _) => Type::Str,
                _ => unreachable!(),
            }),
            Expr::Identifier(name, line) => {
                if let Some(info) = self.lookup(name) {
                    if let Some(ty) = &info.ty {
                        Ok(ty.clone())
                    } else {
                        Err(vec![SemanticError::UndeclaredVariable(name.clone(), *line)])
                    }
                } else {
                    // Always error if variable is not declared
                    Err(vec![SemanticError::UndeclaredVariable(name.clone(), *line)])
                }
            }
            Expr::BinaryOp(left, op, right, line) => {
                let mut errors = Vec::new();
                let ltype = self.type_of_expr(left).map_err(|mut e| { errors.append(&mut e); () }).ok();
                let rtype = self.type_of_expr(right).map_err(|mut e| { errors.append(&mut e); () }).ok();
                match op {
                    crate::ast::BinOp::Add => {
                        if ltype == rtype && (ltype == Some(Type::Int) || ltype == Some(Type::Float) || ltype == Some(Type::Str)) {
                            Ok(ltype.unwrap())
                        } else {
                            errors.push(SemanticError::TypeMismatch {
                                expected: format!("{:?}", ltype),
                                found: format!("{:?}", rtype),
                                line: *line,
                            });
                            Err(errors)
                        }
                    }
                    crate::ast::BinOp::Sub | crate::ast::BinOp::Mul | crate::ast::BinOp::Div => {
                        if ltype == rtype && (ltype == Some(Type::Int) || ltype == Some(Type::Float)) {
                            Ok(ltype.unwrap())
                        } else {
                            errors.push(SemanticError::TypeMismatch {
                                expected: format!("{:?}", ltype),
                                found: format!("{:?}", rtype),
                                line: *line,
                            });
                            Err(errors)
                        }
                    }
                    crate::ast::BinOp::Eq | crate::ast::BinOp::NotEq | crate::ast::BinOp::Lt | crate::ast::BinOp::Gt | crate::ast::BinOp::Le | crate::ast::BinOp::Ge => {
                        if ltype == rtype {
                            Ok(Type::Bool)
                        } else {
                            errors.push(SemanticError::TypeMismatch {
                                expected: format!("{:?}", ltype),
                                found: format!("{:?}", rtype),
                                line: *line,
                            });
                            Err(errors)
                        }
                    }
                    crate::ast::BinOp::And | crate::ast::BinOp::Or => {
                        if ltype == Some(Type::Bool) && rtype == Some(Type::Bool) {
                            Ok(Type::Bool)
                        } else {
                            errors.push(SemanticError::TypeMismatch {
                                expected: "Bool".to_string(),
                                found: format!("{:?}", ltype),
                                line: *line,
                            });
                            Err(errors)
                        }
                    }
                }
            }
            Expr::UnaryOp(op, expr, line) => {
                let t_res = self.type_of_expr(expr);
                let t_val = t_res.as_ref().ok();
                match op {
                    crate::ast::UnOp::Neg => {
                        match t_val {
                            Some(Type::Int) | Some(Type::Float) => t_res,
                            _ => Err(vec![SemanticError::TypeMismatch {
                                expected: "Int or Float".to_string(),
                                found: format!("{:?}", t_res),
                                line: *line,
                            }]),
                        }
                    }
                    crate::ast::UnOp::Not => {
                        if t_val == Some(&Type::Bool) {
                            Ok(Type::Bool)
                        } else {
                            Err(vec![SemanticError::TypeMismatch {
                                expected: "Bool".to_string(),
                                found: format!("{:?}", t_res),
                                line: *line,
                            }])
                        }
                    }
                }
            }
            Expr::Call(name, args, line) => {
                let mut errors = Vec::new();
                // Always check all argument expressions for errors (including print)
                for arg in args {
                    if let Err(mut e) = self.type_of_expr(arg) {
                        errors.append(&mut e);
                    }
                }
                if name == "print" {
                    // Built-in: always valid, accept any argument types
                    if errors.is_empty() {
                        Ok(Type::Str)
                    } else {
                        Err(errors)
                    }
                } else if let Some(info) = self.lookup(name) {
                    if let SymbolKind::Function { param_types, return_type } = &info.kind {
                        if args.len() != param_types.len() {
                            errors.push(SemanticError::FunctionArgMismatch {
                                name: name.clone(),
                                expected: param_types.len(),
                                found: args.len(),
                                line: *line,
                            });
                            return Err(errors);
                        }
                        for (arg, param_ty) in args.iter().zip(param_types.iter()) {
                            if let Some(expected_val) = param_ty {
                                match self.type_of_expr(arg) {
                                    Ok(found) if &found != expected_val => {
                                        errors.push(SemanticError::TypeMismatch {
                                            expected: format!("{:?}", expected_val),
                                            found: format!("{:?}", self.type_of_expr(arg)),
                                            line: *line,
                                        });
                                    }
                                    Err(mut e) => errors.append(&mut e),
                                    _ => {}
                                }
                            }
                        }
                        if errors.is_empty() {
                            Ok(return_type.clone().unwrap_or(Type::Int))
                        } else {
                            Err(errors)
                        }
                    } else {
                        errors.push(SemanticError::TypeMismatch {
                            expected: "function".to_string(),
                            found: "variable".to_string(),
                            line: *line,
                        });
                        Err(errors)
                    }
                } else {
                    errors.push(SemanticError::UndeclaredVariable(name.clone(), *line));
                    Err(errors)
                }
            }
        }
    }

    fn current_scope_mut(&mut self) -> &mut SymbolTable {
        // Always use the innermost scope, but for function signatures, use the global scope (first scope)
        self.scopes.last_mut().unwrap()
    }

    fn lookup(&self, name: &str) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    fn visit_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::LetDecl { name, ty, value, line } => {
                let value_type = self.type_of_expr(value);
                match (&ty, &value_type) {
                    (Some(decl_type), Ok(val_type)) if decl_type != val_type => {
                        self.errors.push(SemanticError::TypeMismatch {
                            expected: format!("{:?}", decl_type),
                            found: format!("{:?}", val_type),
                            line: *line,
                        });
                    }
                    (_, Err(e)) => self.errors.extend(e.iter().cloned()),
                    _ => {}
                }
                // Only error if variable is being declared again in the same scope as a LetDecl
                let current_scope = self.scopes.last_mut().unwrap();
                if let Some(existing) = current_scope.get(name) {
                    // Only error if the existing symbol is a variable (not a function) and this is a LetDecl
                    if let SymbolKind::Variable = existing.kind {
                        self.errors.push(SemanticError::Redeclaration(name.clone(), *line));
                    } else {
                        // Allow shadowing functions with variables
                        let info = SymbolInfo {
                            ty: ty.clone().or(value_type.ok()),
                            line: *line,
                            kind: SymbolKind::Variable,
                        };
                        let _ = current_scope.declare(name, info);
                    }
                } else {
                    let info = SymbolInfo {
                        ty: ty.clone().or(value_type.ok()),
                        line: *line,
                        kind: SymbolKind::Variable,
                    };
                    let _ = current_scope.declare(name, info);
                }
            }
            Statement::Assignment { name, value, line } => {
                if let Some(info) = self.lookup(name) {
                    let lhs_type = info.ty.clone();
                    let rhs_type = self.type_of_expr(value);
                    match (lhs_type, rhs_type) {
                        (Some(lhs), Ok(rhs)) if lhs != rhs => {
                            self.errors.push(SemanticError::TypeMismatch {
                                expected: format!("{:?}", lhs),
                                found: format!("{:?}", rhs),
                                line: *line,
                            });
                        }
                        (_, Err(e)) => self.errors.extend(e.iter().cloned()),
                        _ => {}
                    }
                } else {
                    self.errors.push(SemanticError::UndeclaredVariable(name.clone(), *line));
                }
            }
            Statement::ExprStmt(expr, line) => {
                // Always check for errors in the expression, including undeclared variables
                if let Err(mut e) = self.type_of_expr(expr) {
                    for err in &mut e {
                        // If the error line is 0, use the statement line
                        match err {
                            SemanticError::TypeMismatch { line: l, .. } | SemanticError::FunctionArgMismatch { line: l, .. } | SemanticError::FunctionReturnMismatch { line: l, .. } | SemanticError::UndeclaredVariable(_, l) | SemanticError::Redeclaration(_, l) => {
                                if *l == 0 { *l = *line; }
                            }
                        }
                    }
                    self.errors.append(&mut e);
                }
            }
            Statement::IfStmt { cond, then_branch, elif_branches, else_branch, line } => {
                if let Err(mut e) = self.type_of_expr(cond) {
                    for err in &mut e {
                        match err {
                            SemanticError::TypeMismatch { line: l, .. } | SemanticError::FunctionArgMismatch { line: l, .. } | SemanticError::FunctionReturnMismatch { line: l, .. } | SemanticError::UndeclaredVariable(_, l) | SemanticError::Redeclaration(_, l) => {
                                if *l == 0 { *l = *line; }
                            }
                        }
                    }
                    self.errors.append(&mut e);
                } else if self.type_of_expr(cond).ok() != Some(Type::Bool) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: format!("{:?}", self.type_of_expr(cond).ok()),
                        line: *line,
                    });
                }
                self.enter_scope();
                for s in then_branch {
                    self.visit_stmt(s);
                }
                self.exit_scope();
                for elif in elif_branches {
                    if let Err(mut e) = self.type_of_expr(&elif.cond) {
                        for err in &mut e {
                            match err {
                                SemanticError::TypeMismatch { line: l, .. } | SemanticError::FunctionArgMismatch { line: l, .. } | SemanticError::FunctionReturnMismatch { line: l, .. } | SemanticError::UndeclaredVariable(_, l) | SemanticError::Redeclaration(_, l) => {
                                    if *l == 0 { *l = elif.line; }
                                }
                            }
                        }
                        self.errors.append(&mut e);
                    } else if self.type_of_expr(&elif.cond).ok() != Some(Type::Bool) {
                        self.errors.push(SemanticError::TypeMismatch {
                            expected: "Bool".to_string(),
                            found: format!("{:?}", self.type_of_expr(&elif.cond).ok()),
                            line: elif.line,
                        });
                    }
                    self.enter_scope();
                    for s in &elif.body {
                        self.visit_stmt(s);
                    }
                    self.exit_scope();
                }
                if let Some(else_body) = else_branch {
                    self.enter_scope();
                    for s in else_body {
                        self.visit_stmt(s);
                    }
                    self.exit_scope();
                }
            }
            Statement::WhileStmt { cond, body, line: _line } => {
                if let Err(mut e) = self.type_of_expr(cond) {
                    for err in &mut e {
                        match err {
                            SemanticError::TypeMismatch { line: l, .. } | SemanticError::FunctionArgMismatch { line: l, .. } | SemanticError::FunctionReturnMismatch { line: l, .. } | SemanticError::UndeclaredVariable(_, l) | SemanticError::Redeclaration(_, l) => {
                                if *l == 0 { *l = *_line; }
                            }
                        }
                    }
                    self.errors.append(&mut e);
                } else if self.type_of_expr(cond).ok() != Some(Type::Bool) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: format!("{:?}", self.type_of_expr(cond).ok()),
                        line: *_line,
                    });
                }
                self.enter_scope();
                self.in_loop += 1;
                for s in body {
                    self.visit_stmt(s);
                }
                self.in_loop -= 1;
                self.exit_scope();
            }
            Statement::ForStmt { init, cond, step, body, line: _line } => {
                self.enter_scope();
                self.visit_stmt(init);
                if let Err(mut e) = self.type_of_expr(cond) {
                    for err in &mut e {
                        match err {
                            SemanticError::TypeMismatch { line: l, .. } | SemanticError::FunctionArgMismatch { line: l, .. } | SemanticError::FunctionReturnMismatch { line: l, .. } | SemanticError::UndeclaredVariable(_, l) | SemanticError::Redeclaration(_, l) => {
                                if *l == 0 { *l = *_line; }
                            }
                        }
                    }
                    self.errors.append(&mut e);
                } else if self.type_of_expr(cond).ok() != Some(Type::Bool) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: format!("{:?}", self.type_of_expr(cond).ok()),
                        line: *_line,
                    });
                }
                self.in_loop += 1;
                for s in body {
                    self.visit_stmt(s);
                }
                self.visit_stmt(step);
                self.in_loop -= 1;
                self.exit_scope();
            }
            Statement::Break(line) => {
                if self.in_loop == 0 {
                    self.errors.push(SemanticError::UndeclaredVariable("break outside of loop".to_string(), *line));
                }
            }
            Statement::FnDecl { name, params, return_type, body, line } => {
                // Function signature already added in first pass, so skip redeclaration here
                self.enter_scope();
                for param in params {
                    let param_info = SymbolInfo { ty: param.ty.clone(), line: param.line, kind: SymbolKind::Variable };
                    let _ = self.current_scope_mut().declare(&param.name, param_info);
                }
                let prev_fn = self.current_function.clone();
                self.current_function = Some((name.clone(), return_type.clone()));
                for s in body {
                    self.visit_stmt(s);
                }
                // Check for missing return in non-void functions
                if let Some(ret_ty) = return_type {
                    let mut has_return = false;
                    for stmt in body {
                        if contains_return(stmt) {
                            has_return = true;
                            break;
                        }
                    }
                    if !has_return {
                        self.errors.push(SemanticError::FunctionReturnMismatch {
                            name: name.clone(),
                            expected: format!("{:?}", ret_ty),
                            found: "None".to_string(),
                            line: *line,
                        });
                    }
                }
                self.current_function = prev_fn;
                self.exit_scope();
            }
            Statement::Return(expr, line) => {
                let ret_type = self.type_of_expr(expr);
                if let Some((fn_name, fn_ret_type)) = &self.current_function {
                    match (fn_ret_type, ret_type) {
                        (Some(expected), Ok(found)) if expected != &found => {
                            self.errors.push(SemanticError::FunctionReturnMismatch {
                                name: fn_name.clone(),
                                expected: format!("{:?}", expected),
                                found: format!("{:?}", found),
                                line: *line,
                            });
                        }
                        (_, Err(e)) => self.errors.extend(e.iter().cloned()),
                        _ => {}
                    }
                }
            }
        }
    }
}

// Helper to check if a statement contains a Return
fn contains_return(stmt: &Statement) -> bool {
    match stmt {
        &Statement::Return(_, _) => true,
        &Statement::IfStmt { ref then_branch, ref elif_branches, ref else_branch, .. } => {
            then_branch.iter().any(contains_return)
                || elif_branches.iter().any(|elif| elif.body.iter().any(contains_return))
                || else_branch.as_ref().map_or(false, |b| b.iter().any(contains_return))
        }
        &Statement::WhileStmt { ref body, .. } => body.iter().any(contains_return),
        &Statement::ForStmt { ref body, .. } => body.iter().any(contains_return),
        _ => false,
    }
} 