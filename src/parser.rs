use crate::lexer::Token;
use crate::ast::{Statement, Expr, BinOp, UnOp, Type, Param};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            if self.check(&Token::Eof) {
                break;
            }
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Some(Token::Fn) => self.parse_fn_decl(),
            Some(Token::If) => self.parse_if_stmt(),
            Some(Token::While) => self.parse_while_stmt(),
            Some(Token::Return) => self.parse_return_stmt(),
            Some(Token::Identifier(_)) => {
                // Could be variable declaration or assignment
                if self.is_variable_decl() {
                    self.parse_variable_decl()
                } else {
                    self.parse_assignment_or_expr_stmt()
                }
            }
            _ => self.parse_expr_stmt(),
        }
    }

    fn is_variable_decl(&self) -> bool {
        // Look ahead: identifier [':' type] '='
        if let Some(Token::Identifier(_)) = self.peek() {
            if self.peek_n(1) == Some(&Token::Colon) {
                // x: type = ...
                if self.peek_n(3) == Some(&Token::Eq) {
                    return true;
                }
            } else if self.peek_n(1) == Some(&Token::Eq) {
                // x = ...
                return true;
            }
        }
        false
    }

    fn parse_variable_decl(&mut self) -> Result<Statement, String> {
        let name = self.expect_identifier()?;
        let ty = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(Token::Eq)?;
        let value = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::LetDecl { name, ty, value })
    }

    fn parse_assignment_or_expr_stmt(&mut self) -> Result<Statement, String> {
        // Could be assignment or expr_stmt
        if self.peek_n(1) == Some(&Token::Eq) {
            // Assignment
            let name = self.expect_identifier()?;
            self.expect(Token::Eq)?;
            let value = self.parse_expression()?;
            self.expect(Token::Semicolon)?;
            Ok(Statement::Assignment { name, value })
        } else {
            self.parse_expr_stmt()
        }
    }

    fn parse_fn_decl(&mut self) -> Result<Statement, String> {
        self.expect(Token::Fn)?;
        let name = self.expect_identifier()?;
        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let param_name = self.expect_identifier()?;
                let param_ty = if self.check(&Token::Colon) {
                    self.advance();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                params.push(Param { name: param_name, ty: param_ty });
                if self.check(&Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(Token::RParen)?;
        let body = self.parse_block()?;
        Ok(Statement::FnDecl { name, params, body })
    }

    fn parse_if_stmt(&mut self) -> Result<Statement, String> {
        self.expect(Token::If)?;
        self.expect(Token::LParen)?;
        let cond = self.parse_expression()?;
        self.expect(Token::RParen)?;
        let then_branch = self.parse_block()?;
        let mut elif_branches = Vec::new();
        while self.check(&Token::Elif) {
            self.advance();
            self.expect(Token::LParen)?;
            let elif_cond = self.parse_expression()?;
            self.expect(Token::RParen)?;
            let elif_body = self.parse_block()?;
            elif_branches.push(crate::ast::ElifBranch { cond: elif_cond, body: elif_body });
        }
        let else_branch = if self.check(&Token::Else) {
            self.advance();
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Statement::IfStmt { cond, then_branch, elif_branches, else_branch })
    }

    fn parse_while_stmt(&mut self) -> Result<Statement, String> {
        self.expect(Token::While)?;
        self.expect(Token::LParen)?;
        let cond = self.parse_expression()?;
        self.expect(Token::RParen)?;
        let body = self.parse_block()?;
        Ok(Statement::WhileStmt { cond, body })
    }

    fn parse_return_stmt(&mut self) -> Result<Statement, String> {
        self.expect(Token::Return)?;
        let expr = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::Return(expr))
    }

    fn parse_expr_stmt(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Statement::ExprStmt(expr))
    }

    fn parse_block(&mut self) -> Result<Vec<Statement>, String> {
        self.expect(Token::LBrace)?;
        let mut statements = Vec::new();
        while !self.check(&Token::RBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        self.expect(Token::RBrace)?;
        Ok(statements)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        match self.peek() {
            Some(Token::Identifier(s)) if s == "int" => { self.advance(); Ok(Type::Int) },
            Some(Token::Identifier(s)) if s == "float" => { self.advance(); Ok(Type::Float) },
            Some(Token::Identifier(s)) if s == "bool" => { self.advance(); Ok(Type::Bool) },
            Some(Token::Identifier(s)) if s == "str" => { self.advance(); Ok(Type::Str) },
            _ => Err("Expected type".to_string()),
        }
    }

    // --- Expression Parsing ---
    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_logic_or()
    }

    fn parse_logic_or(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_logic_and()?;
        while self.check(&Token::Or) {
            self.advance();
            let right = self.parse_logic_and()?;
            expr = Expr::BinaryOp(Box::new(expr), BinOp::Or, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_logic_and(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_equality()?;
        while self.check(&Token::And) {
            self.advance();
            let right = self.parse_equality()?;
            expr = Expr::BinaryOp(Box::new(expr), BinOp::And, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_comparison()?;
        while self.check(&Token::EqEq) || self.check(&Token::NotEq) {
            let op = if self.check(&Token::EqEq) {
                self.advance(); BinOp::Eq
            } else {
                self.advance(); BinOp::NotEq
            };
            let right = self.parse_comparison()?;
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_term()?;
        while self.check(&Token::Lt) || self.check(&Token::Gt) || self.check(&Token::Le) || self.check(&Token::Ge) {
            let op = match self.peek() {
                Some(Token::Lt) => { self.advance(); BinOp::Lt },
                Some(Token::Gt) => { self.advance(); BinOp::Gt },
                Some(Token::Le) => { self.advance(); BinOp::Le },
                Some(Token::Ge) => { self.advance(); BinOp::Ge },
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_factor()?;
        while self.check(&Token::Plus) || self.check(&Token::Minus) {
            let op = if self.check(&Token::Plus) {
                self.advance(); BinOp::Add
            } else {
                self.advance(); BinOp::Sub
            };
            let right = self.parse_factor()?;
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_unary()?;
        while self.check(&Token::Star) || self.check(&Token::Slash) {
            let op = if self.check(&Token::Star) {
                self.advance(); BinOp::Mul
            } else {
                self.advance(); BinOp::Div
            };
            let right = self.parse_unary()?;
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        if self.check(&Token::Minus) {
            self.advance();
            let expr = self.parse_unary()?;
            Ok(Expr::UnaryOp(UnOp::Neg, Box::new(expr)))
        } else if self.check(&Token::Not) {
            self.advance();
            let expr = self.parse_unary()?;
            Ok(Expr::UnaryOp(UnOp::Not, Box::new(expr)))
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;
        while self.check(&Token::LParen) {
            self.advance();
            let mut args = Vec::new();
            if !self.check(&Token::RParen) {
                loop {
                    args.push(self.parse_expression()?);
                    if self.check(&Token::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            self.expect(Token::RParen)?;
            match expr {
                Expr::Identifier(name) => {
                    expr = Expr::Call(name, args);
                }
                _ => return Err("Can only call functions by identifier".to_string()),
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token::IntLiteral(n)) => { let n = *n; self.advance(); Ok(Expr::IntLiteral(n)) },
            Some(Token::FloatLiteral(f)) => { let f = *f; self.advance(); Ok(Expr::FloatLiteral(f)) },
            Some(Token::BoolLiteral(b)) => { let b = *b; self.advance(); Ok(Expr::BoolLiteral(b)) },
            Some(Token::StrLiteral(s)) => { let s = s.clone(); self.advance(); Ok(Expr::StrLiteral(s)) },
            Some(Token::Identifier(s)) => { let s = s.clone(); self.advance(); Ok(Expr::Identifier(s)) },
            Some(Token::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err("Expected expression".to_string()),
        }
    }

    // --- Utility functions ---
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
    fn peek_n(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n)
    }
    fn check(&self, token: &Token) -> bool {
        self.peek() == Some(token)
    }
    fn advance(&mut self) {
        self.pos += 1;
    }
    fn expect(&mut self, token: Token) -> Result<(), String> {
        if self.check(&token) {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, found {:?}", token, self.peek()))
        }
    }
    fn expect_identifier(&mut self) -> Result<String, String> {
        match self.peek() {
            Some(Token::Identifier(s)) => { let s = s.clone(); self.advance(); Ok(s) },
            _ => Err("Expected identifier".to_string()),
        }
    }
    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len() || self.tokens[self.pos] == Token::Eof
    }
} 