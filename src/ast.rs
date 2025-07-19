#[derive(Debug, Clone, PartialEq)]
pub struct ElifBranch {
    pub cond: Expr,
    pub body: Vec<Statement>,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetDecl { name: String, ty: Option<Type>, value: Expr, line: usize },
    Assignment { name: String, value: Expr, line: usize },
    FnDecl { name: String, params: Vec<Param>, return_type: Option<Type>, body: Vec<Statement>, line: usize },
    IfStmt {
        cond: Expr,
        then_branch: Vec<Statement>,
        elif_branches: Vec<ElifBranch>,
        else_branch: Option<Vec<Statement>>,
        line: usize,
    },
    WhileStmt { cond: Expr, body: Vec<Statement>, line: usize },
    ForStmt {
        init: Box<Statement>,
        cond: Expr,
        step: Box<Statement>,
        body: Vec<Statement>,
        line: usize,
    },
    Return(Expr, usize),
    ExprStmt(Expr, usize),
    Break(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinaryOp(Box<Expr>, BinOp, Box<Expr>, usize),
    UnaryOp(UnOp, Box<Expr>, usize),
    Call(String, Vec<Expr>, usize),
    Identifier(String, usize),
    IntLiteral(i64, usize),
    FloatLiteral(f64, usize),
    BoolLiteral(bool, usize),
    StrLiteral(String, usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Eq, NotEq, Lt, Gt, Le, Ge, And, Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Neg, Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Option<Type>,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
} 