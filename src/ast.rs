#[derive(Debug, Clone, PartialEq)]
pub struct ElifBranch {
    pub cond: Expr,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetDecl { name: String, ty: Option<Type>, value: Expr },
    Assignment { name: String, value: Expr },
    FnDecl { name: String, params: Vec<Param>, body: Vec<Statement> },
    IfStmt {
        cond: Expr,
        then_branch: Vec<Statement>,
        elif_branches: Vec<ElifBranch>,
        else_branch: Option<Vec<Statement>>,
    },
    WhileStmt { cond: Expr, body: Vec<Statement> },
    Return(Expr),
    ExprStmt(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    UnaryOp(UnOp, Box<Expr>),
    Call(String, Vec<Expr>),
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StrLiteral(String),
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
} 