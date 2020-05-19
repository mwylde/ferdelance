#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum MonOp {
    Add1,
    Sub1,
    IsNum,
    IsBool,
    IsTuple,
    Neg,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
    Less,
    Greater,
    Equal,
    Mod,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Number(i64),
    Bool(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Prim1(MonOp, Box<Expr>),
    Prim2(BinOp, Box<Expr>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    Tuple(Vec<Expr>),
    GetItem(Box<Expr>, Box<Expr>),
    SetItem(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Lambda(Vec<String>, Box<Expr>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Decl {
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub init: Expr,
}
