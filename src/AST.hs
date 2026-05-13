module AST where

data Program =
    Program [Statement]
    deriving Show

data Statement =
    Comment String
    | Print Expr
    | Assignation Expr Expr
    | ExprStmt Expr
    deriving Show

data Expr =
    -- Literals
    IntLit Int
    | FloatLit Double
    | StrLit String
    | Var String
    | BoolLit Bool

    -- Operations
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Fac Expr

    -- Comparisons
    | EqualComp Expr Expr
    deriving Show

data Type =
    IntType
    | FloatType
    | StringType
    | BoolType
    | Null