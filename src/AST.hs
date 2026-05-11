module AST where

data Expr =
    IntLit Int
    | StrLit String
    | Var String

    | Print Expr

    | Add Expr Expr
    | Sub Expr Expr
    | Times Expr Expr
    | Div Expr Expr
    deriving Show