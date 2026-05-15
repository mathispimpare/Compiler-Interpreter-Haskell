module AST where

data Program =
    Program [Expr]
    deriving Show

type Env = [(String, Expr)]
type TypeEnv = [(String, Type)]

data Val =
    VInt Int
    | VFloat Double
    | VString String
    | VBool Bool
    | VFun String Expr Env
    deriving (Show)

data Expr =
    -- Literals
    IntLit Int
    | FloatLit Double
    | StrLit String
    | BoolLit Bool

    | Comment String
    | Print Expr
    | Var String
    | Assign String Expr
    | App Expr Expr
    | Fun String Expr Env
    | Lam String Type Expr

    -- Operations
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Fac Expr

    -- Comparisons
    | If Expr Expr Expr
    | EqualComp Expr Expr
    | GreaterThan Expr Expr
    | GreaterThanEq Expr Expr
    | LessThan Expr Expr
    | LessThanEq Expr Expr
    deriving Show

data Type =
    IntType
    | FloatType
    | StringType
    | BoolType
    | FunType Type Type
    | Null
    deriving (Show, Eq)