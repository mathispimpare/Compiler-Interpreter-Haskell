module TypeCheck where

import AST
import Parser( zeroOf )

typeCheck :: [Statement] -> Either String [Statement]
typeCheck [] = Right []
typeCheck (stmt:rest) = case typeCheckStatement stmt of
    Left err -> Left err
    Right checkedStmt -> case typeCheck rest of
        Left err -> Left err
        Right checkedRest -> Right (checkedStmt:checkedRest)

typeCheckStatement :: Statement -> Either String Statement
typeCheckStatement stmt = case stmt of
    Comment _ -> Right stmt
    Print expr -> case typeCheckExpr expr of
        Right _ -> Right stmt
        Left err -> Left err
    Assignation _ expr -> case typeCheckExpr expr of
        Right _ -> Right stmt
        Left err -> Left err
    ExprStmt expr -> case typeCheckExpr expr of
        Right _ -> Right stmt
        Left err -> Left err

typeCheckExpr :: Expr -> Either String Expr
typeCheckExpr expr = case expr of
    IntLit n -> Right (IntLit n)
    FloatLit n -> Right (FloatLit n)
    BoolLit n -> Right (BoolLit n)

    Add a b -> case (typeCheckExpr a, typeCheckExpr b) of
        (Right checkedA, Right checkedB) ->
            if canOp (Add checkedA checkedB) (typeOf (checkedA, Null)) (typeOf (checkedB, Null))
                then Right (Add checkedA checkedB)
            else Left "Type error : invalid addition"
        (Left err, _) -> Left err
        (_, Left err) -> Left err

    Sub a b -> case (typeCheckExpr a, typeCheckExpr b) of
        (Right checkedA, Right checkedB) ->
            if canOp (Sub checkedA checkedB) (typeOf (checkedA, Null)) (typeOf (checkedB, Null))
                then Right (Sub checkedA checkedB)
            else Left "Type error : invalid substraction"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    Mul a b -> case (typeCheckExpr a, typeCheckExpr b) of
        (Right checkedA, Right checkedB) ->
            if canOp (Mul checkedA checkedB) (typeOf (checkedA, Null)) (typeOf (checkedB, Null))
                then Right (Mul checkedA checkedB)
            else Left "Type error : invalid multiplication"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    Div a b -> case (typeCheckExpr a, typeCheckExpr b) of
        (Right checkedA, Right checkedB) ->
            if canOp (Div checkedA checkedB) (typeOf (checkedA, Null)) (typeOf (checkedB, Null))
                then Right (Div checkedA checkedB)
            else Left "Type error : invalid division"
        (Left err, _) -> Left err
        (_, Left err) -> Left err

    -- Var a -> Right (Var a)

canOp :: Expr -> Type -> Type -> Bool
canOp x t1 t2 = case (x, t1, t2) of
    (Add _ _, y, z) -> case (y, z) of
        (StringType, StringType) -> True
        (_, _) -> validateNumOp y z
    (Sub _ _, y, z) -> validateNumOp y z
    (Mul _ _, y, z) -> validateNumOp y z
    (Div _ denom, y, z) -> case denom of
        IntLit 0 -> False
        FloatLit 0 -> False
        BoolLit False -> False
        _ -> validateNumOp y z

validateNumOp :: Type -> Type -> Bool
validateNumOp a b = case (a, b) of
        (IntType, IntType) -> True
        (IntType, FloatType) -> True
        (IntType, BoolType) -> True

        (FloatType, FloatType) ->True
        (FloatType, IntType) -> True
        (FloatType, BoolType) -> True

        (BoolType, BoolType) -> True
        (BoolType, IntType) -> True
        (BoolType, FloatType) -> True
        _ -> False

typeOf :: (Expr, Type) -> Type
typeOf (expr, Null) = case expr of
    (Div _ _) -> FloatType
    (Add _ (FloatLit _)) -> FloatType
    (Add (FloatLit _) _) -> FloatType
    (Sub _ (FloatLit _)) -> FloatType
    (Sub (FloatLit _) _) -> FloatType
    (Mul _ (FloatLit _)) -> FloatType
    (Mul (FloatLit _) _) -> FloatType
    _ -> IntType
typeOf (expr, t) = t