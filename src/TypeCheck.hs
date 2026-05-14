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
typeOf (_, t) | t /= Null = t
typeOf (expr, Null) =
  case expr of
    IntLit _ -> IntType
    FloatLit _ -> FloatType
    StrLit _ -> StringType
    BoolLit _ -> BoolType

    Add a b -> typeOfAdd (typeOf (a, Null)) (typeOf (b, Null))
    Sub a b -> typeOfNum (typeOf (a, Null)) (typeOf (b, Null))
    Mul a b -> typeOfMul (typeOf (a, Null)) (typeOf (b, Null))
    Div a b -> typeOfDiv (typeOf (a, Null)) (typeOf (b, Null))

    EqualComp _ _ -> BoolType
    Var _ -> Null
    Fac a -> typeOf (a, Null)

typeOfAdd :: Type -> Type -> Type
typeOfAdd StringType StringType = StringType
typeOfAdd a b = typeOfNum a b

typeOfNum :: Type -> Type -> Type
typeOfNum FloatType _ = FloatType
typeOfNum _ FloatType = FloatType
typeOfNum IntType IntType = IntType
typeOfNum BoolType BoolType = BoolType
typeOfNum IntType BoolType = IntType
typeOfNum BoolType IntType = IntType
typeOfNum _ _ = Null

typeOfMul :: Type -> Type -> Type
typeOfMul StringType IntType = StringType
typeOfMul IntType StringType = StringType
typeOfMul a b = typeOfNum a b

typeOfDiv :: Type -> Type -> Type
typeOfDiv StringType _ = Null
typeOfDiv _ StringType = Null
typeOfDiv Null _ = Null
typeOfDiv _ Null = Null
typeOfDiv _ _ = FloatType