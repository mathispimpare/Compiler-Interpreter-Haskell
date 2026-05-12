module TypeCheck where

import AST
import Parser( zeroOf )

typeCheck :: [Statement] -> Either String [Statement]
typeCheck (x:xs) = case x of
    Comment string -> case typeCheck xs of
        Right rest -> Right (Comment string : rest)
        Left err -> Left err
    Print expr -> case typeCheckExpr expr of
        Right rest -> case typeCheck xs of
            Right rest' -> Right (Print expr : rest')
            Left err -> Left err
        Left err -> Left err
    Assignation var expr -> case typeCheckExpr expr of
        Right rest -> case typeCheck xs of
            Right rest' -> Right (Assignation var expr : rest')
            Left err -> Left err
        Left err -> Left err
    ExprStmt expr -> case typeCheckExpr expr of
        Right rest -> case typeCheck xs of
            Right rest' -> Right (ExprStmt expr : rest')
            Left err -> Left err
        Left err -> Left err

typeCheckExpr :: Expr -> Either String Expr
typeCheckExpr expr = case expr of
    Add a b -> case (typeCheckExpr a, typeCheckExpr b) of
        (Right checkedA, Right checkedB) ->
            if canOp (Add checkedA checkedB)
                then Right (Add checkedA checkedB)
            else Left "Type error : invalid addition"
        (Left err, _) -> Left err
        (_, Left err) -> Left err

    Sub a b -> case (typeCheckExpr a, typeCheckExpr b) of
        (Right checkedA, Right checkedB) ->
            if canOp (Sub checkedA checkedB)
                then Right (Sub checkedA checkedB)
            else Left "Type error : invalid substraction"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    Mul a b -> case (typeCheckExpr a, typeCheckExpr b) of
        (Right checkedA, Right checkedB) ->
            if canOp (Mul checkedA checkedB)
                then Right (Mul checkedA checkedB)
            else Left "Type error : invalid multiplication"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    Div a b -> case (typeCheckExpr a, typeCheckExpr b) of
        (Right checkedA, Right checkedB) ->
            if canOp (Div checkedA checkedB)
                then Right (Div checkedA checkedB)
            else Left "Type error : invalid division"
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    
    IntLit n -> Right (IntLit n)

    Var a -> Right (Var a)

canOp :: Expr -> Bool
canOp x = case x of
    Add y z -> case (y, z) of
        (StrLit _, StrLit _) -> True
        (y, z) -> validateNumOp y z
    Sub y z -> validateNumOp y z
    Mul y z -> validateNumOp y z
    Div y z -> case (y, z) of
        (_, IntLit 0) -> False
        (_, FloatLit 0) -> False
        (_, BoolLit False) -> False
        (y, z) -> validateNumOp y z

validateNumOp :: Expr -> Expr -> Bool
validateNumOp a b = case (a, b) of
        (IntLit _, IntLit _) -> True
        (IntLit _, FloatLit _) -> True
        (IntLit _, BoolLit _) -> True

        (FloatLit _, FloatLit _) ->True
        (FloatLit _, IntLit _) -> True
        (FloatLit _, BoolLit _) -> True

        (BoolLit _, BoolLit _) -> True
        (BoolLit _, IntLit _) -> True
        (BoolLit _, FloatLit _) -> True
        _ -> False