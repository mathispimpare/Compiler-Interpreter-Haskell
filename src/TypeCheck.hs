module TypeCheck where

import AST
import Parser( zeroOf )

typeCheck :: TypeEnv -> [Expr] -> Either String Bool
typeCheck _ [] = Right True
typeCheck env l = let (instr:rest) = filter isNotComment l in case typeCheckExpr env instr of
    Left err -> Left err
    Right (newEnvType, _) -> case typeCheck newEnvType rest of
        Left err -> Left err
        Right checkedRest -> Right True
    where
        isNotComment (Comment _) = False
        isNotComment _ = True

typeCheckExpr :: TypeEnv -> Expr -> Either String (TypeEnv, Type)
typeCheckExpr env expr = case expr of
    -- Literals
    IntLit _ -> Right (env, IntType)
    FloatLit _ -> Right (env, FloatType)
    BoolLit _ -> Right (env, BoolType)
    StrLit _ -> Right (env, StringType)

    -- Variables and Functions
    Var x -> case searchVarType x env of
        Right exprType -> Right (env, exprType)
        Left err -> Left err

    Assign name expr -> case typeCheckExpr env expr of
        Right (envT, exprType) -> Right ((name, exprType):env, Null)
        Left err -> Left err

    -- Arithmetic operations
    Add a b -> case (typeCheckExpr env a, typeCheckExpr env b) of
        (Right(envTA, typeExprA), Right (envTB, typeExprB)) -> case typeOfAdd typeExprA typeExprB of
            Null -> Left "Type error : invalid addition."
            t -> Right (env, t)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    Sub a b -> case (typeCheckExpr env a, typeCheckExpr env b) of
        (Right(envTA, typeExprA), Right (envTB, typeExprB)) -> case typeOfNum typeExprA typeExprB of
            Null -> Left "Type error : invalid substraction."
            t -> Right (env, t)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    Mul a b -> case (typeCheckExpr env a, typeCheckExpr env b) of
        (Right(envTA, typeExprA), Right (envTB, typeExprB)) -> case typeOfMul typeExprA typeExprB of
            Null -> Left "Type error : invalid multiplication."
            t -> Right (env, t)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    Div a b -> case (typeCheckExpr env a, typeCheckExpr env b) of
        (Right(envTA, typeExprA), Right (envTB, typeExprB)) -> case typeOfDiv typeExprA typeExprB of
            Null -> Left "Type error : invalid division."
            t -> Right (env, t)
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    Fac expr -> case typeCheckExpr env expr of
        Right(envT, IntType) -> Right (env, IntType)
        Right(envT, FloatType) -> Right (env, FloatType)
        Right(envT, StringType) -> Right(env, IntType) -- Every substring possible with theses characters in a list.
        _ -> Left "Invalid factorial expression."
    
    -- Comparisons
    GreaterThan a b -> case typeOfComp env a b of
        Right t -> Right (env, t)
        Left err -> Left (err ++ ">'.")
    GreaterThanEq a b -> case typeOfComp env a b of
        Right t -> Right (env, t)
        Left err -> Left (err ++ ">='.")
    LessThan a b -> case typeOfComp env a b of
        Right t -> Right (env, t)
        Left err -> Left (err ++ "<'.")
    LessThanEq a b -> case typeOfComp env a b of
        Right t -> Right (env, t)
        Left err -> Left (err ++ "<='.")
    EqualComp a b -> case typeOfComp env a b of
        Right t -> Right (env, t)
        Left err -> Left (err ++ "=='.")

    Print expr -> case typeCheckExpr env expr of
        Right (envT, typeExpr) -> Right (env, typeExpr)
        Left err -> Left err

searchVarType :: String -> TypeEnv -> Either String Type
searchVarType x [] = Left ("Undefined variable : " ++ x ++ ".")
searchVarType x ((y, t) : rest)
  | x == y = Right t
  | otherwise = searchVarType x rest

typeOfAdd :: Type -> Type -> Type
typeOfAdd StringType StringType = StringType
typeOfAdd a b = typeOfNum a b

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

typeOfNum :: Type -> Type -> Type
typeOfNum t1 t2 = if (t1 == t2) && t1 /= StringType && t1 /= BoolType -- Reject string - string and process Bool as Int
    then t1
    else case (t1, t2) of
        (StringType, _) -> Null
        (_, StringType) -> Null
        (FloatType, _) -> FloatType
        (_, FloatType) -> FloatType
        (IntType, BoolType) -> IntType
        (BoolType, IntType) -> IntType
        (BoolType, BoolType) -> IntType
        (_, _) -> Null

typeOfComp :: TypeEnv -> Expr -> Expr -> Either String Type
typeOfComp env a b = case (typeCheckExpr env a, typeCheckExpr env b) of
    (Right (env1, t1), Right (env2, t2)) -> if t1 == t2
            then Right t1
            else case (t1, t2) of
                (IntType, FloatType) -> Right BoolType
                (FloatType, IntType) -> Right BoolType
                (_, _) -> Left "Invalid comparison '"
    (_, _) -> Left "Invalid comparison '"