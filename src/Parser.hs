module Parser where

import Lexer
import Token
import AST

parse :: [Token] -> [Expr]
parse (checkTNewLine:tokens) = case checkTNewLine of
    TNewLine -> parse tokens
    _ -> let parsedTokens = parseLines(splitOnNewLine (checkTNewLine:tokens)) in case parsedTokens of
        [] -> error "Empty source file."
        _ -> parsedTokens
parse [] = error "Empty source file."

parseLines :: [[Token]] -> [Expr]
parseLines ([] : xs) = parseLines xs
parseLines (line : xs) = parseExprFull line : parseLines xs
parseLines _ = []

parseExprFull :: [Token] -> Expr
parseExprFull (TPrint:rest) = Print (parseExprFull rest)

parseExprFull (TIdentifier name : TEqual : valueTok) = Assign name (parseExprFull valueTok)

parseExprFull tokens =
    case parseExpr tokens of
        (expr, []) -> expr
        (_, rest)  -> error ("Unexpected tokens: " ++ show rest)

parseExpr :: [Token] -> (Expr, [Token])
parseExpr tokens =
    let (left, rest) = parseFactor tokens
    in parseExpr' left rest

parseExpr' :: Expr -> [Token] -> (Expr, [Token])
-- Comparisons
parseExpr' left (TGt : rest) =
    let (right, rest') = parseExpr rest
    in parseExpr' (GreaterThan left right) rest'
parseExpr' left (TGtEq : rest) =
    let (right, rest') = parseExpr rest
    in parseExpr' (GreaterThanEq left right) rest'
parseExpr' left (TLt : rest) =
    let (right, rest') = parseExpr rest
    in parseExpr' (LessThan left right) rest'
parseExpr' left (TLtEq : rest) =
    let (right, rest') = parseExpr rest
    in parseExpr' (LessThanEq left right) rest'
parseExpr' left (TEqualComp : rest) =
    let (right, rest') = parseExpr rest
    in parseExpr' (EqualComp left right) rest'

-- Arithmetic operations
parseExpr' left (TPlus : rest) =
    let (right, rest') = parseFactor rest
    in parseExpr' (Add left right) rest'

parseExpr' left (TSub : rest) =
    let (right, rest') = parseFactor rest
    in parseExpr' (Sub left right) rest'

parseExpr' left (TMul : rest) =
    let (right, rest') = parseFactor rest
    in parseExpr' (Mul left right) rest'

parseExpr' left (TDiv : rest) =
    let (right, rest') = parseFactor rest
    in parseExpr' (Div left right) rest'

parseExpr' left (TFac : rest) =
    parseExpr' (Fac left) rest

parseExpr' left rest =
    (left, rest)


parseFactor :: [Token] -> (Expr, [Token])
parseFactor (TInt x : rest) =
    (IntLit x, rest)

parseFactor (TFloat x : rest) =
    (FloatLit x, rest)

parseFactor (TString s : rest) =
    (StrLit s, rest)

parseFactor (TBool b : rest) =
    (BoolLit b, rest)

parseFactor (TLParen : rest) =
    case parseExpr rest of
        (expr, TRParen : rest') -> (expr, rest')
        _ -> error "Missing closing parenthesis."

-- Sub case of : -Int, -Float, -Bool (without the left part of the substraction)
parseFactor (TSub : rest) =
    let (expr, rest') = parseFactor rest
    in (Sub (zeroOf expr) expr, rest')

parseFactor (TIdentifier name : rest) = (Var name, rest)

parseFactor _ = error "Expected expression."

-- Split the list of Tokens in a List of List of Tokens,
-- each sublist being instructions written on a line
splitOnNewLine :: [Token] -> [[Token]]
splitOnNewLine (TNewLine:xs) = case xs of
    [] -> [[]]
    TNewLine:ys -> [] : splitOnNewLine ys
    _ -> [] : splitOnNewLine xs
splitOnNewLine (x:xs) =
    let lines = splitOnNewLine xs
    in (x : head lines) : tail lines
splitOnNewLine [] = [[]]

-- Return the zero of the corresponding type.
zeroOf :: Expr -> Expr
zeroOf (FloatLit _) = FloatLit 0
zeroOf (IntLit _) = IntLit 0
zeroOf (BoolLit _) = IntLit 0
zeroOf _ = IntLit 0