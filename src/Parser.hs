module Parser where

import Lexer
import Token
import AST

parse :: [Token] -> [Statement]
parse (checkTNewLine:tokens) = case checkTNewLine of
    TNewLine -> parse tokens
    _ -> let parsedTokens = parseLines(splitOnNewLine (checkTNewLine:tokens)) in case parsedTokens of
        [] -> error "Empty source file."
        _ -> parsedTokens
parse [] = error "Empty source file."

parseLines :: [[Token]] -> [Statement]
-- parseLines [] = []
parseLines ([] : xs) = parseLines xs
parseLines (line : xs) = parseLine line : parseLines xs
parseLines _ = []


parseLine :: [Token] -> Statement
parseLine [TComment comment] = Comment comment

parseLine (TPrint:rest) = Print (parseExprFull rest)

parseLine (TIdentifier name : TEqual : valueTok) = Assignation (Var name) (parseExprFull valueTok)

parseLine tokens = ExprStmt (parseExprFull tokens)

parseExprFull :: [Token] -> Expr
parseExprFull tokens =
    case parseExpr tokens of
        (expr, []) -> expr
        (_, rest)  -> error ("Unexpected tokens: " ++ show rest)

parseExpr :: [Token] -> (Expr, [Token])
parseExpr tokens =
    let (left, rest) = parseFactor tokens
    in parseExpr' left rest

parseExpr' :: Expr -> [Token] -> (Expr, [Token])

parseExpr' left (TEqualComp : rest) =
    let (right, rest') = parseExpr rest
    in parseExpr' (EqualComp left right) rest'

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
parseFactor (TSub : rest) =
    let (expr, rest') = parseFactor rest
    in (Sub (zeroOf expr) expr, rest')

parseFactor (TInt x : rest) =
    (IntLit x, rest)

parseFactor (TFloat x : rest) =
    (FloatLit x, rest)

parseFactor (TString s : rest) =
    (StrLit s, rest)

parseFactor (TBool b : rest) =
    (BoolLit b, rest)

parseFactor (TIdentifier name : rest) =
    (Var name, rest)

parseFactor (TLParen : rest) =
    case parseExpr rest of
        (expr, TRParen : rest') -> (expr, rest')
        _ -> error "Missing closing parenthesis."

parseFactor _ = error "Expected expression."


splitOnNewLine :: [Token] -> [[Token]]
splitOnNewLine (TNewLine:xs) = case xs of
    [] -> [[]]
    TNewLine:ys -> [] : splitOnNewLine ys
    _ -> [] : splitOnNewLine xs
splitOnNewLine (x:xs) =
    let lines = splitOnNewLine xs
    in (x : head lines) : tail lines
splitOnNewLine [] = [[]]

zeroOf :: Expr -> Expr
zeroOf (FloatLit _) = FloatLit 0
zeroOf (IntLit _) = IntLit 0
zeroOf (BoolLit _) = IntLit 0
zeroOf _ = IntLit 0