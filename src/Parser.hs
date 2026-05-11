module Parser where

import Token
import AST

parse :: [Token] -> [Expr]
parse tokens = map parseLine(splitOnNewLine tokens)

parseLine :: [Token] -> Expr
parseLine [TPlus, TInt b] = Add (IntLit 0) (IntLit b)
parseLine [TInt a, TPlus, TInt b] = Add (IntLit a) (IntLit b)
parseLine [TMinus, TInt b] = Sub (IntLit 0) (IntLit b)
parseLine [TInt a, TMinus, TInt b] = Sub (IntLit a) (IntLit b)

parseLine [TPrint, TString string] = Print (StrLit string)
parseLine [TPrint, TIdentifier string] = Print (Var string)

splitOnNewLine :: [Token] -> [[Token]]
splitOnNewLine [] = [[]]
splitOnNewLine (TNewLine:xs) = [] : splitOnNewLine xs
splitOnNewLine (x:xs) =
    let lines = splitOnNewLine xs
    in (x : head lines) : tail lines