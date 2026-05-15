module Lexer where

import Token
import Data.Char

lexer :: String -> [Token]
lexer [] = []

lexer ('@':xs) = case xs of
    '@':after -> let (a, as) = commentEnd after in
        lexer as -- Ignore comments (no need to put them in the Tokens list)

lexer ('>':xs) = case xs of
    '=':after -> TGtEq : lexer after
    _ -> TGt : lexer xs
lexer ('<':xs) = case xs of
    '=':after -> TLtEq : lexer after
    _ -> TLt : lexer xs
lexer ('=':xs) = case xs of
    '=':after -> TEqualComp : lexer after
    _ -> TEqual : lexer xs
lexer ('+':xs) = TPlus : lexer xs
lexer ('-':xs) = TSub : lexer xs
lexer ('*':xs) = TMul : lexer xs
lexer ('/':xs) = TDiv : lexer xs
lexer ('!':xs) = TFac : lexer xs

lexer ('"':xs) = let (str, rest) = span(/= '"') xs in case rest of
    '"':after -> TString str : lexer after
    _ -> error "String not closed"
lexer ('(':xs) = TLParen : lexer xs
lexer (')':xs) = TRParen : lexer xs
lexer (',':xs) = TComma : lexer xs

lexer ('\n':xs) = TNewLine : lexer xs

lexer (x:xs)
  | isSpace x = lexer xs
  | isDigit x =
      let (beforeDot, rest) = span isDigit (x:xs)
      in case rest of
        '.':y:ys | isDigit y ->
            let (afterDot, rest) = span isDigit (y:ys)
            in TFloat (read (beforeDot ++ "." ++ afterDot)) : lexer rest
        _ -> TInt (read beforeDot) : lexer rest
  | isAlpha x =
      let (word, rest) = span isAlphaNum (x:xs)
      in wordIdentifier word : lexer rest

wordIdentifier :: String -> Token
wordIdentifier "if" = TIf
-- Commands
wordIdentifier "print" = TPrint
-- Bool
wordIdentifier "true" = TBool True
wordIdentifier "false" = TBool False
-- Identifiers (variables)
wordIdentifier name = TIdentifier name

commentEnd :: String -> (String, String)
commentEnd ('\n':xs) = ("", xs)
commentEnd (x:xs) = let (a, b) = commentEnd xs in
    (x:a, b)