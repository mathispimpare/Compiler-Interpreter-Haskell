module Lexer where

import Token
import Data.Char

lexer :: String -> [Token]
lexer [] = []

lexer ('+':xs) = TPlus : lexer xs
lexer ('-':xs) = TMinus : lexer xs
lexer ('*':xs) = TTimes : lexer xs
lexer ('/':xs) = TDiv : lexer xs
lexer ('\n':xs) = TNewLine : lexer xs
lexer ('"':xs) = let (str, rest) = span(/= '"') xs in case rest of
    '"':after -> TString str : lexer after
    _ -> error "String non fermée"
lexer ('(':xs) = TLParen : lexer xs
lexer (')':xs) = TRParen : lexer xs
lexer (x:xs)
  | isSpace x = lexer xs
  | isDigit x =
      let (n, rest) = span isDigit (x:xs)
      in TInt (read n) : lexer rest
  | isAlpha x =
      let (word, rest) = span isAlphaNum (x:xs)
      in keywordOrIdentifier word : lexer rest

keywordOrIdentifier :: String -> Token
keywordOrIdentifier "print" = TPrint
keywordOrIdentifier name = TIdentifier name